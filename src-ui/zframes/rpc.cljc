(ns zframes.rpc
  (:require [re-frame.db :as db]
            [cognitect.transit :as t]
            [re-frame.core :as rf]))

(defonce debounce-state (atom {}))
(defonce abort-state (atom {}))

(defn to-transit [x]
  (let [ w (t/writer :json)]
    (t/write w x)))

(defn from-transit [x]
  (let [r (t/reader :json)]
    (t/read r x)))

(defn to-json [x]
  #?(:cljs (js/JSON.stringify (clj->js x))))


(defn from-json [x]
  #?(:cljs (js->clj (js/JSON.parse x) :keywordize-keys true)))


(defn abort-ctrl []
  #?(:cljs (js/AbortController.)))

(defn fetch [opts cb]
  #?(:cljs
     (-> (js/fetch (str "/")
                   (clj->js {:method "post", :headers {"accept" "application/json", "Content-Type" "application/transit+json"},
                             :body (to-transit (select-keys opts [:method :params :id])), :signal (:signal opts)} :keyword-fn (fn [x] (subs (str x) 1))))
         (.then
          (fn [resp]
            (.then (.text resp)
                   (fn [doc]
                     (let [cdoc (from-transit doc)]
                       (if-let [res  (and (< (.-status resp) 299) (:result cdoc))]
                         (cb {:result res})
                         (cb {:error (or (:error cdoc) cdoc)})))))))
         (.catch (fn [err] (cb {:error err}))))))

(defn *rpc-fetch [{:keys [path debounce force success error silent-alerts transport] :as opts}]
  #?(:cljs
     (if (and debounce path (not force))
       (do
         (when-let [t (get @debounce-state path)]
           (js/clearTimeout t))
         (swap! debounce-state assoc path (js/setTimeout #(*rpc-fetch (assoc opts :force true)) debounce)))
       (let [db db/app-db
             dispatch-event (fn [event payload]
                              (when (:event event)
                                (rf/dispatch [(:event event) (merge event {:request opts} payload)])))]

        (when path
          (swap! db assoc-in (conj path :loading) true)
          (swap! db assoc-in (conj path :status) :loading))

        (-> (js/fetch (str (aget js/window "api-base-url") "/")
                      (clj->js {:method "post", :mode "cors",
                                :headers {"accept" "application/json", "Cache-Control" "no-cache",
                                          "Content-Type" (case (some-> transport name)
                                                           "json"    "application/json"
                                                           "transit" "application/transit+json"
                                                           "application/transit+json")},
                                :cache "no-store",
                                :body (let [payload (select-keys opts [:method :params :id])]
                                        (case (some-> transport name)
                                          "json"    (to-json payload)
                                          "transit" (to-transit payload)
                                          (to-transit payload)))}
                               :keyword-fn (fn [x] (subs (str x) 1))))
            (.then
             (fn [resp]
               (.then (.text resp)
                      (fn [doc]
                        (let [cdoc (case (some-> transport name)
                                     "json"    (from-json doc)
                                     "transit" (from-transit doc)
                                     (from-transit doc))]
                          (if-let [res  (and (< (.-status resp) 299) (or (:result cdoc) (get cdoc "result")))]
                            (do (swap! db update-in path merge {:status :ok :loading false :data res})
                                (when success (dispatch-event success {:response resp :data res})))
                            (do
                              (swap! db update-in path merge {:status :error :loading false :error (or (:error cdoc) cdoc)})
                              (when-not silent-alerts
                                (rf/dispatch [:alerts/push-message {:type :error
                                                                    :message [:<>
                                                                              [:span [:strong "Status: "] (.-status resp)]
                                                                              [:br]
                                                                              [:span [:strong "RPC: "] (:method opts)]
                                                                              [:br]
                                                                              [:span [:strong "Body: "] cdoc]]}]))
                              (when error
                                (dispatch-event error {:response resp :data (or (:error cdoc) cdoc)})))))))))
            (.catch (fn [err]
                      (.error js/console err)
                      (swap! db update-in path merge {:status :error :loading false :error {:err err}})
                      (when-not silent-alerts
                        (rf/dispatch [:alerts/push-message {:type :error
                                                            :message [:<>
                                                                      [:span [:strong "RPC: "] (:method opts)]
                                                                      [:br]
                                                                      [:span [:strong "Error: "] (js->clj err :keywordize-keys true)]]}]))
                      (when error
                        (dispatch-event error {:error err})))))))))

(defn rpc-fetch [opts]
  (when opts
    (if (vector? opts)
      (doseq [o opts] (when (some? o) (*rpc-fetch o)))
      (*rpc-fetch opts))))

(rf/reg-fx :rpc rpc-fetch)

(defn stop-poll [{id :poll-id}]
  (when-let [abort (:abort (get @abort-state id))]
    #?(:cljs (.abort abort) :clj nil)
    (swap! abort-state dissoc id)))

(defn start-poll [{id :poll-id :as opts}]
  (stop-poll opts)
  (let [abort (abort-ctrl)]
    (fetch (-> opts
               (assoc :signal #?(:cljs (.-signal abort) :clj nil))
               (assoc-in [:params :long-polling] true))
           (fn [{err :error res :result}]
             (when-let [suc (and res (:success opts))]
               (rf/dispatch [(:event suc) (assoc suc :data res)]))
             (when (:txid res)
               (start-poll (assoc-in opts [:params :txid] (:txid res))))
             (when (and err #?(:cljs (not (= (.-name err) "AbortError")) :clj true))
               #?(:cljs (js/setTimeout #(start-poll opts) 1000)))))
    (swap! abort-state assoc id {:abort abort :txid (get-in opts [:params :txid])})))


(rf/reg-fx :rpc-start-poll start-poll)
(rf/reg-fx :rpc-stop-poll stop-poll)
