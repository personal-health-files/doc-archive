(ns zframes.xhr
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [clojure.string :as str]
            [re-frame.db :as db]
            [cognitect.transit :as transit]
            [re-frame.core :as rf]))

(defn sub-query-by-spaces
  [k s] (->> (str/split s #"\s+")
             (mapv (fn [v] (str (name k) "=" v)))
             (str/join "&")))

(defn to-query [params]
  (->> params
       (mapcat (fn [[k v]]
                 (cond
                   (vector? v) (mapv (fn [vv] (str (name k) "=" vv)) v)
                   (set? v) [(str (name k) "=" (str/join "," v))]
                   :else [(str (name k) "=" (js/encodeURIComponent v)) #_(sub-query-by-spaces k v)])))
       (str/join "&")))

(defn base-url [db url]
  (str (get-in db [:config :base-url]) url))

(defn make-form-data [files]
  (let [form-data (js/FormData.)]
    (doall
     (for [[i file] (map-indexed vector files)]
       (.append form-data (str "file" i) file (str "file" i))))
    form-data))

(defonce requests (atom {}))

(defn license-check-alert
  [resp]
  (let [license-mode-info (some->> (.get (.-headers resp) "license-mode-info")
                                   (.parse js/JSON)
                                   ((fn [obj] (js->clj obj :keywordize-keys true))))
        build-msgs (fn [type] (->> license-mode-info
                                   type
                                   (map (fn [[id {msg :message}]]
                                          {:type type
                                           :id id
                                           :ui-state :showing
                                           :message msg}))))]
    (when-let [msgs (seq (concat (build-msgs :error) (build-msgs :warning)))]
      (rf/dispatch [:alerts/push-message msgs]))))


(defn *json-fetch [{:keys [uri format headers is-fetching-path path params success error cache] :as opts}]
  (let [{token :token base-url :base-url}    (get-in @db/app-db [:xhr/config])
        fmt (get {"json" "application/json"
                  "edn"  "application/edn"
                  "yaml" "text/yaml"
                  :specified-in-headers nil}
                 format
                 (or (:default-format opts)
                     "application/json"))

        content-type-not-specified-but-has-body
        (and (not (str/blank? (get headers "accept")))
             (str/blank? (get headers "content-type"))
             (nil? fmt)
             (seq (:body opts)))

        default-content-type (or (:default-format opts)
                                 "application/json")

        headers (cond-> {"authorization" (str "Bearer " token)}
                  fmt                                     (assoc "accept" fmt)
                  (or (nil? token) (str/blank? token))    (dissoc "authorization")
                  (and fmt (nil? (:files opts)))          (assoc "content-type" fmt)
                  content-type-not-specified-but-has-body (assoc "content-type" default-content-type)
                  :always                                 (merge (or headers {})))

        fetch-opts (-> (merge {:method "get" :mode "cors"} opts)
                       (dissoc :uri :headers :success :error :params :files)
                       (assoc :headers headers)
                       (assoc :cache "no-store"))
        abort (when (:key opts) (js/AbortController.))
        fetch-opts (cond-> fetch-opts
                     (:body opts) (assoc :body (if (string? (:body opts)) (:body opts) (.stringify js/JSON (clj->js (:body opts) :keyword-fn (fn [x] (subs (str x) 1))))))
                     (:files opts) (assoc :body (make-form-data (:files opts)))
                     abort (assoc :signal (aget abort "signal")))
        url (str base-url uri)]

    (when is-fetching-path (rf/dispatch [::fetch-start is-fetching-path]))
    (when path (rf/dispatch [::save path {:status "loading"}]))

    ;; inacivity tracking
    (swap! db/app-db assoc :xhr/last-request (.getTime (js/Date.)))

    (when-let [req-key (:key opts)]
      (when-let [prev-abort (get @requests req-key)]
        (.abort prev-abort))
      (swap! requests assoc req-key abort))

    (->
     (js/fetch (str url (when params (str "?" (to-query params)))) (clj->js fetch-opts :keyword-fn (fn [x] (subs (str x) 1))))
     (.then
      (fn [resp]
        (license-check-alert resp)
        (when is-fetching-path (rf/dispatch [::fetch-end is-fetching-path]))
        (when path (rf/dispatch [::save path {:status "done"}]))
        (if (= 304 (.-status resp))
          (rf/dispatch [(or (:event cache) :json/fetch-cache) cache])
          (if (:dont-parse opts)
            (.then (.text resp)
                   (fn [doc]
                     (if (< (.-status resp) 299)
                       (when (:event success)
                         (rf/dispatch [(:event success)
                                       (merge success
                                              {:request opts
                                               :response resp
                                               :data doc})]))
                       (do (rf/dispatch [(or (:event error) :json/fetch-error)
                                         (merge error
                                                {:request opts
                                                 :response resp
                                                 :data doc})])
                           (rf/dispatch [:alerts/push-message {:type :error
                                                               :message [:<>
                                                                         [:span [:strong "Status: "] (.-status resp)]
                                                                         [:br]
                                                                         [:span [:strong "URL: "] (.-url resp)]
                                                                         [:br]
                                                                         [:span [:strong "Body: "] (js->clj doc :keywordize-keys true)]]}]))))

                   ;; No json
                   (fn [doc]
                     (println "Error:" doc)
                     (rf/dispatch
                       [(:event success)
                        (merge success
                               {:request opts
                                :response resp
                                :data doc})])))
            (let [clone (.clone resp)]
              (.then (.json resp)
                     (fn [doc]
                       (let [data (js->clj doc :keywordize-keys true)]
                         (if (< (.-status resp) 299)
                           (do
                             (when path (rf/dispatch [::save path {:data data}]))
                             (when (:event success)
                               (rf/dispatch [(:event success)
                                             (merge success
                                                    {:request opts
                                                     :response resp
                                                     :original-data (.stringify js/JSON doc)
                                                     :data data})])))
                           (do
                             (rf/dispatch (rf/dispatch [::save path {:error data}]))
                             (when (:event error)
                               (rf/dispatch [(:event error)
                                             (merge error
                                                    {:request opts
                                                     :response resp
                                                     :data data})]))
                             (rf/dispatch [:alerts/push-message {:type :error
                                                                 :message [:<>

                                                                           [:span [:strong "Status: "] (.-status resp)]
                                                                           [:br]
                                                                           [:span [:strong "URL: "] (.-url resp)]
                                                                           [:br]
                                                                           [:span [:strong "Body: "] (js->clj doc :keywordize-keys true)]]}])))))
                     ;; No json
                     (fn [doc]
                       (-> (.text clone)
                           (.then (fn [original]
                                    (println "Error:" doc ":(")
                                    (when (:event error)
                                      (rf/dispatch
                                       [(:event error)
                                        (merge success
                                               {:request opts
                                                :response resp
                                                :data doc
                                                :plain original})]))))))))))))
     (.catch (fn [err]
               (.error js/console "Error:" err)
               (when-not (= (aget err "name") "AbortError")
                 (rf/dispatch [(:event error)
                               (merge error
                                      {:request opts
                                       :error err})])))))))


(defn json-fetch [opts]
  (if (vector? opts)
    (doseq [o (remove nil? opts)] (*json-fetch o))
    (*json-fetch opts)))

(rf/reg-fx :json/fetch json-fetch)
(rf/reg-fx ::json-fetch json-fetch)

(rf/reg-event-fx
 :json/fetch
 (fn [{db :db} [_ arg]]
   {:db db
    :json/fetch arg}))

(rf/reg-event-db
 ::save
 (fn [db [_ path data]]
   (update-in db path merge data)))

(rf/reg-event-db
 ::fetch-start
 (fn [db [_ path]]
   (assoc db path true)))

(rf/reg-event-db
 ::fetch-end
 (fn [db [_ path]]
   (assoc db path false)))
