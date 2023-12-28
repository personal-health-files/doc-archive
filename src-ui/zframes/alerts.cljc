(ns zframes.alerts
  (:require [re-frame.core :as rf]
            [zf]
            [utils])
  (:import
   #?(:clj [java.util UUID])))

(zf/defx remove-message
  [{db :db} id]
  {:db (-> db
           (update-in [:alerts :by-id] dissoc id)
           (update-in [:alerts :order]
                      (fn [ids]
                        (filterv #(not= % id) ids))))})

(zf/defx hide-message
  [{db :db} id]
  {:db (assoc-in db [:alerts :by-id id :ui-state] :hiding)})

(zf/defx show-message
  [{db :db} id]
  {:db (assoc-in db [:alerts :by-id id :ui-state] :showing)})

(zf/defx disable-alerts
  [{db :db} & _]
  {:db (assoc-in db [:alerts :ignore] true)})

(zf/defx enable-alerts
  [{db :db} & _]
  {:db (assoc-in db [:alerts :ignore] false)})

(defn gen-uuid []
  #?(:clj (str (UUID/randomUUID))
     :cljs (str (random-uuid))))

(rf/reg-cofx
 :gen-uuid
 (fn [coeffects]
   (assoc coeffects :uuid (gen-uuid))))


(rf/reg-event-fx
 :push-message
 [(rf/inject-cofx :gen-uuid)]
 (fn [cofx [_ msg]]
   (let [id (or (:id msg) (:uuid cofx))
         item (merge {:id id
                      :ui-state :new}
                     msg)]
     (when-not (get-in (:db cofx) [:alerts :ignore])
       {:db (-> (:db cofx)
                (assoc-in [:alerts :by-id id] item)
                (update-in [:alerts :order]
                           (fn [old-ord]
                             (-> (or old-ord [])
                                 (conj id)
                                 distinct
                                 vec))))}))))

(zf/defe :alerts/push-message
  [msg]
  (if (sequential? msg)
    (doseq [m msg]
      (zf/dispatch [:push-message m]))
    (zf/dispatch [:push-message msg])))

(zf/defe :alerts/remove-message
  [{id :id}]
  (zf/dispatch [remove-message id]))

(zf/defe :alerts/hide-message
  [{id :id}]
  (zf/dispatch [hide-message id]))

(zf/defe
  :timeout/set
  [opts]
  #?(:cljs (js/setTimeout
            #(zf/dispatch (:event opts))
            (:timeout opts))))
