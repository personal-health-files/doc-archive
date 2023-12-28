(ns zform
  (:require [zf :as zf]
            [re-frame.core :as rf]
            [re-frame.registrar]
            [clojure.string :as str]))

(defn find-by-pattern [pattern xs]
  (or (first (keep-indexed (fn [idx x]
                             (when (= (select-keys x (keys pattern)) pattern)
                               idx)) xs))
      (count xs)))

(defn assoc-in*
  [m [k & ks] v]
  (letfn [(assoc* [m k v]
             (cond (and (int? k)
                        (or (nil? m) (vector? m))
                        (>= k (count m)))
                   (assoc (into (or m []) (repeat (- k (count m)) nil)) k v)
                   (and (map? k) (or (vector? m) (nil? m)))
                   (assoc (or m []) (find-by-pattern k m) (merge v k))
                   :else
                   (assoc m k v)))]
    (if ks
      (assoc* m k (assoc-in* (get m k) ks v))
      (assoc* m k v))))

(defn get-in*
  [m [k & ks]]
  (letfn [(get* [m k]
            (if (and (map? k) (or (vector? m) (nil? m)))
              (get m (find-by-pattern k m))
              (get m k)))]
    (if ks
      (get-in* (get* m k) ks)
      (get* m k))))

(defn update-in*
   [m ks f & args]
   (assoc-in* m ks (apply f (get-in* m ks) args)))


;; generate id for testing
(defn get-id** [root path & subpath]
  (->> (concat root path subpath)
       (map (fn [kw] (if (number? kw) (keyword (str kw)) kw)))
       (mapv (fn [kw] (cond->> (name kw) (namespace kw) (str (namespace kw) \/))))
       (str/join \.)))

;; (get-id** [::form] [:name])

;; [input ::search :query]

(def get-id (memoize get-id**))

;; NEW BLOCK
(defn get-value [db root path]
  (get-in* db (concat root [:data] path)))

(rf/reg-sub
 :f/value
 (fn [db [_ root path]]
   (get-value db root path)))

(defn get-error
  [db root path]
  (get-in* db (concat root [:error] path)))

(rf/reg-sub
 :f/error
 (fn [db [_ root path]]
   (get-error db root path)))


(defn get-state [db root path state]
  (get-in* db (concat root [:state] path [state])))

(rf/reg-sub
 :f/state
 (fn [db [_ root path state]]
   (get-state db root path state)))


;; todo validation
(defn *do-set-value
  [db root path val]
  (let [val (if (and val (string? val) (str/blank? val)) nil val)]
    (assoc-in* db (concat root [:data] path) val)))

(rf/reg-event-fx
 :f/set-value
 (fn [{db :db} [_ root path v]]
   (let [db' (-> db
                 (assoc-in* (concat root [:state] path [:dirty?]) true)
                 (*do-set-value root path v))]
     {:db db'})))

(defn set-state [db root path state v]
  (assoc-in* db (concat root [:state] path [state]) v))

(rf/reg-event-fx
 :f/set-state
 (fn [{db :db} [_ root path state v]]
   {:db (set-state db root path state v)}))

(rf/reg-sub
  :f/error
  (fn [db [_ root path]]
    (get-error db root path)))


(zf/defe :dom/select
  [{:keys [id start end]}]
  #?(:cljs
     (when-let [el (js/document.getElementById id)]
       (set! (.-selectionStart el) start)
       (set! (.-selectionEnd el) end))))


(zf/defe :dom/focus
  [id]
  #?(:cljs
     (when-let [el (js/document.getElementById id)]
       (.focus el))))


(zf/defe :dom/blur
  [id]
  #?(:cljs
     (if id
       (some-> (js/document.getElementById id) .blur)
       (some-> js/document.activeElement .blur))))

(zf/defe :dom/scroll-to-bottom
  [id]
  #?(:cljs
     (when-let [element (js/document.getElementById id)]
       (set! (.-scrollTop element) (.-scrollHeight element)))))


(zf/defe :dom/scroll-into
  [id]
  #?(:cljs
     (when-let [el (js/document.getElementById id)]
       (.scrollIntoView el false))))


;; (rf/reg-event-fx
;;  :f/focus
;;  (fn [{db :db} [_ root path]]
;;    (let [event (:on-focus (get-in* db root))]
;;      {:db (-> db
;;               ;; (assoc-in* (state-path opts [:zf/pre-commit-value])
;;               ;;            (get-value db root path))
;;               ;; (assoc-in* (state-path opts [:zf/focused]) true)
;;               )
;;       :dispatch-n [(when event (conj (if (vector? event) event [event]) opts))]})))

;; (rf/reg-event-fx
;;   :f/blur
;;   (fn [{db :db} [_ opts]]
;;     (let [event (:on-blur (get-in* db (:zf/root opts)))]
;;       {:db (-> db
;;                (assoc-in* (state-path opts [:dirty?]) true)
;;                (assoc-in* (state-path opts [:zf/focused]) false))
;;        :dispatch-n [(when event (conj (if (vector? event) event [event]) opts))]})))



