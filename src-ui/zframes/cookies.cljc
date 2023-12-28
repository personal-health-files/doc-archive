(ns zframes.cookies
  (:refer-clojure :exclude [get set!])
  (:require #?(:cljs [goog.net.cookies])
            #?(:cljs [cljs.reader :as reader])
            [re-frame.core :as rf]))



(defn get-cookie
  "Returns the cookie after parsing it with cljs.reader/read-string."
  [k]
  #?(:cljs (reader/read-string (or (.get goog.net.cookies (name k)) "nil"))
     :clj  "nil"))

(defn set-cookie
  "Stores the cookie value using pr-str."
  [k v]
  #?(:cljs (.set goog.net.cookies (name k) (pr-str v))
     :clj  (pr-str v)))

(defn remove! [k]
  #?(:cljs (.remove goog.net.cookies (name k))
     :clj  k))

(defn watch-expires [k]
  ;; todo
  )

(rf/reg-cofx
 ::get
 (fn [coeffects key]
   (assoc-in coeffects [:cookie key] (get-cookie key))))

(rf/reg-cofx
 :get-cookie
 (fn [coeffects key]
   (assoc-in coeffects [:cookie key] (get-cookie key))))

(rf/reg-cofx
 :get-cookies
 (fn [coeffects keys]
   (assoc coeffects
          :cookies
          (->> keys
               (reduce (fn [acc k]
                         (if-let [v (get-cookie k)]
                           (assoc acc k v)
                           acc))
                       {})))))

(rf/reg-fx
 ::set
 (fn [{k :key v :value :as opts}]
   (set-cookie k v)))

(rf/reg-fx
 :cookie/set
 (fn [{k :key v :value :as opts}]
   (set-cookie k v)))

(rf/reg-fx
 ::remove
 (fn [k] (remove! k)))

(rf/reg-fx
 :cookie/remove
 (fn [k]
   (println :remove k)
   (remove! k)))
