(ns dojo.routes
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [zframes.window-location :as window-location]
            [route-map.core :as route-map]))

(def routes {:. :dojo.timeline/ctx
             "timeline" {:. :dojo.timeline/ctx}
             "fhir" {[:rt] {[:id] {:. :dojo.welcome/ctx}}}})

(defn to-query-params [params]
  (->> params
       (map (fn [[k v]] (str (name k) "=" v)))
       (str/join "&")))

(defn href [& parts]
  (let [params (if (map? (last parts)) (last parts) nil)
        parts (if params (butlast parts) parts)
        url (str "/" (str/join "/" (map (fn [x] (if (keyword? x) (name x) (str x))) parts)))]
    (when-not  (route-map/match [:. url] routes)
      (println (str url " is not matches routes")))
    (str "#" url (when (and params (not (empty? params))) (window-location/gen-query-string params)))))

(defn back [fallback]
  {:href (or @(rf/subscribe [:pop-route]) fallback)})
