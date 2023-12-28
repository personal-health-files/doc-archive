(ns dojo.welcome
  (:require [zf]
            [dojo.routes :refer [href back]]
            [dojo.pages :as pages]
            [dojo.styles :as styles]
            [stylo.core :refer [c]]))

(zf/defx ctx
  [{db :db} phase params]
  (println :load-timeline phase params))

(defn index []
  [:div {:class (c [:p 10])}
   [:h1 {:class styles/h1} "Resource"]])

(pages/reg-page ::ctx index)
