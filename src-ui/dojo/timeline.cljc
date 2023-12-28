(ns dojo.timeline
  (:require [zf]
            [dojo.utils :refer [date-fmt]]
            [dojo.routes :refer [href back]]
            [dojo.pages :as pages]
            [clojure.string :as str]
            [dojo.styles :as styles]
            [stylo.core :refer [c]]))

(zf/defs timeline [db _]
  (:data (::timeline db)))

(zf/defx ctx
  [{db :db} phase params]
  (cond
    (= phase :init)
    {:rpc [{:method 'dojo/timeline
            :params {}
            :path   [::timeline]}]}))

(def icons
  {"DocumentReference" [:i.fa-regular.fa-file]
   "Observation" [:i.fa-solid.fa-temperature-half]})

(zf/defv grid [timeline]
  (let [items timeline]
    [:div {:class (c :border)}
     (for [it items]
       [:details
        [:summary {:key (:id it) :class (c [:px 4] [:py 2] [:bg :white]
                                           :cursor-pointer
                                           [:hover [:bg :blue-100]]
                                           :border-b :flex :items-top [:space-x 2])
                   :target "_blank"}
         [:div {:class (c :text-xs [:text :gray-600] [:w-min 20])} (:date it)]
         [:div
          [:div {:class (c :flex :items-baseline [:space-x 2])}
           (if-let [i (get icons (:type it))]
             i
             [:div (:type it)])
           [:div {:class (c {:white-space "nowrap"})}(:display it)]]
          [:div {:class (c :text-xs [:text :gray-800])}
           (:summary it)]
          [:a {:href (href "fhir" (:type it) (:id it))} "->>"]]]
        [:iframe {:src (str "/static/" (:file it)) :class (c {:width "100%" :height "600px"})}]]
       )]))

(zf/defv search []
  [:div {:class (c :flex [:space-x 4] :items-baseline [:my 2] :border-b [:py 1])}
   [:input {:class (c [:px 2] [:py 1] :border :rounded :block :flex-1 :text-m)
            :on-change (fn [ev] (zf/dispatch [::do-search (-> ev .-target .-value)]))}]])


(zf/defs doc-types [db _]
  (->> (get-in db [::timeline :data])
       (reduce (fn [acc {d :display}]
                 (update acc d (fn [x] (inc (or x 0)))))
               {})
       (sort-by first)))

(zf/defv docs-filter [doc-types]
  [:div {:class (c [:px 4] [:py 2])}
   (for [[k v] doc-types]
     [:div {:class (c :flex :text-xs [:px 4] [:py 1] [:space-x 2])}
      [:div {:class (c {:white-space "nowrap"})} (str k)]
      [:b (str v)]])])

;; [search]
(defn index []
  [:div {:class (c :flex :items-top [:space-x 2])}
   [docs-filter]
   [:div {:class (c [:px 4] [:py 2] [:w 260])}
    [grid]]])

(pages/reg-page ::ctx index)
