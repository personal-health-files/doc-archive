(ns dojo.timeline
  (:require [zf]
            [dojo.utils :refer [date-fmt]]
            [dojo.routes :refer [href back]]
            [dojo.pages :as pages]
            [clojure.string :as str]
            [dojo.styles :as styles]
            [clojure.string :as str]
            [stylo.core :refer [c]]))

(zf/defs timeline [db _]
  (let [items (:data (::timeline db))
        search (str/lower-case (or (::search db) ""))]
    (if (str/blank? search)
      items
      (->> items
           (filterv (fn [x] (str/includes? (str/lower-case (or (:display x) "")) search)))))))

(zf/defx ctx
  [{db :db} phase params]
  (cond
    (= phase :init)
    {:rpc [{:method 'dojo/timeline
            :params {}
            :path   [::timeline]}]
     :db (dissoc db ::search)}))

(def icons
  {"DocumentReference" [:i.fa-regular.fa-file]
   "Observation" [:i.fa-solid.fa-temperature-half]})

(zf/defv grid [timeline]
  (let [items timeline]
    [:div {:class (c [:space-y 2])}
     (for [it items]
       [:details {:key (:id it)
                  :class (c  [:bg :white]
                             :border
                             :box-shadow
                             [:hover [:bg :blue-100]]
                             {:border-radius "7px"})}
        [:summary {:class (c [:px 4] [:py 2] :cursor-pointer :flex :items-top [:space-x 2])
                   :target "_blank"}
         [:div {:class (c :text-sm [:text :gray-600] [:w-min 20])} (:date it)]
         [:div {:class (c :flex-1)}
          [:div {:class (c :border-b :flex :items-baseline [:space-x 2])}
           (if-let [i (get icons (:type it))]
             i
             [:div (:type it)])
           [:div {:class (c {:white-space "nowrap" :font-weight "500"})}
            (:display it)]]
          [:div {:class (c :text-sm [:text :gray-700] [:py 2])}
           (:summary it)]]
         [:a {:href (href "fhir" (:type it) (:id it))} [:i.fa-regular.fa-comments]]]
        ;;
        [:iframe {:src (str "/static/" (:file it)) :class (c {:width "100%" :height "600px"})}]])]))

(zf/defx do-search [{db :db} txt]
  {:db (assoc db ::search txt)})

(zf/defv search []
  [:div {:class (c :flex [:space-x 4] :items-baseline [:my 2] :border-b [:py 1])}
   [:input {:class (c [:px 2] [:py 1] :border :rounded :block :flex-1 :text-m)
            :on-input (fn [ev]
                        (zf/dispatch [::do-search (-> ev .-target .-value)]))}]])


(zf/defs doc-types [db _]
  (->> (get-in db [::timeline :data])
       (reduce (fn [acc {d :display}]
                 (update acc d (fn [x] (inc (or x 0)))))
               {})
       (sort-by first)))

(zf/defv docs-filter [doc-types]
  [:div {:class (c [:px 4] [:py 4])}
   (for [[k v] doc-types]
     [:div {:key (str k)
            :class (c :flex :text-xs [:px 4] [:py 1] [:space-x 2])}
      [:div {:class (c {:white-space "nowrap"})} (str k)]
      [:b (str v)]])])

(defn index []
  [:div {:class (c :flex :items-top [:space-x 2])}
   [docs-filter]
   [:div {:class (c [:px 4] [:py 2] [:w 220])}
    [search]
    [grid]]])

(pages/reg-page ::ctx index)
