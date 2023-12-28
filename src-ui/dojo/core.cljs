(ns dojo.core
  (:require [reagent.dom]
            [dojo.pages :as pages]
            [dojo.routes :as routes]
            [dojo.welcome]
            [dojo.timeline]
            [dojo.inputs]
            [re-frame.core :as rf]
            [zframes.routing]
            [zframes.rpc]
            [zframes.redirect]
            [zframes.cookies]
            [dojo.styles :as styles]
            [zf]
            [zinputs]
            [stylo.core :refer [c]]))

(rf/reg-event-fx
 :alerts/push-message
 (fn [fx data]
   (println :message data)))

(zf/defs repos [db _]
  (get-in db [:repos]))

(def tab-c (c :block [:px 4] [:py 3] [:text :gray-600] [:hover [:text :blue-600]]))

(zf/defs current-user [db _]
  (:user db))

(zf/defv user-badge [current-user]
  [:div {:class (c [:px 4] [:py 4])}
   (if (:photo current-user)
     [:img {:src (:photo current-user) :class (c [:w 6] [:h 6] :border)}]
     [:i.fa-solid.fa-user])
   #_(get-in current-user [:name])])

(zf/defx logout [fx _]
  {:cookie/remove :dojo-session
   :page/reload true})

(defn layout [cnt]

  [:div
   zinputs/style
   [:div {:class (c :flex)}
    #_[:div {:class (c :border-r :flex-col :items-start [:px 0] [:py 4] [:bg :gray-300] {:height "100vh"})}
     [:a {:class (c [:py 4] [:px 4] :block :text-lg) :href "#"}
      [:i.fa-solid.fa-address-card]]
     [:a {:class tab-c :href "#timeline"} [:i.fa-solid.fa-clock]]
     [:div {:class (c :flex-1)}]
     ]
    [:div {:class (c :flex-1 [:bg :gray-100])}
     cnt]]])

(zf/defs current-route [db _] (:route-map/current-route db))

(zf/defv current-page [current-route]
  (let [page (:match current-route)
        params (:params current-route)
        cmp (get @pages/pages page)]
    [layout
     [:<>
      (if page
        (if-let [cmp (get @pages/pages page)]
          [cmp params]
          [:div.not-found (str "Page not found [" (str page) "]" )])
        [:div.not-found (str "Routing error") (pr-str current-route)])]]))

(rf/reg-event-fx
 ::initialize
 (fn [{db :db}]
   {:db (assoc db
               :route-map/routes routes/routes
               :user (:user (js->clj (.-user js/window) :keywordize-keys true)))
    :route-map/start {}}))

(defn mount-root []
   (reagent.dom/render [current-page] (.getElementById js/document "app")))

 (defn init! []
   (rf/dispatch [::initialize])
   (mount-root))
