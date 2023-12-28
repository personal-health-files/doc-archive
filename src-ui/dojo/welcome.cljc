(ns dojo.welcome
  (:require [zf]
            [dojo.routes :refer [href back]]
            [dojo.pages :as pages]
            [dojo.styles :as styles]
            [stylo.core :refer [c]]))

(zf/defx ctx
  [{db :db} phase params]
  (cond
    (= phase :init)
    {:rpc [{:method 'dojo/resource
            :params params
            :path   [::resource]}]
     :db (assoc db ::resource-id (:id params))}))

(zf/defs resource-sub [db _]
  (:data (::resource db)))

(zf/defx ask-gpt [{db :db} text]
  {:rpc [{:method 'dojo/ask-gpt
          :params {:id (get db ::resource-id)
                   :text text}
          :path   [::gpt-response]}]})

(zf/defv index [resource-sub]
  [:div {:class (c [:p 10])}
   [:a {:href "#/"} "< Back"]
   [:h1 {:class styles/h1} (get-in resource-sub [:type :text])]
   [:div {:class (c :flex [:space-x 2])}
    [:iframe {:class (c [:h 300] {:width "50%"})
              :src (str "static/" (get-in resource-sub [:content 0 :attachment :url]))}]
    [:div {:class (c :flex-1)}
     [:div "Chat"]
     [:textarea {:on-key-down (fn [x]
                                (when (and (= "Enter" (aget x "code")) (aget x "ctrlKey"))
                                  (zf/dispatch [::ask-gpt (.-value (.-target x))])))
                 :class (c :border
                           [:p 4]
                           {:box-sizing "border-box"
                                    :display "block"
                                    :width "100%"
                                    :height "200px"})}]]]])

(pages/reg-page ::ctx index)
