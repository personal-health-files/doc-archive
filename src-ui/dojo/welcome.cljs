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
     :db (-> (assoc db ::resource-id (:id params))
             (dissoc ::gpt-response ::resource))}))

(zf/defs resource-sub [db _]
  (:data (::resource db)))

(zf/defs gpt-resp [db _]
  (::gpt-response db))

(zf/defx ask-gpt [{db :db} text]
  {:rpc [{:method 'dojo/ask-gpt
          :params {:id (get db ::resource-id) :text text}
          :path   [::gpt-response]}]})

(zf/defv gpt-resp-view [gpt-resp]
  [:div {:class (c [:bg :white] [:p 4] :border [:my 2])}
   (if (:loading gpt-resp)
     [:i.fas.fa-spinner.fa-spin]
     [:code (:data gpt-resp)])])

(zf/defv index [resource-sub]
  [:div {:class (c [:px 4] [:py 2])}
   [:a {:href "#/" :class (c [:text :blue-600])} "<< Back"]
   [:h1 {:class styles/h1} [:span (get-in resource-sub [:type :text])]]
   [:div {:class (c :flex [:space-x 2])}
    [:div {:class (c :flex-1)}
     [:iframe {:class (c [:h 300] {:width "100%"})
               :src (str "static/" (get-in resource-sub [:content 0 :attachment :url]))}]
     [:details
      [:summary "Json"]
      [:pre (get-in resource-sub [:text :div])]]]
    [:div {:class (c :flex-1)}
     [:textarea {:on-key-down (fn [x]
                                (when (and (= "Enter" (aget x "code")) (aget x "ctrlKey"))
                                  (zf/dispatch [::ask-gpt (.-value (.-target x))])))
                 :placeholder "Ask GPT press <Ctrl>-<Enter>"
                 :class (c :border
                           [:p 4]
                           {:box-sizing "border-box"
                            :border-radius "5px"
                            :display "block"
                            :width "100%"
                            :height "100px"})}]
     [gpt-resp-view]]]])

(pages/reg-page ::ctx index)
