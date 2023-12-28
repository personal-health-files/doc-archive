(ns dojo.inputs
  (:require [zf]
            [zform]
            [zinputs :refer [input select text button row btns-row]]
            [clojure.string :as str]
            [dojo.routes :refer [href back]]
            [dojo.pages :as pages]
            [dojo.styles :as styles]
            [stylo.core :refer [c]]))

(zf/defx ctx
  [{db :db} phase params]
  {:rpc {:method 'dojo/get-person
         :params {:id "112953399014022942270"}
         :path   [::form]}})


(defn index []
  [:div {:class (c [:p 10])}

   [:div {:class (c [:w 120])}
    [input [::href] [:search] {}]]

   [:h1 {:class styles/h1} "Inputs "]
   [:div {:class (c [:w 120])}
    [row [input [::form] [:name]]]
    [row [input [::form] [:employee-since] ]]
    [row [select [::form] [:role :value]
          {:label "role" :opts ["CTO" "CEO" "Engineer"]}]]
    [row [input [::form] [:telegram]]]
    [row [input [::form] [:phone]]]
    [row [input [::form] [:email]]]
    [row
     [input [::form] [:salary :money]]
     [select [::form] [:salary :curency] {:opts ["$" "€" "₽"]}]]
    [row [select [::form] [:gender] {:opts ["male" "female"]}]]
    [row [text  [::form] [:comment]]]

    [btns-row [button {:label "Validate"}]]]

   [:br]

   [zinputs/debug-form [::form :data]]

   ])

(pages/reg-page ::ctx index)
