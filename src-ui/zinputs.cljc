(ns zinputs
  (:require [zf]
            [zform]
            #?(:cljs [cljs.reader])
            [clojure.string :as str]
            [stylo.core :refer [c]]))

(defn get-id* [root path & subpath]
  (->> (concat root path subpath)
       (map (fn [kw] (if (number? kw) (keyword (str kw)) kw)))
       (mapv (fn [kw] (cond->> (name kw)
                       (namespace kw) (str (namespace kw) \/))))
       (str/join \.)))

(def get-id (memoize get-id*))

(defn target-value [ev]
  #?(:cljs (.. ev -target -value)
     :clj  (-> ev :target :value)))

;; textarea
(defn input [root path {lbl :label v :value :as opts}]
  (let [value       (zf/subscribe [:f/value root path])
        lbl (or lbl (name (last path)))
        placeholder (:placeholder opts)
        on-change   #(zf/dispatch [:f/set-value root path (target-value %)])]
    (fn [_opts]
      [:label.infield
       [:span  {} (or lbl "?")]
       [:input {:value       (or @value "")
                :type (:type opts)
                :id          (get-id root path)
                :placeholder (or (and placeholder @placeholder) (:placeholder opts))
                :on-change   on-change}]])))

(defn parse-timestamp [v]
  #?(:cljs (cljs.reader/parse-timestamp v)))

(zf/defx update-date [{db :db} root path v]
  {:db (zform/*do-set-value db root path (if v (parse-timestamp v) nil))})

(zf/reg-sub date-value
 (fn [db [_ root path]]
   (when-let [d  (zform/get-value db root path)]
     (first (str/split (.toISOString d) #"T")))))

(defn date [root path opts {lbl :label}]
  (let [value       (zf/subscribe [::date-value root path])
        lbl (or lbl (name (last path)))
        placeholder (:placeholder opts)
        on-change   #(zf/dispatch [::update-date root path (target-value %)])]
    (fn [_opts]
      [:label.infield
       [:span  {} (or lbl "?")]
       [:input {:value       (or @value "")
                :type        "date"
                :id          (get-id opts)
                :placeholder (or (and placeholder @placeholder) (:placeholder opts))
                :on-change   on-change}]])))


;; textarea with auto-resize
(defn text [root path {lbl :label v :value :as opts}]
  (let [value       (zf/subscribe [:f/value root path])
        lbl (or lbl (name (last path)))
        placeholder (:placeholder opts)
        on-change   #(zf/dispatch [:f/set-value root path (target-value %)])]
    (fn [opts]
      [:label.infield
       [:span  {} (or lbl "?")]
       [:div.text
        [:pre {:aria-hidden true} (str @value ".")]
        [:textarea {:value       (or @value "")
                    :id          (get-id opts)
                    :placeholder placeholder
                    :on-change   on-change}]]])))


(zf/reg-sub
 select-options
 (fn [[_ root path] _]
   [(zf/subscribe [:f/state root path :filter])
    (zf/subscribe [:f/state root path :options])])
 (fn [[filter options] _]
   (if (or (nil? filter) (str/blank? filter))
     (:items options)
     (->> (:items options)
          (filterv (fn [x] (str/includes? (str/lower-case (str (:display x) (:value x))) filter)))))))

(zf/reg-sub
 select-value
 (fn [[_ root path] _]
   [(zf/subscribe [:f/value root path])
    (zf/subscribe [:f/state root path :options])])
 (fn [[value options] _]
   (or (get-in options [:idx value])
       value)))

(defn mk-opts [opts]
  {:items opts
   :idx (->> opts (reduce (fn [acc {v :value :as o}] (assoc acc v o)) {}))})

(zf/defx options-loaded
  [{db :db} {data :data root :root path :path}]
  {:dispatch [:f/set-state root path :options (mk-opts data)]})

(zf/defx load-options
  [{db :db} root path rpc]
  {:rpc (assoc rpc :success {:event ::options-loaded :root root :path path})})

(defn render-option [o]
  [:<>
   (when-let [img (:image o)] [:img {:src img :class (c :border [:h 6] {:padding "1px" :border-radius "100%"})}])
   [:div (or (get o :display)
             (get o :value)
             (pr-str o))]])

(defn on-click-outside [id f]
  #?(:cljs
     (let [node (js/document.getElementById id)]
       (js/document.addEventListener
        "click"
        (fn onclck [ev]
          (when-not (.contains node (.-target ev))
            (js/console.log "click")
            (js/document.removeEventListener "click" onclck)
            (f)))))))

(zf/defx open-options
  [{db :db} root path id]
  (on-click-outside id #(zf/dispatch [:f/set-state root path :open false]))
  {:db (zform/set-state db root path :open true)})


(defn select [root path {lbl :label v :value :as opts}]
  (let [id         (get-id root path)
        value       (zf/subscribe [::select-value root path])
        open        (zf/subscribe [:f/state root path :open])
        filter      (zf/subscribe [:f/state root path :filter])
        lbl         (or lbl (name (last path)))
        placeholder (or (:placeholder opts) "Search...")
        on-filter   #(zf/dispatch [:f/set-state root path :filter (target-value %)])
        options      (zf/subscribe [select-options root path])]
    (when-let [o (:opts opts)] (zf/dispatch [:f/set-state root path :options (mk-opts o)]))
    (when-let [rpc (:rpc opts)] (zf/dispatch [::load-options root path rpc]))
    (fn [_opts]
      [:label.infield {:id id}
       [:span  {} (or lbl "?")]
       [:div {:class (c [:px 2] [:py 1])}
        (if-let [v @value]
          [:div {:on-click #(zf/dispatch [::open-options root path id])
                 :class    (c :flex :items-center)}
           [:div {:class (c :flex-1 :flex :items-center [:space-x 2])} (render-option v)]
           [:div {:class (c [:px 4] [:text :gray-600] [:hover [:text :red-600]] :cursor-pointer) :on-click #(zf/dispatch [:f/set-value root path nil])}
            [:i.fa-solid.fa-xmark]]]
          [:div {:class (c :flex :items-center [:text :gray-500])
                 :on-click #(zf/dispatch [::open-options root path id])}
           [:div {:class (c :flex-1)} placeholder]
           [:div {:class (c [:px 4] [:text :gray-600])}
            [:i.fa-solid.fa-chevron-down]]])]
       (when @open
         [:div {:class (c {:position "relative"
                           :margin-left "-6px"
                           :margin-right "-6px"})}
          [:div {:class (c :box-shadow [:bg :gray-100] :w-full
                           {:border-radius "0 0 4px 4px"
                            :border "1px solid black"
                            :margin-bottom "10px"
                            :position "absolute"
                            :box-shadow "rgba(0, 0, 0, 0.16) 0px 1px 4px"
                            :z-index 1000
                            :height "300px"
                            :overflow-y "hidden"})
                 :on-click #(zf/dispatch [:f/set-state root path :open false])}
           [:input {:auto-focus true
                    :on-change  on-filter
                    :class (c [:px 2] [:py 1] :block :w-full
                              [:bg :white]
                              {:border-bottom "1px solid black"
                               :border-radius "0px"
                               :outline "none"})
                    :value @filter}]
           [:div {:class (c {:position "absolute" :left 0 :right 0 :bottom 0 :top "33px" :overflow-y "auto"})}
            (for [o @options]
              [:div {:class (c :cursor-pointer [:px 2] [:py 1] [:hover [:bg :blue-200]] :flex :items-center [:space-x 2])
                     :key (:value o)
                     :on-click #(zf/dispatch [:f/set-value root path (or (:value o) o)])}
               (render-option o)])]]])])))


(defn button [{lbl :label :as opts}]
  [:button (merge opts {:class (c :cursor-pointer
                                  :shadow
                                  [:bg :gray-200]
                                  :border [:hover [:bg :gray-300]] [:px 6] [:py 2]
                                  {:border-radius "4px" :font-weight 600})}) lbl])

(defn row [& els]
  (into [:div {:class (c :flex)}]  els))

(zf/defs data [db [_ path]]
  (get-in db path))

(defn debug-form [path]
  (let [data (zf/subscribe [::data path])]
    (fn []
      [:div  {:class (c [:w 180] [:bg :gray-100] :border [:p 2])}
       (for [[k v] @data]
         [:div {:key k :class (c :flex [:py 1] [:space-x 4])}
          [:b (str k)]
          [:pre (pr-str v)]])])))


(defn btns-row [& els]
  (into [:div {:class (c :flex [:px 4] [:py 2] [:space-x 2] {:border "1px solid #ddd" :margin-left "-1px" :margin-top "-1px"})}
         [:div {:class (c :flex-1)}]] els))

(defn title [txt]
  [:div {:class (c :flex [:px 2] [:py 1] [:space-x 2] :text-xl :bold {:border "1px solid #ddd" :margin-left "-1px" :margin-top "-1px"})}
   txt])

(def style
  [:style "

.infield {
  display: flex;
  flex-direction: column;
  padding: 5px;
  border: 1px solid #ddd;
  margin-left: -1px;
  margin-top: -1px;
  flex: 1;
}

.infield > span {
  color: black;
  opacity: 0.5;
  padding-left: 0.3rem;
  font-size: 12px;
  line-height: 14px;
  font-weight: 500;
  margin-bottom: 3px;
}

.infield > input,
.infield > .text > textarea
 {
  border: none;
  outline: none;
  background: none;
  font-size: 16px;
  padding: 0 0.3rem;
  width: 100%;
}

.infield:focus-within {
  background: rgb(247, 250, 252);
  border: 1px solid black;
  z-index: 1000;
}

.infield:focus-within > span {
  //opacity: 0.8;
}

 .infield > .text {
		 position: relative;
 }

 .infield > .text > pre {
     border: none;
		 box-sizing: border-box;
		 overflow: hidden;
     padding: 0 0.3rem;
     font-family: Arial;
     opacity: 0;
     min-height: 4em;
     font-size: 16px;
     font-weight: bold;
 }

.infield > .text > textarea {
     border: none;
		 position: absolute;
		 width: 100%;
		 height: 100%;
		 top: 0;
     font-family: Arial;
		 resize: none;
		 overflow: hidden;
     font-size: 16px;
 }


"])
