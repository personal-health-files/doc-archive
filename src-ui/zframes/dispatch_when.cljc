(ns zframes.dispatch-when
  (:require [re-frame.core :as rf]))

(defn dispatch-when [{:keys [event sub]}]
  #?(:clj (let [s (rf/subscribe sub)]
            (while (not @s)
              (Thread/sleep 20))
            (rf/dispatch event))
     :cljs (let [s (rf/subscribe sub)]
             (if @s
               (rf/dispatch event)
               (add-watch s (keyword (gensym))
                          (fn [k _ref _old-state new-state]
                            (when new-state
                              (rf/dispatch event)
                              (remove-watch s k))))))))

(rf/reg-fx :dispatch-when dispatch-when)
