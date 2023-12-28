(ns dojo.utils)

(def monthes
  ["Jan"
   "Feb"
   "Mar"
   "Apr"
   "May"
   "Jun"
   "Jul"
   "Aug"
   "Sep"
   "Oct"
   "Nov"
   "Dec"])

(defn date-fmt [x]
  #?(:cljs
     (when x
       (let [d (js/Date. x)]
         (str
          (.getDate d)
          " "
          (get monthes (.getMonth d)))))))
