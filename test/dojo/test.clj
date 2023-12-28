(ns dojo.test
  (:require [zen.core :as zen]
            [dojo.core]
            [matcho.core :as matcho]))

(defn mk-ztx []
  (let [ztx (zen/new-context)]
    (zen/read-ns ztx 'dojo)
    (zen/read-ns ztx 'demo)
    (zen/errors ztx)
    (zen/start-system ztx 'demo/test-system)
    ztx))



(defonce ztx (mk-ztx))

(comment
  (zen/stop-system ztx)
  (def ztx (mk-ztx))
  )


(defn call-op [op params]
  (zen/op-call ztx op params))

(defn save [res]
  (dojo.core/save ztx res))

(defmacro match-op [op params patt]
  `(let [res# (zen/op-call ztx ~op ~params)]
     (matcho/match res# ~patt)
     res#))


(defn sql [q]
  (:result (call-op 'pg/query {:params {:sql (if (string? q) [q] q)}})))

(defn sql-first [q]
  (first (sql q)))

(defn sql-val [q]
  (first (vals (sql-first q))))

(defn read-ns [ns]
  (zen/read-ns ztx ns))

(defn truncate [& [q]]
  (->> (call-op
        'xtdb/query
        {:params {:query (or q '{:find [?e] :where [[?e :xt/id ?id]]})}})
       :result
       (mapv (fn [[id]] (println id) (call-op 'xtdb/evict {:params {:id id}})))))

(comment
  (call-op 'pg/query {:params {:sql ["select 1"]}})




  )


