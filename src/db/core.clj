(ns db.core
  (:require
   [zen.core :as zen]
   [clojure.java.io :as io]
   [xtdb.api :as xt]))


(defmethod zen/start 'xtdb/db
  [ztx config]
  (let [dir (or (:dir config) "data/dev")]
    (letfn [(kv-store [dir] {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store :db-dir (io/file dir) :sync? true}})]
      (xt/start-node
       {:xtdb/tx-log (kv-store (str dir "/tx-log"))
        :xtdb/document-store (kv-store (str dir "/doc-store"))
        :xtdb/index-store (kv-store (str dir "/index-store"))}))))


(defmethod zen/stop 'xtdb/db
  [_ztx _config  state]
  (.close state))


(defmethod zen/op
  'xtdb/query
  [ztx _config {params :params} & [_session]]
  (let [db (zen/get-state ztx :xtdb)]
    {:result (apply xt/q (xt/db db) (:query params) (:args params))}))

(defn query [ztx query & args]
  (let [db (zen/get-state ztx :xtdb)]
    (apply xt/q (xt/db db) query args)))

(defn simple-query [ztx query & args]
  (let [db (zen/get-state ztx :xtdb)]
    (->> (apply xt/q (xt/db db) query args)
         (mapv first))))

(defn pull
  [ztx query eid]
  (let [db (xt/db (zen/get-state ztx :xtdb))]
    (xt/pull db query eid)))

(defmethod zen/op
  'xtdb/put
  [ztx _config {params :params} & [_session]]
  (let [db (zen/get-state ztx :xtdb)
        res (merge params (xt/submit-tx db [[::xt/put params]]))]
    (xt/sync db)
    {:result res}))

(defn evict
  [ztx id]
  (let [db (zen/get-state ztx :xtdb)]
    (xt/submit-tx db [[::xt/evict id]])
    (xt/sync db)
    {:result id}))

(defmethod zen/op
  'xtdb/evict
  [ztx _config {params :params} & [_session]]
  (let [db (zen/get-state ztx :xtdb)]
    (xt/submit-tx db [[::xt/evict (:id params)]])
    (xt/sync db)
    {:result params}))


(comment

  (def ztx (zen/new-context))

  (zen/load-ns ztx
   '{ns test-db
     import #{xtdb}

     xtdb
     {:engine   xtdb/db
      :dir "data/dev"}


     system
     {:zen/tags #{zen/system}
      :start [xtdb]}})

  (zen/start-system ztx 'test-db/system)
  (zen/stop-system ztx)


  (zen/op-call ztx 'xtdb/put {:params {:xt/id "p1"      :type :person :person/name "Ivan"}})
  (zen/op-call ztx 'xtdb/put {:params {:xt/id "niquola" :type :person :person/name "Nikolai Ryzhikov"}})

  (zen/op-call ztx 'xtdb/put {:params {:xt/id "validation" :type :team :team/name "Validation"}})
  (zen/op-call ztx 'xtdb/put {:params {:xt/id (java.util.UUID/randomUUID) :type :member :team "validation" :person "p1"}})
  (zen/op-call ztx 'xtdb/put {:params {:xt/id (java.util.UUID/randomUUID) :type :member :team "other team" :person "p1"}})


  (zen/op-call ztx 'xtdb/query
               {:params {:query '{:find [(pull ?e [*])
                                         (pull ?t [*])]
                                  :where [[?e :type :person]
                                          [?m :team ?t]
                                          [?m :person ?e]]}}})

  (zen/op-call ztx 'xtdb/query
               {:params {:query '{:find [(pull ?e [*])]
                                  :where [[?e :xt/id "niquola"]]}}})


  (zen/op-call ztx 'xtdb/query
               {:params {:query '{:find [?n (pull ?e [*])]
                                  :where [[?e :type :person]
                                          [?e :person/name ?n ]]
                                  :order-by [[?n :asc]]}}})



  )
