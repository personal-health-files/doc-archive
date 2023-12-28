(ns dojo.core
  (:require
   [http.core]
   [db.core]
   [zen.core :as zen]
   [clojure.string :as str]
   [pdfboxing.text :as text]
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [org.httpkit.client :as http])
  (:import
   [java.security MessageDigest]
   [java.io FileInputStream])
  (:gen-class))


(defn sha1
  "Compute the SHA-1 of a File's contents and return the hex string"
  [file]
  (with-open [f (FileInputStream. file)]
    (let [buffer (byte-array 1024)
          md (MessageDigest/getInstance  "SHA-1") ]
      (loop [nread (.read f buffer)]
        (if (pos? nread) 
          (do (.update md buffer 0 nread)
              (recur (.read f buffer)))
          (format "%040x" (BigInteger. 1 (.digest md)) 16))))))

(defn now []
  (java.util.Date.))

(defmethod zen/op
  'dojo/ping
  [ztx _cfg _req & [_session]]
  {:result "Hello"})

(defn save [ztx data]
  (zen/op-call ztx 'xtdb/put {:params (assoc data :xt/id (or (:xt/id data) (:id data)))}))


(defn query [ztx q & args]
  (zen/op-call ztx 'xtdb/query {:params {:query q :args args}}))


(defn simple-query [ztx q & args]
  (mapv first (:result (zen/op-call ztx 'xtdb/query {:params {:query q :args args}}))))

(defn simple-query-first [ztx q & args]
  (first (apply simple-query ztx q args)))


(def api-key
  (or (System/getenv "OPENAIKEY")
      ))

(defn completition [req]
  (let [resp (http/request
             {:url "https://api.openai.com/v1/chat/completions"
              :method :post
              :headers {"Content-Type" "application/json"
                        "Authorization" (str "Bearer " api-key)}
              :timeout 50000
              :body (json/generate-string req)})]
    (-> @resp
        :body
        (cheshire.core/parse-string keyword))))

(defn docref-summary [doc-text]
  (->
   (completition {:model "gpt-3.5-turbo-1106"
                  :response_format { :type "json_object" }
                  :messages [{:role "system"
                              :content "You are expert, who helps to extract structured information from clinical documents"}
                             {:role "user"
                              :content (str "Here is a content of clinical document: " doc-text)}
                             {:role "user"
                              :content "Extract json object with date of document as 'date' attribute in ISO format, type of document in English in about 2-5 words as 'title' attribute, and short text summary in English as 'summary' attribute"}]})
   (get-in [:choices 0 :message :content])
   (cheshire.core/parse-string keyword)))


(defn load-test-documents []
  (->>
   (for [f (file-seq (io/file "test-docs/public"))]
     (let [p (.getPath f)]
       (when (str/ends-with? f ".pdf")
         (let [t (text/extract p)
               r (docref-summary t)]
           (println r)
           (-> r
               (assoc :file (.getName f) :sha (sha1 f)))))))
   (filterv identity)))

(defn read [ztx {id :id :as data}]
  (if-not id
    {:error {:message (str "id is required")}}
    (let [{res :result} (query ztx '{:find [(pull ?e [*])] :where [[?e :xt/id id]] :in [id]} id)]
      {:result (ffirst res)})))

(defn patch [ztx rt {id :id :as data}]
  (if-not id
    {:error {:message (str "id is required")}}
    (let [{res :result :as resp} (read ztx {:id id})]
      (if res
        (do
          ;; (println :patch rt res data)
          (save ztx rt (merge res data)))
        resp))))

(defmulti get-title (fn [x] (:resourceType x)))

(defmethod get-title "DocumentReference"
  [x]
  (or (get-in x [:type :text]) (get-in x [:type :coding 0 :display])))

(defmethod get-title "Observation"
  [x]
  (or (get-in x [:code :text]) (get-in x [:code :coding 0 :display])))

(defmethod zen/op
  'dojo/timeline
  [ztx _cfg {params :params} & [_session]]
  {:result
   (->> (simple-query ztx '{:find [(pull ?e [*])]
                        :where [[?e :resourceType ?rt]]})
        (map (fn [x]
               {:id (:id x)
                :type (:resourceType x)
                :date (when-let [d (or (:effectiveDateTime x) (get-in x [:period :start]))]
                        (first (str/split d #"T" 2)))
                :summary (get-in x [:text :div])
                :file (get-in x [:content 0 :attachment :url])
                :display (get-title x)}))
        (sort-by :date)
        (reverse))})



(comment

  (def ztx (zen/new-context {}))

  (zen/read-ns ztx 'demo)
  (zen/start-system ztx 'demo/system)

  (zen/stop-system ztx)

  (doseq [d (load-test-documents)]
    (println d)
    (save ztx
     {:id (:sha d)
      :resourceType "DocumentReference"
      :period {:start (:date d)}
      :type {:text (:title d) :coding []}
      :text {:div (:summary d)}
      :content [{:attachment {:contentType "pdf" :url (:file d)}}]}))

  (zen/op-call ztx 'dojo/timeline {:params {}})

  :ok)
