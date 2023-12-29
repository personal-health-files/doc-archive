(ns dojo.core
  (:require
   [http.core]
   [db.core]
   [zen.core :as zen]
   [clojure.string :as str]
   [pdfboxing.text :as text]
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [org.httpkit.client :as http]
   [morse.handlers :as h]
   [morse.api :as t]
   [dojo.keys]
   [morse.polling :as p])
  (:import
   [java.security MessageDigest]
   [java.io FileInputStream]
   com.spire.pdf.PdfDocument
   com.spire.pdf.PdfPageBase
   com.spire.pdf.texts.PdfTextExtractOptions
   com.spire.pdf.texts.PdfTextExtractor)
  (:gen-class))

(def api-key dojo.keys/api-key)
(def telegram-token dojo.keys/telegram-token)


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
  (zen/op-call ztx 'xtdb/put {:params (assoc data :xt/id (or (:id data)))}))


(defn query [ztx q & args]
  (zen/op-call ztx 'xtdb/query {:params {:query q :args args}}))


(defn simple-query [ztx q & args]
  (mapv first (:result (zen/op-call ztx 'xtdb/query {:params {:query q :args args}}))))

(defn simple-query-first [ztx q & args]
  (first (apply simple-query ztx q args)))



(defn completition [req]
  (let [resp
        (http/request
         {:url "https://api.openai.com/v1/chat/completions"
          :method :post
          :headers {"Content-Type" "application/json"
                    "Authorization" (str "Bearer " api-key)}
          :timeout 50000
          :body (json/generate-string req)})]
    (-> @resp
        :body
        (cheshire.core/parse-string keyword))))

(def summary-model "gpt-4-1106-preview")
(def summary-model "gpt-3.5-turbo-1106")

(defn docref-summary [doc-text]
  (->
   (completition {:model summary-model 
                  :response_format { :type "json_object" }
                  :messages [{:role "system"
                              :content "You are expert, who helps to extract structured information from clinical documents"}
                             {:role "user"
                              :content (str "Here is a content of clinical document: " doc-text)}
                             {:role "user"
                              :content "Extract json object with date of document as 'date' attribute in ISO format, type of document in English in about 2-3 words as 'title' attribute, and short summary of document in English as 'summary' attribute"}]})
   (get-in [:choices 0 :message :content])
   (cheshire.core/parse-string keyword)))

(comment
  (docref-summary "Ibuprofen 100mg prn")

  )

(defn pdf-text-old [p]
  (text/extract p))


(defn pdf-text [p]
  (try 
    (let [doc (com.spire.pdf.PdfDocument.)
          _ (.loadFromFile doc p)
          extractOptions (PdfTextExtractOptions.)]
      (->> (iterator-seq  (.iterator (.getPages doc)))
           (map-indexed (fn [i page]
                          (println :page i)
                          (let [textExtractor (PdfTextExtractor. page)
                                text (.extract textExtractor extractOptions)]
                            (str/replace text #"Evaluation Warning : The document was created with Spire.PDF for java.\n" ""))))
           (str/join "\n")))
    (catch Exception e
      (println :pdf.spire/error)
      (pdf-text-old p))))

(defn process-pdf [file]
  (println "check is file PDF")
  (let [p (.getPath file)]
    (when (str/ends-with? (str/lower-case file) ".pdf")
      (println "process file")
      (let [t (pdf-text-old p)
            r (docref-summary t)]
        (println "processed")
        {:resourceType "DocumentReference"
         :id           (sha1 file)
         :period       {:start (:date r)}
         :type         {:text (:title r) :coding []}
         :text         {:summary (:summary r) :div t}
         :content      [{:attachment {:contentType "pdf" :url (.getName file)}}]}))))



(defn test-docs-list []
  (filter #(and (str/ends-with? (.getName %) "pdf") (.isFile %)) (file-seq (io/file "test-docs/public"))))


(defmulti get-title (fn [x] (:resourceType x)))

(defmethod get-title "DocumentReference"
  [x]
  (or (get-in x [:type :text]) (get-in x [:type :coding 0 :display])))

(defmulti get-obs-title (fn [x] (get-in x [:code :coding 0 :code])))

(defmethod get-obs-title
  "85354-9"
  [obs]
  (str ": "
       (get-in obs [:component 0 :valueQuantity :value])
       "/"
       (get-in obs [:component 1 :valueQuantity :value])
       " "
       (get-in obs [:component 1 :valueQuantity :unit])))

(defmethod get-title "Observation"
  [x]
  (str 
   (or (get-in x [:code :text]) (get-in x [:code :coding 0 :display]))
   (get-obs-title x)))

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
                :summary (get-in x [:text :summary])
                :file (get-in x [:content 0 :attachment :url])
                :display (get-title x)}))
        (sort-by :date)
        (reverse))})

(defmethod zen/op
  'dojo/resource
  [ztx _cfg {params :params} & [_session]]
  {:result (first (simple-query ztx {:find '[(pull ?e [*])] :where [['?e :xt/id (:id params)]]}))})


(def dialog-model "gpt-4")
(def dialog-model "gpt-3.5-turbo")

(defmethod zen/op
  'dojo/ask-gpt
  [ztx _cfg {params :params} & [_session]]
  (println :ask-gpt params)
  (let [res (first (simple-query ztx {:find '[(pull ?e [*])] :where [['?e :xt/id (:id params)]]}))
        doc-text (get-in res [:text :div])]
    {:result (-> (completition
                  {:model dialog-model
                   :messages [{:role "system"
                               :content "You are expert, who helps to extract information from clinical documents"}
                              {:role "user"
                               :content (str "Here is a content of clinical document: " doc-text)}
                              {:role "user"
                               :content (:text params)}]})
                 (get-in [:choices 0 :message :content]))}))

;; --------------------------------- telegram bot ------------------------------------------



(defn get-file-path [file-id]
  (let [resp @(http/get (str "https://api.telegram.org/bot"
                             telegram-token "/getFile")
                        {:query-params {:file_id file-id}})]
    (when (= (resp :status) 200)
      (-> (json/parse-string (:body resp))
          (get-in ["result" "file_path"])))))


(defn get-file-bytes [file-path]
  (let [resp @(http/get (str "https://api.telegram.org/file/bot"
                             telegram-token "/" file-path))]
    (when (= 200 (:status resp))
      (.readAllBytes (:body resp)))))

(defn check-unique-file [dir file-name]
  (let [nfile (io/file dir file-name)]
    (if-not (.exists nfile)
      nfile
      (let [dot-pos     (str/last-index-of file-name ".")
            fname       (subs file-name 0 dot-pos)
            ext         (subs file-name (inc dot-pos))
            postfix     (str "__" (subs (str (java.util.UUID/randomUUID)) 0 4))
            clean-fname (str/replace-first fname #"__.{4}$" "")
            new-fname   (str clean-fname postfix "." ext)]
        (check-unique-file dir new-fname)))))


;; (check-unique-file "docs" "hello.doc.txt")
;; (check-unique-file "docs" "1.txt")

(defn save-file [dir file-name bytes]
  (let [tgt-file (check-unique-file dir file-name)
        opts (into-array java.nio.file.OpenOption [])]
    (io/make-parents tgt-file)
    (java.nio.file.Files/write (.toPath tgt-file) bytes opts)
    tgt-file))


(defn download-file
  [target-dir {{:keys [file_id file_name]} :document :as msg}]
  (println "Download file" target-dir file_name)
  (let [file-path (get-file-path file_id)
        resp2 (get-file-bytes file-path)]
    (println "Save file")
    (save-file target-dir file_name resp2)))

(defn process-message [ztx dir message]
  (println :chat-id (-> message :chat :id))
  (cond (:document message)
        (let [_ (t/send-text telegram-token (-> message :chat :id) (str "Start processing..."))
              file (download-file dir message)
              pdf-res (process-pdf file)]
          (when pdf-res
            (save ztx pdf-res)
            (t/send-text telegram-token (-> message :chat :id)
                         {:parse_mode "HTML"}
                         (str "File processed - <b>" (get-in pdf-res [:type :text]) "</b> - " (get-in pdf-res [:text :summary])
                              "\n" (str "<a href=\"http://0.0.0.0:5173/#/fhir/DocumentReference/" (:id pdf-res) "\">Document</a>")))))
        :else
        (println "Intercepted message:" message)))

(comment
  (t/send-text telegram-token 165929935 {:parse_mode "HTML"}

               (str "File processed - <b>" "title" "</b> - " "ups"
                    "\n" (str "<a href=\"http://0.0.0.0:5173/#/fhir/DocumentReference/5\">Document</a>")
                    ))

  )

(defmethod zen/start 'telegram/bot
  [ztx config]
  (let [dir "test-docs/public"]
    (h/defhandler bot-api
      (h/command-fn "start" (fn [{{id :id :as chat} :chat}]
                              (println "Bot joined new chat: " chat)
                              (t/send-text telegram-token id "Welcome!")))
      (h/command "help" {{id :id :as chat} :chat}
                 (println "Help was requested in " chat)
                 (t/send-text telegram-token id "Help is on the way"))
      (h/message message (#'process-message ztx dir message)))

    (p/start telegram-token bot-api) ;; TODO: MOVE telegram-token to ZEN
    ))


(defmethod zen/stop 'telegram/bot
  [_ztx _config  state]
  (p/stop state))

(comment


  (def ztx (zen/new-context {}))

  (zen/read-ns ztx 'demo)
  (zen/read-ns ztx 'dojo)

  (zen/start-system ztx 'demo/system)

  (zen/stop-system ztx)

  (second (test-docs-list))

  (def r (process-pdf (second (test-docs-list))))

  (save ztx r)

  (doseq [d (test-docs-list)]
    (println d)
    (let [r (process-pdf d)]
      (save ztx r)))

  (save ztx {:resourceType "Observation" :id "..."})

  (zen/op-call ztx 'dojo/timeline {:params {}})

  (zen/op-call ztx 'dojo/resource {:params {:rt "DocumentReference", :id "793fb593b27f63601c4ebe927c3ce92c20ea20ef"}})

  (simple-query ztx '{:find [(pull ?e [*])]
                      :where [[?e :xt/id ?id]]})

  ;; spire pdf

  (-> (pdf-text "test-docs/public/22-11-04-Labs.pdf")
      (str/split #"\n"))

  (pdf-text "test-docs/public/22-03-15-URINE-ALL.pdf")
  (-> 
   (pdf-text "test-docs/public/23-08-09-endoscopy.pdf")
   (str/split #"\n"))

  (->
   (pdf-text-old "test-docs/public/23-08-09-endoscopy.pdf")
   (str/split #"\n"))

  (db.core/evict ztx "5a5852ab55c16025ad654af1bc933ec26b796dec")

  (def bp {:resourceType "Observation",
         :id (str (random-uuid))
         :effectiveDateTime "2023-12-27",
         :category [{:coding [{:system "http://terminology.hl7.org/CodeSystem/observation-category", :code "vital-signs", :display "Vital Signs"}]}],
         :component
         [{:code {:coding [{:system "http://loinc.org", :code "8480-6", :display "Systolic blood pressure"}]},
           :valueQuantity {:value 107,
                           :unit "mmHg",
                           :system "http://unitsofmeasure.org",
                           :code "mm[Hg]"},}
          {:code
           {:coding [{:system "http://loinc.org", :code "8462-4", :display "Diastolic blood pressure"}]},
           :valueQuantity {:value 60,
                           :unit "mmHg",
                           :system "http://unitsofmeasure.org",
                           :code "mm[Hg]"},
           }],
         :status "final",
         :code {:coding [{:system "http://loinc.org", :code "85354-9", :display "Blood pressure panel with all children optional"}],
                :text "Blood pressure systolic & diastolic"},
         :bodySite {:coding [{:system "http://snomed.info/sct", :code "368209003", :display "Right arm"}]},
         :subject {:reference "Patient/example"},})

  (save ztx bp)

  (get-title bp)

 
  :ok)
