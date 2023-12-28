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
   [morse.polling :as p])
  (:import
   [java.security MessageDigest]
   [java.io FileInputStream])
  (:gen-class))

(def api-key
  (or (System/getenv "OPENAIKEY")
      ))

(def telegram-token (or (System/getenv "TELEGRAM_BOT_TOKEN")
                        ))


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

(defn docref-summary [doc-text]
  (->
   (completition {:model "gpt-3.5-turbo-1106"
                  :response_format { :type "json_object" }
                  :messages [{:role "system"
                              :content "You are expert, who helps to extract structured information from clinical documents"}
                             {:role "user"
                              :content (str "Here is a content of clinical document: " doc-text)}
                             {:role "user"
                              :content "Extract json object with date of document as 'date' attribute in ISO format, type of document in English in about 2-5 words as 'title' attribute, and short text summary in English without patient name as 'summary' attribute"}]})
   (get-in [:choices 0 :message :content])
   (cheshire.core/parse-string keyword)))


(defn process-pdf [file]
  (println "check is file PDF")
  (let [p (.getPath file)]
    (when (str/ends-with? (str/lower-case file) ".pdf")
      (println "process file")
      (let [t (text/extract p)
            r (docref-summary t)]
        (println "processed")
        (println r)
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
                :summary (get-in x [:text :summary])
                :file (get-in x [:content 0 :attachment :url])
                :display (get-title x)}))
        (sort-by :date)
        (reverse))})

(defmethod zen/op
  'dojo/resource
  [ztx _cfg {params :params} & [_session]]
  {:result (first (simple-query ztx {:find '[(pull ?e [*])] :where [['?e :xt/id (:id params)]]}))})

(defmethod zen/op
  'dojo/ask-gpt
  [ztx _cfg {params :params} & [_session]]
  (println :ask-gpt params)
  (let [res (first (simple-query ztx {:find '[(pull ?e [*])] :where [['?e :xt/id (:id params)]]}))
        doc-text (get-in res [:text :div])]
    {:result (-> (completition
                  {:model "gpt-3.5-turbo"
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
  (println "Download file")
  (let [file-path (get-file-path file_id)
        resp2 (get-file-bytes file-path)]
    (println "Save file")
    (save-file target-dir file_name resp2)))

(defn process-message [ztx dir message]
  (cond (:document message)
        (let [_ (t/send-text telegram-token (-> message :chat :id) (str "Start processing..."))
              file (download-file dir message)
              pdf-res (process-pdf file)]
          (println :res pdf-res)
          (when pdf-res
            (save ztx pdf-res)
            (t/send-text telegram-token (-> message :chat :id) (str "File processed - " (get-in pdf-res [:type :text])))))
        :else
        (println "Intercepted message:" message)))

(defmethod zen/start 'telegram/bot
  [ztx config]
  (let [dir (or (:dir config) "data/docs")]
    ;; FIXME replace macro with proper funstions
                                        ; This will define bot-api function, which later could be
                                        ; used to start your bot
    (h/defhandler bot-api
                                        ; Each bot has to handle /start and /help commands.
                                        ; This could be done in form of a function:
      (h/command-fn "start" (fn [{{id :id :as chat} :chat}]
                              (println "Bot joined new chat: " chat)
                              (t/send-text telegram-token id "Welcome!")))

                                        ; You can use short syntax for same purposes
                                        ; Destructuring works same way as in function above
      (h/command "help" {{id :id :as chat} :chat}
                 (println "Help was requested in " chat)
                 (t/send-text telegram-token id "Help is on the way"))

                                        ; Handlers will be applied until there are any of those
                                        ; returns non-nil result processing update.

                                        ; Note that sending stuff to the user returns non-nil
                                        ; response from Telegram API.

                                        ; So match-all catch-through case would look something like this:
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

  r
  (save ztx r)

  (doseq [d (test-docs-list)]
    (let [r (process-pdf d)]
      (println (get-in (save ztx r) [:type]))))

  (zen/op-call ztx 'dojo/timeline {:params {}})

  (zen/op-call ztx 'dojo/resource {:params {:rt "DocumentReference", :id "793fb593b27f63601c4ebe927c3ce92c20ea20ef"}})

  (simple-query ztx '{:find [(pull ?e [*])]
                      :where [[?e :xt/id ?id]]})


  ;; test pdfbox 2.0.9
  ;; (text/extract "test-docs/public/22-11-04-Labs.pdf")

  ;; (spit (io/file "extract_test_orin.txt")
  ;;       (text/extract "test-docs/public/22-11-04-Labs.pdf") )

  ;; test pdfbox 3.0
  ;; (import 'org.apache.pdfbox.text.PDFTextStripper)
  ;; (import 'org.apache.pdfbox.pdfparser.PDFParser)
  ;; ;; (import 'org.apache.pdfbox.io.RandomAccessFile)
  ;; (import 'org.apache.pdfbox.io.RandomAccessRead)
  ;; (import 'org.apache.pdfbox.Loader)
  ;;
  ;; (spit (io/file "extract_test_3.0.0.txt")
  ;;       (.getText
  ;;        (PDFTextStripper.)
  ;;        (org.apache.pdfbox.Loader/loadPDF
  ;;         (io/file "test-docs/public/22-11-04-Labs.pdf"))))
  ;;
  ;; result is the same

  ;; aspose - https://docs.aspose.com/pdf/java/extract-text-from-pdf/
  ;; (import 'com.aspose.pdf.Document)
  ;; (import 'com.aspose.pdf.TextAbsorber)

  ;; (let [d (com.aspose.pdf.Document. "test-docs/public/22-11-04-Labs.pdf")
  ;;       txt-abs (com.aspose.pdf.TextAbsorber.)]
  ;;   (.accept (.getPages d) txt-abs)
  ;;   (.getText txt-abs)
  ;;   #_(spit (io/file "extract_test_aspose.txt")
  ;;           (.getText txt-abs)))

  ;; spire pdf
  (import 'com.spire.pdf.PdfDocument);
  (import 'com.spire.pdf.PdfPageBase);
  (import 'com.spire.pdf.texts.PdfTextExtractOptions);
  (import 'com.spire.pdf.texts.PdfTextExtractor);

  (let [doc (com.spire.pdf.PdfDocument.)
        _ (.loadFromFile doc "test-docs/public/22-11-04-Labs.pdf")
        page (.get (.getPages doc) 1)
        textExtractor (PdfTextExtractor. page)
        extractOptions (PdfTextExtractOptions.)
        text (.extract textExtractor extractOptions)]
    (spit (io/file "extract_test_spire.txt")
          text)
    )


  :ok)
