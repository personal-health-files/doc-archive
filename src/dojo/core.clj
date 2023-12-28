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
                              :content "Extract json object with date of document as 'date' attribute in ISO format, type of document in English in about 2-5 words as 'title' attribute, and short text summary in English as 'summary' attribute"}]})
   (get-in [:choices 0 :message :content])
   (cheshire.core/parse-string keyword)))


(defn process-pdf [file]
  (println "check is file PDF")
  (let [p (.getPath file)]
    (when (str/ends-with? file ".pdf")
      (println "process file")
      (let [t (text/extract p)
            r (docref-summary t)]
        (println "processed")
        (println r)
        (-> r
            (assoc :file (.getName file) :sha (sha1 file)))))) )

(defn pdf-result->docref
  [{:keys [title date summary file sha]}]
  {:id           sha
   :resourceType "DocumentReference"
   :period       {:start date}
   :type         {:text title :coding []}
   :text         {:div summary}
   :content      [{:attachment {:contentType "pdf" :url file}}]})

(defn load-test-documents []
  (->>
   (for [f (filter #(.isFile %) (file-seq (io/file "test-docs/public"))) ]
     (pdf-result->docref (process-pdf f)))
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


;; --------------------------------- telegram bot ------------------------------------------


(def telegram-token (or (System/getenv "TELEGRAM_BOT_TOKEN")
                        ))

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
      (h/message message

                 ;; === Example of message with file ====
                 ;;
                 ;; {:message_id 108,
                 ;;  :from       {:id            487909300,
                 ;;               :is_bot        false,
                 ;;               :first_name    "",
                 ;;               :username      "NeoSero",
                 ;;               :language_code "ru",
                 ;;               :is_premium    true},
                 ;;  :chat       {:id "487909300",
                 ;;               :first_name "",
                 ;;               :username   "NeoSero",
                 ;;               :type       "private"},
                 ;;  :date       1703764115,
                 ;;  :document   {:file_name      "1.txt",
                 ;;               :mime_type      "text/plain",
                 ;;               :file_id        "BQACAgIAAxkBAANsZY1gk1dcyhJNzjtjdcvEmp1EHoYAAmA_AAIeNGhINCLcp_o4Gd4zBA",
                 ;;               :file_unique_id "AgADYD8AAh40aEg",
                 ;;               :file_size      13}}
                 (def message message)
                 (def dir dir)
                 (cond (:document message)
                       (let [file (download-file dir message)
                             pdf-res (process-pdf file)]
                         (when pdf-res
                           (save ztx (pdf-result->docref pdf-res))
                           (t/send-text telegram-token (-> message :chat :id)
                                        (str "File " (:file pdf-res ) " processed"))
                           ))
                       :else
                       (println "Intercepted message:" message))

                 ))

    (p/start telegram-token bot-api) ;; TODO: MOVE telegram-token to ZEN
    ))


(defmethod zen/stop 'telegram/bot
  [_ztx _config  state]
  (p/stop state))

(comment

  (def ztx (zen/new-context {}))

  (zen/read-ns ztx 'demo)
  (zen/start-system ztx 'demo/system)

  (zen/stop-system ztx)

  (doseq [d (load-test-documents)]
    (println d)
    (save ztx
          {:id           (:sha d)
           :resourceType "DocumentReference"
           :period       {:start (:date d)}
           :type         {:text (:title d) :coding []}
           :text         {:div (:summary d)}
           :content      [{:attachment {:contentType "pdf" :url (:file d)}}]}))

  (zen/op-call ztx 'dojo/timeline {:params {}})


  :ok)
