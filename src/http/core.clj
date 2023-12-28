(ns http.core
  (:require
   [cheshire.core]
   [clojure.string :as str]
   [cognitect.transit :as transit]
   [hiccup.page]
   [org.httpkit.client :as http]
   [org.httpkit.server :as server]
   [ring.middleware.head]
   [ring.middleware.params]
   [ring.middleware.cookies]
   [ring.util.codec :as codec]
   [ring.util.io]
   [ring.util.response]
   [zen.core :as zen])
  (:import [java.io BufferedWriter OutputStreamWriter  ByteArrayInputStream ByteArrayOutputStream]))

(defn handle-static [{meth :request-method uri :uri :as req}]
  (let [opts {:root "public"
              :index-files? true
              :allow-symlinks? true}
        path (subs (codec/url-decode (:uri req)) 8)]
    (-> (ring.util.response/resource-response path opts)
        (ring.middleware.head/head-response req))))

(defn render-ui [ztx req]
  {:status 200
   :body (hiccup.page/html5
          [:head
           [:title "Dojo"]
           [:meta {:charset "utf-8"}]
           [:link {:rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
                   :integrity "sha512-iecdLmaskl7CVkqkXNQ/ZH/XLlvWZOJyj7Yy7tcenmpD1ypASozpmT/E0iPtmFIB46ZmdtAc9eNBvH0H/ZpiBw"
                   :crossorigin "anonymous"
                   :referrerpolicy "no-referrer"}]
           [:link {:href "/static/css/stylo.css", :type "text/css", :rel "stylesheet"}]
           [:style "
body {
 padding: 0 !important;
 margin: 0 !important;
 font-family: Arial !important;
 font-size: 16px !important;
}

input, textarea {
 font-family: Arial !important;
 font-size: 16px !important;
}
"]
           [:script (str "var user = " (cheshire.core/generate-string (:session req)))]]
          [:body
           [:div#app]
           [:script {:src (str "/static/js/ui.js")}]])})

(defn http-rpc [ztx req http-req]
  (try
    (let [res (zen/op-call ztx (:method req) (assoc req :ip (:remote-addr http-req)) {})]
      {:status 200
       :body (ring.util.io/piped-input-stream (fn [out] (transit/write (transit/writer out :json) res)))})
    (catch Exception e
      (println :error e)
      {:status 200
       :body (ring.util.io/piped-input-stream (fn [out] (transit/write (transit/writer out :json) {:error {:message (.getMessage e)}})))})))

(defn parse-transit [b]
  (when b
    (let [r (cond (string? b) (transit/reader (ByteArrayInputStream. (.getBytes ^String b)) :json)
                  (instance? java.io.InputStream b) (transit/reader b :json))]
      (transit/read r))))

(defn dispatch [ztx {meth :request-method uri :uri :as req}]
  (cond
    (and (contains? #{:get :head} meth) (str/starts-with? (or uri "") "/static/"))
    (handle-static req)

    (= :get meth)
    (render-ui ztx req)

    (= :post meth)
    (http-rpc ztx (parse-transit (:body req)) req)))

(defmethod zen/start 'http/api
  [ztx config]
  (let [port (or (:port config) 7777)]
    (println :fervdb/api port)
    (server/run-server (fn [req] (#'dispatch ztx req)) {:port port})))

(defmethod zen/stop 'http/api
  [_ztx _config state]
  (state))
