(ns simple-reader.feedreader
  (:require [cljs.core :as cljs]
            [cljs.nodejs :as node]
            [cljs.core.async :refer [chan <! >!] :as a]
            [clojure.string :as str]
            [simple-reader.helpers :as h]
            )
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(defn maybe-decompress [res encoding feed log]
  (let [zlib      (node/require "zlib")
        encoding  (str encoding)
        decomp    (cond
                    (re-find #"\bdeflate\b" encoding) (do (log :info (pr-str "feed-parser: using deflate" feed)) (.createInflate zlib))
                    (re-find #"\bgzip\b" encoding)    (do (log :info (pr-str "feed-parser: using gzip" feed)) (.createGunzip zlib))
                    :else nil)]
    (if decomp
      (.pipe res decomp)
      res)))

(defn maybe-translate [res charset-raw feed log]
  (let [charset (str/replace (str charset-raw) #"(?i).*charset=(.*);?\s*$" "$1")]
    (if (or (re-find #"(?i)utf-*8" charset)
            (re-find #"(?i)xml" charset)) ;; at least for now.
      res
      (try
        (let [iconv (node/require "iconv")
              iconv (.Iconv iconv charset "utf-8")]
          (.pipe res iconv))
        (catch js/Object e (do (log :warning (pr-str "feed-parser:" feed "iconv error" e))
                               (log :warning (pr-str "feed-parser: iconv with:" (str charset-raw) "->" charset))
                               res))))))

;; function maybeTranslate (res, charset) {
;;   var iconv;
;;   // Use iconv if its not utf8 already.
;;   if (!iconv && charset && !/utf-*8/i.test(charset)) {
;;     try {
;;       iconv = new Iconv(charset, 'utf-8');
;;       console.log('Converting from charset %s to utf-8', charset);
;;       iconv.on('error', done);
;;       // If we're using iconv, stream will be the output of iconv
;;       // otherwise it will remain the output of request
;;       res = res.pipe(iconv);
;;     } catch(err) {
;;       res.emit('error', err);
;;     }
;;   }
;;   return res;
;; }


;; function getParams(str) {
;;   var params = str.split(';').reduce(function (params, param) {
;;     var parts = param.split('=').map(function (part) { return part.trim(); });
;;     if (parts.length === 2) {
;;       params[parts[0]] = parts[1];
;;     }
;;     return params;
;;   }, {});
;;   return params;
;; }


(defn extract-article [article-in]
  "read articles one by one, extract things we want"
  (let [hum-date (node/require "human-date")
        article  (h/to-clj article-in)
        result   (select-keys article [:title :author :description :link :guid])]
    (merge result {:pretty-date   (.prettyPrint hum-date (:pubdate article))
                   :date          (:pubdate article)})))

(defn read [feed]
  "Will read each value from the given feed address and them to the returned channel"
  (let [articles        (chan)
        feed-md         (chan)
        logs            (chan)
        log             #(go (>! logs {:level %1 :log %2 :system :feed-reader}))
        close-all!      #(go (a/close! articles)
                             (a/close! logs)
                             (a/close! feed-md))
        log-and-close!  (fn [level msg]
                          (go (when msg 
                                (<! (log level msg)))
                              (close-all!)))
        req             ((node/require "request") feed (cljs/clj->js {:timeout 50000 :pool false}))
        fp              (node/require "feedparser")
        fp              (new fp)]
    (.setMaxListeners req 50)
    (.setHeader req "user-agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36")
    (.setHeader req "accept" "text/html,application/xhtml+xml")
    (.on req "error" #(log-and-close! :error (pr-str "feed-reader: error requesting" feed %)))
    (.on req "response" (fn [result]
                          (if (not= 200 (.-statusCode result))
                            (log-and-close! :error (pr-str "feed-reader: HTTP: request: bad status code:" (.-statusCode result) "on:" feed))
                            (let [headers (h/to-clj (.-headers result))
                                  encoding (:content-encoding headers)
                                  charset (:content-type headers)
                                  res (maybe-decompress result encoding feed log) ;;FIXME feed only for debug.
                                  res (maybe-translate res charset feed log)]
                              (.pipe res fp)))))
    (.on fp "meta" #(go (>! feed-md (h/to-clj %))))
    (.on fp "readable" #(this-as this
                                 (go-loop [post (.read this)]
                                          (when post
                                            (do (>! articles (extract-article post))
                                                (recur (.read this)))))))
    (.on fp "end" #(close-all!))
    (.on fp "error" #(log-and-close! :error (pr-str "feed-reader: feed parser error:" feed %)))
    [feed-md articles logs]))
