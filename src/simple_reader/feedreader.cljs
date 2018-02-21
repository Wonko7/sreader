(ns simple-reader.feedreader
  (:require [cljs.core :as cljs]
            [cljs.nodejs :as node]
            [cljs.core.async :refer [chan <! >!] :as a]
            [clojure.string :as str]
            [simple-reader.helpers :as h])
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
  (let [hum-date  (node/require "human-date")
        article   (h/to-clj article-in)
        media     (get-in article [:media:content (keyword "@")]) ;; media is wrapped in a :@ that seems useless to me
        result    (select-keys article [:title :author :description :link :guid])]
    (merge result {:pretty-date   (.prettyPrint hum-date (:pubdate article))
                   :date          (:pubdate article)
                   :media:content media})))

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
        axios           (node/require "axios")
        fp              (node/require "feedparser")
        fp              (new fp)]

    (-> axios (.request (cljs/clj->js {:url feed :timeout 60000 :responseType :stream}))
        (.then  #(-> % .-data (.pipe fp)))
        (.catch #(log-and-close! :error (print-str "error requesting:" feed (.-message %)))))

    (.on fp "meta" #(go (>! feed-md (h/to-clj %))))
    (.on fp "readable" #(this-as this
                                 (go-loop [post (.read this)]
                                          (when post
                                            (do (>! articles (extract-article post))
                                                (recur (.read this)))))))
    (.on fp "end" #(close-all!))
    (.on fp "error" #(log-and-close! :error (print-str "feedparser error:" feed %)))
    [feed-md articles logs]))
