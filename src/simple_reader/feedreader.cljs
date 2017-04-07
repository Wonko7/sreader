(ns simple-reader.feedreader
  (:require [cljs.core :as cljs]
            [cljs.nodejs :as node]
            [cljs.core.async :refer [chan <! >!] :as a]
            [clojure.string :as str]
            [simple-reader.helpers :as h]
            )
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))


;; function maybeDecompress (res, encoding) {
;;   var decompress;
;;   if (encoding.match(/\bdeflate\b/)) {
;;     decompress = zlib.createInflate();
;;   } else if (encoding.match(/\bgzip\b/)) {
;;     decompress = zlib.createGunzip();
;;   }
;;   return decompress ? res.pipe(decompress) : res;
;; }

(defn maybe-decompress [res encoding]
  (let [zlib      (node/require "zlib")
        encoding  (str encoding)
        decomp    (cond
                    (re-find #"\bdeflate\b" encoding) (do (println "using deflate") (.createInflate zlib))
                    (re-find #"\bgzip\b" encoding)    (do (println "using gzip") (.createGunzip zlib))
                    :else nil)]
    (if decomp
      (.pipe res decomp)
      res)))

(defn maybe-translate [res charset]
  (let [charset (str/replace (str charset) #"(?i).*charset=(.*);?\s*$" "$1")]
    (if (re-find #"(?i)utf-*8" charset)
      res
      (try
        (let [iconv (node/require "iconv")
              iconv (new iconv "utf-8")]
          (println "converted" charset "to utf8")
          (.pipe res iconv))
        (catch js/Object e (do (println "iconv bug")
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


(defn read [feed result-chan]
  "Will read each value from the given feed address and write them to the result-chan."
  (let [req   ((node/require "request") feed (cljs/clj->js {:timeout 1000 :pool false}))
        fp    (node/require "feedparser")
        fp    (new fp)]
    (.setMaxListeners req 50)
    (.setHeader req "user-agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36")
    (.setHeader req "accept" "text/html,application/xhtml+xml")
    (.on req "error" #(dprint "error requesting" feed))
    ;(.on "end" req identity)
    (.on req "response" (fn [result]
                          (when (not= 200 (.-statusCode result))
                            (dprint "bad status code:" (.-statusCode result) )
                            (throw :bad-status-code)) ;; just burn for now.
                          (let [headers (h/to-clj (.-headers result))
                                encoding (:content-encoding headers)
                                charset (:content-type headers)
                                res (maybe-decompress result encoding)
                                res (maybe-translate result charset)]
                            (.pipe result fp))))

    (.on fp "readable" #(this-as this
                                 (go-loop [post (.read this)]
                                          (if post 
                                            (do (>! result-chan post)
                                                (recur (.read this)))
                                            (>! result-chan :done)))))
    ))
