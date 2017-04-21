(ns simple-reader.scrape
  (:require [cljs.core :as cljs]
            [cljs.nodejs :as node]
            [cljs.core.async :refer [chan <! >!] :as a]
            [clojure.string :as str]
            [simple-reader.helpers :as h]
            [simple-reader.feedreader :as fr]
            )
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(defn get-link [link]
  (let [req   ((node/require "request") link (cljs/clj->js {:timeout 50000 :pool false}))
        result-chan (chan)]
    (.setMaxListeners req 50)
    (.setHeader req "user-agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36")
    (.setHeader req "accept" "text/html,application/xhtml+xml")
    (.on req "error" #(go (println "scrape: error requesting" link %)
                          (a/close! result-chan)))
    (.on req "response" (fn [response]
                          (when (not= 200 (.-statusCode response))
                            (println "scrape: HTTP: request: bad status code:" (.-statusCode response) "on:" link)
                            (a/close! result-chan))
                          (.on response "data" #(go (>! result-chan %)))))
    (.on req "end" #(a/close! result-chan))

    (go (js/Buffer.concat (cljs/clj->js (<! (a/reduce conj [] result-chan)))))))

(defn simple-scrape [selector rss-entry]
  (go (if (:link rss-entry)
        (let [$       (.load (node/require "cheerio") (<! (get-link (:link rss-entry))))
              scraped (.html $ selector)]
          (or scraped {}))
        (do (println "scrape: error: no link in:" rss-entry)
            {}))))

(def scrape-data ;; should be external to main app, in config somewhere, but I'm the only user so meh for now.
  {"Explosm.net"
   {:scrape-fn (partial simple-scrape "img#main-comic")}
   "Buttersafe"
   {:scrape-fn (partial simple-scrape "div#comic img")}
   "Mary Death"
   {:scrape-fn (partial simple-scrape "div#comic img")}
   "Send More Paramedics"
   {:scrape-fn (partial simple-scrape "div.post")}
   "les_joies_du_code();"
   {:scrape-fn (partial simple-scrape "div.blog-post-content img")}
   "the_coding_love();"
   {:scrape-fn (partial simple-scrape "div#post1 img")}
   "LWN.net"
   {:scrape-fn (fn [rss-entry]
                 (let [title (:title rss-entry)]
                   (if (re-find #"^\[\$\]" title)
                     (let [link (:link rss-entry)
                           link (str/replace link #"rss$" "")]
                       (simple-scrape "div.ArticleEntry" {:link link}))
                     (go {}))))
    :overwrite true}
   "NASA Image of the Day"
   {:scrape-fn (fn [{link :link}]
                 (go (let [$ (.load (node/require "cheerio") (<! (get-link link)))
                           img (-> ($ "meta[property='og:image']")
                                   (.attr "content"))]
                       (if (nil? img)
                         {}
                         (str "<img src=\"" img "\">")))))}
   })

(defn scrape [feed article already-scraped]
  (go (let [scrape-md (-> feed scrape-data)]
        (if (or (and (:scraped-data already-scraped) (not (:overwrite scrape-md))) ;; already scraped
                (nil? scrape-md)) ;; feed has no scraping to do
          {}
          (let [scraped-data (<! ((:scrape-fn scrape-md) article))]
            {:scraped-data scraped-data})))))
