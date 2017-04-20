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
                          (>! result-chan :error)))
    (.on req "response" (fn [response]
                          (when (not= 200 (.-statusCode response))
                            (println "scrape: HTTP: request: bad status code:" (.-statusCode response) "on:" link)
                            (go (>! result-chan :error)))
                          (.on response "data" #(go (>! result-chan %)))))
    (.on req "end" #(go (>! result-chan :done)))

    ;; FIXME squash let
    (go (let [html (js/Buffer.concat (cljs/clj->js (<! (go-loop [part (<! result-chan) buffers []]
                                                                (println part)
                                                                (if (and (not= part :done) (not= part :error))
                                                                  (recur (<! result-chan) (concat buffers [part]))
                                                                  buffers)))))]
          html))))

(defn simple-scrape [selector rss-entry]
  (go (let [$ (.load (node/require "cheerio") (<! (get-link (:link rss-entry))))]
        (.html $ selector))))

(def scrape-data
  {"Explosm.net"
   {:scrape-fn (partial simple-scrape "img#main-comic")}
   "Buttersafe"
   {:scrape-fn (partial simple-scrape "div#comic img")}
   "Mary Death"
   {:scrape-fn (partial simple-scrape "div#comic img")}
   "Send More Paramedics"
   {:scrape-fn (partial simple-scrape "div.post")}
   "LWN.net"
   {:scrape-fn (fn [rss-entry]
                 (let [title (:title rss-entry)]
                   (if (re-find #"^\[\$\]" title)
                     (let [link (:link rss-entry)
                           link (str/replace link #"(.*)rss$" "$1")]
                       (simple-scrape "div.ArticleEntry" {:link link}))
                     (go {}))))
    :overwrite true}})

(defn scrape [feed article]
  (go (if-let [scrape-fn (-> feed scrape-data :scrape-fn)]
        (let [scraped-data (<! (scrape-fn article))]
          (if (not= {} scraped-data)
            {:scraped-data scraped-data}
            {}))
        {})))
