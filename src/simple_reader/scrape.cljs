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

(def scrape-data
  {"Explosm.net"
   {:scrape-fn #(let [$ (.load (node/require "cheerio") %)]
                  (.html $ "img#main-comic"))}
   "Buttersafe"
   {:scrape-fn #(let [$ (.load (node/require "cheerio") %)]
                  (.html $ "div#comic img"))}
   "Mary Death"
   {:scrape-fn #(let [$ (.load (node/require "cheerio") %)]
                  (.html $ "div#comic img"))}
   "Send More Paramedics"
   {:scrape-fn #(let [$ (.load (node/require "cheerio") %)]
                  (.html $ "div.post"))}
   })

(defn scrape [feed link]
  (if-let [scrape (-> feed scrape-data :scrape-fn)]
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

      (go (let [html (js/Buffer.concat (cljs/clj->js (<! (go-loop [part (<! result-chan) buffers []]
                                                                  (if (and (not= part :done) (not= part :error))
                                                                    (recur (<! result-chan) (concat buffers [part]))
                                                                    buffers)))))]
            {:scraped-data (scrape html)})))
    (go {})))
