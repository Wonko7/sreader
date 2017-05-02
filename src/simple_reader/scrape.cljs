(ns simple-reader.scrape
  (:require [cljs.core :as cljs]
            [cljs.nodejs :as node]
            [cljs.core.async :refer [chan <! >!] :as a]
            [clojure.string :as str]
            [simple-reader.helpers :as h]
            [simple-reader.feedreader :as fr])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(defn- log [logs level msg]
  (go (>! logs {:level level :log msg :system :scrape})))

(defn get-link [link logs]
  (let [req   ((node/require "request") link (cljs/clj->js {:timeout 50000 :pool false}))
        result-chan (chan)
        close-all! #(go (a/close! result-chan)
                        (a/close! logs))
        log-and-close! #(go (>! logs {:level %1 :log %2 :system :scrape})
                            (close-all!))]

    (.setMaxListeners req 50)
    (.setHeader req "user-agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36")
    (.setHeader req "accept" "text/html,application/xhtml+xml")
    (.on req "error" #(log-and-close! :error (str "scrape: error requesting" link %)))
    (.on req "response" (fn [response]
                          (if (not= 200 (.-statusCode response))
                            (log-and-close! :error (str "scrape: HTTP: request: bad status code:" (.-statusCode response) "on:" link))
                            (.on response "data" #(go (>! result-chan %))))))
    (.on req "end" #(close-all!))

    (go (js/Buffer.concat (cljs/clj->js (<! (a/reduce conj [] result-chan)))))))

(defn simple-scrape [selector rss-entry logs]
  (go (if (:link rss-entry)
        (let [$       (.load (node/require "cheerio") (<! (get-link (:link rss-entry) logs)))
              scraped (.html $ selector)]
          (if-not scraped
            (do (log logs :warning (str "scraping returned nothing" :url (:link rss-entry)))
                {})
            scraped))
        (do (log logs :warning (str "scrape: error: no link in:" rss-entry))
            {}))))

(defn mk-youtube-embedded [{link :link} logs]
  (let [embed-link (str/replace link #"/watch\?v=" "/embed/")]
    (go (str "<div class=\"video-wrapper\">" ;; if we end up doing more of this we'll need a templating lib.
             "<iframe src=\"" embed-link "\"> </iframe>"
             "</div>"))))

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
   {:scrape-fn (fn [rss-entry logs]
                 (let [title (:title rss-entry)]
                   (if (re-find #"^\[\$\]" title)
                     (let [link (:link rss-entry)
                           link (str/replace link #"rss$" "")]
                       (simple-scrape "div.ArticleEntry" {:link link} logs))
                     (go {}))))
    :overwrite true}
   "NASA Image of the Day"
   {:scrape-fn (fn [{link :link} logs]
                 (go (let [$ (.load (node/require "cheerio") (<! (get-link link logs)))
                           img (-> ($ "meta[property='og:image']")
                                   (.attr "content"))]
                       (if (nil? img)
                         (do (log logs :warning (str "scraping returned nothing" :url (:link rss-entry)))
                             {})
                         (str "<img src=\"" img "\">")))))}
   "Last Week Tonight"
   {:scrape-fn mk-youtube-embedded}
   "Explosm Shorts"
   {:scrape-fn mk-youtube-embedded}
   "Cyriak"
   {:scrape-fn mk-youtube-embedded}
   })

(defn scrape [feed article already-scraped logs]
  (let [logs (chan)]
    (go (let [scrape-md (-> feed scrape-data)]
           (if (or (and (:scraped-data already-scraped) (not (:overwrite scrape-md))) ;; already scraped
                   (nil? scrape-md)) ;; feed has no scraping to do
             {}
             (let [scraped-data (<! ((:scrape-fn scrape-md) article logs))]
               {:scraped-data scraped-data}))))))
