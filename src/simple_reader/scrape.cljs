(ns simple-reader.scrape
  (:require [cljs.core :as cljs]
            [cljs.nodejs :as node]
            [cljs.core.async :refer [chan <! >!] :as a]
            [clojure.string :as str]
            [simple-reader.logs :as log]
            [simple-reader.helpers :as h]
            [simple-reader.feedreader :as fr])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(defn- log [logs level msg]
  (go (>! logs {:level level :log msg :system :scrape})))

(defn get-link [link logs]
  (let [axios           (node/require "axios")
        result-chan     (chan)
        close-all!      #(go (a/close! result-chan)
                             (a/close! logs))
        log-and-close!  #(go (>! logs {:level %1 :log %2 :system :scrape})
                             (close-all!))]

    (-> axios (.request (cljs/clj->js {:url link :timeout 60000}))
        (.then  #(go (>! result-chan (.-data %))
                     (close-all!)))
        (.catch #(log-and-close! :error (print-str "error requesting:" link (.-message %)))))

    result-chan))

(defn simple-scrape [selector rss-entry logs & [data-type]]
  (go (if (:link rss-entry)
        (let [html      (<! (get-link (:link rss-entry) logs))
              $         (.load (node/require "cheerio") (or html ""))
              scraped   (.html $ selector)
              data-type (or data-type :scraped-data)]
          (if-not scraped
            (do (log logs :warning (print-str "scraping returned nothing" :url (:link rss-entry)))
                {})
            {data-type scraped}))
        (do (log logs :warning (print-str "error: no link in:" rss-entry))
            {}))))

(defn mk-youtube-embedded [{link :link} logs]
  (let [embed-link (str/replace link #"/watch\?v=" "/embed/")]
    ;; FIXME: sablono might work on node!
    (go {:scraped-data (str "<div class=\"video-wrapper\">" ;; if we end up doing more of this we'll need a templating lib.
                            "<iframe src=\"" embed-link "\"> </iframe>"
                            "</div>")})))

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
   "Saturday Morning Breakfast Cereal"
   {:scrape-fn #(simple-scrape "div#aftercomic img" %1 %2 :scraped-data-bottom)}
   "LWN.net -- disabled"
   {:scrape-fn (fn [rss-entry logs]
                 (let [title (:title rss-entry)]
                   (if (re-find #"^\[\$\]" title)
                     (let [link (:link rss-entry)
                           ; link (str/replace link #"rss$" "")
                           scraped (simple-scrape "div.ArticleEntry" {:link link} logs)]
                       scraped)
                     (go {}))))
    :overwrite true}
   "NASA Image of the Day"
   {:scrape-fn (fn [{link :link} logs]
                 (go (let [$   (.load (node/require "cheerio") (<! (get-link link logs)))
                           img (-> ($ "meta[property='og:image']")
                                   (.attr "content"))]
                       (if (nil? img)
                         (do (log logs :warning (str "scraping returned nothing" :url link))
                             {})
                         {:scraped-data (str "<img src=\"" img "\">")}))))}
   "xkcd comic"
   {:scrape-fn (fn [{html :description :as entry} logs]
                 (go (let [$   (.load (node/require "cheerio") html)
                           alt (-> ($ "img")
                                   (.attr "alt"))]
                       (if (nil? alt)
                         (do (log logs :warning (str "could not find img alt text" :url (:url entry)))
                             {})
                         {:scraped-data-bottom alt}))))}
   "Jam2go"
   {:scrape-fn mk-youtube-embedded}
   "Babish"
   {:scrape-fn mk-youtube-embedded}
   "Last Week Tonight"
   {:scrape-fn mk-youtube-embedded}
   "Explosm Shorts"
   {:scrape-fn mk-youtube-embedded}
   "Cyriak"
   {:scrape-fn mk-youtube-embedded}})

(defn scrape [feed article already-scraped]
  (let [logs      (chan)
        scrape-md (-> feed scrape-data)]
    (if (or (and (:scraped-data already-scraped) (not (:overwrite scrape-md))) ;; already scraped
            (nil? scrape-md)) ;; feed has no scraping to do
      [(go {}) logs]
      [((:scrape-fn scrape-md) article logs) logs])))
