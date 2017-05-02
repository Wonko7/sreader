(ns simple-reader.core
  (:require
    [cljs.nodejs :as node]
    [cljs.core.async :refer [timeout chan <! >!] :as a]
    [clojure.set :as set]
    [simple-reader.feedreader :as fr]
    [simple-reader.helpers :as h]
    [simple-reader.logs :as log]
    [cognitect.transit :as json]
    [simple-reader.feed-file-io :as io]
    [simple-reader.http :as http]
    [simple-reader.scrape :as scrape]
    [com.rpl.specter :as s :refer [collect setval select-one select transform view filterer keypath pred srange ALL ATOM FIRST MAP-VALS]])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint try->empty]]))

(node/enable-util-print!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; feed-md state :

(def feed-md (atom {}))

(defn get-subs-by-tags []
  (try->empty
    (reset! feed-md (io/load-feeds-md))
    (let [tags-md   (io/load-tags-md)
          tag-list  (sort-by #(-> % tags-md :position) (keys tags-md))
          get-feeds-by-tag (fn [tag]
                             (sort-by first (select [ATOM MAP-VALS (fn [v] (some #(= tag %) (:tags v))) :name] feed-md)))]
      {:tag-order tag-list
       :tag-metadata tags-md
       :tag-content (into {} (map (fn [tag] {tag (get-feeds-by-tag tag)}) tag-list))
       :subscriptions @feed-md})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Process client requests:

(defn get-subscriptions [_]
  "read subscriptions"
  (h/write-json (get-subs-by-tags)))

(defn get-feed [{feed :feed nb :nb}]
  "read feeds web client"
  (try->empty (let [metadata (io/load-feed-md feed)
                    view     (or (:view-art-status metadata) "unread")
                    order    (or (:order metadata)  "oldest then saved")
                    articles (io/load-feed feed)
                    articles (condp = view
                               "unread"  (let [unread  (filter #(not= "read" (-> % :metadata :status)) articles)
                                               unsaved (filter #(not= "saved" (-> % :metadata :status)) unread)]
                                           (if (= 0 (count unsaved))
                                             articles
                                             unread))
                               "saved"   (filter #(= "saved" (-> % :metadata :status)) articles)
                               articles)
                    articles (condp = order
                               "oldest" (sort-by :date articles)
                               "newest" (reverse (sort-by :date articles))
                               (let [unsaved (sort-by :date (filter #(not= "saved" (-> % :metadata :status)) articles))
                                     saved   (sort-by :date (filter #(= "saved" (-> % :metadata :status)) articles))]
                                 (concat unsaved saved)))]
                (h/write-json {:feed-data {:title feed}
                               :metadata metadata
                               :articles articles}))))

(defn change-article-md [{{feed :feed article :article} :id new-md :metadata}]
  "handle article metadata changes"
  (try->empty (let [cur-md  (io/load-article-md feed article)
                    md      (merge cur-md new-md)]
                (io/save-article-md feed article md)
                md)))

(defn change-feed-md [{{feed :feed} :id new-md :metadata}]
  "handle feed metadata changes:"
  (try->empty (let [cur-md  (io/load-feed-md feed)
                    md      (merge cur-md new-md)]
                (io/save-feed-md feed md)
                md)))

(defn change-tag-md [{{tag :tag} :id new-md :metadata} ]
  "handle tag metadata changes:"
  (try->empty (let [cur-md  (io/load-tag-md tag)
                    md      (merge cur-md new-md)]
                (io/save-tag-md tag md)
                md)))

(defn update-feeds []
  "scrape subscriptions"
  (let [timestamp       (new js/Date)

        process-article (fn [feed {cnt :count kept-articles :kept} article]
                          (let [already-scraped (try->empty (io/load-article-scraped feed (:guid article))) ;; FIXME scrape-fn makes that decision
                                logs            (chan)
                                scraped         (scrape/scrape feed article already-scraped logs)]
                            (a/pipeline 1 log/logs (map #(merge % {:timestamp timeout :feed feed})) logs false)
                            (go (try->empty (io/save-article feed article (<! scraped))))
                            {:count (inc cnt) :kept (conj kept-articles (:guid article))}))

        process-logs    (fn [feed status {new-status :level :as entry}]
                          (let [s (if (or (= :error status) (= :error new-status)) :error :info)]
                            (log/raw (merge {:feed feed :timestamp timestamp} entry))
                            s))

        purge           (fn [feed to-keep]
                          (let [all       (try->empty (io/load-feed feed))
                                read      (into #{} (select [ALL #(= "read" (-> % :metadata :status)) :guid] all))
                                out-dated (set/difference read to-keep)]
                            (map #(try->empty (io/rm-article feed %)) out-dated)))]

    (println "\ncore:" (.toLocaleTimeString timestamp) "starting update feeds")

    (doseq [[k {link :url feed :name www-link :www-link :as fmd}] @feed-md
            :let [[feed-meta articles feed-logs] (fr/read link)]]

      (go (let [new-link (:link (<! feed-meta))]
            (when (not= new-link www-link)
              (try->empty (io/save-feed-fmd feed (merge (dissoc fmd :unread-count :saved-count) {:www-link new-link}))))))

      (go (let [log-reduced (a/reduce (partial process-logs feed) :info feed-logs)
                art-reduced (a/reduce (partial process-article feed) {:kept #{} :count 0} articles)
                {cnt :count kept :kept} (<! art-reduced)
                status                  (<! log-reduced)
                purged                  (if (= :info status)
                                          (count (purge feed kept))
                                          0)]
            (log/feed-msg timestamp :core feed status (print-str cnt "articles" "-- purged:" purged)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; app:

(defn -main []
  (let [feed-req    (chan)
        feed-ans    (chan)
        art-md-req  (chan)
        art-md-ans  (chan)
        feed-md-req (chan)
        feed-md-ans (chan)
        tag-md-req  (chan)
        tag-md-ans  (chan)
        subs-req    (chan)
        subs-ans    (chan)]

    (log/init)

    ;; init http
    (http/init feed-req feed-ans
               subs-req subs-ans
               art-md-req art-md-ans
               feed-md-req feed-md-ans
               tag-md-req tag-md-ans)

    (a/pipeline 1 feed-ans (map get-feed) feed-req)
    (a/pipeline 1 subs-ans (map get-subscriptions) subs-req)
    (a/pipeline 1 art-md-ans (map change-article-md) art-md-req)
    (a/pipeline 1 feed-md-ans (map change-feed-md) feed-md-req)
    (a/pipeline 1 tag-md-ans (map change-tag-md) tag-md-req)



    (log/write-logs
{1
 {"Mark Watson's opinions on Java, Ruby, Lisp, AI, and the Semantic Web"
  {:info {:core ["25 articles -- purged: 0"]}},
  "What's new"
  {:error
   {:feed-reader
    ["\"feed-reader: error requesting\" \"http://terrytao.wordpress.com/feed/\" #object[Error Error: socket hang up]"],
    :core ["0 articles -- purged: 0"]}},
  "Planet Gentoo" {:info {:core ["50 articles -- purged: 0"]}},
  "Notre construction à Bazemont"
  {:info {:core ["25 articles -- purged: 0"]}},
  "OpenSSL Valhalla Rampage"
  {:info {:core ["20 articles -- purged: 0"]}},
  "Explosm Shorts" {:info {:core ["15 articles -- purged: 0"]}},
  "Neil Gaiman's Journal" {:info {:core ["25 articles -- purged: 0"]}},
  "blackbag" {:info {:core ["10 articles -- purged: 0"]}},
  "iamdonald"
  {:error
   {:feed-reader
    ["\"feed-reader: feed parser error:\" \"http://www.iamdonald.com/rss\" #object[Error Error: Not a feed]"],
    :core ["0 articles -- purged: 0"]}},
  "QDB: Latest Approved Quotes"
  {:info {:core ["25 articles -- purged: 0"]}},
  "AntiHero's lockpick area"
  {:info {:core ["4 articles -- purged: 0"]
          :lol ["wtf"]}},
  "Gentoo Linux News" {:info {:core ["10 articles -- purged: 0"]}},
  "Whatever" {:info {:core ["10 articles -- purged: 0"]}},
  "Vie de merde" {:info {:core ["20 articles -- purged: 1"]}},
  "Presseurop" {:info {:core ["15 articles -- purged: 0"]}},
  "Send More Paramedics" {:info {:core ["5 articles -- purged: 0"]}},
  "FMyLife" {:info {:core ["20 articles -- purged: 1"]}},
  "Clojure Pipe"
  {:error
   {:feed-reader
    ["\"feed-reader: error requesting\" \"http://pipes.yahoo.com/pipes/pipe.run?_id=4cc8ebb9ae0b852d6ab7d94956ce2638&amp;_render=rss\" #object[Error Error: getaddrinfo ENOTFOUND pipes.yahoo.com pipes.yahoo.com:80]"],
    :core ["0 articles -- purged: 0"]}},
  "Total Survivalist Libertarian Rantfest"
  {:error
   {:feed-reader
    ["\"feed-reader: feed parser error:\" \"http://tslrf.blogspot.com/feeds/posts/default\" #object[Error Error: Not a feed]"],
    :core ["0 articles -- purged: 0"]}},
  "Neil Strauss"
  {:error
   {:feed-reader
    ["\"feed-reader: HTTP: request: bad status code:\" 404 \"on:\" \"http://www.neilstrauss.com/neil-strauss/?feed=rss2\""],
    :core ["0 articles -- purged: 0"]}}
  }})

    (comment (go (while true
          (get-subs-by-tags)
          (update-feeds)
          (<! (timeout (* 1000 60 60))))))))


(set! *main-cli-fn* -main)
