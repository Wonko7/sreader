(ns simple-reader.render
  (:require
    [cljs.nodejs :as nodejs]
    [simple-reader.feedreader :as fr]
    [cljs.core.async :refer [chan <! >!] :as a]
    [cognitect.transit :as json]
    [simple-reader.helpers :as h])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))


(defn mk-article [title date desc link]
  [[:div.article
    [:a.title {:href link} title]
    [:br]
    [:div.small date]
    [:br]
    [:div.content desc]]])


(defn render-articles [articles]
  "read articles one by one and render the html"
  (let [hum-date (nodejs/require "human-date")
        json-writer (json/writer :json)
        rendered (go-loop [a-rendered [:div.feed]]
                          (let [article (h/to-clj (<! articles))]
                            (if (= article :done)
                              (do (println ::done (vec a-rendered))
                                  (json/write json-writer (vec a-rendered)))
                              (recur (concat a-rendered (mk-article (:title article) (.prettyPrint hum-date (:date article)) (:description article) (:link article))))
                              )))
        tmp-feed {:feed-data {:title "feeeeed"} :articles [{:title "t1" :link "https://l1.com" :description "desc1" :date "d1"}
                                                           {:title "t2" :link "https://l2.com" :description "desc2" :date "d2"}] }
        rendered-hc (json/write json-writer tmp-feed)
        ]
    (go rendered-hc)))
