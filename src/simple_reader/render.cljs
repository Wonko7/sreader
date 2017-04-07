(ns simple-reader.render
  (:require
    [cljs.nodejs :as nodejs]
    [simple-reader.feedreader :as fr]
    [cljs.core.async :refer [chan <! >!] :as a]
    [simple-reader.helpers :as h])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))


;; (rum/defc article [title date desc link]
;;   [:div.article
;;    [:a.title {:href link} title]
;;    [:br]
;;    [:div.small date]
;;    [:br]
;;    [:div.content desc]])


(defn render-articles [articles]
  "read articles one by one and render the html"
  (let [hum-date (nodejs/require "human-date")
        rendered (go-loop [article articles rendered ""]
                          (println)
                          (let [article (h/to-clj (<! article))]
                            (if (= article :done)
                              rendered
                              (recur articles (str (:title article) (.prettyPrint hum-date (:date article)) (:title article) (:title article) "\n"))
                              ;;(println (:title article))
                              ;;(println (:link article))
                              ;;(println (:description article))
                              ;;(println (.prettyPrint hum-date (:date article)))
                              ;;;(println (:summary article))
                              ;;;(println (keys article))
                              )))]
    rendered))
