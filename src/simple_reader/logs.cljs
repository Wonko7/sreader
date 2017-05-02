(ns simple-reader.logs
  (:require
    [cljs.nodejs :as node]
    [cljs.core.async :refer [alts! timeout chan <! >!] :as a]
    [cljs.pprint :as pprint]
    [simple-reader.helpers :as h]
    [simple-reader.feed-file-io :as io]
    [com.rpl.specter :as s :refer [collect must setval select-one select transform view filterer keypath pred srange ALL ATOM FIRST MAP-KEYS MAP-VALS VAL NIL->VECTOR]])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint try->empty]]))

(defn- template-log [{f :feed l :level ls :logs}]
  (str "<div class=\"lol\">"))

(def logs (chan))

(defn feed-msg [ts sys f lvl msg]
  (go (>! logs {:timestamp ts :feed f :system sys :log msg :level lvl})))

(defn msg [ts sys lvl msg]
  (go (>! logs {:timestamp ts :system sys :log msg :level lvl})))

(defn raw [thing]
  (go (>! logs thing)))

(defn write-logs [logs]
  (let [mk-div    (fn [class content & [extra]] ;; *sigh*
                    (str "<div class=\"" (name class) "\">" content (or extra "") "</div>"))
        mk-entry  (fn [feed status logs]
                    (str (mk-div :sr-log-title (str feed ": " status))
                         (mk-div :sr-log-wrapper
                                 (apply str (for [[sys logs] (sort-by first logs)
                                                  l logs]
                                              (mk-div :sr-log-line
                                                      (mk-div :sr-log-system sys)
                                                      (mk-div :sr-log-message l)))))))
        mk-descr  (fn [ts status]
                    (apply str (for [[feed data] (sort-by first (logs ts))
                                     data (select [(must ts) (must feed) (must status)] logs) ]
                                 (mk-entry feed status data)
                                 )))
        hum-date  (node/require "human-date")
        tss       (sort (keys logs))
        statuses  [:error :warning :info]]
    (doseq [ts tss
            st statuses]
      (io/save-article "SReader Logs" {:date ts :pretty-date (.prettyPrint hum-date ts)
                                       :title (name st) :description (mk-descr ts st) :guid (str st (.toString ts))} {}))))

(defn- process-logs []
  (go-loop [acc-logs {}]
           (let [to      (timeout (* 1000 10)) ;; fixme could also add an exit sigterm etc
                 [l ch]  (alts! [to logs])]
             (if (= ch logs)
               (let [{ts :timestamp sys :system f :feed msg :log lvl :level} l
                     stdout-msg (if f (print-str sys lvl (str f ":") msg) (print-str sys lvl msg))]
                 (println stdout-msg)
                 (recur (transform [(keypath ts) (keypath f) lvl sys NIL->VECTOR] #(conj % msg) acc-logs)))
               (do (write-logs acc-logs)
                   (recur {})))
             )))

(defn init []
  (process-logs)
  logs)
