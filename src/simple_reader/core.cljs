;(ns ^:figwheel-always simple-reader.core
(ns simple-reader.core
  (:require
    [simple-reader.feedreader :as fr]
    [cljs.core.async :refer [chan <! >!] :as a]
    [cljs.nodejs :as nodejs]))

;; (enable-console-print!)
(nodejs/enable-util-print!)

(println "Starting stuff.")

(defn testing []
  (fr/read "https://xkcd.com/atom.xml" (a/chan)))

(def -main testing)
(set! *main-cli-fn* -main)
