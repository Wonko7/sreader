(ns simple-reader.http
  (:require
    [cljs.nodejs :as nodejs]
    [simple-reader.feedreader :as fr]
    [cljs.core.async :refer [chan <! >!] :as a]
    [simple-reader.helpers :as h])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(defn init []
  (let [express (nodejs/require "express")
        app (new express)]
    ;; routes:
    ;; (.get app "/" (fn [req, res]
    ;;                 (.send res "hello world............")))
    (.use app "/" (.static express "resources/public/"))

;app.use('/static', express.static('public'))

    ;; setup listen:
    (.listen app 3000 #(println "We're listening."))
    ))

	
