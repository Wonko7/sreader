(ns simple-reader.core
  (:require [rum.core :as rum]
            [cljs-http.client :as http]
            [cljs.core.async :refer [chan <! >!] :as a])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(enable-console-print!)
(def host "http://localhost:3000")

(println "This text is printed from src/simple-reader/core.cljs. Go ahead and edit it and see reloading in action.")
(println :lol)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!sssssssssssssss"}))

(rum/defc hello-world []
    [:h1 (:text @app-state)])

(rum/mount (hello-world)
           (. js/document (getElementById "app")))

(go (let [response (<! (http/get "/f/xkcd/42"))]
      (println :got (:body response))
      (swap! app-state assoc-in [:text] (:body response))))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
