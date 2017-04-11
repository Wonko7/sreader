(ns simple-reader.helpers
  (:require [cljs.core :as cljs]
            [cljs.nodejs :as node]
            [cognitect.transit :as json]
            [clojure.walk :as walk]))

(defn to-clj [js-map]
  "Convert a js map to a clojure hashmap with keywords as keys"
  (-> js-map cljs/js->clj walk/keywordize-keys))

(defn to-js [clj-map]
  "Convert a clj map to a js hashmap with strings as keys"
  (-> clj-map walk/stringify-keys cljs/clj->js))

(defn write-json [string]
  (json/write (json/writer :json) string))


(defn read-json [string]
  (json/read (json/reader :json) string))
