(defproject simple-reader "0.1.0"
  :description "simple feed reader"
  :url "https://github.com/Wonko7/sreader"
  :license {:name "BSD"
            :url "just google it"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.229"]
                 [org.clojure/core.async "0.2.391"
                  :exclusions [org.clojure/tools.reader]]
                 [cljs-http "0.1.42"]
                 [com.cognitect/transit-cljs "0.8.239"]
                 [com.rpl/specter "1.0.0"]
                 [secretary "1.2.3"]
                 [rum "0.10.5"]
                 [binaryage/devtools "0.8.2"]]

  :plugins [[lein-cljsbuild "1.1.4" :exclusions [[org.clojure/clojure]]]
            ]

  :source-paths ["src"]

  ;:clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild {:builds
              [{:id "client"
                :source-paths ["src"]

                :compiler {:main client.core
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/simple_reader.js"
                           :output-dir "resources/public/js/compiled/out"
                           :optimizations :none
                           :source-map true
                           :source-map-timestamp true
                           ;; To console.log CLJS data-structures make sure you enable devtools in Chrome
                           ;; https://github.com/binaryage/cljs-devtools
                           :preloads [devtools.preload]}}

               {:id "server"
                :source-paths ["src"]
                :compiler {:main simple-reader.core
                           :output-to "target/simple_reader/core.js"
                           :output-dir "target/simple_reader"
                           :target :nodejs
                           :optimizations :none
                           :source-map true}}

               {:id "client-min"
                :source-paths ["src"]
                :compiler {:output-to "resources/public/js/compiled/simple_reader.js"
                           :main client.core
                           :optimizations :advanced
                           :pretty-print false}}
              
               ]}

  :profiles {:dev {:dependencies [[binaryage/devtools "0.8.2"]]
                   :source-paths ["src" "dev"]
                   }}
)
