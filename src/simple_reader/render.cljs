(ns simple-reader.render
  (:require
    [cljs.nodejs :as nodejs]
    [simple-reader.feedreader :as fr]
    [cljs.core.async :refer [chan <! >!] :as a]
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
        rendered (go-loop [a-rendered [:div.feed]]
                          (let [article (h/to-clj (<! articles))]
                            (if (= article :done)
                              (do (println ::done (vec a-rendered))
                                  (vec a-rendered))
                              (recur (concat a-rendered (mk-article (:title article) (.prettyPrint hum-date (:date article)) (:description article) (:link article))))
                              )))
        ]
    rendered))


;; (:div.feed
;;   [:div.article [:a.title {:href https://xkcd.com/1819/} Sweet 16] [:br] [:div.small April 3rd, 2017] [:br]
;;    [:div.content <img src="https://imgs.xkcd.com/comics/sweet_16.png" title="Every year I make out my bracket at the season, and every year it's busted before the first game when I find out which teams are playing." alt="Every year I make out my bracket at the season, and every year it's busted before the first game when I find out which teams are playing." />]
;;    ])

;; (:div.feed [:div.article [:a.title {:href https://xkcd.com/1820/} Security Advice] [:br] [:div.small April 5th, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/security_advice.png" title="Never give your password or bank account number to anyone who doesn't have a blue check mark next to their name." alt="Never give your password or bank account number to anyone who doesn't have a blue check mark next to their name." />]] [:div.article [:a.title {:href https://xkcd.com/1818/} Rayleigh Scattering] [:br] [:div.small March 31st, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/rayleigh_scattering.png" title="If you ask &quot;why are leaves green?&quot; the usual answer is &quot;because they're full of chlorophyll, and chlorophyll is green,&quot; even though &quot;why does chlorophyll scatter green light?&quot; is a great question too." alt="If you ask &quot;why are leaves green?&quot; the usual answer is &quot;because they're full of chlorophyll, and chlorophyll is green,&quot; even though &quot;why does chlorophyll scatter green light?&quot; is a great question too." />]])
;; [:div.feed [:div.article [:a.title {:href https://xkcd.com/1820/} Security Advice] [:br] [:div.small April 5th, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/security_advice.png" title="Never give your password or bank account number to anyone who doesn't have a blue check mark next to their name." alt="Never give your password or bank account number to anyone who doesn't have a blue check mark next to their name." />]] [:div.article [:a.title {:href https://xkcd.com/1818/} Rayleigh Scattering] [:br] [:div.small March 31st, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/rayleigh_scattering.png" title="If you ask &quot;why are leaves green?&quot; the usual answer is &quot;because they're full of chlorophyll, and chlorophyll is green,&quot; even though &quot;why does chlorophyll scatter green light?&quot; is a great question too." alt="If you ask &quot;why are leaves green?&quot; the usual answer is &quot;because they're full of chlorophyll, and chlorophyll is green,&quot; even though &quot;why does chlorophyll scatter green light?&quot; is a great question too." />]]]
;; (:div.feed [:div.article [:a.title {:href https://xkcd.com/1819/} Sweet 16] [:br] [:div.small April 3rd, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/sweet_16.png" title="Every year I make out my bracket at the season, and every year it's busted before the first game when I find out which teams are playing." alt="Every year I make out my bracket at the season, and every year it's busted before the first game when I find out which teams are playing." />]] [:div.article [:a.title {:href https://xkcd.com/1817/} Incognito Mode] [:br] [:div.small March 29th, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/incognito_mode.png" title="They're really the worst tech support team. And their solutions are always the same. &quot;This OS X update broke something.&quot; &quot;LET'S INFILTRATE APPLE BY MORPHING APPLES!&quot;" alt="They're really the worst tech support team. And their solutions are always the same. &quot;This OS X update broke something.&quot; &quot;LET'S INFILTRATE APPLE BY MORPHING APPLES!&quot;" />]])
;; 


[:div.feed
 [:div.article [:a.title {:href https://xkcd.com/1820/} Security Advice] [:br] [:div.small April 5th, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/security_advice.png" title="Never give your password or bank account number to anyone who doesn't have a blue check mark next to their name." alt="Never give your password or bank account number to anyone who doesn't have a blue check mark next to their name." />]]
 [:div.article [:a.title {:href https://xkcd.com/1818/} Rayleigh Scattering] [:br] [:div.small March 31st, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/rayleigh_scattering.png" title="If you ask &quot;why are leaves green?&quot; the usual answer is &quot;because they're full of chlorophyll, and chlorophyll is green,&quot; even though &quot;why does chlorophyll scatter green light?&quot; is a great question too." alt="If you ask &quot;why are leaves green?&quot; the usual answer is &quot;because they're full of chlorophyll, and chlorophyll is green,&quot; even though &quot;why does chlorophyll scatter green light?&quot; is a great question too." />]]
 ]
[(:div.feed [:div.article [:a.title {:href https://xkcd.com/1820/} Security Advice] [:br] [:div.small April 5th, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/security_advice.png" title="Never give your password or bank account number to anyone who doesn't have a blue check mark next to their name." alt="Never give your password or bank account number to anyone who doesn't have a blue check mark next to their name." />]]
            [:div.article [:a.title {:href https://xkcd.com/1818/} Rayleigh Scattering] [:br] [:div.small March 31st, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/rayleigh_scattering.png" title="If you ask &quot;why are leaves green?&quot; the usual answer is &quot;because they're full of chlorophyll, and chlorophyll is green,&quot; even though &quot;why does chlorophyll scatter green light?&quot; is a great question too." alt="If you ask &quot;why are leaves green?&quot; the usual answer is &quot;because they're full of chlorophyll, and chlorophyll is green,&quot; even though &quot;why does chlorophyll scatter green light?&quot; is a great question too." />]]
            )]
:simple-reader.render/done
[:div.feed [:div.article [:a.title {:href https://xkcd.com/1819/} Sweet 16] [:br] [:div.small April 3rd, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/sweet_16.png" title="Every year I make out my bracket at the season, and every year it's busted before the first game when I find out which teams are playing." alt="Every year I make out my bracket at the season, and every year it's busted before the first game when I find out which teams are playing." />]]
 [:div.article [:a.title {:href https://xkcd.com/1817/} Incognito Mode] [:br] [:div.small March 29th, 2017] [:br] [:div.content <img src="https://imgs.xkcd.com/comics/incognito_mode.png" title="They're really the worst tech support team. And their solutions are always the same. &quot;This OS X update broke something.&quot; &quot;LET'S INFILTRATE APPLE BY MORPHING APPLES!&quot;" alt="They're really the worst tech support team. And their solutions are always the same. &quot;This OS X update broke something.&quot; &quot;LET'S INFILTRATE APPLE BY MORPHING APPLES!&quot;" />]]
 ]
