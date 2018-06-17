(ns find-deps.alpha
  (:require [org.httpkit.client :as http]
            [jsonista.core :as json]
            [inflections.core :refer [hyphenate]]
            [clojure.string :as str]
            [clj-fuzzy.metrics :as fm]
            [clj-async-profiler.core :as prof]
            [clojure.spec.alpha :as s]
            [clojure.tools.deps.alpha.specs :as tds]
            [find-deps.links :refer :all]))

(defmethod search :github
  [_ text]
  (loop [url     "https://api.github.com/search/code"
         results []]
    (prn "fetching " url)
    (let [result (-> url
                     (http/get {:query-params {:q (str "filename:deps.edn")}
                                :headers      {"Authorization" (str "token <token>")}})
                     deref
                     links-response)]
      (if-let [next (-> result :links :next :href)]
        (recur next (conj results result))
        results))))

(defn decode-github
  [result]
  (-> result
      :body
      (json/read-value (json/object-mapper {:decode-key-fn (comp keyword hyphenate)}))))
