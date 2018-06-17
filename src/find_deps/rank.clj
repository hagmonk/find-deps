(ns find-deps.rank
  (:require [clj-fuzzy.metrics :as fm]
            [clojure.data.priority-map :refer [priority-map priority-map-keyfn]]))


(defn- compute-top-groups
  "Note that some groups from mvn have been hard coded in."
  []
  (let [stats
        (-> "https://clojars.org/stats/all.edn"
            org.httpkit.client/get
            deref
            :body
            read-string)]
    (into #{}
          (comp
           (take 20)
           (map first))
          (reverse
           (reduce-kv (fn [m [g a] v]
                        (update m g (fnil + 0) (reduce + (vals v))))
                      (clojure.data.priority-map/priority-map) stats)))))

(def mvn-nudges #{"org.apache" "org.clojure" "io.pedestal" "cognitect"})
(def top-groups (clojure.set/union
                 mvn-nudges
                 #{"ring" "pjstadig" "clj-stacktrace" "clj-tuple" "net.cgrand" "slingshot" "clojure-complete" "clout" "ring-mock" "cheshire" "potemkin" "clj-http" "hiccup" "riddley" "clj-time" "prismatic" "compojure" "metosin" "com.taoensso" "tigris" }))

(defn fuzzy
  [search-string results]
  ;; distance metric + closeness to now
  (->> results
       (reduce
        (fn [m {:keys [lib artifact group timestamp version]}]
          (let [dist     (fm/dice search-string lib)
                tm       (/ (-> timestamp str Long/parseLong) (System/currentTimeMillis))
                top-g    (apply max (map (partial fm/dice group) top-groups))]
            (assoc m (symbol lib) {:mvn/version version
                          :score       (/ 1
                                          (Math/sqrt
                                           (+ (Math/pow (- 1 dist) 2)
                                              (Math/pow (- 1 tm) 2)
                                              (Math/pow (/ (- 1 top-g) 2) 2))))})))
        (priority-map-keyfn :score))))

(defn regex
  [search-regex results]
  (let [pat (re-pattern (str ".*" search-regex ".*"))]
    (->> results
        (reduce
         (fn [m {:keys [lib artifact group timestamp version]}]
           (if (re-matches pat lib)
             (assoc m (symbol lib) {:mvn/version version :score 1})
             m))
         (priority-map-keyfn :score)))))

(defn limit
  [limit results]
  (->> results
       rseq
       (take limit)
       (map
        (fn [[k v]]
          [k (select-keys v [:mvn/version])]))))
