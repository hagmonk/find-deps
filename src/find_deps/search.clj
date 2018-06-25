(ns find-deps.search
  (:require [org.httpkit.client :as http]
            [jsonista.core :as json]
            [inflections.core :refer [hyphenate]]
            [clojure.string :as str]
            [clj-fuzzy.metrics :as fm]
            #_[clj-async-profiler.core :as prof]
            [clojure.spec.alpha :as s]
            [clojure.tools.deps.alpha.specs :as tds])
  (:gen-class))

(defmulti search (fn [service text] service))

(defmulti coerce (fn [service result] service))

(defn http-helper
  ([url]
   (http-helper url {}))
  ([url query]
   (-> url
       (http/get {:query-params query})
       deref
       :body
       (json/read-value (json/object-mapper {:decode-key-fn (comp keyword hyphenate)})))))

(defmethod search :mvn
  [_ text]
  (-> "https://search.maven.org/solrsearch/select"
      (http-helper {:q text :rows 20 :wt "json"})
      :response
      :docs))

;; (first (search :mvn "kafka"))
;; {:repository-id  "central",
;;  :ec             ["-sources.jar" "-javadoc.jar" ".jar" ".pom"],
;;  :g              "org.testcontainers"
;;  :id             "org.testcontainers:kafka"
;;  :version-count  6,
;;  :latest-version "1.8.0"
;;  :timestamp      1528995099000,
;;  :p              "jar"
;;  :a              "kafka"
;;  :text           ["org.testcontainers" "kafka" "-sources.jar" "-javadoc.jar" ".jar" ".pom"]}

(s/def :search/mvn
  (s/and (s/keys :req-un [::a ::g ::timestamp ::latest-version])
         (s/conformer
          #(clojure.set/rename-keys % {:a :artifact
                                       :g :group
                                       :latest-version :version}))))

(defn patch-clojars-snapshots
  "Given a clojars map, if the version supplied is a snapshot, attempt to find the
  latest release version rather than the snapshot."
  [{:keys [jar-name group-name version] :as ctx}]
  (or (when (re-find #"-SNAPSHOT" version)
        (when-let [latest-release (some-> (format "https://clojars.org/api/artifacts/%s/%s" group-name jar-name)
                                          http-helper
                                          :latest-release)]
          (assoc ctx :version latest-release)))
      ctx))

(comment
  (patch-clojars-snapshots {:description "A simple ClojureScript interface to React",
                            :created "1527436309000",
                            :jar-name "reagent",
                            :group-name "reagent",
                            :version "0.8.2-SNAPSHOT"}))

;; swap :version  to :latest-version
;; :jar-name "reagent",
;; :group-name "reagent"

(defmethod search :clojars
  [_ text]
  (pmap patch-clojars-snapshots
        (-> "https://clojars.org/search"
            (http/get {:query-params {:q text :format "json"}})
            deref
            :body
            (json/read-value (json/object-mapper {:decode-key-fn (comp keyword hyphenate)}))
            :results)))

;; (first (search :clojars "http-kit"))
;;   {:description "HTTP Kit adapter for Luminus",
;;    :created "1524405684000",
;;    :jar-name "luminus-http-kit",
;;    :group-name "luminus-http-kit",
;;    :version "0.1.6"}

(s/def :search/clojars
  (s/and (s/keys :req-un [::jar-name ::group-name ::created ::version])
         (s/conformer
          #(clojure.set/rename-keys % {:jar-name   :artifact
                                       :group-name :group
                                       :created    :timestamp
                                       :version    :version}))))

(defmethod search :default
  [_ text]
  [])

(s/def :search/result (s/and (s/or :mvn :search/mvn
                                   :clojars :search/clojars)
                             (s/conformer second)))

(defn query
  [search-string sources]
  (let [xf (comp (mapcat identity)
                 (map (partial s/conform :search/result))
                 (remove #{:clojure.spec.alpha/invalid})
                 (remove (comp #{"lein-template"} :artifact))
                 (map (fn [{:keys [group artifact] :as m}]
                        (assoc m :lib (str group "/" artifact))))
                 (map #(select-keys % [:lib :version :timestamp :artifact :group])))]
    (->> sources
         (pmap #(search % search-string))
         (into [] xf))))

(comment

  (time (query "http-kit" #{:mvn :clojars}))
  (time (query "apache/kafka" #{:mvn :clojars}))
  (time (query "priority-map" #{:mvn :clojars}))
  (time (query "pedestal/interceptors" #{:mvn :clojars}))

  (time (query "reagent" #{:mvn :clojars}))

  (first (filter (comp #(re-find #"SNAPSHOT" %) :version) (search :clojars "reagent")))
  

  (filter (comp #{"reagent"} :artifact) (query "reagent" #{:mvn :clojars}))

  (-> "https://clojars.org/search"
      (http/get {:query-params {:q "reagent" :format "json"}})
      deref
      :body
      (json/read-value (json/object-mapper {:decode-key-fn (comp keyword hyphenate)})))

  "https://clojars.org/api/artifacts/cljsjs/react"

  (-> "https://clojars.org/api/artifacts/reagent/reagent"
      (http/get {:query-params {:q "reagent" :format "json"}})
      deref
      :body
      (json/read-value (json/object-mapper {:decode-key-fn (comp keyword hyphenate)})))
  
  (count (search :clojars "clojure")))
