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

(defmethod search :mvn
  [_ text]
  (-> "https://search.maven.org/solrsearch/select"
      (http/get {:query-params {:q text :rows 20 :wt "json"}})
      deref
      :body
      (json/read-value (json/object-mapper {:decode-key-fn (comp keyword hyphenate)}))
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

(defmethod search :clojars
  [_ text]
  (-> "https://clojars.org/search"
      (http/get {:query-params {:q text :format "json"}})
      deref
      :body
      (json/read-value (json/object-mapper {:decode-key-fn (comp keyword hyphenate)}))
      :results))

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


  (-> "https://clojars.org/search"
      (http/get {:query-params {:q "clojure" :format "json"}})
      deref
      :body
      (json/read-value (json/object-mapper {:decode-key-fn (comp keyword hyphenate)}))
      )
  
  (count (search :clojars "clojure")))
