(ns find-deps.core
  (:require [clojure.tools.deps.alpha.script.parse :refer [parse-kws]]
            [clojure.tools.cli :refer [parse-opts]]
            [find-deps.search :as search]
            [find-deps.rank :as rank]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (java.io InputStream)))

(Thread/setDefaultUncaughtExceptionHandler
 (reify Thread$UncaughtExceptionHandler
   (uncaughtException [_ thread ex]
     (prn (Throwable->map ex))
     (System/exit -1))))

;; borrowed from clj-pid
(def pid
  (memoize
   (fn []
     (-> (java.lang.management.ManagementFactory/getRuntimeMXBean)
         (.getName)
         (str/split #"@")
         (first)))))

(defn debug [& args]
  (binding [*out* *err*]
    (apply prn (cons (str (pid)) args))
    (flush)))

(defn stream-available?
  [^InputStream stream]
  (pos? (.available stream)))

(defn get-deps-edn-stream
  []
  (if (stream-available? System/in)
    (with-open [r (java.io.PushbackReader. (io/reader *in*))]
      (clojure.edn/read r))
    {:deps {}}))

(defn get-deps-edn-file
  ([] (get-deps-edn-file "deps.edn"))
  ([path]
   (let [input (if (stream-available? System/in)
                 *in*
                 (io/as-file path))]
     (with-open [r (java.io.PushbackReader. (io/reader input))]
       (clojure.edn/read r)))))

(def cli-options
  [["-s"
    "--sources=SOURCES"
    "Concatenated source types"
    :parse-fn (comp vec parse-kws)
    :default [:clojars :mvn]
    :default-desc ":clojars:mvn"]

   ["-R"
    "--rank=METHOD"
    "Ranking method - :fuzzy, :regex"
    :parse-fn (comp first parse-kws)
    :default :fuzzy
    :default-desc ":fuzzy"]
   
   ["-F"
    "--format=FORMAT"
    "Format for printing results - :deps, :merged, :table"
    :parse-fn (comp first parse-kws) 
    :default :deps
    :default-desc ":deps"]

   ["-l"
    "--limit=NUM"
    "Limit per-search results to NUM"
    :parse-fn #(some-> % str Long/parseLong)
    :default 1]
   
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["find-deps"
        ""
        "Usage: [options*] [search-strings*]"
        ""
        "Options:"
        options-summary
        ""]
       (str/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn validate-args
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}
      
      errors 
      {:exit-message (error-msg errors)}
      
      (and (<= 1 (count arguments)))
      {:search-strings arguments :options options}
      
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defmulti apply-opts (fn [opt vals ctx] opt))

(defmethod apply-opts :sources
  [_ sources search-strings]
  (zipmap search-strings (pmap #(search/query % sources) search-strings)))

(defmethod apply-opts :rank
  [_ rank ctx]
  (reduce-kv (fn [m search-string results]
               (assoc m search-string
                      (case rank
                        :fuzzy (rank/fuzzy search-string results)
                        :regex (rank/regex search-string results)))) {} ctx))

(defmethod apply-opts :limit
  [_ limit ctx]
  (reduce-kv (fn [m search-string results]
               (assoc m search-string
                      (rank/limit limit results))) {} ctx))

(defmethod apply-opts :format
  [_ fmt ctx]
  (let [deps (->> ctx
                  vals
                  (apply interleave))]
    (case fmt
      :deps   (merge-with merge
                          (get-deps-edn-stream)
                          {:deps (into {} deps)})
      :merged (merge-with merge
                          (get-deps-edn-file)
                          {:deps (into {} deps)}) 
      :table  (clojure.pprint/print-table
               (map (fn [[k v]]
                      {:lib k :version (:mvn/version v)}) deps)))))

(defn query*
  "Perform query"
  {:arglists '([search-strings* opts])}
  [& f]
  (let [opts (last f)
        strs (butlast f)]
    (reduce
     (fn [m o]
       (apply-opts o (get opts o) m))
     strs
     [:sources :rank :limit :format])))

(defn deps
  "Given one or more search strings, return the top matches for each search string
  merged into an empty deps.edn map."
  [& search-strings]
  (apply query* (conj (vec search-strings)
                      {:sources [:clojars :mvn]
                       :rank :fuzzy
                       :limit 1
                       :format :deps})))

(defn merged-deps
  "Given one or more search strings, return this project's deps.edn map with the
  top matches for each search string merged into the :deps key."
  [& search-strings]
  (apply query* (conj (vec search-strings)
                      {:sources [:clojars :mvn]
                       :rank :fuzzy
                       :limit 1
                       :format :merged})))

(defn print-deps
  "Given one or more search strings, print the top match for each string in a table."
  [& search-strings]
  (apply query* (conj (vec search-strings)
                      {:sources [:clojars :mvn]
                       :rank :fuzzy
                       :limit 1
                       :format :table})))

(defn print-all-deps
  "Given one or more search strings, print all available matches in a table."
  [& search-strings]
  (apply query* (conj (vec search-strings)
                      {:sources [:clojars :mvn]
                       :rank :fuzzy
                       :limit 100
                       :format :table})))

(comment
  (query* "http-kit" "pedestal/interceptor"
          {:sources [:clojars :mvn]
           :rank :fuzzy
           :limit 1
           :format :merged})

  (deps "http-kit")

  (merged-deps "http-kit" "pedestal/interceptor")

  (print-deps "http-kit" "pedestal/interceptor")

  (print-all-deps "http-kit" "pedestal/interceptor")

  (apply query* ["http-kit"
    {:sources [:clojars :mvn], :rank :fuzzy, :format :deps, :limit 1}])

  (apply query* ["kafka"
                 {:sources [:mvn], :rank :regex, :format :deps, :limit 1}])
  )

(defn -main
  [& args]
  ;; stuff something in to stdout so other invocations connected with unix pipes
  ;; detect that they have a stdin stream available.
  (print "")
  (flush)
  (let [{:keys [search-strings options exit-message ok?]} (validate-args args)]
    #_(clojure.pprint/pprint (conj (vec search-strings) options))
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (binding [*print-namespace-maps* false]
        (let [result (apply query* (conj (vec search-strings) options))]
          (when-not (some-> options :format #{:table})
            (clojure.pprint/pprint result)))))
    (shutdown-agents)
    (flush)))
