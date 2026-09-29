(ns org.parkerici.datomic.datalog.json-parser-datomic-test
  "Runs parsed exemplar queries against an in-memory Datomic database seeded
  with synthetic data, and checks they return the same (non-empty) results as
  the hand-written edn version of each query."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [datomic.api :as d]
            [org.parkerici.datomic.datalog.json-parser :as sut]))

(def schema
  [{:db/ident :artist/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :release/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :release/year :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
   {:db/ident :release/artists :db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   {:db/ident :track/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :track/artists :db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   {:db/ident :gene/hugo :db/valueType :db.type/string :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :gene/coordinates :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :variant/coordinates :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :measurement/gene :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :measurement/value :db/valueType :db.type/double :db/cardinality :db.cardinality/one}
   {:db/ident :community/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :community/type :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :community.type/twitter}
   {:db/ident :community.type/facebook-page}
   {:db/ident :community.type/blog}])

(def data
  [{:db/id "beatles" :artist/name "The Beatles"}
   {:db/id "dylan" :artist/name "Bob Dylan"}
   {:artist/name "Unreleased Band"}
   {:release/name "Let It Be" :release/year 1970 :release/artists ["beatles"]}
   {:release/name "Abbey Road" :release/year 1969 :release/artists ["beatles"]}
   {:release/name "Blonde on Blonde" :release/year 1966 :release/artists ["dylan"]}
   {:track/name "Outro" :track/artists ["beatles"]}
   {:track/name "Intro" :track/artists ["dylan"]}
   {:db/id "arg2" :gene/hugo "ARG2" :gene/coordinates "chr14:67619778"}
   {:gene/hugo "TP53"}
   {:variant/coordinates "chr17:7676154"}
   {:measurement/gene "arg2" :measurement/value 1.0}
   {:measurement/gene "arg2" :measurement/value 2.0}
   {:measurement/gene "arg2" :measurement/value 2.0}
   {:community/name "tweets" :community/type :community.type/twitter}
   {:community/name "fb" :community/type :community.type/facebook-page}
   {:community/name "posts" :community/type :community.type/blog}])

(def ^:dynamic *db* nil)

(defn with-db [f]
  (let [uri (str "datomic:mem://json-parser-test-" (random-uuid))]
    (d/create-database uri)
    (try
      (let [conn (d/connect uri)]
        @(d/transact conn schema)
        @(d/transact conn data)
        (binding [*db* (d/db conn)]
          (f)))
      (finally
        (d/delete-database uri)))))

(use-fixtures :once with-db)

;; rules are passed as edn here: this exercises parsing of the %-taking query,
;; while the rule parser is exercised by the social-media rules test below.
(def entity-at-rules
  '[[(entity-at ?e ?tx ?t ?inst)
     [?e _ _ ?tx]
     [(datomic.api/tx->t ?tx) ?t]
     [?tx :db/txInstant ?inst]]])

(def exemplar-inputs
  "Query inputs for each exemplar in test/resources, keyed by file stem.
  :allow-empty marks queries that can't be made to match synthetic data."
  {"eq-q" {:inputs (fn [db] [db])}
   "ground-q" {:inputs (fn [db] [db])}
   "missing-q" {:inputs (fn [db] [db])}
   "not-join-q" {:inputs (fn [db] [db])}
   "nowhere-q" {:inputs (fn [db] [db (d/entid db [:gene/hugo "ARG2"])])}
   "or-q" {:inputs (fn [db] [db])}
   "pull-q" {:inputs (fn [db] [db "ARG2"])}
   "re-q" {:inputs (fn [db] [db "beatles"])}
   "scalar-q" {:inputs (fn [db] [db "Bob Dylan"])}
   ;; hard-coded entity id from a production db
   "single-clause-q" {:inputs (fn [db] [db]) :allow-empty true}
   "time-rule-q" {:inputs (fn [db] [db entity-at-rules "The Beatles"])}
   "tuple-q" {:inputs (fn [_] [1 2])}
   "underscore-q" {:inputs (fn [db] [db])}
   "untuple-q" {:inputs (fn [_] [[1 2]])}
   "with-q" {:inputs (fn [db] [db])}})

(defn exemplar-stems []
  (->> (.listFiles (io/file (io/resource "resources")))
       (map #(.getName ^java.io.File %))
       (filter #(str/ends-with? % "-q.json"))
       (map #(str/replace % #"\.json$" ""))
       (sort)))

(defn read-resource [f]
  (slurp (io/resource (str "resources/" f))))

(deftest every-query-exemplar-runs-against-datomic
  (is (= (set (exemplar-stems)) (set (keys exemplar-inputs)))
      "each -q exemplar needs an entry in exemplar-inputs")
  (doseq [stem (exemplar-stems)
          :let [{:keys [inputs allow-empty]} (get exemplar-inputs stem)]
          :when inputs]
    (testing stem
      (let [args (inputs *db*)
            parsed (sut/parse-q (json/read-str (read-resource (str stem ".json"))))
            expected (apply d/q (edn/read-string (read-resource (str stem ".edn"))) args)
            actual (apply d/q parsed args)]
        (is (= expected actual))
        (when-not allow-empty
          (is (seq actual) "query should match the synthetic data"))))))

(deftest spot-check-results
  (let [run (fn [json-q & args]
              (apply d/q (sut/parse-q (json/read-str json-q)) *db* args))]
    (is (= #{["Let It Be" 1970] ["Abbey Road" 1969]}
           (run (read-resource "re-q.json") "beatles")))
    (is (= [[2]] (run (read-resource "not-join-q.json")))
        "Bob Dylan and Unreleased Band have no 1970 release")
    (is (= [[3]] (run (read-resource "with-q.json")))
        ":with keeps duplicate measurement values from collapsing")
    (is (= #{["TP53"]} (run (read-resource "missing-q.json"))))
    (is (= "Blonde on Blonde" (run (read-resource "scalar-q.json") "Bob Dylan"))
        "find-scalar returns a single value")
    (testing "comparison predicates"
      (is (= #{["Abbey Road"] ["Let It Be"]}
             (run "{\":find\": [\"?n\"],
                    \":where\": [[\"?r\", \":release/year\", \"?y\"],
                                 [[\">\", \"?y\", 1968]],
                                 [\"?r\", \":release/name\", \"?n\"]]}"))))
    (testing "get-else and arithmetic"
      (is (= #{["ARG2" "chr14:67619778"] ["TP53" "unknown"]}
             (run "{\":find\": [\"?h\", \"?c\"],
                    \":where\": [[\"?g\", \":gene/hugo\", \"?h\"],
                                 [[\"get-else\", \"$\", \"?g\", \":gene/coordinates\", \"unknown\"], \"?c\"]]}")))
      (is (= #{[1971]}
             (run "{\":find\": [\"?next\"],
                    \":where\": [[\"?r\", \":release/name\", \"Let It Be\"],
                                 [\"?r\", \":release/year\", \"?y\"],
                                 [[\"+\", \"?y\", 1], \"?next\"]]}"))))
    (testing "or-join with required bindings"
      (is (= #{["Blonde on Blonde"]}
             (run "{\":find\": [\"?n\"],
                    \":where\": [[\"?a\", \":artist/name\", \"Bob Dylan\"],
                                 [\"or-join\", [[\"?a\"], \"?r\"],
                                   [\"?r\", \":release/artists\", \"?a\"]],
                                 [\"?r\", \":release/name\", \"?n\"]]}"))))
    (testing "or-join with and branches"
      (is (= #{["The Beatles"] ["Bob Dylan"]}
             (run "{\":find\": [\"?n\"],
                    \":where\": [[\"?a\", \":artist/name\", \"?n\"],
                                 [\"or-join\", [\"?a\"],
                                   [\"and\", [\"?t\", \":track/artists\", \"?a\"],
                                             [\"?t\", \":track/name\", \"Outro\"]],
                                   [\"and\", [\"?r\", \":release/artists\", \"?a\"],
                                             [\"?r\", \":release/year\", 1966]]]]}"))))))

(deftest parsed-rules-run-against-datomic
  (let [q '[:find ?name :in $ % :where [?c :community/name ?name] (social-media ?c)]
        parsed-rules (sut/parse-rules (json/read-str (read-resource "social-media-rules.json")))
        edn-rules (edn/read-string (read-resource "social-media-rules.edn"))]
    (is (= #{["tweets"] ["fb"]}
           (d/q q *db* parsed-rules)
           (d/q q *db* edn-rules)))))

(deftest parsed-rules-with-expressions-in-body
  (let [q '[:find ?n :in $ % :where [?r :release/name ?n] (recent-beatles ?r)]
        json-rules "[[[\"recent\", \"?r\"],
                      [\"?r\", \":release/year\", \"?y\"],
                      [[\">\", \"?y\", 1968]]],
                     [[\"recent-beatles\", \"?r\"],
                      [\"recent\", \"?r\"],
                      [\"?r\", \":release/artists\", \"?a\"],
                      [\"or\", [\"?a\", \":artist/name\", \"The Beatles\"],
                               [\"?a\", \":artist/name\", \"The Beetles\"]],
                      [\"not\", [\"?r\", \":release/name\", \"Abbey Road\"]]]]"]
    (is (= '[[(recent ?r)
              [?r :release/year ?y]
              [(> ?y 1968)]]
             [(recent-beatles ?r)
              (recent ?r)
              [?r :release/artists ?a]
              [or [?a :artist/name "The Beatles"]
                  [?a :artist/name "The Beetles"]]
              [not [?r :release/name "Abbey Road"]]]]
           (sut/parse-rules (json/read-str json-rules))))
    (is (= #{["Let It Be"]}
           (d/q q *db* (sut/parse-rules (json/read-str json-rules)))))
    (testing "fns in rule bodies go through the where expression whitelist"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"whitelist"
                            (sut/parse-rules [[["r" "?x"] [["launch-missiles" "?x"]]]]))))))
