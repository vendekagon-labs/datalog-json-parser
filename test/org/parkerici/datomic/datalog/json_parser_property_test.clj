(ns org.parkerici.datomic.datalog.json-parser-property-test
  (:require [clojure.test :refer [use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [datomic.api :as d]
            [org.parkerici.datomic.datalog.json-parser :as sut]
            [org.parkerici.datomic.datalog.json-parser-gen :as g]
            [org.parkerici.datomic.datalog.json-parser-datomic-test :as datomic-test]))

(use-fixtures :once datomic-test/with-db)

(defspec parsed-json-query-equals-edn-query 300
  (prop/for-all [q (g/gen-query {:rules? true})]
    (= q (sut/parse-q (g/->json q)))))

(defspec parsed-json-rules-equal-edn-rules 300
  (prop/for-all [rules g/gen-rules]
    (= rules (sut/parse-rules (g/->json rules)))))

(defn- run-q [q inputs]
  (try
    [:ok (apply d/q q inputs)]
    ;; datomic reports some invalid queries with AssertionError. Messages echo
    ;; the query (and gensyms, object hashes), so compare the kind of error only.
    (catch Throwable e
      [:error (class e) (:db/error (ex-data e))])))

(defspec parsed-json-query-gets-same-results-in-datomic 300
  (prop/for-all [q g/gen-valid-query]
    (let [expected (run-q q [datomic-test/*db*])]
      (and (= :ok (first expected))
           (= expected (run-q (sut/parse-q (g/->json q)) [datomic-test/*db*]))))))

(defspec parsed-json-query-fails-like-edn-query-in-datomic 150
  ;; Queries from the broader generator are mostly invalid (e.g. unbound vars),
  ;; so the property is that parsed and edn queries get the same result or error.
  (prop/for-all [q (g/gen-query {:rules? false})
                 input-vals (gen/vector gen/small-integer 2)]
    (let [inputs (cons datomic-test/*db* (take (dec (count (:in q))) input-vals))]
      (= (run-q q inputs)
         (run-q (sut/parse-q (g/->json q)) inputs)))))

(defspec junk-input-parses-or-throws-ex-info 1000
  (prop/for-all [junk g/gen-json-junk]
    (every? (fn [parse]
              (try (parse junk) true
                   (catch clojure.lang.ExceptionInfo _ true)
                   (catch Exception _ false)))
            [(fn [x] (sut/parse-q {":find" ["?a"] ":where" [x]}))
             (fn [x] (sut/parse-q {":find" [x] ":where" [x]}))
             (fn [x] (sut/parse-rules [x]))])))
