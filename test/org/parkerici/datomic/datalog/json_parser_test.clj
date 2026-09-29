(ns org.parkerici.datomic.datalog.json-parser-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.data :as data]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [org.parkerici.datomic.datalog.json-parser :as sut]))


(def resource-dir
  (io/file (io/resource "resources")))

(defn json+edn-pairs
  "This helper functions gets all the edn and json queries in test/resources
  and provides a map representation of each example query pair.

  suffix allows filtering by '-q' or '-rules' prior to edn/json extension."
  [suffix]
  (let [json-files (->> (.listFiles ^java.io.File resource-dir)
                        (map str)
                        (filter #(str/ends-with? % (str suffix ".json")))
                        (sort))]
    (for [jsonf json-files
          :let [ednf (str/replace jsonf #"\.json$" ".edn")]]
      {:file jsonf
       :json (-> jsonf slurp json/read-str)
       :edn (when (.exists (io/file ednf))
              (edn/read-string (slurp ednf)))})))

(defn check-exemplars [suffix parse-fn]
  (let [pairs (json+edn-pairs suffix)]
    (is (seq pairs) (str "no " suffix " exemplars found in " resource-dir))
    (doseq [{:keys [file json edn]} pairs]
      (testing (str "parsing: " file)
        (is (some? edn) "missing .edn counterpart for .json exemplar")
        (let [parsed (parse-fn json)]
          (is (= edn parsed)
              (let [[parsed-only edn-only] (data/diff parsed edn)]
                {:parsed (with-out-str (pp/pprint parsed))
                 :json-q-diff (with-out-str (pp/pprint parsed-only))
                 :edn-q-diff (with-out-str (pp/pprint edn-only))})))))))

(deftest every-exemplar-is-paired
  (let [names (map #(.getName ^java.io.File %) (.listFiles ^java.io.File resource-dir))
        stems (fn [ext] (->> names
                             (filter #(str/ends-with? % ext))
                             (map #(subs % 0 (- (count %) (count ext))))
                             (set)))]
    (is (= (stems ".json") (stems ".edn")))))

(deftest exemplar-q-tests
  (check-exemplars "-q" sut/parse-q))

(deftest exemplar-rule-tests
  (check-exemplars "-rules" sut/parse-rules))

(deftest lexer
  (testing "prefixed strings are coerced"
    (is (= '?x (sut/str-parse "?x")))
    (is (= :a/b (sut/str-parse ":a/b")))
    (is (= :find (sut/str-parse ":find")))
    (is (= '$ (sut/str-parse "$")))
    (is (= '$src (sut/str-parse "$src")))
    (is (= '_ (sut/str-parse "_")))
    (is (= '% (sut/str-parse "%")))
    (is (= '... (sut/str-parse "..."))))
  (testing "coerced values have the right types"
    (is (symbol? (sut/str-parse "?x")))
    (is (keyword? (sut/str-parse ":a/b")))
    (is (= "a" (namespace (sut/str-parse ":a/b")))))
  (testing "other strings are left alone"
    (doseq [s ["" "?" ":" "hello" "_yeah" "%x" ".." "a?b" "a:b" "x_" "(?i)"]]
      (is (= s (sut/str-parse s)) (pr-str s)))))

(deftest whitespace-in-symbols-and-keywords
  (doseq [s [":a b" "?a b" ":a\tb" "$a b"]]
    (let [ex (try (sut/str-parse s) nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) (pr-str s))
      (is (= s (::sut/bad-string (ex-data ex)))))))

(deftest keywords-must-read-exactly
  (doseq [s ["::foo" "::a/b" ":a,b" ":a;b" ":a(b)" ":a[b]" ":a\"b" ":a/b/"]]
    (let [ex (try (sut/str-parse s) nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) (pr-str s))
      (is (= s (::sut/bad-string (ex-data ex))) (pr-str s))))
  (testing "in a query"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/parse-q {":find" ["?e"] ":where" [["?e" "::name" "?n"]]}))))
  (testing "valid keywords still parse"
    (doseq [s [":a" ":a/b" ":a.b/c-d" ":db/txInstant" ":a/b?" ":a-b_c/d!" ":a#b" ":a'b" ":1"]]
      (is (= s (str (sut/str-parse s))) s))))

(deftest clauses-starting-with-unqualified-keywords-throw
  (doseq [q [{":find" ["?d"] ":where" [[":foo" ":db/doc" "?d"]]}
             {":find" ["?e"] ":where" [["?e" ":a/b" "?v"] ["not" [":foo" ":a/b" "?v"]]]}
             {":find" ["?e"] ":where" [["or" ["?e" ":a/b" 1] [":foo" ":a/c" "?e"]]]}]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unqualified keyword"
                          (sut/parse-q q))
        (pr-str q)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unqualified keyword"
                        (sut/parse-rules [[["r" "?d"] [":foo" ":db/doc" "?d"]]])))
  (testing "namespaced idents can start a clause"
    (is (= '{:find [?d] :where [[:my/ident :db/doc ?d]]}
           (sut/parse-q {":find" ["?d"] ":where" [[":my/ident" ":db/doc" "?d"]]}))))
  (testing "unqualified keywords elsewhere in a clause are fine"
    (is (= '{:find [?e] :where [[?e :a/b :foo]]}
           (sut/parse-q {":find" ["?e"] ":where" [["?e" ":a/b" ":foo"]]})))))

(deftest find-specs
  (testing "pull pattern with nested map and ..."
    (is (= '{:find [(pull ?e [* {:a/ref [:a/name]} {:a/parent ...}])]}
                 (sut/parse-q {":find" [["pull" "?e" ["*" {":a/ref" [":a/name"]}
                                                          {":a/parent" "..."}]]]}))))
  (testing "multiple aggregates alongside a plain var"
    (is (= '{:find [?g (count ?e) (avg ?v)]}
                 (sut/parse-q {":find" ["?g" ["count" "?e"] ["avg" "?v"]]}))))
  (testing "find-coll"
    (is (= '{:find [[?e ...]]}
                 (sut/parse-q {":find" [["?e" "..."]]}))))
  (testing "find-scalar"
    (is (= '{:find [?e .]}
           (sut/parse-q {":find" ["?e" "."]})))
    (is (symbol? (second (:find (sut/parse-q {":find" ["?e" "."]})))))
    (is (= '{:find [(count ?e) .]}
           (sut/parse-q {":find" [["count" "?e"] "."]})))
    (is (= '{:find [(pull ?e [*]) .]}
           (sut/parse-q {":find" [["pull" "?e" ["*"]] "."]}))))
  (testing "a . elsewhere in a query is left as a string"
    (is (= '{:find [?e] :where [[?e :a/b "."]]}
           (sut/parse-q {":find" ["?e"] ":where" [["?e" ":a/b" "."]]})))))

(deftest invalid-where-expressions-throw
  (let [ex (try (sut/parse-q {":find" ["?x"]
                              ":where" [[[">" ["nested" "coll"] "?x"]]]})
                nil
                (catch clojure.lang.ExceptionInfo e e))]
    (is (some? ex))
    (is (= '> (:expression-fn (ex-data ex))))
    (is (some? (:explain-data (ex-data ex))))))

(deftest non-whitelisted-where-fns-throw
  (doseq [f ["launch-missiles" "clojure.core/eval" "java.lang.System/exit"]]
    (let [ex (try (sut/parse-q {":find" ["?x"]
                                ":where" [["?e" ":a/b" "?x"]
                                          [[f "?x"]]]})
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) f)
      (is (= f (:expression-fn (ex-data ex))))
      (is (str/includes? (ex-message ex) "whitelist")))))

(deftest invalid-rules-throw
  (doseq [bad [[["no-clauses" "?x"]]
               "not a rule"]]
    (is (thrown? clojure.lang.ExceptionInfo (sut/parse-rules bad)) (pr-str bad))))

(comment
  (run-tests *ns*))

(comment
  :test-parsing
  (def from-json (->> (io/file "test/resources/underscore-q.json")
                      (slurp)
                      (json/read-str)))
  (prn (sut/parse-q from-json)))
