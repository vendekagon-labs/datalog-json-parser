(ns org.parkerici.datomic.datalog.json-parser-gen
  "test.check generators for datalog queries and rules in edn, and an encoder
  that turns them into the plain JSON a client would send."
  (:require [clojure.test.check.generators :as gen]
            [clojure.data.json :as json]
            [clojure.walk :as walk]))

(defn ->json
  "Encodes edn query data as a client would: symbols and keywords as strings,
  lists as arrays. Round-trips through JSON text to be faithful to the wire."
  [form]
  (-> (walk/postwalk (fn [v]
                       (cond
                         (or (symbol? v) (keyword? v)) (str v)
                         (seq? v) (vec v)
                         :else v))
                     form)
      (json/write-str)
      (json/read-str)))

(def gen-var
  (gen/elements '[?a ?b ?c ?d ?e]))

(def gen-attr
  (gen/elements [:artist/name :release/name :release/year :release/artists
                 :track/name :gene/hugo :gene/coordinates]))

(def gen-string-constant
  ;; strings starting with ? : $ _ % or equal to ... are documented as unsupported
  (gen/fmap (fn [[c s]] (str c s))
            (gen/tuple gen/char-alpha gen/string-alphanumeric)))

(def gen-constant
  (gen/one-of [gen/small-integer gen-string-constant]))

(def gen-data-pattern
  (gen/let [e (gen/one-of [gen-var (gen/return '_)])
            a gen-attr
            v (gen/one-of [gen-var gen-constant (gen/return '_)])
            full? gen/boolean]
    (if full? [e a v] [e a])))

(def gen-predicate
  (gen/let [op (gen/elements '[> < >= <= = !=])
            x gen-var
            y (gen/one-of [gen-var gen/small-integer])]
    [(list op x y)]))

(def gen-fn-expr
  (gen/one-of
    [(gen/let [op (gen/elements '[+ - *]) x gen-var n gen/small-integer out gen-var]
       [(list op x n) out])
     (gen/let [x gen-var s gen-string-constant out gen-var]
       [(list 'str s x) out])
     (gen/let [e gen-var a gen-attr d gen-constant out gen-var]
       [(list 'get-else '$ e a d) out])
     (gen/let [e gen-var a gen-attr]
       [(list 'missing? '$ e a)])
     (gen/let [c gen-constant out gen-var]
       [(list 'ground c) out])
     (gen/let [x gen-var y gen-var out gen-var]
       [(list 'tuple x y) out])
     (gen/let [t gen-var x gen-var y gen-var]
       [(list 'untuple t) [x y]])]))

(def gen-rule-expr
  (gen/let [rule-name (gen/elements '[ancestor my-rule social-media])
            args (gen/vector (gen/one-of [gen-var gen-constant]) 1 3)]
    (apply list rule-name args)))

(defn gen-clause
  "Where clause generator. Rule invocations are optional so that generated
  queries can be run without having to supply rules."
  [{:keys [rules?]}]
  (let [leaf (gen/frequency (cond-> [[5 gen-data-pattern]
                                     [2 gen-predicate]
                                     [2 gen-fn-expr]]
                              rules? (conj [1 gen-rule-expr])))]
    (gen/recursive-gen
      (fn [inner]
        (gen/one-of
          [(gen/let [op (gen/elements '[or not])
                     clauses (gen/vector inner 1 3)]
             (into [op] clauses))
           (gen/let [op (gen/elements '[or-join not-join])
                     vars (gen/vector-distinct gen-var {:min-elements 1 :max-elements 3})
                     clauses (gen/vector inner 1 3)]
             (into [op vars] clauses))
           (gen/let [clauses (gen/vector inner 1 3)]
             ;; and is only valid inside or / or-join
             ['or (into ['and] clauses) (into ['and] clauses)])]))
      leaf)))

(def gen-find-elem
  (gen/one-of
    [gen-var
     (gen/let [agg (gen/elements '[count count-distinct distinct sum min max avg median
                                   variance stddev])
               v gen-var]
       (list agg v))
     (gen/let [v gen-var
               pattern (gen/elements '[[*] [:artist/name] [* {:release/artists [:artist/name]}]])]
       (list 'pull v pattern))]))

(defn gen-query [opts]
  (gen/let [find (gen/one-of [(gen/vector gen-find-elem 1 3)
                              ;; find-scalar
                              (gen/fmap #(vector % '.) gen-find-elem)])
            in (gen/vector gen-var 0 2)
            with (gen/vector gen-var 0 1)
            where (gen/vector (gen-clause opts) 1 4)]
    (cond-> {:find find
             :in (vec (concat (if (:rules? opts) '[$ %] '[$]) in))
             :where where}
      (seq with) (assoc :with with))))

(def gen-rules
  (gen/vector
    (gen/let [rule-name (gen/elements '[ancestor my-rule social-media])
              vars (gen/vector-distinct gen-var {:min-elements 1 :max-elements 3})
              clauses (gen/vector (gen-clause {:rules? true}) 1 3)]
      (into [(apply list rule-name vars)] clauses))
    1 3))

(def gen-json-junk
  "Arbitrary JSON-shaped data, biased toward strings the parser treats specially."
  (gen/recursive-gen
    (fn [inner] (gen/one-of [(gen/vector inner 0 5)
                             (gen/map (gen/elements [":find" ":where" ":in" ":with" "x"]) inner
                                      {:max-elements 4})]))
    (gen/one-of
      [(gen/elements ["?a" "?b" ":a/b" ":find" ":where" "$" "$src" "_" "%" "..." "." "*"
                      "pull" "count" "or" "or-join" "not" "not-join" "and" ">" "str"
                      "missing?" "ground" "tuple" "untuple" "foo" "a/b" "?" ":" ""
                      "?a b" ":a b" "::kw" ":a/b/c"])
       gen/small-integer gen/boolean (gen/return nil) gen/string-alphanumeric])))

;; -- queries valid against the synthetic data in json-parser-datomic-test --
;; Base clauses bind every var, so generated clauses can use them freely:
;; ?r release, ?n release name, ?y release year, ?a artist, ?an artist name.

(def base-clauses
  '[[?r :release/name ?n]
    [?r :release/year ?y]
    [?r :release/artists ?a]
    [?a :artist/name ?an]])

(def gen-year (gen/elements [1965 1966 1969 1970 1971]))
(def gen-release-name (gen/elements ["Let It Be" "Abbey Road" "Blonde on Blonde" "Nope"]))
(def gen-artist-name (gen/elements ["The Beatles" "Bob Dylan" "Nobody"]))

(def gen-release-fact
  "A clause about ?r alone, possibly negated."
  (gen/recursive-gen
    (fn [inner]
      (gen/fmap (fn [cs] (into '[not] cs)) (gen/vector inner 1 2)))
    (gen/one-of
      [(gen/fmap (fn [y] ['?r :release/year y]) gen-year)
       (gen/fmap (fn [n] ['?r :release/name n]) gen-release-name)])))

(def gen-or-branch
  ;; and is only valid directly as an or / or-join branch
  (gen/one-of
    [gen-release-fact
     (gen/fmap (fn [cs] (into '[and] cs))
               (gen/vector gen-release-fact 1 2))]))

(def gen-valid-clause
  (gen/one-of
    [(gen/let [op (gen/elements '[> < >= <= = !=]) y gen-year]
       [(list op '?y y)])
     (gen/let [s gen/string-alphanumeric]
       [(list 'str '?n s) '?s])
     (gen/let [d gen/small-integer]
       [(list 'get-else '$ '?r :release/year d) '?g])
     (gen/let [op (gen/elements '[+ - *]) n gen/small-integer]
       [(list op '?y n) '?z])
     (gen/return '[(missing? $ ?r :gene/hugo)])
     (gen/fmap (fn [y] [(list 'ground y) '?y]) gen-year)
     (gen/return '[(tuple ?n ?y) ?t])
     (gen/fmap (fn [n] ['?a :artist/name n]) gen-artist-name)
     (gen/let [branches (gen/vector gen-or-branch 2 3)]
       (into '[or] branches))
     (gen/let [branches (gen/vector gen-or-branch 1 3)]
       (into '[or-join [?r]] branches))
     (gen/fmap (fn [cs] (into '[not] cs)) (gen/vector gen-release-fact 1 2))
     (gen/fmap (fn [cs] (into '[not-join [?r]] cs)) (gen/vector gen-release-fact 1 2))]))

(def valid-find-elems
  "Find elements for each var. Datomic rejects e.g. [:find ?a (pull ?a ...)],
  so a query uses each var at most once."
  '{?r [?r (count ?r) (pull ?r [*])]
    ?n [?n (distinct ?n) (count-distinct ?n)]
    ?y [?y (min ?y) (max ?y) (sum ?y) (avg ?y)]
    ?a [?a (count-distinct ?a) (pull ?a [:artist/name])]
    ?an [?an]})

(def gen-valid-query
  (gen/let [vars (gen/shuffle (keys valid-find-elems))
            n-find (gen/choose 1 3)
            with? gen/boolean
            scalar? (gen/frequency [[3 (gen/return false)] [1 (gen/return true)]])
            find (apply gen/tuple (map #(gen/elements (valid-find-elems %))
                                       (take (if scalar? 1 n-find) vars)))
            clauses (gen/vector gen-valid-clause 0 4)]
    (cond-> {:find (if scalar? (conj find '.) find)
             :in '[$]
             :where (into base-clauses clauses)}
      ;; a :with var must not also appear in :find
      with? (assoc :with [(nth vars n-find)]))))
