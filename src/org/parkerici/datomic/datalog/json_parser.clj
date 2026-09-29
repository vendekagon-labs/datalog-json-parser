(ns org.parkerici.datomic.datalog.json-parser
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :as walk]))

;; Not (yet) supported:
;; -- any where expression not in white list
;; -- custom aggregates definitions
;; -- string literals that start with ":" or "?"
;; -- generic Clojure fn or Java method calls not in whitelist

(set! *warn-on-reflection* true)

(def aggregates
  {"count" ['count ::aggregate]
   "count-distinct" ['count-distinct ::aggregate]
   "distinct" ['distinct ::aggregate]
   "sum" ['sum ::aggregate]
   "median" ['median ::aggregate]
   "avg" ['avg ::aggregate]
   "variance" ['variance ::aggregate]
   "stddev" ['stddev ::aggregate]
   "max" ['max ::aggregate]
   "min" ['min ::aggregate]
   "rand" ['rand ::aggregate]
   "sample" ['sample ::aggregate]})

(def aggregate-whitelist
  (into #{} (keys aggregates)))

(s/def ::aggregate-fn
  (into #{} (map symbol aggregate-whitelist)))

(s/def ::aggregate
  (s/cat :fn ::aggregate-fn :args (s/+ any?)))

(s/def ::raw-aggregate-expr
  (s/cat :fn aggregate-whitelist :args (s/+ any?)))

(s/def ::pull-literal
  #{"pull"})

;; don't need structural, conditional parsing to disambiguate
;; literals in pull patterns
(s/def ::pattern
  coll?)

(s/def ::raw-pull-expr
  (s/cat :pull ::pull-literal
         :var symbol?
         :pattern ::pattern))

;; the . in a find-scalar spec, e.g. [:find ?e . ...]
(s/def ::raw-scalar-marker
  #{"."})

(s/def ::resolvable-find-elem
  (s/or :pull-expr ::raw-pull-expr
        :aggr-expr ::raw-aggregate-expr
        :scalar-marker ::raw-scalar-marker))

(s/def ::fn-arg
  (complement coll?))

(s/def ::fn-expr
  (s/cat :fn symbol? :args (s/+ ::fn-arg)))

(def where-expressions
  {">" ['> ::fn-expr]
   "<" ['< ::fn-expr]
   ">=" ['>= ::fn-expr]
   "<=" ['<= ::fn-expr]
   "=" ['= ::fn-expr]
   "!=" ['!= ::fn-expr]
   "+" ['+ ::fn-expr]
   "-" ['- ::fn-expr]
   "*" ['* ::fn-expr]
   "/" ['/ ::fn-expr]
   "str" ['str ::fn-expr]
   "re-pattern" ['re-pattern ::fn-expr]
   "re-find" ['re-find ::fn-expr]
   "tuple" ['tuple ::fn-expr]
   "untuple" ['untuple ::fn-expr]
   "get-else" ['get-else ::fn-expr]
   "get-some" ['get-some ::fn-expr]
   "missing?" ['missing? ::fn-expr]
   "ground" ['ground ::fn-expr]})


(s/def ::where-expr-whitelist
  (into #{} (keys where-expressions)))

(s/def ::var
  (s/and symbol?
         #(.startsWith (str %) "?")))

(s/def ::vars (s/and sequential?
                     (s/coll-of ::var)))
(s/def ::binding (s/or :var ::var
                       :vars ::vars))


(s/def ::expression-clause
  (s/cat :expr vector? :binding (s/? ::binding)))


(s/def ::query-primitive
  (s/or
     :var ::var
     :underscore #{"_"}
     :constant (complement (or ::var #{"_"}))))

(s/def ::data-pattern
  (s/cat :data-elems (s/+ ::query-primitive)))

(def clause-types
  {"or" 'or
   "or-join" 'or-join
   "not" 'not
   "not-join" 'not-join
   "and" 'and})

(s/def ::clause-type
  (into #{} (keys clause-types)))

(s/def ::join-vars
  (s/and vector? (s/coll-of ::var :min-count 1)))

;; e.g. [[?a ?b] ?c], where ?a and ?b must be bound before the or-join
(s/def ::join-vars-with-required
  (s/and vector?
         (s/cat :required ::join-vars
                :other (s/* ::var))))

(s/def ::alternative-clause
  (s/or :or-join (s/cat :clause-type #{"or-join"}
                        :vars (s/or :vars ::join-vars
                                    :with-required ::join-vars-with-required)
                        :clauses (s/+ ::clause))
        :not-join (s/cat :clause-type #{"not-join"}
                         :vars ::join-vars
                         :clauses (s/+ ::clause))
        :or-not-and (s/cat :clause-type #{"or" "not" "and"}
                           :clauses (s/+ ::clause))))

(defn unqualified-symbol-str?
  "True if v coerces to a symbol (as by clojure.core/symbol) without a namespace."
  [v]
  (cond
    (string? v) (nil? (namespace (symbol v)))
    (symbol? v) (nil? (namespace v))
    (keyword? v) (nil? (namespace v))
    ;; vars coerce to qualified symbols; anything else doesn't coerce
    :else false))


(s/def ::rule-name unqualified-symbol-str?)

(s/def ::rule-expr
  (s/cat :rule-name ::rule-name
         :rule-args (s/+ ::query-primitive)))


(s/def ::clause
  (s/or :expression ::expression-clause
        :alternative-clause ::alternative-clause
        :rule-expr ::rule-expr
        :data-pattern ::data-pattern))


;; -- lightweight query lexer --
(defn throw-if-whitespace!
  [s]
  (when-let [whitespace (re-find #"\s" s)]
    (throw (ex-info (str "Invalid query: symbol or keyword string '"
                         s
                         "' contained whitespace.")
                    {::bad-string s
                     ::whitespace-chars whitespace}))))


(defn coerce-symbol [s]
  (throw-if-whitespace! s)
  (symbol s))

(defn coerce-kw [s]
  (throw-if-whitespace! s)
  (let [kw (try
             (read-string s)
             (catch Exception _ nil))]
    ;; the reader reads only as far as the first form, so e.g. ":a,b" would read
    ;; as :a, and "::a" would resolve against whatever namespace is current
    (if (and (keyword? kw) (= s (str kw)))
      kw
      (throw (ex-info (str "Invalid query: keyword string '" s
                           "' does not read as that keyword.")
                      {::bad-string s})))))

(def q-lex
  [[#"\?.+" coerce-symbol]
   [#"\:.+" coerce-kw]
   [#"\$.*" coerce-symbol]
   [#"\_" coerce-symbol]
   [#"\%" coerce-symbol]
   [#"\.\.\." coerce-symbol]])

(def ^:private q-lex-first-chars
  "Every q-lex regex requires one of these as the first character."
  #{\? \: \$ \_ \% \.})

(defn str-parse [^String s]
  (or (when (and (pos? (.length s))
                 (q-lex-first-chars (.charAt s 0)))
        (some (fn [[regex parse-fn]]
                (when (re-matches regex s)
                  (parse-fn s)))
              q-lex))
      s))


(defn resolve-aggregate [aggregate]
  (let [aggr-fn-str (first aggregate)
        [aggr-fn aggr-spec] (get aggregates aggr-fn-str)
        aggr-w-fn (conj (rest aggregate) aggr-fn)]
    (if (s/valid? aggr-spec aggr-w-fn)
      aggr-w-fn
      (throw (ex-info "Invalid aggregate in :find of query."
                      {:aggregate aggregate
                       :aggregate-fn aggr-fn
                       :explain-data (s/explain-data aggr-spec aggr-w-fn)})))))


(defn parse-pull-pattern
  [pull-pattern]
  (walk/postwalk
    ;; anon fn here as substitution map for replace returns (\*) instead of '* for symbol.
    (fn [v]
      (cond
        (#{"*"} v) '*
        (#{"..."} v) '...
        :else v))
    pull-pattern))

(defn resolve-pull
  "Return pull expression with pull symbol literal instead of string."
  [[_ pull-var pull-pattern]]
  (list 'pull pull-var (parse-pull-pattern pull-pattern)))

(defn resolve-find-elems
  [find-rel]
  (mapv (fn [find-elem]
          (cond
            (s/valid? ::raw-aggregate-expr find-elem) (resolve-aggregate find-elem)
            (s/valid? ::raw-pull-expr find-elem) (resolve-pull find-elem)
            (s/valid? ::raw-scalar-marker find-elem) '.
            :else find-elem))
        find-rel))

(defn resolve-where-expression [clause]
  (let [[expr binds] clause
        [f expr-spec] (get where-expressions (first expr))
        expr-w-fn (conj (rest expr) f)]
    (when-not f
      (throw (ex-info (str "Invalid :where expression clause in query: function "
                           (pr-str (first expr))
                           " is not in the where expression whitelist.")
               {:clause clause
                :expression-fn (first expr)
                :whitelist (sort (keys where-expressions))})))
    (if (s/valid? expr-spec expr-w-fn)
      ;; drops binds portion if nil
      (into [] (remove nil? [expr-w-fn binds]))
      (throw (ex-info "Invalid :where expression clause in query"
               {:clause clause
                :expression-fn f
                :explain-data (s/explain-data expr-spec expr-w-fn)})))))

(declare resolve-where-clauses)

(defn- resolve-conformed-alternative-clause
  "Resolves clause, given its value as conformed to ::alternative-clause."
  [clause conformed]
  (let [[_ {:keys [clause-type vars]}] conformed
        clause-symbol (get clause-types clause-type)
        ;; the nested clauses as given, rather than unformed from their
        ;; conformed values, which would turn vectors inside them into lists
        resolved-clauses (resolve-where-clauses (drop (if vars 2 1) clause))]
    (vec (if vars
           (concat [clause-symbol (second clause)] resolved-clauses)
           (concat [clause-symbol] resolved-clauses)))))

(defn resolve-alternative-clause [clause]
  (let [conformed (s/conform ::alternative-clause clause)]
    (if (= conformed ::s/invalid)
      (throw (ex-info "[or,not,and]?(-join) clause of invalid form."
               {:clause clause
                :explain-data (s/explain-data ::alternative-clause clause)}))
      (resolve-conformed-alternative-clause clause conformed))))

(defn resolve-rule-expr [[rule-name & rule-args]]
  (cons (symbol rule-name) rule-args))


(defn resolve-where-clauses [where-clauses]
  (mapv (fn [clause]
          ;; conform once rather than validating and then conforming again,
          ;; which repeats the work at every level of nested or/not/and
          (let [alternative (s/conform ::alternative-clause clause)]
            (cond
              (and (sequential? clause)
                   (simple-keyword? (first clause)))
              (throw (ex-info (str "Invalid :where clause in query: " (pr-str clause)
                                   " starts with an unqualified keyword, which is neither"
                                   " a rule name nor a (namespaced) entity ident.")
                              {:clause clause}))

              (not= alternative ::s/invalid) (resolve-conformed-alternative-clause clause alternative)
              ;; throws, explaining why the clause doesn't conform
              (and (sequential? clause)
                   (contains? clause-types (first clause))) (resolve-alternative-clause clause)
              (s/valid? ::expression-clause clause) (resolve-where-expression clause)
              (s/valid? ::rule-expr clause) (resolve-rule-expr clause)
              :else clause)))
        where-clauses))

(defn parse-json-tree
  "First pass parse from json str to EDN (for parsing that does not need structural context
  to be completed). Takes a json query form as clojure data and returns edn substitutions where
  they can be made."
  [q-form]
  (clojure.walk/postwalk
                 (fn [v]
                   (if (string? v)
                     (str-parse v)
                     v))
                 q-form))

(defn parse-q
  [q-form]
  (let [as-edn (parse-json-tree q-form)
        where-expressions (some (partial s/valid? ::clause) (:where as-edn))
        resolvable-find-elems (some (partial s/valid? ::resolvable-find-elem) (:find as-edn))]
    (cond-> as-edn
       where-expressions (assoc :where (resolve-where-clauses (:where as-edn)))
       resolvable-find-elems (assoc :find (resolve-find-elems (:find as-edn))))))

;; -- rule parsing --
(s/def ::rule-vars
  (s/alt :flat (s/+ ::var)
         :some-nested (s/cat :nested (s/coll-of ::var)
                             :maybe-flat (s/* ::var))))

(s/def ::rule-head
  (s/cat :rule-name ::rule-name
         :rule-vars ::rule-vars))

(s/def ::rule-def
  (s/cat :rule-head (s/spec ::rule-head)
         :rule-clauses (s/+ ::clause)))

(s/def ::rule
  (s/cat :rule-defs (s/+ (s/spec ::rule-def))))

(defn parse-rules
  [rule-form]
  (let [partially-parsed (parse-json-tree rule-form)]
    (if (s/valid? ::rule partially-parsed)
      (vec (for [[[rule-name & vars] & clauses] partially-parsed]
             (vec (concat [(cons (symbol rule-name) vars)]
                          (resolve-where-clauses clauses)))))
      (throw (ex-info "Rule definition is not valid."
               {:rule rule-form
                :explain-data (s/explain-data ::rule rule-form)})))))
