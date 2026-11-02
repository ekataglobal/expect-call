(ns whitepages.expect-call-hooks-test
  "Tests for the exported clj-kondo config and hook, run through clj-kondo's API."
  (:require [clj-kondo.core :as clj-kondo]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def config-dir "resources/clj-kondo.exports/whitepages/expect-call")

(def preamble
  ["(ns fixture"
   "  (:require [whitepages.expect-call :refer [expect-call with-expect-call]])"
   "  (:import (java.util Locale)))"
   "(defn f [& _])"
   "(defn g [& _])"
   "(def gvar 1)"])

(defn lint
  "Lints src-lines appended to preamble, with the exported config and some
  off-by-default linters enabled. Returns findings with :row relative to the
  first of src-lines."
  [& src-lines]
  (let [{:keys [findings]}
        (with-in-str (str/join "\n" (concat preamble src-lines))
          (clj-kondo/run! {:lint ["-"]
                           :lang :clj
                           :repro true
                           :cache false
                           :config-dir config-dir
                           :config [{:linters {:shadowed-var {:level :warning}
                                               :redundant-fn-wrapper {:level :warning}
                                               :redundant-ignore {:level :warning}
                                               :used-underscored-binding {:level :warning}
                                               ;; preamble noise
                                               :unused-namespace {:level :off}
                                               :unused-referred-var {:level :off}
                                               :unused-import {:level :off}}}]}))]
    (mapv #(-> %
               (select-keys [:type :level :row :col :message])
               (update :row - (count preamble)))
          findings)))

(defn summarize [findings]
  (mapv (juxt :type :level :row :col) findings))

(defn- is-clean [& src-lines]
  (is (= [] (apply lint src-lines))))

(deftest var-wildcard
  (testing "non-local symbols resolving to vars are reported as wildcards"
    (let [findings (lint "(expect-call (f [gvar name *ns*]) (f 1 2 3))")]
      (is (= [[:expect-call/var-wildcard :warning 1 18]
              [:expect-call/var-wildcard :warning 1 23]
              [:expect-call/var-wildcard :warning 1 28]]
             (summarize findings)))
      (is (str/includes? (:message (first findings)) "fixture/gvar"))
      (is (str/includes? (:message (second findings)) "clojure.core/name"))))

  (testing "nested pattern positions are reported"
    (is (= [[:expect-call/var-wildcard :warning 1 22]
            [:expect-call/var-wildcard :warning 1 31]
            [:expect-call/var-wildcard :warning 1 38]]
           (summarize (lint "(expect-call (f [{:a gvar} [1 gvar] (gvar :guard pos?)]) (f))")))))

  (testing "flattened :<< is not an operator: its operand is a pattern"
    (is (= [[:expect-call/var-wildcard :warning 1 24]]
           (summarize (lint "(expect-call (f [x :<< gvar 3] x) (f 1 :<< 2 3))")))))

  (testing "with-expect-call and multiple expectations"
    (is (= [[:expect-call/var-wildcard :warning 1 24]
            [:expect-call/var-wildcard :warning 2 24]]
           (summarize (lint "(with-expect-call [(f [gvar])"
                            "                   (g [gvar])] (f 1) (g 1))")))))

  (testing "used in body: no :shadowed-var or :redundant-ignore on top of the wildcard finding"
    (is (= [[:expect-call/var-wildcard :warning 1 18]]
           (summarize (lint "(expect-call (f [gvar] gvar) (f 1))")))))

  (testing "can be ignored"
    (is-clean "#_{:clj-kondo/ignore [:expect-call/var-wildcard]}"
              "(expect-call (f [gvar]) (f 1))")))

(deftest clean-patterns
  (testing "locals are compared by value"
    (is-clean "(let [x 1 y 2] (expect-call (f [x (:or y 3) {:a x}]) (f 1 2 {:a 1})))")
    (is-clean "#_{:clj-kondo/ignore [:shadowed-var]} (let [name 1 gvar 2] (expect-call (f [name gvar]) (f 1 2)))"))

  (testing "underscore wildcards"
    (is-clean "(expect-call (f [_ _ _gvar]) (f 1 2 3))"))

  (testing "no args pattern"
    (is-clean "(expect-call [(f) (:more f) (:never g)] (f 1))"))

  (testing "rest args"
    (is-clean "(expect-call (f [1 & more] more) (f 1 2))"))

  (testing ":guard, :when and :<< operands are expressions, not patterns"
    (is-clean "(expect-call [(f [(_ :guard #(= % gvar))])"
              "              (f [(x :guard pos?)] x)"
              "              (f [(y :when even?)] y)"
              "              (f [z :guard pos? w :when even?] [z w])"
              "              (f [(2 :<< inc)])]"
              "  (f 1) (f 1) (f 2) (f 1) (f 1 2))"))

  (testing ":or and :as"
    (is-clean "(expect-call [(f [(:or 1 2)])"
              "              (f [([a b] :as v)] [a b v])]"
              "  (f 1) (f [1 2]))"))

  (testing "map and vector destructuring"
    (is-clean "(expect-call (f [{:a a :b {:c c}} [d [e]]] [a c d e]) (f {:a 1 :b {:c 2}} [3 [4]]))"))

  (testing "literals"
    (is-clean "(expect-call (f [:k \"s\" 1 nil true {:a :b}]) (f :k \"s\" 1 nil true {:a :b}))"))

  (testing "a body forwarding the bindings is not a redundant fn wrapper"
    (is-clean "(expect-call (f [r cb] (g r cb)) (f 1 2))")))

(deftest map-keys
  (testing "non-keyword map pattern keys are lookup expressions, not bindings"
    (is-clean "(let [k :x] (expect-call (f [{k 1 Locale/US 2 \"s\" 3 1 v}] v) (f {})))"))

  (testing "they are still analyzed as expressions"
    (is (= [[:unresolved-symbol :error 1 19]]
           (summarize (lint "(expect-call (f [{undefined-key 1}]) (f {}))")))))

  (testing "map values next to non-keyword keys are still patterns"
    (is (= [[:expect-call/var-wildcard :warning 1 29]]
           (summarize (lint "(expect-call (f [{Locale/US gvar}]) (f {}))"))))))

(deftest genuine-findings-kept
  (testing "unused pattern bindings that don't shadow a var are still reported"
    (is (= [[:unused-binding :warning 1 18]]
           (summarize (lint "(expect-call (f [x]) (f 1))"))))))

(deftest validation
  (testing "non-vector arg pattern"
    (is (= [[:expect-call/args :error 1 17]]
           (summarize (lint "(expect-call (f gvar) (f 1))"))))))
