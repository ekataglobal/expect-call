(ns whitepages.expect-call-test
  (:require [clojure.test :refer :all]
            [whitepages.expect-call :as sut]
            [whitepages.expect-call.internal :as internal]))

;; These tests follow the examples in the README

(defn log [& args]
  (apply println args)
  :logged)

(defn check-error [a b]
  (when (= a :error)
    (log "ERROR:" (pr-str b))))

(defn destructuring [_x])

(defn dummy [_x])

(def dummy-expected-x 5)

(defmacro expecting-failure
  "Execute body, expecting it to report a test failure.

   It would be neat to implement this as:

   (with-expect-call ~'(report [{:type :fail}])
     ~@body)

   ...however, expect-call disables its interception hooks
   while calling (report), to avoid going into an infinite
   loop. So it doesn't work in this case. Sadly."
  [& body]
  `(let [reported?# (atom false)]
     (with-redefs [report (fn [m#]
                            (when-not m#
                              (throw (Exception. "(report) requires a parameter")))
                            (swap! reported?# #(or % m#)))]
       (try
         ~@body
         (finally
          (cond
           (not @reported?#) (report {:type :fail,
                                      :expected {:type :fail},
                                      :actual nil,
                                      :message "Expected test to fail"})
           (not= (:type @reported?#) :fail) (report @reported?#)

           :else :ok))))
     @reported?#))

(deftest mocks
  (let [make-mock internal/make-mock
        mock (eval (make-mock `(#{} log [:error ~'_] :return-value)))
        do-mock (eval (make-mock `(#{:do} log [:error ~'_])))]

    (is (= (mock :error "abc") :return-value))

    (expecting-failure
      (mock :not-an-error "abc"))

    (is (= (do-mock :error "abc") :logged) ":do mocks actually call the function")

    :ok))

(deftest readme-examples

  ;; These are patterned after (although not quite identical to) the examples
  ;; in the README.

  (testing "Basic pass"
    (sut/with-expect-call (log ["ERROR:" _])
      (check-error :error "abc")))

  (testing "Basic fail"
    (expecting-failure
      (sut/with-expect-call (log ["ERROR:" _])
        (check-error :success "abc"))))

  (testing "Omitting parameters means we don't care what they are"
    (sut/with-expect-call (log)
      (check-error :error "abc")))

  (testing "Function body executes"
    (sut/with-expect-call (log ["ERROR:" msg] (is (= msg "\"abc\"")))
      (check-error :error "abc")
      (check-error :success "xyz")))

  (testing "Enforce multiple calls"
    (expecting-failure
     (sut/with-expect-call [(log ["ERROR:" "\"abc\""])
                            (log ["ERROR:" "\"xyz\""])]
       (check-error :error "abc")
       (check-error :error "xyz")
       (check-error :error "Surprise!"))))

  (testing "Multiple calls"
    (sut/with-expect-call [(log ["ERROR:" "\"abc\""])
                           (log ["ERROR:" "\"xyz\""])]
      (check-error :error "abc")
      (check-error :error "xyz")))

  (testing "arg checking against binding"
    (let [foo ":foo"]
      (sut/with-expect-call (log ["ERROR:" foo])
                            (check-error :error :foo))
      (expecting-failure
        (sut/with-expect-call (log ["ERROR:" foo])
                              (check-error :error :bar)))))

  (testing "sequence destructuring"
    (sut/with-expect-call
     (destructuring [[a b]] (is (= a b)))
     (destructuring (mapv inc [3 3])))

    (expecting-failure
     (sut/with-expect-call
      (destructuring [[a b]] (is (= a b)))
      (destructuring (mapv inc [3 4]))))

    (sut/with-expect-call
     (destructuring [[a [b [c :foo]]]] (is (= a b c)))
     (destructuring [1 [1 [1 :foo]]]))

    ;; :as not supported
    #_(sut/with-expect-call
     (destructuring [[a b :as c]] (is (= [4 4] c)))
     (destructuring (mapv inc [3 3])))

    (testing "with arg checking against binding"
      (let [foo :foo]
        (sut/with-expect-call
         (destructuring [[foo bar]] (is (= foo bar)))
         (destructuring [:foo :foo]))

        (expecting-failure
         (sut/with-expect-call
          (destructuring [[foo bar]] (is (= foo bar)))
          (destructuring [:bar :bar])))

        (sut/with-expect-call
         (destructuring [[foo :bar]])
         (destructuring [:foo :bar]))

        (expecting-failure
         (sut/with-expect-call
          (destructuring [[foo :bar]])
          (destructuring [:foo :foo])))

        (expecting-failure
         (sut/with-expect-call
          (destructuring [[foo :bar]])
          (destructuring [:bar :bar])))))

    ;; map destructuring not allowed
    #_(sut/with-expect-call
       (destructuring [{foo :foo}] (is (= foo 1)))
       (destructuring [{:foo 1 :bar 1}]))
    #_(expecting-failure
       (sut/with-expect-call
        (destructuring [{foo :foo bar :bar}] (is (= foo bar)))
        (destructuring {:foo 1 :bar 2}))))

  (testing "matching literal vectors and maps"
    (sut/with-expect-call
     (destructuring [[1 1]])
     (destructuring (mapv inc [0 0])))

    (sut/with-expect-call
     (destructuring [[nil nil]])
     (destructuring (into [] (repeat 2 nil))))

    (sut/with-expect-call
     (destructuring [[:foo :bar]])
     (destructuring [:foo :bar]))

    (expecting-failure
     (sut/with-expect-call
      (destructuring [[1 1]])
      (destructuring (mapv inc [0 3]))))

    (sut/with-expect-call
     (destructuring [{:foo :bar}])
     (destructuring (zipmap [:foo] [:bar])))

    (expecting-failure
     (sut/with-expect-call
      (destructuring [{:foo :bar}])
      (destructuring (zipmap [:foo] [:qux])))))

  (testing "matching against global defs doesn't work - https://github.com/clojure/core.match/wiki/Overview#local-scope-and-symbols"
    (testing "passing in expected value passes"
      #_{:clj-kondo/ignore [:unused-binding]}
      (sut/with-expect-call
       (dummy [dummy-expected-x])
       (dummy dummy-expected-x)))
    (testing "passing in unexpected value also passes"
      #_{:clj-kondo/ignore [:unused-binding]}
      (sut/with-expect-call
       (dummy [dummy-expected-x])
       (dummy (inc dummy-expected-x))))))

(defmacro with-any-order-identity-calls
  "Expects `(destructuring i)` returning `i` for each i below n, in any order."
  [n & body]
  `(sut/with-expect-call ~(vec (for [i (range n)] `(:any-order destructuring [~i] ~i)))
     ~@body))

(deftest any-order
  (testing "calls may happen in any order"
    (sut/with-expect-call [(:any-order log [:a])
                           (:any-order log [:b])
                           (:any-order check-error [:c _])]
      (check-error :c 1)
      (log :b)
      (log :a)))

  (testing "arguments pick the expectation, and its body"
    (sut/with-expect-call [(:any-order log [:a] :from-a)
                           (:any-order log [:b] :from-b)]
      (is (= :from-b (log :b)))
      (is (= :from-a (log :a)))))

  (testing "each expectation is matched at most once"
    (expecting-failure
     (sut/with-expect-call [(:any-order log [:a])]
       (log :a)
       (log :a))))

  (testing "each expectation has to be matched"
    (expecting-failure
     (sut/with-expect-call [(:any-order log [:a])
                            (:any-order log [:b])]
       (log :a))))

  (testing "arguments have to match"
    (expecting-failure
     (sut/with-expect-call [(:any-order log [:a])]
       (log :b))))

  (testing "can be mixed with ordered expectations"
    (sut/with-expect-call [(:never destructuring)
                           (check-error [:first _])
                           (:any-order log [:x])
                           (:any-order log [:z])
                           (check-error [:second _])
                           (:more check-error [:third _])]
      (log :x)
      (check-error :first 1)
      (check-error :second 2)
      (check-error :third 3)
      (log :z)
      (check-error :third 3)))


  (testing "can be combined with :do"
    (sut/with-expect-call [(:do :any-order log [:a])]
      (is (= :logged (log :a)))))

  (testing "concurrent calls"
    (with-any-order-identity-calls 50
      (let [results (->> (range 50)
                         shuffle
                         (mapv (fn [i] (future [i (destructuring i)])))
                         (mapv deref))]
        (is (every? (fn [[i result]] (= i result)) results)))))
  :ok)

(defmacro check-line [expr]
  `(let [report# (expecting-failure ~expr)
         ~'file-and-line (str (:file report#) ":" (:line report#))]
     (is (~'= ~'file-and-line ~(str "expect_call_test.clj:" (:line (meta expr)))))
     (when-not (= ~'file-and-line ~(str "expect_call_test.clj:" (:line (meta expr))))
       (println "Actual report:" (pr-str report#))
       (println "Actual stack trace:")
       (doseq [s# (take 10 (:stack-trace report#))]
         (println s#)))))

(deftest line-number-reporting
  ;; Test that every (report) mode we have yields the correct
  ;; line number

  (testing ":never"
    (check-line
     (sut/with-expect-call (:never log) (log :test))))

  (testing "Not called"
    (check-line
     (sut/with-expect-call (log) (inc 1))))

  (testing "Wrong function"
    (check-line (sut/with-expect-call [(log) (println)] (println "hi"))))

  (testing "Wrong args"
    (check-line (sut/with-expect-call (log [:x]) (log :y)))))
