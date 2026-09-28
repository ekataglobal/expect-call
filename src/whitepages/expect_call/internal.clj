(ns whitepages.expect-call.internal
  (:require [clojure.test :refer :all]
            [clojure.core.match :refer [match]]))

(def ^:dynamic *disable-interception* false)

(defn my-report
  "Disable interception, to prevent looping if the test reporting code
   uses a function we're intercepting. Also put accurate file and line
   information into the message."
  [msg]

  (binding [*disable-interception* true]
    (report
     (merge
      (let [stack-trace (.getStackTrace (new Throwable))
            ^StackTraceElement s (first (drop-while #(.startsWith (.getClassName ^StackTraceElement %) "whitepages.expect_call.internal$") stack-trace))]
        {:file (.getFileName s) :line (.getLineNumber s)
         :stack-trace (seq stack-trace)})
      msg))))

(defn- take-any-order!
  "Removes and returns the first :any-order expectation for `real-fn` whose
   pattern matches `args`, or nil."
  [any-order real-fn args]
  (let [matches? (fn [[ex-real-fn _ _ _ ex-matches?]]
                   (and (= real-fn ex-real-fn) (apply ex-matches? args)))
        [old]    (swap-vals! any-order
                             (fn [expectations]
                               (let [[before [_ & after]] (split-with (complement matches?) expectations)]
                                 (concat before after))))]
    (first (filter matches? old))))

(defn -expected-call
  "Used by (expect-call) macro. You don't call this."
  [[more-fns calls any-order :as _state] real-fn real-fn-name args]
  (if *disable-interception*
    (apply real-fn args)

    (let [[ex-real-fn ex-fn ex-real-fn-name ex-args] (first @calls)]
      (if (= real-fn ex-real-fn)
        (do ; It matched the next explicit expectation. Run it.
          (swap! calls rest)
          (apply ex-fn args))

        ;; It didn't match an explicit expectation - did it match
        ;; an :any-order, a :more or a :never?
        (if-let [[_ any-order-fn] (take-any-order! any-order real-fn args)]
          (apply any-order-fn args)
          (if-let [more-fn (more-fns real-fn)]
            (apply more-fn args)

            ;; Nope - it's just wrong
            (my-report {:type :fail
                        :message (if ex-real-fn
                                   "Wrong function called"
                                   (str "Too many calls to " real-fn-name))
                        :expected (cons ex-real-fn-name ex-args)
                        :actual (cons real-fn-name args)})))))))

(defn make-mock [[tags real-fn-name & [args & body]]]
  (let [args (or args '[& _])
        real-fn (gensym "real-fn")]
    `(let [~real-fn ~real-fn-name]
       (fn ~(gensym (str (name real-fn-name) "-mock")) [& ~'myargs]
         (match (apply vector ~'myargs)
                ~args (do ~@body ~@(when (:do tags) `((apply ~real-fn ~'myargs))))
                :else (my-report {:type :fail
                                  :message "Unexpected arguments"
                                  :expected (quote ~(cons real-fn-name args))
                                  :actual (cons (quote ~real-fn-name)
                                                ~'myargs)}))))))

(defn make-matcher [[_tags _real-fn-name & [args]]]
  (let [args (or args '[& _])]
    `(fn [& ~'myargs]
       (match (apply vector ~'myargs)
              ~args true
              :else false))))

(defmacro -expect-call
  "expected-fns: (fn arg-match body...)
                 or [(fn arg-match body...), (fn arg-match body...)...]
   Each fn may be preceded by keywords :more, :never, :any-order or :do."
  [expected-fns & body]

  (let [expected-fns (if (vector? expected-fns) expected-fns [expected-fns])
        expected-fns (for [fspec expected-fns]
                       (cons (apply hash-set (take-while keyword? fspec))
                             (drop-while keyword? fspec)))

        state (gensym "state")]

    `(let [;; Format: {function closure, function closure}
           more-fns#
           ~(apply merge {}
                   (for [[tags real-fn :as expected-fn] expected-fns
                         :when (or (:more tags) (:never tags))]
                     (if (:more tags)
                       {real-fn (make-mock expected-fn)}
                       {real-fn `(fn [& args#]
                                   (my-report {:type :fail
                                               :message ~(str real-fn " should not be called")
                                               :expected (quote (:never ~real-fn))
                                               :actual (cons (quote ~real-fn)
                                                             args#)}))})))

           ;; Format: ([function closure fn-name arg-form],
           ;;          [function closure arg-form], ...)
           calls# (atom
                   (list
                    ~@(for [[tags real-fn args :as expected-fn] expected-fns
                            :when (not (or (:more tags) (:never tags) (:any-order tags)))]
                        [real-fn (make-mock expected-fn)
                         `(quote ~real-fn) `(quote ~args)])))

           ;; Format: ([function closure fn-name arg-form matcher], ...)
           any-order# (atom
                       (list
                        ~@(for [[tags real-fn args :as expected-fn] expected-fns
                                :when (:any-order tags)]
                            [real-fn (make-mock expected-fn)
                             `(quote ~real-fn) `(quote ~args) (make-matcher expected-fn)])))

           ~state [more-fns# calls# any-order#]]

       (let [result#
             (with-redefs
               ~(apply vector
                       (let [fns (reduce (fn [set [_ real-fn]] (conj set real-fn))
                                         #{} expected-fns)]
                         (apply
                          concat
                          (for [f fns]
                            [f `(let [f# ~f]
                                  (fn ~(symbol (str (name f) "-mock")) [& a#]
                                    (-expected-call ~state f# (quote ~f) a#)))]))))
               ~@body)]
         ;; If we haven't used up all our calls, we error out
         (when-let [[_# _# ex-fn-name# ex-args#] (or (first @calls#) (first @any-order#))]
           (my-report {:type :fail
                       :message (str "Function " ex-fn-name# " was not called")
                       :expected (cons ex-fn-name# ex-args#)
                       :actual nil}))
         result#))))
