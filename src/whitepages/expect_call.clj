(ns whitepages.expect-call
  (:require
   [clojure.core.match :refer [match]]
   [whitepages.expect-call.internal :refer :all]
   [clojure.spec.alpha :as s]))

(s/def ::expected-fn
  (s/cat :tags (s/* #{:do :more :once :never})
         :fdef (s/cat :fname symbol?
                      :args (s/coll-of any? :kind vector)
                      :body (s/* any?))))

(s/def ::expected-fns
  (s/coll-of ::expected-fn :kind vector?))

(defn mock-call [state var args]
  (let [ret (atom nil)]
    ;; put all in swap! to sync mock calls
    (swap! state
           (fn [state]
             (let [var-matches (filter #(= (:mocked-var %) var) state)
                   [{:keys [tags body-fn calls orig-fn]
                     :as match} & _] (filter #((:match-fn %) args) var-matches)]
               ;; report when no matches
               (cond
                 (nil? match)
                 (my-report {:type :fail
                             :message "found no matching mock!"
                             :expected (->> var-matches
                                            (map (fn [{:keys [args mocked-var]}]
                                                   (list (symbol mocked-var) args)))
                                            (cons :one-of))
                             :actual (list (symbol var) args)}
                            2)
                 (and (:once tags)
                      (> calls 0))
                 (my-report {:type :fail
                             :message "call with :once tag called more than once"
                             :actual (list (symbol var) args)}
                            2))

               (reset! ret
                       (if (:do ret)
                         (apply orig-fn args)
                         (body-fn)))

               (-> state
                   (disj match)
                   (conj (update match :calls inc))))))
    @ret))

(defmacro with-expect-call2 [expected-fns & body]
  (let [conformed (s/conform ::expected-fns
                             expected-fns)
        state-sym (gensym "excpect-call-state")]
    (when (s/invalid? conformed)
      (throw (ex-info "expect-call did not conform to spec"
                      (s/explain-data ::expected-fns
                                      expected-fns))))
    `(let [~state-sym
           (atom ~(->> conformed
                       (map (fn [{tags                      :tags
                                  {:keys [fname args body]} :fdef}]
                              {:mocked-var `(resolve (quote ~fname))
                               :args       args
                               :body-fn    `(fn ~'do-body []
                                              ~@body)
                               :calls      0
                               :match-fn   `(fn [args#]
                                              (match (into [] args#)
                                                     ~args true
                                                     :else false))
                               :orig-fn    fname
                               :tags       (into #{} tags)}))
                       (into #{})))]
       (with-redefs ~(->> conformed
                          (into #{}
                                (map (comp :fname :fdef)))
                          (mapcat (fn [fname]
                                    [fname
                                     `(fn [~'& args#]
                                        (mock-call ~state-sym (resolve (quote ~fname)) args#))]))
                          (into []))
         ~@body))))

(defmacro expect-call
  "expected-fns: (fn arg-match body...)
                 or [(fn arg-match body...), (fn arg-match body...)...]
   Each fn may be preceded by keywords :more, :never, :once or :do.
  :more allows fns to be called any number of times in any order
  :do calls through to the underlying fn, allowing you to expect args
  :never ensures the fn is never called
  :once makes the function called once during the duration of the body, allowing any order"
  [expected-fns & body]

  `(-expect-call ~expected-fns ~@body))

;; This is an alias
(defmacro with-expect-call
  "This is an alias for expect-call, with a with- prefix
   so emacs clojure-mode indents it more nicely"
  [& args]
  `(-expect-call ~@args))
