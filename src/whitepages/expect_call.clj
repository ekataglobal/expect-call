(ns whitepages.expect-call
  (:require [whitepages.expect-call.internal :refer [-expect-call]]))

(defmacro expect-call
  "expected-fns: (fn arg-match body...)
                 or [(fn arg-match body...), (fn arg-match body...)...]
   Each fn may be preceded by keywords :more, :never, :any-order or :do."
  [expected-fns & body]

  `(-expect-call ~expected-fns ~@body))

;; This is an alias
(defmacro with-expect-call
  "expected-fns: (fn arg-match body...)
                 or [(fn arg-match body...), (fn arg-match body...)...]
   Each fn may be preceded by keywords :more, :never, :any-order or :do.

   This is an alias for expect-call, with a with- prefix
   so emacs clojure-mode indents it more nicely"
  [expected-fns & body]
  `(-expect-call ~expected-fns ~@body))
