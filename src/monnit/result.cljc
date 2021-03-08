(ns monnit.result
  "A computation that can fail with an error value; for when there is nothing
  exceptional about getting an error and the error should be handled sooner
  rather than later (or never). Implements [[monnit.core/Functor]], [[monnit.core/Monad]]
  and [[monnit.core/Alternative]]."
  (:require [monnit.core :as m]
            [monnit.impl.util :refer [typecase]]))

(defprotocol Result
  "A computation that can fail with an error value; for when there is nothing
  exceptional about getting an error and the error should be handled sooner
  rather than later (or never). Implements [[monnit.core/Functor]], [[monnit.core/Monad]]
  and [[monnit.core/Alternative]]."
  (result? [self] "Is `self` a [[Result]]?")
  (err? [self] "Is `self` an [[Err]]?")
  (ok? [self] "Is `self` an [[Ok]]?")
  (-run-result [self on-error on-success]
    "[[run]] with the [[Result]] first. An implementation detail; call [[run]]
    instead."))

(extend-protocol Result
  #?(:clj Object, :cljs default)
  (result? [_] false)
  (err? [_] false)
  (ok? [_] false)
  (-run-result [self _ _]
    (assert false (str "-run-result called on non-Result value " self)))

  nil
  (result? [_] false)
  (err? [_] false)
  (ok? [_] false)
  (-run-result [self _ _]
    (assert false (str "-run-result called on non-Result value " self))))

(defrecord ^{:doc "An error [[Result]]"} Err [value]
  Result
  (result? [_] true)
  (err? [_] true)
  (ok? [_] false)
  (-run-result [_ on-error _] (on-error value))

  m/Functor
  (-fmap [self _] self)
  (-fmap [self _ _] self)
  (-fmap [self _ _ _] self)
  (-fmap [self _ _ _ _] self)
  (-fmap [self _ _ _ _ _] self)

  m/Monad
  (bind [self _] self)

  m/Alternative
  (alt [_ other] other))

(def ^{:arglists '([error])} err "Make a [[Result]] that failed with `error`." ->Err)

(defrecord ^{:doc "A successful [[Result]]"} Ok [value]
  Result
  (result? [_] true)
  (err? [_] false)
  (ok? [_] true)
  (-run-result [_ _ on-success] (on-success value))

  m/Functor
  (-fmap [_ f] (Ok. (f value)))
  (-fmap [_ f b]
    (typecase [b b]
      Ok (Ok. (f value (.-value b)))
      Err b))
  (-fmap [_ f b c]
    (typecase [b b]
      Ok (typecase [c c]
           Ok (Ok. (f value (.-value b) (.-value c)))
           Err c)
      Err b))
  (-fmap [_ f b c d]
    (typecase [b b]
      Ok (typecase [c c]
           Ok (typecase [d d]
                Ok (Ok. (f value (.-value b) (.-value c) (.-value d)))
                Err d)
           Err c)
      Err b))
  (-fmap [_ f b c d args]
    (typecase [b b]
      Ok (typecase [c c]
           Ok (typecase [d d]
                Ok (let [args (reduce (fn [args arg]
                                        (typecase [arg arg]
                                          Ok (conj! args (.-value arg))
                                          Err (reduced arg)))
                                      (transient []) args)]
                     (if (instance? Err args)
                       args
                       (Ok. (apply f value (.-value b) (.-value c) (.-value d) (persistent! args)))))
                Err d)
           Err c)
      Err b))

  m/Monad
  (bind [_ f] (f value))

  m/Alternative
  (alt [self _] self))

(def ^{:arglists '([value])} ok "Make a [[Result]] that succeeded with `value`." ->Ok)

(def ^{:arglists '([value])} pure "Make a [[Result]] that succeeded with `value`." ok)

(defmethod m/pure Result [_ v] (pure v))

(defn try->result
  "Call `thunk` with no arguments. If it throws an Exception (clj) or Error (cljs),
  catch that and wrap it with [[err]]. Otherwise wrap the result with [[ok]].

  Calls `thunk` immediately instead of waiting for [[run]].

  Does not catch non-Exception/Error throwables."
  [thunk]
  (try
    (Ok. (thunk))
    (catch #?(:clj Exception, :cljs js/Error) exn (Err. exn))))

(defn run
  "If `res` contains an error, call `on-error` with the error. If `res` is successful,
  call `on-success` with the contained value."
  [on-error on-success res]
  (-run-result res on-error on-success))

