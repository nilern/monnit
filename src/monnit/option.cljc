(ns monnit.option
  "A computation that can fail to produce a meaningful result; a more robust
  alternative to returning a value that may be `nil`. Implements [[monnit.core/Functor]],
  [[monnit.core/Monad]] and [[monnit.core/Alternative]]."
  (:refer-clojure :exclude [some some?])
  (:require [monnit.core :as m]
            [monnit.impl.util :refer [typecase]]))

(defprotocol Option
  "A computation that can fail to produce a meaningful result; a more robust
  alternative to returning a value that may be `nil`. Implements [[monnit.core/Functor]],
  [[monnit.core/Monad]] and [[monnit.core/Alternative]]."
  (option? [self] "Is `self` an option ([[Some]] or [[None]])?")
  (some? [self] "Is `self` a [[Some]]?")
  (none? [self] "Is `self` a [[None]]?")
  (-run-option [self default f]
    "[[run]] with the [[Option]] first. An implementation detail; call [[run]]
    instead."))

(extend-protocol Option
  #?(:clj Object, :cljs default)
  (option? [_] false)
  (none? [_] false)
  (some? [_] false)
  (-run-option [self _ _]
    (assert false (str "-run-option called on non-Option value " self)))

  nil
  (option? [_] false)
  (none? [_] false)
  (some? [_] false)
  (-run-option [self _ _]
    (assert false (str "-run-option called on non-Option value " self))))

(defrecord ^{:doc "An [[Option]] that contains no value."} None []
  Option
  (option? [_] true)
  (none? [_] true)
  (some? [_] false)
  (-run-option [_ default _] default)

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

(def none "An instance of [[None]]." (None.))

(defrecord ^{:doc "An [[Option]] that contains a value."} Some [value]
  Option
  (option? [_] true)
  (none? [_] false)
  (some? [_] true)
  (-run-option [_ _ f] (f value))

  m/Functor
  (-fmap [_ f] (Some. (f value)))
  (-fmap [_ f b]
    (typecase [b b]
      Some (Some. (f value (.-value b)))
      None b))
  (-fmap [_ f b c]
    (typecase [b b]
      Some (typecase [c c]
              Some (Some. (f value (.-value b) (.-value c)))
              None c)
      None b))
  (-fmap [_ f b c d]
    (typecase [b b]
      Some (typecase [c c]
              Some (typecase [d d]
                      Some (Some. (f value (.-value b) (.-value c) (.-value d)))
                      None d)
              None c)
      None b))
  (-fmap [_ f b c d args]
    (typecase [b b]
      Some (typecase [c c]
              Some (typecase [d d]
                      Some (let [args (reduce (fn [args arg]
                                                 (typecase [arg arg]
                                                   Some (conj! args (.-value arg))
                                                   None (reduced arg)))
                                               (transient []) args)]
                              (if (instance? None args)
                                args
                                (Some. (apply f value (.-value b) (.-value c) (.-value d) (persistent! args)))))
                      None d)
              None c)
      None b))

  m/Monad
  (bind [_ f] (f value))

  m/Alternative
  (alt [self _] self))

(def ^{:arglists '([v])} some "Wrap `v` into a [[Some]]." ->Some)

(def ^{:arglists '([v])} pure "Wrap `v` into a [[Some]]." some)

(defmethod m/pure Option [_ v] (pure v))

(defn nilable->option
  "If `v` is `nil`, return [[none]]. Otherwise wrap `v` with [[some]]."
  [v]
  (if (nil? v) none (some v)))

(defn run
  "Apply `f` to the contents of `self` if `(some? self)` and return `default` if `(none? self)`."
  [default f e]
  (-run-option e default f))

