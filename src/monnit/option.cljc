(ns monnit.option
  (:refer-clojure :exclude [some some?])
  (:require [monnit.core :as m]
            [monnit.impl.util :refer [typecase]]))

(defprotocol Option
  (option? [self])
  (some? [self])
  (none? [self])
  (-run-option [self default f]))

(extend-protocol Option
  #?(:clj Object, :cljs default)
  (option? [_] false)
  (none? [_] false)
  (some? [_] false)
  (-run-option [self _ _] (assert false (str "fold called on non-Option value " self)))

  nil
  (option? [_] false)
  (none? [_] false)
  (some? [_] false)
  (-run-option [self _ _] (assert false (str "fold called on non-Option value " self))))

(defrecord None []
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

(def none (None.))

(defrecord Some [value]
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

(def some ->Some)

(def pure some)

(defmethod m/pure Option [_ v] (pure v))

(defn run [default f e] (-run-option e default f))

