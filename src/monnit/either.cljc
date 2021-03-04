(ns monnit.either
  (:require [monnit.core :as m]
            [monnit.impl.util :refer [typecase]]))

(defprotocol Either
  (either? [self]))

(extend-protocol Either
  #?(:clj Object, :cljs default)
  (either? [_] false)

  nil
  (either? [_] false))

(defrecord Left [value]
  Either
  (either? [_] true)

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

(defrecord Right [value]
  Either
  (either? [_] true)

  m/Functor
  (-fmap [_ f] (Right. (f value)))
  (-fmap [_ f b]
    (typecase [b b]
      Right (Right. (f value (.-value b)))
      Left b))
  (-fmap [_ f b c]
    (typecase [b b]
      Right (typecase [c c]
              Right (Right. (f value (.-value b) (.-value c)))
              Left c)
      Left b))
  (-fmap [_ f b c d]
    (typecase [b b]
      Right (typecase [c c]
              Right (typecase [d d]
                      Right (Right. (f value (.-value b) (.-value c) (.-value d)))
                      Left d)
              Left c)
      Left b))
  (-fmap [_ f b c d args]
    (typecase [b b]
      Right (typecase [c c]
              Right (typecase [d d]
                      Right (let [args (reduce (fn [args arg]
                                                 (typecase [arg arg]
                                                   Right (conj! args (.-value arg))
                                                   Left (reduced arg)))
                                               (transient []) args)]
                              (if (instance? Left args)
                                args
                                (apply f value (.-value b) (.-value c) (.-value d) (persistent! args))))
                      Left d)
              Left c)
      Left b))

  m/Monad
  (bind [_ f] (f value))

  m/Alternative
  (alt [self _] self))

(def pure ->Right)

(defmethod m/pure Either [_ v] (pure v))

