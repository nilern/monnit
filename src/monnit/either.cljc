(ns monnit.either
  (:require [monnit.core :as m]))

(defprotocol Either
  (either? [self]))

(extend-protocol Either
  #?(:clj Object, :cljs default)
  (either? [_] false))

(defrecord Left [value]
  Either
  (either? [_] true)

  m/Functor
  (-fmap [self f] self)

  m/Applicative
  (fapply [self _] self)

  m/Monad
  (bind [self _] self)

  m/Alternative
  (alt [_ other] other))

(defrecord Right [value]
  Either
  (either? [_] true)

  m/Functor
  (-fmap [_ f] (Right. (f value)))

  m/Applicative
  (fapply [_ fv]
    (assert (either? fv))
    (m/-fmap fv value))

  m/Monad
  (bind [_ f] (f value))

  m/Alternative
  (alt [self _] self))

(def pure ->Right)

(defmethod m/pure Either [_ v] (pure v))

