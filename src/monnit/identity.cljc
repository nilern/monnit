(ns monnit.identity
  (:require [monnit.core :as m]))

(defprotocol Identity
  (identity? [self])
  (run-identity [self]))

(extend-protocol Identity
  #?(:clj Object, :cljs default)
  (identity? [_] false)
  (run-identity [self]
    (assert false (str "run-identity called on non-Identity value " self)))

  nil
  (identity? [_] false)
  (run-identity [self] (assert false "run-identity called on nil")))

(deftype Pure [v]
  Identity
  (identity? [_] true)
  (run-identity [_] v)

  m/Functor
  (-fmap [_ f] (Pure. (f v)))
  (-fmap [_ f b] (Pure. (f v (.-v ^Pure b))))
  (-fmap [_ f b c] (Pure. (f v (.-v ^Pure b) (.-v ^Pure c))))
  (-fmap [_ f b c d] (Pure. (f v (.-v ^Pure b) (.-v ^Pure c) (.-v ^Pure d))))
  (-fmap [_ f b c d args]
    (Pure. (apply f v (.-v ^Pure b) (.-v ^Pure c) (.-v ^Pure d)
              (map (fn [^Pure arg] (.-v arg)) args))))

  m/Monad
  (bind [_ f] (f v)))

(def pure ->Pure)

(defmethod m/pure Identity [_ v] (pure v))

(def run run-identity)

