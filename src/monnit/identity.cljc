(ns monnit.identity
  "A pure computation that wraps a single value. Only useful because it
  implements Functor and Monad."
  (:require [monnit.core :as m]))

(defprotocol Identity
  "A pure computation that wraps a single value. Only useful because it
  implements [[m/Functor]] and [[m/Monad]]."
  (identity? [self] "Is `self` an [[Identity]]?")
  (run-identity [self]
    "Extract the contained value. An implementation detail; call [[run]]
    instead."))

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

(def ^{:arglists '([v])} pure "Wrap `v` in an [[Identity]]." ->Pure)

(defmethod m/pure Identity [_ v] (pure v))

(def ^{:arglists '([id])} run "Extract the value contained in `id`." run-identity)

