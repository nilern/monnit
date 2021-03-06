(ns monnit.result
  (:require [monnit.core :as m]
            [monnit.impl.util :refer [typecase]]))

(defprotocol Result
  (result? [self])
  (err? [self])
  (ok? [self])
  (-run-result [self lf rf]))

(extend-protocol Result
  #?(:clj Object, :cljs default)
  (result? [_] false)
  (err? [_] false)
  (ok? [_] false)
  (-run-result [self _ _] (assert false (str "fold called on non-Result value " self)))

  nil
  (result? [_] false)
  (err? [_] false)
  (ok? [_] false)
  (-run-result [self _ _] (assert false (str "fold called on non-Result value " self))))

(defrecord Err [value]
  Result
  (result? [_] true)
  (err? [_] true)
  (ok? [_] false)
  (-run-result [_ lf _] (lf value))

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

(def err ->Err)

(defrecord Ok [value]
  Result
  (result? [_] true)
  (err? [_] false)
  (ok? [_] true)
  (-run-result [_ _ rf] (rf value))

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

(def ok ->Ok)

(def pure ok)

(defmethod m/pure Result [_ v] (pure v))

(defn run [lf rf e] (-run-result e lf rf))

