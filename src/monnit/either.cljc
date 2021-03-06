(ns monnit.either
  (:require [monnit.core :as m]
            [monnit.impl.util :refer [typecase]]))

(defprotocol Either
  (either? [self])
  (left? [self])
  (right? [self])
  (-run-either [self lf rf]))

(extend-protocol Either
  #?(:clj Object, :cljs default)
  (either? [_] false)
  (left? [_] false)
  (right? [_] false)
  (-run-either [self _ _] (assert false (str "fold called on non-Either value " self)))

  nil
  (either? [_] false)
  (left? [_] false)
  (right? [_] false)
  (-run-either [self _ _] (assert false (str "fold called on non-Either value " self))))

(defrecord Left [value]
  Either
  (either? [_] true)
  (left? [_] true)
  (right? [_] false)
  (-run-either [_ lf _] (lf value))

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

(def left ->Left)

(defrecord Right [value]
  Either
  (either? [_] true)
  (left? [_] false)
  (right? [_] true)
  (-run-either [_ _ rf] (rf value))

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
                                (Right. (apply f value (.-value b) (.-value c) (.-value d) (persistent! args)))))
                      Left d)
              Left c)
      Left b))

  m/Monad
  (bind [_ f] (f value))

  m/Alternative
  (alt [self _] self))

(def right ->Right)

(def pure right)

(defmethod m/pure Either [_ v] (pure v))

(defn run [lf rf e] (-run-either e lf rf))

