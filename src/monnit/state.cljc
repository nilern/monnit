(ns monnit.state
  (:refer-clojure :exclude [get set update])
  #?(:cljs (:require-macros [monnit.state-macros :refer [defstatetype]]))
  (:require [monnit.core :as m]
            #?(:clj [monnit.state-macros :refer [defstatetype]])
            [monnit.pair #?@(:cljs [:refer [Pair]])])
  #?(:clj (:import [monnit.pair Pair])))

(defprotocol State
  (state? [self])
  (-run-state [self s]))

(extend-protocol State
  #?(:clj Object, :cljs default)
  (state? [_] false)
  (-run-state [_ _] (assert false))) ; FIXME: error message

(declare ->FMap1 ->FMap2 ->FMap3 ->FMap4 ->FMapN ->Bind)

(defstatetype FMap1 [f fa]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [^Pair sa (-run-state fa s)]
      (Pair. (.-left sa) (f (.-right sa))))))

(defstatetype FMap2 [f fa fb]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [^Pair sa (-run-state fa s)
          ^Pair sb (-run-state fb (.-left sa))]
      (Pair. (.-left sb) (f (.-right sa) (.-right sb))))))

(defstatetype FMap3 [f fa fb fc]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [^Pair sa (-run-state fa s)
          ^Pair sb (-run-state fb (.-left sa))
          ^Pair sc (-run-state fc (.-left sb))]
      (Pair. (.-left sc) (f (.-right sa) (.-right sb) (.-right sc))))))

(defstatetype FMap4 [f fa fb fc fd]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [^Pair sa (-run-state fa s)
          ^Pair sb (-run-state fb (.-left sa))
          ^Pair sc (-run-state fc (.-left sb))
          ^Pair sd (-run-state fd (.-left sc))]
      (Pair. (.-left sc) (f (.-right sa) (.-right sb) (.-right sc) (.-right sd))))))

(defstatetype FMapN [f fa fb fc fd fargs]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [^Pair sa (-run-state fa s)
          ^Pair sb (-run-state fb (.-left sa))
          ^Pair sc (-run-state fc (.-left sb))
          ^Pair sd (-run-state fd (.-left sc))
          s (volatile! (.-left sd))
          args (mapv (fn [arg]
                       (let [^Pair sarg (-run-state arg @s)]
                         (vreset! s (.-left sarg))
                         (.-right sarg)))
                     fargs)]
      (Pair. @s (apply f (.-right sa) (.-right sb) (.-right sc) (.-right sd) args)))))

(defstatetype Bind [mv f]
  State
  (state? [_] true)
  (-run-state [self s]
    (let [^Pair sa (-run-state mv s)]
      (-run-state (f (.-right sa)) (.-left sa)))))

(deftype Pure [v]
  State
  (state? [_] true)
  (-run-state [_ s] (Pair. s v))

  m/Functor
  (-fmap [_ f] (Pure. (f v)))
  (-fmap [self f b] (->FMap2 f self b))
  (-fmap [self f b c] (->FMap3 f self b c))
  (-fmap [self f b c d] (->FMap4 f self b c d))
  (-fmap [self f b c d args] (->FMapN f self b c d args))

  m/Monad
  (bind [_ f] (f v)))

(def pure ->Pure)

(defmethod m/pure State [_ v] (pure v))

(defstatetype Get []
  State
  (state? [_] true)
  (-run-state [_ s] (Pair. s s)))

(def get (->Get))

(defstatetype Set [s]
  State
  (state? [_] true)
  (-run-state [_ _] (Pair. s nil)))

(def set ->Set)

(defstatetype Update [f]
  State
  (state? [_] true)
  (-run-state [_ s] (let [s (f s)] (Pair. s s))))

(def update ->Update)

(defn run [s sm] (-run-state sm s))

