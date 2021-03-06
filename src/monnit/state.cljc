(ns monnit.state
  (:refer-clojure :exclude [get set update])
  #?(:cljs (:require-macros [monnit.impl.state-macros :refer [defstatetype]]))
  (:require [monnit.core :as m]
            #?(:clj [monnit.impl.state-macros :refer [defstatetype]])
            [monnit.pair #?@(:cljs [:refer [Pair]])])
  #?(:clj (:import [monnit.pair Pair])))

(defprotocol State
  (state? [self])
  (-run-state [self s]))

(extend-protocol State
  #?(:clj Object, :cljs default)
  (state? [_] false)
  (-run-state [self _]
    (assert false (str "-run-state called on non-State value " self)))

  nil
  (state? [_] false)
  (-run-state [self _] (assert false "-run-state called on nil")))

(declare ->FMap1 ->FMap2 ->FMap3 ->FMap4 ->FMapN ->Bind)

(defstatetype FMap1 [f fa]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [^Pair sa (-run-state fa s)]
      (Pair. (.-fst sa) (f (.-snd sa))))))

(defstatetype FMap2 [f fa fb]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [^Pair sa (-run-state fa s)
          ^Pair sb (-run-state fb (.-fst sa))]
      (Pair. (.-fst sb) (f (.-snd sa) (.-snd sb))))))

(defstatetype FMap3 [f fa fb fc]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [^Pair sa (-run-state fa s)
          ^Pair sb (-run-state fb (.-fst sa))
          ^Pair sc (-run-state fc (.-fst sb))]
      (Pair. (.-fst sc) (f (.-snd sa) (.-snd sb) (.-snd sc))))))

(defstatetype FMap4 [f fa fb fc fd]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [^Pair sa (-run-state fa s)
          ^Pair sb (-run-state fb (.-fst sa))
          ^Pair sc (-run-state fc (.-fst sb))
          ^Pair sd (-run-state fd (.-fst sc))]
      (Pair. (.-fst sc) (f (.-snd sa) (.-snd sb) (.-snd sc) (.-snd sd))))))

(defstatetype FMapN [f fa fb fc fd fargs]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [^Pair sa (-run-state fa s)
          ^Pair sb (-run-state fb (.-fst sa))
          ^Pair sc (-run-state fc (.-fst sb))
          ^Pair sd (-run-state fd (.-fst sc))
          s (volatile! (.-fst sd))
          args (mapv (fn [arg]
                       (let [^Pair sarg (-run-state arg @s)]
                         (vreset! s (.-fst sarg))
                         (.-snd sarg)))
                     fargs)]
      (Pair. @s (apply f (.-snd sa) (.-snd sb) (.-snd sc) (.-snd sd) args)))))

(defstatetype Bind [mv f]
  State
  (state? [_] true)
  (-run-state [self s]
    (let [^Pair sa (-run-state mv s)]
      (-run-state (f (.-snd sa)) (.-fst sa)))))

(deftype Pure [v]
  State
  (state? [_] true)
  (-run-state [_ s] (Pair. s v))

  m/Functor
  (-fmap [_ f] (Pure. (f v)))
  (-fmap [self f b] (FMap2. f self b))
  (-fmap [self f b c] (FMap3. f self b c))
  (-fmap [self f b c d] (FMap4. f self b c d))
  (-fmap [self f b c d args] (FMapN. f self b c d args))

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

