(ns monnit.state
  (:refer-clojure :exclude [get set update])
  #?(:cljs (:require-macros [monnit.state-macros :refer [defstatetype]]))
  (:require [monnit.core :as m]
            #?(:clj [monnit.state-macros :refer [defstatetype]])))

;;; OPTIMIZE: PersistentVector is a bulky pair

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
    (let [[s a] (-run-state fa s)]
      [s (f a)])))

(defstatetype FMap2 [f fa fb]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [[s a] (-run-state fa s)
          [s b] (-run-state fb s)]
      [s (f a b)])))

(defstatetype FMap3 [f fa fb fc]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [[s a] (-run-state fa s)
          [s b] (-run-state fb s)
          [s c] (-run-state fc s)]
      [s (f a b c)])))

(defstatetype FMap4 [f fa fb fc fd]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [[s a] (-run-state fa s)
          [s b] (-run-state fb s)
          [s c] (-run-state fc s)
          [s d] (-run-state fd s)]
      [s (f a b c d)])))

(defstatetype FMapN [f fa fb fc fd fargs]
  State
  (state? [_] true)
  (-run-state [_ s]
    (let [[s a] (-run-state fa s)
          [s b] (-run-state fb s)
          [s c] (-run-state fc s)
          [s d] (-run-state fd s)
          s (volatile! s)
          args (mapv (fn [arg] (vreset! (-run-state arg @s))) fargs)]
      [s (apply f a b c d args)])))

(defstatetype Bind [mv f]
  State
  (state? [_] true)
  (-run-state [self s]
    (let [[s a] (-run-state mv s)]
      (-run-state (f a) s))))

(deftype Pure [v]
  State
  (state? [_] true)
  (-run-state [_ s] [s v])

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
  (-run-state [_ s] [s s]))

(def get (->Get))

(defstatetype Set [s]
  State
  (state? [_] true)
  (-run-state [_ _] [s nil]))

(def set ->Set)

(defstatetype Update [f]
  State
  (state? [_] true)
  (-run-state [_ s] (let [s (f s)] [s s])))

(def update ->Update)

(defn run [s sm] (-run-state sm s))

