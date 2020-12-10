(ns monnit.state
  (:refer-clojure :exclude [get set update])
  (:require [monnit.core :as m]))

;;; OPTIMIZE: PersistentVector is a bulky pair
;;; OPTIMIZE: Fn + wrapper -> run-state method (?)

(deftype State [run-state]
  m/Functor
  (-fmap [_ f] (State. (fn [s] (let [[s a] (run-state s)] [s (f a)]))))
  (-fmap [_ f b]
    (State. (fn [s]
              (let [[s a] (run-state s)
                    [s b] ((.-run-state ^State b) s)]
                [s (f a b)]))))
  (-fmap [_ f b c]
    (State. (fn [s]
              (let [[s a] (run-state s)
                    [s b] ((.-run-state ^State b) s)
                    [s c] ((.-run-state ^State c) s)]
                [s (f a b c)]))))
  (-fmap [_ f b c d]
    (State. (fn [s]
              (let [[s a] (run-state s)
                    [s b] ((.-run-state ^State b) s)
                    [s c] ((.-run-state ^State c) s)
                    [s d] ((.-run-state ^State d) s)]
                [s (f a b c d)]))))
  (-fmap [_ f b c d args]
    (State. (fn [s]
              (let [[s a] (run-state s)
                    [s b] ((.-run-state ^State b) s)
                    [s c] ((.-run-state ^State c) s)
                    [s d] ((.-run-state ^State d) s)
                    [s args] (reduce (fn [[s args] ^State arg]
                                       (let [[s arg] ((.-run-state arg) s)]
                                         [s (conj! args arg)]))
                                     [s (transient [])] args)]
                [s (apply f a b c d (persistent! args))]))))

  m/Monad
  (bind [_ f] (State. (fn [s] (let [[s v] (run-state s)] ((.-run-state ^State (f v)) s))))))

(defn pure [v] (State. (fn [s] [s v])))

(defmethod m/pure State [_ v] (pure v))

(def get (State. (fn [s] [s s])))

(defn set [s*] (State. (fn [_] [s* nil])))

(defn update [f] (State. (fn [s] [(f s) nil])))

(defn run [s, ^State sm] ((.-run-state sm) s))

