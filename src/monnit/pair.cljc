(ns monnit.pair
  "A two-element tuple, smaller than a vector."
  (:require [monnit.core :as m]))

(declare ->Pair)

(defrecord ^{:doc "A two-element tuple, smaller than a vector."} Pair [fst snd]
  m/Functor
  (-fmap [_ f] (->Pair fst (f snd)))
  (-fmap [_ f b]
    (->Pair (m/sconcat fst (.-fst ^Pair b))
            (f snd (.-snd ^Pair b))))
  (-fmap [_ f b c]
    (->Pair (m/sconcat fst (.-fst ^Pair b) (.-fst ^Pair c))
            (f snd (.-snd ^Pair b) (.-snd ^Pair c))))
  (-fmap [_ f b c d]
    (->Pair (m/sconcat fst (.-fst ^Pair b) (.-fst ^Pair c) (.-fst ^Pair d))
            (f snd (.-snd ^Pair b) (.-snd ^Pair c) (.-snd ^Pair d))))
  (-fmap [_ f b c d args]
    (->Pair (apply m/sconcat fst (.-fst ^Pair b) (.-fst ^Pair c) (.-fst ^Pair d)
                   (map (fn [^Pair arg] (.-fst arg)) args))
            (apply f snd (.-snd ^Pair b) (.-snd ^Pair c) (.-snd ^Pair d)
                   (map (fn [^Pair arg] (.-snd arg)) args)))))

(def ^{:arglists '([fst snd])} pair "Create a [[Pair]]" ->Pair)

(defn pair? "Is `p` a pair?" [p] (instance? Pair p))

(defn fst "Get the first element of the [[Pair]] `p`" [^Pair p] (.-fst p))

(defn snd "Get the second element of the [[Pair]] `p`" [^Pair p] (.-snd p))

