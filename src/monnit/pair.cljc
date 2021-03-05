(ns monnit.pair
  (:require [monnit.core :as m]))

(declare ->Pair)

(defrecord Pair [fst snd]
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

(def pair ->Pair)

(defn pair? [p] (instance? Pair p))

(defn fst [^Pair p] (.-fst p))

(defn snd [^Pair p] (.-snd p))

