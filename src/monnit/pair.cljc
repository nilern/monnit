(ns monnit.pair
  (:require [monnit.core :as m]))

(declare ->Pair)

(defrecord Pair [left right]
  m/Functor
  (-fmap [_ f] (->Pair left (f right)))
  (-fmap [_ f b]
    (->Pair (m/sconcat left (.-left ^Pair b))
            (f right (.-right ^Pair b))))
  (-fmap [_ f b c]
    (->Pair (m/sconcat left (.-left ^Pair b) (.-left ^Pair c))
            (f right (.-right ^Pair b) (.-right ^Pair c))))
  (-fmap [_ f b c d]
    (->Pair (m/sconcat left (.-left ^Pair b) (.-left ^Pair c) (.-left ^Pair d))
            (f right (.-right ^Pair b) (.-right ^Pair c) (.-right ^Pair d))))
  (-fmap [_ f b c d args]
    (->Pair (apply m/sconcat left (.-left ^Pair b) (.-left ^Pair c) (.-left ^Pair d)
                   (map (fn [^Pair arg] (.-left arg)) args))
            (apply f right (.-right ^Pair b) (.-right ^Pair c) (.-left ^Pair d)
                   (map (fn [^Pair arg] (.-right arg)) args)))))

