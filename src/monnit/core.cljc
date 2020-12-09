(ns monnit.core)

(defprotocol Functor
  (-fmap [self f]))

(defprotocol Monad
  (-flat-map [self f]))

(defn fmap [f fv] (-fmap fv f))

(defn flat-map [f mv] (-flat-map mv f))

(def bind -flat-map)

