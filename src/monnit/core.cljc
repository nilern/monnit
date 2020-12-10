(ns monnit.core)

(defprotocol Functor
  (-fmap [self f]))

(defn fmap [f fv] (-fmap fv f))

(defprotocol Applicative
  (fapply [self fv]))

(defprotocol Monad
  (bind [self f]))

(defn flat-map [f mv] (bind mv f))

(defmulti pure (fn [type _] type))

(defprotocol Alternative
  (alt [self other]))

