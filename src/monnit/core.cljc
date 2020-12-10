(ns monnit.core)

(defprotocol Functor
  (-fmap [a f] [a f b] [a f b c] [a f b c d] [a f b c d args]))

(defn fmap
  ([f a] (-fmap a f))
  ([f a b] (-fmap a f b))
  ([f a b c] (-fmap a f b c))
  ([f a b c d] (-fmap a f b c d))
  ([f a b c d & args] (-fmap a f b c d args)))

(defprotocol Monad
  (bind [self f]))

(defn flat-map [f mv] (bind mv f))

(defmulti pure (fn [type _] type))

(defprotocol Alternative
  (alt [self other]))

