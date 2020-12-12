(ns monnit.core)

(defprotocol Semigroup
  (-sconcat [self b] [self b c] [self b c d] [self b c d args]))

(defn sconcat
  ([a] a)
  ([a b] (-sconcat a b))
  ([a b c] (-sconcat a b c))
  ([a b c d] (-sconcat a b c d))
  ([a b c d & args] (-sconcat a b c d args)))

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

