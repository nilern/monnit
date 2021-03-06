(ns monnit.core
  "A library of Functors, Monads and other category theory abstractions.")

(defprotocol Semigroup
  "A type whose values can be combined with an associative binary operation."
  (-sconcat [self b] [self b c] [self b c d] [self b c d args]
    "Combine `self` with the other arguments, left to right. An implementation
    detail, call [[sconcat]] instead."))

(defn sconcat
  "Combine the arguments with their [[Semigroup]] combination operation."
  ([a] a)
  ([a b] (-sconcat a b))
  ([a b c] (-sconcat a b c))
  ([a b c d] (-sconcat a b c d))
  ([a b c d & args] (-sconcat a b c d args)))

(defprotocol Functor
  "A type whose 'contents' can be mapped with a function, producing a new value
  of the same type."
  (-fmap [a f] [a f b] [a f b c] [a f b c d] [a f b c d args]
    "[[fmap]] with the first [[Functor]] value first. An implementation
    detail, call [[fmap]] instead."))

(defn fmap
  "Map the function `f` over the [[Functor]]-implementing arguments, producing
  a new value of the same [[Functor]] type."
  ([f a] (-fmap a f))
  ([f a b] (-fmap a f b))
  ([f a b c] (-fmap a f b c))
  ([f a b c d] (-fmap a f b c d))
  ([f a b c d & args] (-fmap a f b c d args)))

(defprotocol Monad
  "A type whose 'contents' can be mapped with a function to other values of the containing
  type and then combined. When implementing this on a type, you should also implement
  [[pure]] for that type."
  (bind [self f]
    "[[flat-map]], but with the [[Monad]] value first."))

(defn flat-map
  "For a [[Monad]] `mv` (of type T<a>), call the function `f` (of type `a -> T<b>`)
  on the contained values and combine the results back into on result (of type `T<b>`)."
  [f mv]
  (bind mv f))

(defmulti pure "Wrap `v` into the [[Monad]] `type`." (fn [type v] type))

(defprotocol Alternative
  "A generalization of `or`."
  (alt [self other]
    "Return `self` unless it is considered a failure; otherwise return `other`."))

(extend-type #?(:clj clojure.lang.APersistentVector, :cljs PersistentVector)
  Semigroup
  (-sconcat
    ([a b] (persistent! (reduce conj! (transient a) b)))
    ([a b c]
     (let [acc (transient a)
           acc (reduce conj! acc b)
           acc (reduce conj! acc c)]
       (persistent! acc)))
    ([a b c d]
     (let [acc (transient a)
           acc (reduce conj! acc b)
           acc (reduce conj! acc c)
           acc (reduce conj! acc d)]
       (persistent! acc)))
    ([a b c d args]
     (let [acc (transient a)
           acc (reduce conj! acc b)
           acc (reduce conj! acc c)
           acc (reduce conj! acc d)
           acc (reduce (fn [acc arg] (reduce conj! acc arg)) acc args)]
       (persistent! acc))))

  Functor
  (-fmap
    ([self f] (mapv f self))
    ([self f b] (mapv f self b))
    ([self f b c] (mapv f self b c))
    ([self f b c d] (mapv f self b c d))
    ([self f b c d args] (apply mapv f self b c d args)))

  Monad
  (bind [self f] (into [] (mapcat f) self)))

(defmethod pure #?(:clj clojure.lang.APersistentVector, :cljs PersistentVector) [_ v] [v])

