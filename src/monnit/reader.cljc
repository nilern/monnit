(ns monnit.reader
  "A computation with an implicit context parameter. An alternative to noisy
  explicit context parameters and the fragile thread locality of dynamically
  bound Vars. Implements [[monnit.core/Functor]] and [[monnit.core/Monad]]."
  (:refer-clojure :exclude [get])
  #?(:cljs (:require-macros [monnit.impl.reader-macros :refer [defreadertype]]))
  (:require [monnit.core :as m]
            #?(:clj [monnit.impl.reader-macros :refer [defreadertype]])))

(defprotocol Reader
  "A computation with an implicit context parameter. An alternative to noisy
  explicit context parameters and the fragile thread locality of dynamically
  bound Vars. Implements [[monnit.core/Functor]] and [[monnit.core/Monad]]."
  (reader? [self] "Is `self` a [[Reader]]?")
  (-run-reader [self ctx]
    "[[run]] with the [[Reader]] first. An implementation detail; call [[run]]
    instead."))

(extend-protocol Reader
  #?(:clj Object, :cljs default)
  (reader? [_] false)
  (-run-reader [self _]
    (assert false (str "-run-reader called on non-Reader value " self)))

  nil
  (reader? [_] false)
  (-run-reader [self _] (assert false "-run-reader called on nil")))

(declare ->FMap1 ->FMap2 ->FMap3 ->FMap4 ->FMapN ->Bind)

(defreadertype ^{:doc "A [[Reader]] that gets the value of the context parameter."} Get []
  Reader
  (reader? [_] true)
  (-run-reader [_ ctx] ctx))

(def get
  "A [[Reader]] that gets the value of the context parameter. An instance of [[Get]]"
  (Get.))

(defreadertype FMap1 [f a]
  Reader
  (reader? [_] true)
  (-run-reader [_ ctx] (f (-run-reader a ctx))))

(defreadertype FMap2 [f a b]
  Reader
  (reader? [_] true)
  (-run-reader [_ ctx] (f (-run-reader a ctx) (-run-reader b ctx))))

(defreadertype FMap3 [f a b c]
  Reader
  (reader? [_] true)
  (-run-reader [_ ctx] (f (-run-reader a ctx) (-run-reader b ctx) (-run-reader c ctx))))

(defreadertype FMap4 [f a b c d]
  Reader
  (reader? [_] true)
  (-run-reader [_ ctx]
    (f (-run-reader a ctx) (-run-reader b ctx) (-run-reader c ctx) (-run-reader d ctx))))

(defreadertype FMapN [f a b c d args]
  Reader
  (reader? [_] true)
  (-run-reader [_ ctx]
    (apply f (-run-reader a ctx) (-run-reader b ctx) (-run-reader c ctx) (-run-reader d ctx)
           (map #(-run-reader % ctx) args))))

(defreadertype Bind [a f]
  Reader
  (reader? [_] true)
  (-run-reader [_ ctx] (-run-reader (f (-run-reader a ctx)) ctx)))

(deftype ^{:doc "A [[Reader]] that just contains a value and does not use the context parameter."} Pure [v]
  Reader
  (reader? [_] true)
  (-run-reader [_ _] v)

  m/Functor
  (-fmap [_ f] (Pure. (f v)))
  (-fmap [self f b] (FMap2. f self b))
  (-fmap [self f b c] (FMap3. f self b c))
  (-fmap [self f b c d] (FMap4. f self b c d))
  (-fmap [self f b c d args] (FMapN. f self b c d args))

  m/Monad
  (bind [_ f] (f v)))

(def ^{:arglists '([v])} pure "Wrap `v` in a [[Reader]]."->Pure)

(defmethod m/pure Reader [_ v] (pure v))

(defn run
  "Run the [[Reader]] computation `rm` with `ctx` as the context parameter value."
  [ctx rm]
  (-run-reader rm ctx))

