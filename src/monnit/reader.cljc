(ns monnit.reader
  (:refer-clojure :exclude [get])
  #?(:cljs (:require-macros [monnit.reader-macros :refer [defreadertype]]))
  (:require [monnit.core :as m]
            #?(:clj [monnit.reader-macros :refer [defreadertype]])))

(defprotocol Reader
  (reader? [self])
  (-run-reader [self ctx]))

(extend-protocol Reader
  #?(:clj Object, :cljs default)
  (reader? [_] false)
  (-run-reader [_ _] (assert false))) ; FIXME: error message

(declare ->FMap1 ->FMap2 ->FMap3 ->FMap4 ->FMapN ->Bind)

(defreadertype Get []
  Reader
  (reader? [_] true)
  (-run-reader [_ ctx] ctx))

(def get (Get.))

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

(deftype Pure [v]
  Reader
  (reader? [_] true)
  (-run-reader [_ _] v)

  m/Functor
  (-fmap [_ f] (Pure. (f v)))
  (-fmap [self f b] (->FMap2 self f b))
  (-fmap [self f b c] (->FMap3 self f b c))
  (-fmap [self f b c d] (->FMap4 self f b c d))
  (-fmap [self f b c d args] (->FMapN self f b c d args))

  m/Monad
  (bind [_ f] (f v)))

(def pure ->Pure)

(defmethod m/pure Reader [_ v] (pure v))

(defn run [ctx rm] (-run-reader rm ctx))
