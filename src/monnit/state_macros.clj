(ns monnit.state-macros
  (:require [monnit.core :as m]))

(defmacro defstatetype [name fields & impls]
  (concat
    `(deftype ~name ~fields ~@impls)
    '(m/Functor
      (-fmap [self f] (->FMap1 f self))
      (-fmap [self f b] (->FMap2 f self b))
      (-fmap [self f b c] (->FMap3 f self b c))
      (-fmap [self f b c d] (->FMap4 f self b c d))
      (-fmap [self f b c d args] (->FMapN f self b c d args))

      m/Monad
      (bind [self f] (->Bind self f)))))

