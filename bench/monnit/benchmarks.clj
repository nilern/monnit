(ns monnit.benchmarks
  (:require [monnit.core :as m]
            [monnit.state :as ms]
            [monnit.pair :as mp]

            [clojure.algo.monads :as a]

            [cats.core :as c]
            [cats.monad.state :as cs]

            [criterium.core :as criterium]))

(defn make-tree [depth]
  (if (= depth 0)
    [:leaf]
    [:branch (make-tree (dec depth)) (make-tree (dec depth))]))

(defn label-tree-core [tree]
  (letfn [(label [i tree]
            (case (first tree)
              :leaf [(inc i) [:leaf i]]
              :branch (let [[_ l r] tree
                            [i l] (label i l)
                            [i r] (label i r)]
                        [i [:branch l r]])))]
    (second (label 0 tree))))

(defn label-tree-monnit [tree]
  (letfn [(label [tree]
            (case (first tree)
              :leaf (m/bind ms/get
                            (fn [i] (m/bind (ms/set (inc i))
                                            (fn [_] (ms/pure [:leaf i])))))
              :branch (let [[_ l r] tree]
                        (m/bind (label l)
                                (fn [l] (m/bind (label r)
                                                (fn [r] (ms/pure [:branch l r]))))))))]
    (mp/snd (ms/run 0 (label tree)))))

(defn label-tree-algo [tree]
  (letfn [(label [tree]
            (case (first tree)
              :leaf (a/domonad a/state-m
                      [i (a/fetch-state)
                       _ (a/set-state (inc i))]
                      [:leaf i])
              :branch (let [[_ l r] tree]
                        (a/domonad a/state-m
                          [l (label l)
                           r (label r)]
                          [:branch l r]))))]
    (first ((label tree) 0))))

(defn label-tree-cats [tree]
  (letfn [(label [tree]
            (case (first tree)
              :leaf (c/mlet [i (cs/get)
                             _ (cs/put (inc i))]
                      (c/return [:leaf i]))
              :branch (let [[_ l r] tree]
                        (c/mlet [l (label l)
                                 r (label r)]
                          (c/return [:branch l r])))))]
    (cs/eval (label tree) 0)))

(defn benchmark-labeling [depth]
  (let [tree (make-tree depth)]
    (assert (= (label-tree-core tree)
               (label-tree-monnit tree)
               (label-tree-algo tree)
               (label-tree-cats tree)))

    (println "# clojure.core")
    (criterium/bench (label-tree-core tree))

    (println "# monnit.state")
    (criterium/bench (label-tree-monnit tree))

    (println "# clojure.algo.monads/state-m")
    (criterium/bench (label-tree-monnit tree))

    (println "# cats.monad.state")
    (criterium/bench (label-tree-cats tree))))

