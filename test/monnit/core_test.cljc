(ns monnit.core-test
  (:require [clojure.test :refer [deftest is]]
            [monnit.core :as m]
            [monnit.state :as s]
            [monnit.pair :as p]))

(deftest vector-extensions
  (is (= (m/sconcat [1 2 3] [4 5 6]) [1 2 3 4 5 6]))
  (is (= (m/fmap inc [1 2 3]) [2 3 4]))
  (is (= (m/flat-map (fn [x] [x x]) [1 2 3]) [1 1 2 2 3 3]))
  (is (= (m/pure #?(:clj clojure.lang.APersistentVector, :cljs PersistentVector) 5) [5])))

(deftest test-mlet
  (is (= (p/pair 0 5) (s/run 0 (m/mlet [] (s/pure 5)))))
  (is (= (p/pair 0 6) (s/run 0 (m/mlet [i s/get] (s/pure (+ 5 (inc i)))))))
  (is (= (p/pair 1 6) (s/run 0 (m/mlet [i s/get
                                        _ (s/set (inc i))
                                        i s/get]
                                 (s/pure (+ 5 i))))))
  (is (= (p/pair 1 6) (s/run 0 (m/mlet [i s/get
                                        _ (s/set (inc i))
                                        i s/get
                                        :let [res (+ 5 i)]]
                                 (s/pure res))))))

