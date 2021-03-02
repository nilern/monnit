(ns monnit.core-test
  (:require [clojure.test :refer [deftest is]]
            [monnit.core :as m]))

(deftest vector-extensions
  (is (= (m/sconcat [1 2 3] [4 5 6]) [1 2 3 4 5 6]))
  (is (= (m/fmap inc [1 2 3]) [2 3 4]))
  (is (= (m/flat-map (fn [x] [x x]) [1 2 3]) [1 1 2 2 3 3]))
  (is (= (m/pure #?(:clj clojure.lang.APersistentVector, :cljs PersistentVector) 5) [5])))

