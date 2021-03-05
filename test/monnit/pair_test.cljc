(ns monnit.pair-test
  (:require [clojure.test :refer [deftest testing is]]
            [monnit.core :as m]
            [monnit.pair :as p]))

(deftest test-pair
  (let [p (p/pair 1 2)]
    (is (= true (p/pair? p)))
    (is (= 1 (p/fst p)))
    (is (= 2 (p/snd p)))))

(deftest test-functor
  (doseq [n (range 1 10)]
    (testing (str "fmap " n)
      (let [p (apply m/fmap + (map #(p/pair [%] 5) (range n)))]
        (is (= true (p/pair? p)))
        (is (= (range n) (p/fst p)))
        (is (= (* n 5) (p/snd p)))))))

