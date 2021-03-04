(ns monnit.identity-test
  (:require [clojure.test :refer [deftest testing is]]
            [monnit.core :as m]
            [monnit.identity :as id]))

(deftest test-pure
  (let [mv (id/pure 5)]
    (is (= true (id/identity? mv)))
    (is (= 5 (id/run mv)))

    (doseq [n (range 1 10)]
      (testing (str "fmap " n)
        (let [mv (apply m/fmap + (map id/pure (repeat n 5)))]
          (is (= true (id/identity? mv)))
          (is (= (* n 5) (id/run mv))))))

    (is (= 6 (id/run (m/bind (id/pure 5) (comp id/pure inc)))))))

(deftest test-multi-pure
  (let [mv (m/pure id/Identity 5)]
    (is (= true (id/identity? mv)))
    (is (= 5 (id/run mv)))))

