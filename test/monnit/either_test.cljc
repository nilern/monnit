(ns monnit.either-test
  (:require [clojure.test :refer [deftest testing is]]
            [monnit.core :as m]
            [monnit.either :as e]))

(defn- unreachable [_] (assert false "unreachable"))

(deftest test-left
  (let [mv (e/left :oh-noes)]
    (is (= true (e/either? mv)))
    (is (= true (e/left? mv)))
    (is (= false (e/right? mv)))
    (is (= :oh-noes (e/fold identity unreachable mv)))

    (doseq [n (range 1 10)
            li (range 0 n)]
      (testing (str "fmap " n " " li)
        (let [mvs (-> (into [] (repeat n (e/right 5)))
                      (assoc li (e/left :oh-noes)))
              mv (apply m/fmap + mvs)]
          (is (= true (e/either? mv)))
          (is (= true (e/left? mv)))
          (is (= false (e/right? mv)))
          (is (= :oh-noes (e/fold identity unreachable mv))))))

    (testing "bind"
      (let [mv (m/bind mv (comp e/pure inc))]
        (is (= true (e/either? mv)))
        (is (= true (e/left? mv)))
        (is (= false (e/right? mv)))
        (is (= :oh-noes (e/fold identity unreachable mv)))))

    (testing "alt"
      (let [mv (m/alt mv (e/pure 5))]
        (is (= (e/right 5) mv))))))

(deftest test-right
  (let [mv (e/right 5)]
    (is (= true (e/either? mv)))
    (is (= false (e/left? mv)))
    (is (= true (e/right? mv)))
    (is (= 6 (e/fold unreachable inc mv)))

    (doseq [n (range 1 10)]
      (testing (str "fmap " n)
        (let [mv (apply m/fmap + (repeat n mv))]
          (is (= true (e/either? mv)))
          (is (= false (e/left? mv)))
          (is (= true (e/right? mv)))
          (is (= (inc (* n 5)) (e/fold unreachable inc mv))))))

    (testing "bind"
      (let [mv (m/bind mv (comp e/pure inc))]
        (is (= true (e/either? mv)))
        (is (= false (e/left? mv)))
        (is (= true (e/right? mv)))
        (is (= 7 (e/fold unreachable inc mv)))))

    (testing "alt"
      (let [mv (m/alt mv (e/pure 13))]
        (is (= (e/right 5) mv))))))

(deftest test-pure
  (let [mv (e/pure 5)]
    (is (= true (e/either? mv)))
    (is (= false (e/left? mv)))
    (is (= true (e/right? mv)))
    (is (= 6 (e/fold unreachable inc mv))))

  (testing "multimethod"
    (let [mv (e/pure 5)]
      (is (= true (e/either? mv)))
      (is (= false (e/left? mv)))
      (is (= true (e/right? mv)))
      (is (= 6 (e/fold unreachable inc mv))))))

