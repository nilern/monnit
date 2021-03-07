(ns monnit.state-test
  (:require [clojure.test :refer [deftest testing is]]
            [monnit.core :as m]
            [monnit.state :as s]
            [monnit.pair :refer [->Pair]]))

(deftest test-get
  (let [mv s/get]
    (is (= true (s/state? mv)))
    (is (= (->Pair {} {}) (s/run {} mv)))

    (doseq [n (range 1 10)]
      (testing (str "fmap " n)
        (let [mv (apply m/fmap + (repeat n mv))]
          (is (= true (s/state? mv)))
          (is (= (->Pair 5 (* n 5)) (s/run 5 mv)))

          (doseq [m (range 1 10)]
            (testing (str "nested fmap " m)
              (let [mv (apply m/fmap + (repeat m mv))]
                (is (= true (s/state? mv)))
                (is (= (->Pair 5 (* n m 5)) (s/run 5 mv))))))

          (testing "nested bind"
            (let [mv (m/bind mv (comp s/pure inc))]
              (is (= true (s/state? mv)))
              (is (= (->Pair 5 (inc (* n 5))) (s/run 5 mv))))))))

    (let [mv (m/bind mv (comp s/pure inc))]
      (is (= true (s/state? mv)))
      (is (= (->Pair 5 6) (s/run 5 mv))))))

(deftest test-set
  (let [mv (s/set [])]
    (is (= true (s/state? mv)))
    (is (= (->Pair [] nil) (s/run {} mv)))

    (let [mv (m/fmap identity mv)]
      (is (= true (s/state? mv)))
      (is (= (->Pair [] nil) (s/run {} mv))))

    (let [mv (m/bind mv (fn [_] s/get))]
      (is (= true (s/state? mv)))
      (is (= (->Pair [] []) (s/run {} mv))))))

(deftest test-update
  (let [mv (s/update inc)]
    (is (= true (s/state? mv)))
    (is (= (->Pair 6 6) (s/run 5 mv)))

    (let [mv (m/fmap inc mv)]
      (is (= true (s/state? mv)))
      (is (= (->Pair 6 7) (s/run 5 mv))))

    (let [mv (m/bind mv (comp s/pure inc))]
      (is (= true (s/state? mv)))
      (is (= (->Pair 6 7) (s/run 5 mv))))))

(deftest test-pure
  (let [mv (s/pure 5)]
    (is (= true (s/state? mv)))
    (is (= (->Pair {} 5) (s/run {} mv)))

    (doseq [n (range 1 10)]
      (testing (str "fmap " n)
        (let [mv (apply m/fmap + (map s/pure (repeat n 5)))]
          (is (= true (s/state? mv)))
          (is (= (->Pair {} (* n 5)) (s/run {} mv)))

          (doseq [m (range 1 10)]
            (testing (str "nested fmap " m)
              (let [mv (apply m/fmap + (repeat m mv))]
                (is (= true (s/state? mv)))
                (is (= (->Pair {} (* n m 5)) (s/run {} mv))))))

          (testing "nested bind"
            (let [mv (m/bind mv (comp s/pure inc))]
              (is (= true (s/state? mv)))
              (is (= (->Pair {} (inc (* n 5))) (s/run {} mv))))))))

    (let [mv (m/bind (s/pure 5) (comp s/pure inc))]
      (is (= true (s/state? mv)))
      (is (= (->Pair {} 6) (s/run {} mv))))))

(deftest test-multi-pure
  (let [mv (m/pure s/State 5)]
    (is (= true (s/state? mv)))
    (is (= (->Pair {} 5) (s/run {} mv)))))

(deftest test-eval
  (is (= 5 (s/eval {} (s/pure 5)))))

(deftest test-exec
  (is (= {} (s/exec {} (s/pure 5)))))

