(ns monnit.reader-test
  (:require [clojure.test :refer [deftest testing is]]
            [monnit.core :as m]
            [monnit.reader :as r]))

(deftest test-get
  (let [mv r/get]
    (is (= true (r/reader? mv)))
    (is (= {} (r/run {} mv)))

    (doseq [n (range 1 10)]
      (testing (str "fmap " n)
        (let [mv (apply m/fmap + (repeat n mv))]
          (is (= true (r/reader? mv)))
          (is (= (* n 5) (r/run 5 mv)))

          (doseq [m (range 1 10)]
            (testing (str "nested fmap " m)
              (let [mv (apply m/fmap + (repeat m mv))]
                (is (= true (r/reader? mv)))
                (is (= (* n m 5) (r/run 5 mv))))))

          (testing "nested bind"
            (let [mv (m/bind mv (comp r/pure inc))]
              (is (= true (r/reader? mv)))
              (is (= (inc (* n 5)) (r/run 5 mv))))))))

    (let [mv (m/bind mv (comp r/pure inc))]
      (is (= true (r/reader? mv)))
      (is (= 6 (r/run 5 mv))))))

(deftest test-pure
  (let [mv (r/pure 5)]
    (is (= true (r/reader? mv)))
    (is (= 5 (r/run {} mv)))

    (doseq [n (range 1 10)]
      (testing (str "fmap " n)
        (let [mv (apply m/fmap + (map r/pure (repeat n 5)))]
          (is (= true (r/reader? mv)))
          (is (= (* n 5) (r/run {} mv)))

          (doseq [m (range 1 10)]
            (testing (str "nested fmap " m)
              (let [mv (apply m/fmap + (repeat m mv))]
                (is (= true (r/reader? mv)))
                (is (= (* n m 5) (r/run {} mv))))))

          (testing "nested bind"
            (let [mv (m/bind mv (comp r/pure inc))]
              (is (= true (r/reader? mv)))
              (is (= (inc (* n 5)) (r/run {} mv))))))))

    (let [mv (m/bind (r/pure 5) (comp r/pure inc))]
      (is (= true (r/reader? mv)))
      (is (= 6 (r/run {} mv))))))

(deftest test-multi-pure
  (let [mv (m/pure r/Reader 5)]
    (is (= true (r/reader? mv)))
    (is (= 5 (r/run {} mv)))))

