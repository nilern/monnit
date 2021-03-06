(ns monnit.option-test
  (:require [clojure.test :refer [deftest testing is]]
            [monnit.core :as m]
            [monnit.option :as o]))

(defn- unreachable [_] (assert false "unreachable"))

(deftest test-none
  (let [mv o/none]
    (is (= true (o/option? mv)))
    (is (= true (o/none? mv)))
    (is (= false (o/some? mv)))
    (is (= ::nope (o/run ::nope unreachable mv)))

    (doseq [n (range 1 10)
            li (range 0 n)]
      (testing (str "fmap " n " " li)
        (let [mvs (-> (into [] (repeat n (o/some 5)))
                      (assoc li o/none))
              mv (apply m/fmap + mvs)]
          (is (= true (o/option? mv)))
          (is (= true (o/none? mv)))
          (is (= false (o/some? mv)))
          (is (= ::nope (o/run ::nope unreachable mv))))))

    (testing "bind"
      (let [mv (m/bind mv (comp o/pure inc))]
        (is (= true (o/option? mv)))
        (is (= true (o/none? mv)))
        (is (= false (o/some? mv)))
        (is (= ::nope (o/run ::nope unreachable mv)))))

    (testing "alt"
      (let [mv (m/alt mv (o/pure 5))]
        (is (= (o/some 5) mv))))))

(deftest test-some
  (let [mv (o/some 5)]
    (is (= true (o/option? mv)))
    (is (= false (o/none? mv)))
    (is (= true (o/some? mv)))
    (is (= 6 (o/run nil inc mv)))

    (doseq [n (range 1 10)]
      (testing (str "fmap " n)
        (let [mv (apply m/fmap + (repeat n mv))]
          (is (= true (o/option? mv)))
          (is (= false (o/none? mv)))
          (is (= true (o/some? mv)))
          (is (= (inc (* n 5)) (o/run nil inc mv))))))

    (testing "bind"
      (let [mv (m/bind mv (comp o/pure inc))]
        (is (= true (o/option? mv)))
        (is (= false (o/none? mv)))
        (is (= true (o/some? mv)))
        (is (= 7 (o/run nil inc mv)))))

    (testing "alt"
      (let [mv (m/alt mv (o/pure 13))]
        (is (= (o/some 5) mv))))))

(deftest test-pure
  (let [mv (o/pure 5)]
    (is (= true (o/option? mv)))
    (is (= false (o/none? mv)))
    (is (= true (o/some? mv)))
    (is (= 6 (o/run nil inc mv))))

  (testing "multimethod"
    (let [mv (o/pure 5)]
      (is (= true (o/option? mv)))
      (is (= false (o/none? mv)))
      (is (= true (o/some? mv)))
      (is (= 6 (o/run nil inc mv))))))

