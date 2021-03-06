(ns monnit.result-test
  (:require [clojure.test :refer [deftest testing is]]
            [monnit.core :as m]
            [monnit.result :as r]))

(defn- unreachable [_] (assert false "unreachable"))

(deftest test-err
  (let [mv (r/err :oh-noes)]
    (is (= true (r/result? mv)))
    (is (= true (r/err? mv)))
    (is (= false (r/ok? mv)))
    (is (= :oh-noes (r/run identity unreachable mv)))

    (doseq [n (range 1 10)
            li (range 0 n)]
      (testing (str "fmap " n " " li)
        (let [mvs (-> (into [] (repeat n (r/ok 5)))
                      (assoc li (r/err :oh-noes)))
              mv (apply m/fmap + mvs)]
          (is (= true (r/result? mv)))
          (is (= true (r/err? mv)))
          (is (= false (r/ok? mv)))
          (is (= :oh-noes (r/run identity unreachable mv))))))

    (testing "bind"
      (let [mv (m/bind mv (comp r/pure inc))]
        (is (= true (r/result? mv)))
        (is (= true (r/err? mv)))
        (is (= false (r/ok? mv)))
        (is (= :oh-noes (r/run identity unreachable mv)))))

    (testing "alt"
      (let [mv (m/alt mv (r/pure 5))]
        (is (= (r/ok 5) mv))))))

(deftest test-ok
  (let [mv (r/ok 5)]
    (is (= true (r/result? mv)))
    (is (= false (r/err? mv)))
    (is (= true (r/ok? mv)))
    (is (= 6 (r/run unreachable inc mv)))

    (doseq [n (range 1 10)]
      (testing (str "fmap " n)
        (let [mv (apply m/fmap + (repeat n mv))]
          (is (= true (r/result? mv)))
          (is (= false (r/err? mv)))
          (is (= true (r/ok? mv)))
          (is (= (inc (* n 5)) (r/run unreachable inc mv))))))

    (testing "bind"
      (let [mv (m/bind mv (comp r/pure inc))]
        (is (= true (r/result? mv)))
        (is (= false (r/err? mv)))
        (is (= true (r/ok? mv)))
        (is (= 7 (r/run unreachable inc mv)))))

    (testing "alt"
      (let [mv (m/alt mv (r/pure 13))]
        (is (= (r/ok 5) mv))))))

(deftest test-pure
  (let [mv (r/pure 5)]
    (is (= true (r/result? mv)))
    (is (= false (r/err? mv)))
    (is (= true (r/ok? mv)))
    (is (= 6 (r/run unreachable inc mv))))

  (testing "multimethod"
    (let [mv (r/pure 5)]
      (is (= true (r/result? mv)))
      (is (= false (r/err? mv)))
      (is (= true (r/ok? mv)))
      (is (= 6 (r/run unreachable inc mv))))))

