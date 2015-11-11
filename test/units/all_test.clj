(ns units.all-test
  (:use [units quantity siunits unit])
  (:require [clojure.test :refer :all]))

(deftest equality
  (testing "Unit Equality Testing"
    (is (same-units meter meter))
    (is (same-units meter (q* 2 meter)))
    (is (same-units 0 (qd meter meter)))
    (is (same-units 0 0))

    ;; NOTE: 0 != 0 m
    (is (q= meter meter))
    (is (q= 0 0.0))
    ))

(deftest arith-test
  (testing "Arithmetic"
    (is (q= (q* 2 meter) (q+ meter meter)))
    (is (thrown? AssertionError (q+ meter sec)))

    (is (q= (q* 0 meter) (q- meter meter)))
    (is (thrown? AssertionError (q- meter sec)))

    (is (q= 1/6 (qd 1 2 3)))
    ))
