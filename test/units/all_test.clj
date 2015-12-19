(ns units.all-test
  (:use [units quantity siunits unit]
        [clojure.test.check [clojure-test :only [defspec]]])
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def siunit-gen
  (gen/fmap (partial apply ->SIUnit)
            (apply gen/tuple (repeat 7 gen/int))))

(def quantity-gen
  (gen/let [n gen/int u siunit-gen]
    (quantity n u)))

(def quantities-same-unit-gen
  (gen/let [n gen/int m gen/int u siunit-gen]
    [(quantity n u) (quantity m u)]))


(defspec quant-eq-prop
  100
  (prop/for-all [n quantity-gen]
                (q= n n)))

(defspec quant-add-assoc-prop
  100
  (prop/for-all [[n m] quantities-same-unit-gen]
                (q= (q+ n m) (q+ m n))))

(defspec quant-mult-ident-prop
  100
  (prop/for-all [n quantity-gen]
                (and (q= (q* n 1) n)
                     (q= (qd n 1) n))))

(defspec quant-mult-assoc-prop
  100
  (prop/for-all [n quantity-gen m quantity-gen]
                (q= (q* n m) (q* m n))))

(defspec same-unit-prop
  100
  (prop/for-all [[n m] quantities-same-unit-gen]
                (same-units n m)))
