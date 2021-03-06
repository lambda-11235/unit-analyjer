
(ns units.quantity
  (:use units.unit))

(declare quantity?)

(defrecord Quantity [measure unit]
  Object

  (toString [this]
    (if (unitless? (:unit this))
      (str (:measure this))
      (str (:measure this) " " (:unit this))))
  )

(defmethod print-method Quantity [q ^java.io.Writer w]
  (.write w (str q)))


(defn quantity
  "Given on argument this function coerces it into a quantity.
  Given a number and a unit it constructs a quantity."
  ([x] {:pre [(or (quantity? x) (number? x) (unit? x))]}
   (cond (quantity? x) x
         (number? x) (Quantity. x unitless)
         (unit? x) (Quantity. 1 x)))

  ([m u] {:pre [(number? m) (unit? u)]}
   (Quantity. m u)))

(defn quantity? [x] (instance? Quantity x))

(defn same-units
  "Determine if two quantities have the same units."
  [x y]
  (let [xu (:unit (quantity x)) yu (:unit (quantity y))]
    (or (and (unitless? xu) (unitless? yu))
        (= xu yu))))

(defn map-measure
  "Applies a function to the measure part of a quantity."
  [f x]
  {:pre [(fn? f) (quantity? x)]}
  (quantity (f (:measure x)) (:unit x)))

(defn map-unit
  "Applies a function to the unit part of a quantity."
  [f x]
  {:pre [(fn? f) (quantity? x)]}
  (quantity (:measure x) (f (:unit x))))

(defn q= [x y]
  (let [xq (quantity x) yq (quantity y)]
    (and (== (:measure xq) (:measure yq))
         (same-units xq yq))))

(defn- -q+ [x y]
  {:pre [(or (q= x 0) (q= y 0) (same-units x y))]
   :post [(quantity? %)]}
  (let [xq (quantity x) yq (quantity y)]
    (cond (q= xq 0) yq
          (q= yq 0) xq
          :else (quantity (+ (:measure xq) (:measure yq)) (:unit xq)))))

(defn q+ [& xs] (reduce -q+ xs))

(defn- -q- [x y]
  {:pre [(or (q= x 0) (q= y 0) (same-units x y))]
   :post [(quantity? %)]}
  (let [xq (quantity x) yq (quantity y)]
    (cond (q= xq 0) yq
          (q= yq 0) xq
          :else (quantity (- (:measure xq) (:measure yq)) (:unit xq)))))

(defn q- [& xs] (reduce -q- xs))

(defn- -q* [x y]
  {:post [(quantity? %)]}
  (let [xq (quantity x) yq (quantity y)]
    (quantity (* (:measure xq) (:measure yq)) (umul (:unit xq) (:unit yq)))))

(defn q* [& xs] (reduce -q* xs))

(defn- -qd [x y]
  {:post [(quantity? %)]}
  (let [xq (quantity x) yq (quantity y)]
    (quantity (/ (:measure xq) (:measure yq)) (udiv (:unit xq) (:unit yq)))))

(defn qd [& xs] (reduce -qd xs))

(defn qpow [x n]
  {:pre [(integer? n)]}
  (cond (== n 0) 1
        (< n 0) (qd 1 (qpow x (- n)))
        :else (loop [res x i n]
                (if (== i 1)
                  res
                  (recur (q* res x) (dec i))))))

(defn qsqrt [x]
  (let [q (quantity x)]
    (quantity (Math/sqrt (:measure q)) (usqrt (:unit q)))))
