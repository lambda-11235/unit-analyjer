
(ns units.unit)

(declare unitless)
(declare unitless?)
(declare upow)

(defprotocol Unit
  (umul- [x y] "Multiply one unit by another.")
  (udiv- [x y] "Divide one unit by another.")
  (upow- [x n] "Raise a unit to the nth power.")
  (unitless?- [x] "Is this unit unitless."))

(defn umul [& xs] (cond (empty? xs) unitless
                        (unitless? (first xs)) (apply #'umul (rest xs))
                        :else (umul- (first xs) (apply #'umul (rest xs)))))

(defn udiv [& xs] (cond (empty? xs) unitless
                        (== (count xs) 1) (first xs)
                        :else (let [[x y & ys] xs]
                                (if (unitless? x)
                                  (apply #'udiv (upow y -1) ys)
                                  (apply #'udiv (udiv- x y) ys)))))

(defn upow [x n] (cond (unitless? x) unitless
                       :else (upow- x n)))

(defn unit? [x] (or (satisfies? Unit x)
                    (= unitless x)))

(def unitless "The unitless unit" 'unitless)

(defn unitless? [x] (or (= x unitless)
                        (if (unit? x) (unitless?- x))))
