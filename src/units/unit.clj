
(ns units.unit)

(declare unitless)
(declare unitless?)

(defprotocol Unit
  (umul- [x y] "Multiply one unit by another.")
  (udiv- [x y] "Divide one unit by another.")
  (uinv- [x] "Takes the inverse of a unit.")
  (usqrt- [x] "Takes the square root of a unit.")
  (unitless?- [x] "Is this unit unitless."))

(defn umul [& xs] (cond (empty? xs) unitless
                        (unitless? (first xs)) (apply #'umul (rest xs))
                        :else (umul- (first xs) (apply #'umul (rest xs)))))

(defn udiv [& xs] (cond (empty? xs) unitless
                        (== (count xs) 1) (first xs)
                        :else (let [[x y & ys] xs]
                                (if (unitless? x)
                                  (if (unitless? y)
                                    unitless
                                    (uinv- y))
                                  (apply #'udiv (udiv- x y) ys)))))

(defn usqrt [x] (cond (unitless? x) unitless
                      :else (usqrt- x)))

(defn unit? [x] (or (satisfies? Unit x)
                    (= unitless x)))

(def unitless "The unitless unit" 'unitless)

(defn unitless? [x] (or (= x unitless)
                        (if (unit? x) (unitless?- x))))
