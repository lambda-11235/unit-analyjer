
(ns units.siunits
  (:use units.unit)
  (:use units.quantity))

(declare siunit?)
(declare hertz newton pascal joule watt coulomb volt farad ohm siemens weber
         tesla henry lumen lux becquerel gray sievert katal)

(defrecord SIUnit [m kg s A K mol cd]
  Object

  (toString [this]
    (condp = this
      hertz "Hz"
      newton "N"
      pascal "Pa"
      joule "J"
      watt "W"
      coulomb "C"
      volt "V"
      farad "F"
      ohm "Ω"
      siemens "S"
      weber "Wb"
      tesla "T"
      henry "H"
      lumen "lm"
      lux "lx"
      becquerel "Bq"
      gray "Gy"
      sievert "Sv"
      katal "kat"

      (let [xs (filter #(not (== (second %) 0)) this)]
        (apply #'str (interpose " " (map #(str (name (first %)) "^" (second %))
                                         xs))))))

  Unit

  (umul- [x y]
    {:pre [(siunit? y)]}
    (if (unitless? y)
      x
      (let [{m1 :m kg1 :kg s1 :s A1 :A K1 :K mol1 :mol cd1 :cd} x
            {m2 :m kg2 :kg s2 :s A2 :A K2 :K mol2 :mol cd2 :cd} y]
        (SIUnit. (+ m1 m2) (+ kg1 kg2) (+ s1 s2) (+ A1 A2) (+ K1 K2)
                 (+ mol1 mol2) (+ cd1 cd2)))))

  (udiv- [x y]
    {:pre [(siunit? y)]}
    (if (unitless? y)
      x
      (let [{m1 :m kg1 :kg s1 :s A1 :A K1 :K mol1 :mol cd1 :cd} x
            {m2 :m kg2 :kg s2 :s A2 :A K2 :K mol2 :mol cd2 :cd} y]
        (SIUnit. (- m1 m2) (- kg1 kg2) (- s1 s2) (- A1 A2) (- K1 K2)
                 (- mol1 mol2) (- cd1 cd2)))))

  (upow- [{m1 :m kg1 :kg s1 :s A1 :A K1 :K mol1 :mol cd1 :cd} n]
    {:pre [(number? n)]}
    (SIUnit. (* m1 n) (* kg1 n) (* s1 n) (* A1 n) (* K1 n) (* mol1 n)
             (* cd1 n)))

  (unitless?- [u]
    (every? zero? (vals u)))
  )

(defmethod print-method SIUnit [u ^java.io.Writer w]
  (.write w (str u)))

(defn siunit [m kg s A K mol cd]
  {:pre [(every? number? [m kg s A K mol cd])]}
  (SIUnit. m kg s A K mol cd))

(defn siunit? [x] (or (instance? SIUnit x)
                      (unitless? x)))


;; Unit prefixes
(defn yotta [q] (q* q 1e24))
(defn zetta [q] (q* q 1e21))
(defn exa [q] (q* q 1e18))
(defn peta [q] (q* q 1e15))
(defn tera [q] (q* q 1e12))
(defn giga [q] (q* q 1e9))
(defn mega [q] (q* q 1e6))
(defn kilo [q] (q* q 1000))
(defn hecto [q] (q* q 100))
(defn deka [q] (q* q 10))
(defn deci [q] (qd q 10))
(defn centi [q] (qd q 100))
(defn milli [q] (qd q 1000))
(defn micro [q] (qd q 1e6))
(defn nano [q] (qd q 1e9))
(defn pico [q] (qd q 1e12))
(defn femto [q] (qd q 1e15))
(defn atto [q] (qd q 1e18))
(defn zepto [q] (qd q 1e21))
(defn yocto [q] (qd q 1e24))

;; Base units
(def meter (siunit 1 0 0 0 0 0 0))
(def kilogram (siunit 0 1 0 0 0 0 0))
(def sec (siunit 0 0 1 0 0 0 0))
(def ampere (siunit 0 0 0 1 0 0 0))
(def kelvin (siunit 0 0 0 0 1 0 0))
(def mole (siunit 0 0 0 0 0 1 0))
(def candela (siunit 0 0 0 0 0 0 1))

(def gram (qd kilogram 1000))


;; Derived units
(def radian unitless)
(def steradian unitless)
(def hertz (siunit 0 0 -1 0 0 0 0))
(def newton (siunit 1 1 -2 0 0 0 0))
(def pascal (siunit -1 1 -2 0 0 0 0))
(def joule (siunit 2 1 -2 0 0 0 0))
(def watt (siunit 2 1 -3 0 0 0 0))
(def coulomb (siunit 0 0 1 1 0 0 0))
(def volt (siunit 2 1 -3 -1 0 0 0))
(def farad (siunit -2 -1 4 2 0 0 0))
(def ohm (siunit 2 1 -3 -2 0 0 0))
(def siemens (siunit -2 -1 3 2 0 0 0))
(def weber (siunit 2 1 -2 -1 0 0 0))
(def tesla (siunit 0 1 -2 -1 0 0 0))
(def henry (siunit 2 1 -2 -2 0 0 0))
(def lumen (siunit 0 0 0 0 0 0 1))
(def lux (siunit -2 0 0 0 0 0 1))
(def becquerel (siunit 0 0 -1 0 0 0 0))
(def gray (siunit 2 0 -2 0 0 0 0))
(def sievert (siunit 2 0 -2 0 0 0 0))
(def katal (siunit 0 0 -1 0 0 1 0))
