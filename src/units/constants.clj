
(ns units.constants
  (:use units.siunits)
  (:use units.quantity)
  (:use units.unit))

(def speed-of-light (q* 299792458 meter (qpow sec -1)))
(def amu "The atomic mass unit." (q* 1.660538782e-27 kilogram))
(def avogadros-const (qd 6.02214179e23 mole))
(def boltzmanas-const (q* 1.3806504e-23 joule (qpow kelvin -1)))
(def gas-const (q* 8.314472 (q* joule mole kelvin)))
(def grav-const (q* 6.67428e-11 newton (qpow meter 2) (qpow kilogram -2)))
(def planks-const (q* 6.62606896e-34 joule sec))
(def bar-planks-const (qd planks-const 2 Math/PI))

(def permeability-fs (q* 4e-7 Math/PI tesla meter (qpow ampere -1)))
(def permittivity-fs (qd 1 (q* permeability-fs (qpow speed-of-light 2))))
(def coulomb-const (qd 1 (q* 4 Math/PI permittivity-fs)))

(def elem-charge (q* 1.602176487e-19 coulomb))
(def electron-volt (q* elem-charge volt))
(def proton-mass (q* 1.672621637e-27 kilogram))
(def neutron-mass (q* 1.674927211e-27 kilogram))
(def electron-mass (q* 9.10938215e-31 kilogram))
