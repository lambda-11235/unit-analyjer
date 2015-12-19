
=============
unit-analyjur
=============

unit-analyjur is a small, but practical, library for doing SI unit anlysis in
clojure.  This library's goal is to provide a clean interface for carrying SI
units through complex equations. Run lein codox for documentation.

Example:

.. code :: clojure
   user=> (use '[units unit quantity siunits constants])
   nil
   user=> (def m (q* 31 (kilo gram)))
   #'user/m
   user=> m
   31 kg^1
   user=> (defn mass->energy [mass] (q* mass (qpow speed-of-light 2)))
   #'user/mass->energy
   user=> (mass->energy m)
   2786141054084134684 J
