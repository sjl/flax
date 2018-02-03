(defpackage :flax.base
  (:use :cl :iterate :losh :flax.quickutils)
  (:export :rand :with-seed))

(defpackage :flax.coordinates
  (:use :cl :iterate :losh :flax.base :flax.quickutils)
  (:export
    :coord :x :y
    :distance
    :clerp))

(defpackage :flax.drawing
  (:use :cl :iterate :losh :flax.base :flax.quickutils
    :flax.coordinates)
  (:export
    :render
    :triangle
    :line))

(defpackage :flax.looms.001-triangles
  (:use :cl :iterate :losh :flax.base :flax.quickutils
    :flax.coordinates)
  (:export :loom))


