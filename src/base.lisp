(in-package :flax.base)

(defun rand (bound)
  (pcg:pcg-random t bound))

(defmacro with-seed (seed &body body)
  `(let ((pcg::*global-generator* (pcg:make-pcg :seed ,seed)))
     (losh::clear-gaussian-spare)
     ,@body))


(defun round-to (number precision)
  "Round `number` to the given `precision`.

  Examples:

    (round-to 13 10)      ; => 10
    (round-to 15 10)      ; => 20
    (round-to 44 25)      ; => 50
    (round-to 457/87 1/2) ; => 11/2

  "
  (* precision (round number precision)))
