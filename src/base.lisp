(in-package :flax.base)

(setf lparallel:*kernel* (lparallel:make-kernel 1))

(defun rand (bound)
  (pcg:pcg-random t bound))

(defmacro with-seed (seed &body body)
  `(let ((pcg::*global-generator* (pcg:make-pcg :seed ,seed))) ,@body))
