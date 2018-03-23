(in-package :flax.base)

;;;; Randomness ---------------------------------------------------------
(defun rand (bound)
  (pcg:pcg-random t bound))

(defmacro with-seed (seed &body body)
  (once-only (seed)
    `(let ((pcg::*global-generator*
             (pcg:make-pcg :seed (pr (or ,seed (random (expt 2 31)))))))
       (losh::clear-gaussian-spare)
       ,@body)))

(defmacro random-or (value random-form)
  (once-only (value random-form)
    `(or ,value ,random-form)))

(defmacro randomly-initialize (bindings &body body)
  `(let ,(iterate (for (symbol init-form) :in bindings)
                  (collect `(,symbol (random-or ,symbol ,init-form))))
     ,@body))


;;;; Math ---------------------------------------------------------
(defun round-to (number precision)
  "Round `number` to the given `precision`.

  Examples:

    (round-to 13 10)      ; => 10
    (round-to 15 10)      ; => 20
    (round-to 44 25)      ; => 50
    (round-to 457/87 1/2) ; => 11/2

  "
  (* precision (round number precision)))
