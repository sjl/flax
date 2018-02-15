(in-package :flax.looms.004-turtle-curves)

;;;; Turtle Graphics ----------------------------------------------------------
(defvar *step* 0.1)
(defvar *angle* 1/4tau)
(defvar *starting-angle* (- 1/4tau))
(defvar *color* nil)

(defstruct turtle
  (x 0.5)
  (y 0.5)
  (angle *starting-angle*))

(define-with-macro turtle x y angle)


(defun rot (angle amount)
  (mod (+ angle amount) tau))

(define-modify-macro rotf (amount) rot)


(defgeneric perform-command (turtle command))

(defmethod perform-command (turtle (command (eql 'f)))
  (with-turtle (turtle)
    (list (flax.drawing:path
            (list (coord x y)
                  (progn (perform-command turtle 's)
                         (coord x y)))
            :color *color*))))

(defmethod perform-command (turtle (command (eql 'fl)))
  (perform-command turtle 'f))

(defmethod perform-command (turtle (command (eql 'fr)))
  (perform-command turtle 'f))

(defmethod perform-command (turtle (command (eql 's)))
  (with-turtle (turtle)
    (incf x (* *step* (cos angle)))
    (incf y (* *step* (sin angle))))
  nil)

(defmethod perform-command (turtle (command (eql '-)))
  (rotf (turtle-angle turtle) *angle*)
  nil)

(defmethod perform-command (turtle (command (eql '+)))
  (rotf (turtle-angle turtle) (- *angle*))
  nil)


(defun find-bounds (paths)
  (iterate (for path :in paths)
           (for (p1 p2) = (flax.drawing:points path))
           (maximizing (x p1) :into max-x)
           (maximizing (x p2) :into max-x)
           (maximizing (y p1) :into max-y)
           (maximizing (y p2) :into max-y)
           (minimizing (x p1) :into min-x)
           (minimizing (x p2) :into min-x)
           (minimizing (y p1) :into min-y)
           (minimizing (y p2) :into min-y)
           (finally (return (list min-x min-y max-x max-y)))))

(defun scale (paths)
  (iterate
    (with (min-x min-y max-x max-y)  = (find-bounds paths))
    (with factor = (min (/ (- max-x min-x))
                        (/ (- max-y min-y))))
    (with x-padding = (/ (- 1.0 (* factor (- max-x min-x))) 2))
    (with y-padding = (/ (- 1.0 (* factor (- max-y min-y))) 2))
    (with offset-x = (+ (- min-x) x-padding))
    (with offset-y = (+ (- min-y) y-padding))
    (for path :in paths)
    (for (p1 p2) = (flax.drawing:points path))
    (zapf
      (x p1) (* factor (+ offset-x %))
      (y p1) (* factor (+ offset-y %))
      (x p2) (* factor (+ offset-x %))
      (y p2) (* factor (+ offset-y %)))))

(defun turtle-draw (commands)
  (let ((paths (mapcan (curry #'perform-command (make-turtle)) commands)))
    (scale paths)
    paths))


;;;; L-Systems ----------------------------------------------------------------
(defun expand (word productions)
  (mappend (lambda (letter)
             (ensure-list (or (getf productions letter) letter)))
           word))

(defun run-l-system (axiom productions iterations)
  (iterate
    (repeat iterations)
    (for word :initially axiom :then (expand word productions))
    (finally (return word))))


(defmacro define-l-system (name-and-options axiom &body productions)
  (destructuring-bind (name &key (angle 1/4tau))
      (ensure-list name-and-options)
    `(defun ,name (iterations)
       (values (run-l-system ',(ensure-list axiom)
                             ',productions
                             iterations)
               ,angle))))

(define-l-system quadratic-koch-island-a (f - f - f - f)
  f (f - f + f + f f - f - f + f))

(define-l-system quadratic-koch-island-b (f - f - f - f)
  f (f + f f - f f - f - f + f + f f - f - f + f + f f + f f - f))

(define-l-system quadratic-snowflake (- f)
  f (f + f - f - f + f))

(define-l-system islands-and-lakes (f + f + f + f)
  f (f + s - f f + f + f f + f s + f f - s + f f - f - f f - f s - f f f)
  s (s s s s s s))

(define-l-system unnamed-koch-a (f - f - f - f)
  f (f f - f - f - f - f - f + f))

(define-l-system unnamed-koch-b (f - f - f - f)
  f (f f - f - f - f - f f))

(define-l-system unnamed-koch-c (f - f - f - f)
  f (f f - f + f - f - f f))

(define-l-system unnamed-koch-d (f - f - f - f)
  f (f f - f - - f - f))

(define-l-system unnamed-koch-e (f - f - f - f)
  f (f - f f - - f - f))

(define-l-system unnamed-koch-f (f - f - f - f)
  f (f - f + f - f - f))

(define-l-system dragon-curve fl
  fl (fl + fr +)
  fr (- fl - fr))

(define-l-system (sierpinski-gasket :angle (/ tau 6)) fr
  fl (fr + fl + fr)
  fr (fl - fr - fl))

(define-l-system (hexagonal-gosper-curve :angle (/ tau 6)) fl
  fl (fl + fr + + fr - fl - - fl fl - fr +)
  fr (- fl + fr fr + + fr + fl - - fl - fr))


;;;; Main ---------------------------------------------------------------------
(defun loom (seed filename width height)
  (nest
    (with-seed seed)
    (destructuring-bind (l-system min-iterations max-iterations)
        (random-elt '((quadratic-koch-island-a 2 5)
                      (quadratic-koch-island-b 2 4)
                      (quadratic-snowflake 3 7)
                      (islands-and-lakes 1 4)
                      (unnamed-koch-a 3 5)
                      (unnamed-koch-b 3 6)
                      (unnamed-koch-c 3 6)
                      (unnamed-koch-d 2 5)
                      (unnamed-koch-e 5 7)
                      (unnamed-koch-f 5 7)
                      (dragon-curve 7 16)
                      (sierpinski-gasket 4 10)
                      (hexagonal-gosper-curve 3 6))
                    #'rand))
    (let ((*starting-angle* (rand tau))
          (bg (hsv (rand 1.0) (rand 1.0) (random-range 0.0 0.2 #'rand)))
          (*color* (hsv (rand 1.0)
                        (random-range 0.5 1.0 #'rand)
                        (random-range 0.8 1.0 #'rand)))
          (iterations (random-range-inclusive min-iterations
                                              max-iterations
                                              #'rand))))
    (flax.drawing:with-rendering (image filename width height :background bg))
    (multiple-value-bind (shapes *angle*) (funcall l-system iterations))
    (progn (-<> shapes
             (turtle-draw <>)
             (flax.drawing:render image <>))
           (list l-system iterations))))

;; (time (loom nil "out.png" 1000 1000))
