(in-package :flax.looms.004-turtle-curves)

;;;; Turtle Graphics ----------------------------------------------------------
(defvar *step* 0.1)
(defvar *angle* 1/4tau)
(defvar *starting-angle* (- 1/4tau))
(defvar *color* nil)

(defstruct turtle
  (x 0.5)
  (y 0.5)
  (angle *starting-angle*)
  (state nil))

(define-with-macro (turtle :conc-name turtle-) x y angle state)


(defun rot (angle amount)
  (mod (+ angle amount) tau))

(define-modify-macro rotf (amount) rot)


(defgeneric perform-command (turtle command n))

(defmethod perform-command (turtle command n)
  nil)

(defmethod perform-command (turtle (command (eql 'f)) n)
  (with-turtle (turtle)
    (list (flax.drawing:path
            (list (vec x y)
                  (progn (perform-command turtle 's n)
                         (vec x y)))
            :color *color*))))

(defmethod perform-command (turtle (command integer) n)
  (perform-command turtle 'f n))

(defmethod perform-command (turtle (command (eql 's)) n)
  (do-repeat n
    (with-turtle (turtle)
      (incf x (* *step* (cos angle)))
      (incf y (* *step* (sin angle)))))
  nil)

(defmethod perform-command (turtle (command (eql '-)) n)
  (rotf (turtle-angle turtle) (* n *angle*))
  nil)

(defmethod perform-command (turtle (command (eql '+)) n)
  (rotf (turtle-angle turtle) (* n (- *angle*)))
  nil)

(defmethod perform-command (turtle (command (eql '%)) n)
  (rotf (turtle-angle turtle) 1/2tau)
  nil)

(defmethod perform-command (turtle (command (eql '<)) n)
  (do-repeat n
    (with-turtle (turtle)
      (push (list x y angle) state)))
  nil)

(defmethod perform-command (turtle (command (eql '>)) n)
  (do-repeat n
    (with-turtle (turtle)
      (when-let ((prev (pop state)))
        (destructuring-bind (ox oy oa) prev
          (setf x ox y oy angle oa)))))
  nil)

(defmethod perform-command (turtle (command (eql '[)) n)
  (perform-command turtle '< n))

(defmethod perform-command (turtle (command (eql '])) n)
  (perform-command turtle '> n))


(defun find-bounds (paths)
  (iterate (for path :in paths)
           (for (p1 p2) = (flax.drawing:points path))
           (maximizing (vx p1) :into max-x)
           (maximizing (vx p2) :into max-x)
           (maximizing (vy p1) :into max-y)
           (maximizing (vy p2) :into max-y)
           (minimizing (vx p1) :into min-x)
           (minimizing (vx p2) :into min-x)
           (minimizing (vy p1) :into min-y)
           (minimizing (vy p2) :into min-y)
           (finally (return (values min-x min-y max-x max-y)))))

(defun transform-to-fit (paths)
  (multiple-value-bind (min-x min-y max-x max-y) (find-bounds paths)
    (let* ((x-span (- max-x min-x))
           (y-span (- max-y min-y))
           (factor (min (/ x-span) (/ y-span)))
           (x-padding (/ (- 1.0 (* factor x-span)) 2.0))
           (y-padding (/ (- 1.0 (* factor y-span)) 2.0))
           (transform (transformation
                        (translate (- min-x) (- min-y))
                        (scale factor factor)
                        (translate x-padding y-padding))))
      (ntransform paths transform))))


(defun encode (commands)
  (iterate
    (with n = 1)
    (for (command . next) :on commands)
    (if (eq command (car next))
      (incf n)
      (progn (collect (cons command n))
             (setf n 1)))))

(defun turtle-draw (commands)
  (iterate (with turtle = (make-turtle))
           (for (command . n) :in (encode commands))
           (appending (perform-command turtle command n))))


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

(defun run-named-l-system (l-system iterations)
  (run-l-system (axiom l-system)
                (productions l-system)
                iterations))


(defclass* l-system ()
  ((name)
   (axiom)
   (productions)
   (recommended-angle)))

(defun make-l-system (name axiom productions recommended-angle)
  (make-instance 'l-system
    :name name
    :axiom (ensure-list axiom)
    :productions productions
    :recommended-angle recommended-angle))


(defmacro define-l-system (name-and-options axiom &body productions)
  (destructuring-bind (name &key (angle 1/4tau))
      (ensure-list name-and-options)
    `(defparameter ,(symb '* name '*)
       (make-l-system ',name ',axiom ',productions ,angle))))


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

(define-l-system dragon-curve 1
  1 (1 + 2 +)
  2 (- 1 - 2))

(define-l-system (sierpinski-gasket :angle (/ tau 6)) 2
  1 (2 + 1 + 2)
  2 (1 - 2 - 1))

(define-l-system (hexagonal-gosper-curve :angle (/ tau 6)) 1
  1 (1 + 2 + + 2 - 1 - - 1 1 - 2 +)
  2 (- 1 + 2 2 + + 2 + 1 - - 1 - 2))


(define-l-system (tree-a :angle (radians 25.7)) f
  f (f < + f > f < - f > f))

(define-l-system (tree-b :angle (radians 20)) f
  f (f < + f > f < - f > < f >))

(define-l-system (tree-c :angle (radians 22.5)) f
  f (f f - < - f + f + f > + < + f - f - f >))

(define-l-system (tree-d :angle (radians 20)) x
  x (f < + x > f < - x > + x)
  f (f f))

(define-l-system (tree-e :angle (radians 25.7)) x
  x (f < + x > < - x > f x)
  f (f f))

(define-l-system (tree-f :angle (radians 22.5)) x
  x (f - < < x > + x > + f < + f x > - x)
  f (f f))


;;; http://paulbourke.net/fractals/lsys/

(define-l-system (saupe-pine :angle (radians 20)) (v z f f f)
  v (< + + + w > < - - - w > y v)
  w (+ x < - w > z)
  x (- w < + x > z)
  y (y z)
  z (< - f f f > < + f f f > f))

(define-l-system (bourke-bush :angle (radians 25.7)) y
  x (x < - f f f > < + f f f > f x)
  y (y f x < + y > < - y >))

(define-l-system (bourke-weed :angle (radians 22.5)) f
  f (f f - < x y > + < x y >)
  x (+ f y)
  y (- f x))

(define-l-system (bourke-triangle :angle (radians 120)) (f + f + f)
  f (f - f + f))

(define-l-system (bourke-pentaplexity :angle (radians 36)) (F + + F + + F + + F + + F)
  f (f + + f + + f % f - f + + f))

(define-l-system (bourke-mango :angle (radians 60)) (Y - - - Y)
  x (f - f f - f - - < - - x > f - f f - f - - f - f f - f - -)
  y (s - f + x + f - s y))

(define-l-system (square-sierpinski :angle (radians 90)) (f + x f + f + x f)
  x (x f - f + f - x f + f + x f - f + f - x))

(define-l-system (peano-curve :angle (radians 90)) x
  x (x f y f x + f + y f x f y - f - x f y f x)
  y (y f x f y - f - x f y f x + f + y f x f y))

(define-l-system (hilbert-curve :angle (radians 90)) x
  x (- y f + x f x + f y -)
  y (+ x f - y f y - f x +))

(define-l-system (quadratic-gosper :angle (radians 90)) (- y f)
  x (x f x - y f - y f + f x + f x - y f - y f f x + y f + f x f x y f -
     f x + y f + f x f x + y f - f x y f - y f - f x + f x + y f y f -)
  y (+ f x f x - y f - y f + f x + f x y f + f x - y f y f - f x - y f +
     f x y f y f - f x - y f f x + f x + y f - y f - f x + f x + y f y))

(define-l-system (lévy-curve :angle (radians 45)) f
  f (- f + + f -))


;;; http://www.kevs3d.co.uk/dev/lsystems/

(define-l-system (penrose :angle (radians 36))
    ([ 7 ] + + [ 7 ] + + [ 7 ] + + [ 7 ] + + [ 7 ])
  6 (8 x + + 9 x - - - - 7 x [ - 8 x - - - - 6 x ] + +)
  7 (+ 8 x - - 9 x [ - - - 6 x - - 7 x ] +)
  8 (- 6 x + + 7 x [ + + + 8 x + + 9 x ] -)
  9 (- - 8 x + + + + 6 x [ + 9 x + + + + 7 x ] - - 7 x)
  x ())


;;;; Mutation -----------------------------------------------------------------
(defun insert (val target n)
  (append (subseq target 0 n)
          (list val)
          (subseq target n)))

(defun remove-nth (list n)
  (concatenate 'list (subseq list 0 n) (subseq list (1+ n))))

(defun mutation-transpose (result)
  (rotatef (elt result (rand (length result)))
           (elt result (rand (length result))))
  result)

(defun mutation-insert (result)
  (zapf result (insert (random-elt (union (remove-duplicates result)
                                          '(f s - + < > %))
                                   #'rand)
                       %
                       (rand (length result))))
  result)

(defun mutation-remove (result)
  (remove-nth result (rand (length result))))

(defun mutate-production (result)
  (if (<= (length result) 2)
    result
    (ecase (rand 3)
      (0 (mutation-transpose result))
      (1 (mutation-insert result))
      (2 (mutation-remove result)))))

(defun mutate-productions% (productions)
  (iterate (for (letter production . nil) :on productions :by #'cddr)
           (appending (list letter (mutate-production (copy-list production))))))

(defun mutate-productions (productions)
  (iterate
    ;; complete no-op mutations are boring
    (for new = (mutate-productions% productions))
    (finding new :such-that (not (equal new productions)))))

(defun maybe-mutate-productions (productions)
  (let ((should-mutate (randomp 0.6 #'rand))
        (mutation-seed (rand (expt 2 31))))
    (if should-mutate
      (with-seed mutation-seed
        (values (mutate-productions productions) mutation-seed))
      productions)))


;;;; Main ---------------------------------------------------------------------
(defun select-l-system ()
  (random-elt `((,*quadratic-koch-island-a* 2 5)
                (,*quadratic-koch-island-b* 2 4)
                (,*quadratic-snowflake* 3 7)
                (,*islands-and-lakes* 1 4)
                (,*unnamed-koch-a* 3 5)
                (,*unnamed-koch-b* 3 6)
                (,*unnamed-koch-c* 3 6)
                (,*unnamed-koch-d* 2 5)
                (,*unnamed-koch-e* 5 7)
                (,*unnamed-koch-f* 5 7)
                (,*dragon-curve* 7 16)
                (,*sierpinski-gasket* 4 10)
                (,*hexagonal-gosper-curve* 3 6)
                (,*bourke-triangle* 4 8)
                (,*bourke-pentaplexity* 3 5)
                (,*bourke-mango* 3 25)
                (,*square-sierpinski* 3 6)
                (,*peano-curve* 3 5)
                (,*hilbert-curve* 5 7)
                (,*quadratic-gosper* 2 3)
                (,*lévy-curve* 6 14)
                (,*penrose* 3 7)
                (,*tree-a* 3 7 ,(- 1/4tau))
                (,*tree-b* 3 7 ,(- 1/4tau))
                (,*tree-c* 3 5 ,(- 1/4tau))
                (,*tree-d* 6 7 ,(- 1/4tau))
                (,*tree-e* 6 8 ,(- 1/4tau))
                (,*tree-f* 4 7 ,(- 1/4tau))
                (,*saupe-pine* 7 12 ,(- 1/4tau))
                (,*bourke-bush* 5 7 ,(- 1/4tau))
                (,*bourke-weed* 5 8 ,(- 1/4tau))
                )
              #'rand))


(defun loom (seed filename filetype width height
             &key l-system iterations starting-angle pure)
  (nest
    (with-seed seed)
    (destructuring-bind
        (random-l-system min-iterations max-iterations &optional random-starting-angle)
        (select-l-system))
    (randomly-initialize
      ((starting-angle (random-or random-starting-angle (rand tau)))
       (iterations (random-range-inclusive min-iterations max-iterations #'rand))
       (l-system random-l-system)))
    (let* ((*starting-angle* starting-angle)
           (bg (hsv (rand 1.0) (rand 1.0) (random-range 0.0 0.2 #'rand)))
           (*color* (hsv (rand 1.0)
                         (random-range 0.5 0.8 #'rand)
                         (random-range 0.9 1.0 #'rand)))
           (axiom (axiom l-system))
           (*angle* (recommended-angle l-system))))
    (multiple-value-bind (productions mutagen)
        (if pure
          (values (productions l-system) nil)
          (maybe-mutate-productions (productions l-system))))
    (flax.drawing:with-rendering
        (canvas filetype filename width height :background bg :padding 0.05))
    (progn
      (-<> (run-l-system axiom productions iterations)
        turtle-draw
        transform-to-fit
        (flax.drawing:render canvas <>))
      (values (name l-system)
              iterations
              mutagen))))

