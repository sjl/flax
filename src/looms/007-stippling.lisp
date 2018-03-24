(in-package :flax.looms.007-stipple)


;;;; Convert ------------------------------------------------------------------
(defun convert (points)
  (mapcar #'flax.drawing:point points))


;;;; Shapes -------------------------------------------------------------------
(defstruct (rectangle (:conc-name nil)) a b)
(defstruct (circle (:conc-name nil)) center radius)


(define-with-macro (rectangle :conc-name "") a b)
(define-with-macro (circle :conc-name "") center radius)


(defun random-coord ()
  (coord (rand 1.0) (rand 1.0)))

(defun gen-rectangle ()
  (make-rectangle :a (random-coord) :b (random-coord)))

(defun gen-circle ()
  (make-circle :center (random-coord)
               :radius (random-range 0.01 0.2 #'rand)))

(chancery:define-rule (gen-shape :distribution :weighted)
  (1 gen-rectangle)
  (1 gen-circle))

(defun gen (shapes)
  (gimme shapes (gen-shape)))



;;;; Bounds -------------------------------------------------------------------
(defgeneric bounding-box (shape))

(defmethod bounding-box ((shape rectangle))
  (cons (a shape) (b shape)))

(defmethod bounding-box ((shape circle))
  (with-circle (shape c r)
    (let ((x (x c))
          (y (y c)))
      (cons (coord (- x r) (- y r))
            (coord (+ x r) (+ y r))))))

(defun random-point-in-bounding-box (bounding-box)
  (destructuring-bind (a . b) bounding-box
    (let ((x1 (min (x a) (x b)))
          (x2 (max (x a) (x b)))
          (y1 (min (y a) (y b)))
          (y2 (max (y a) (y b))))
      (coord (random-range-inclusive x1 x2 #'rand)
             (random-range-inclusive y1 y2 #'rand)))))


;;;; Area ---------------------------------------------------------------------
(defgeneric area (shape))

(defmethod area ((shape rectangle))
  (with-rectangle (shape)
    (* (abs (- (x a) (x b)))
       (abs (- (y a) (y b))))))

(defmethod area ((shape circle))
  (* 1/2tau (square (radius shape))))


;;;; Containment --------------------------------------------------------------
(defgeneric containsp (shape point)
  (:documentation
    "Return whether `shape` contains `point`.

  `point` is assumed to lie somewhere inside `shape`'s bounding box.

  "))

(defmethod containsp ((shape rectangle) point)
  t)

(defmethod containsp ((shape circle) point)
  (<= (distance point (center shape))
      (radius shape)))

(defun canvas-contains-p (point)
  (and (<= 0 (x point) 1)
       (<= 0 (y point) 1)))

(defun random-point-in-shape (shape)
  (iterate
    (with bb = (bounding-box shape))
    (for p = (random-point-in-bounding-box bb))
    (finding p :such-that (and (canvas-contains-p p)
                               (containsp shape p)))))


;;;; Stipple ------------------------------------------------------------------
(defun perturb-ratio (ratio)
  (* ratio (clamp 0 10 (random-gaussian 1.0 20/100 #'rand))))

(defun stipple-shape (shape ratio)
  (gimme (round (* (perturb-ratio ratio)
                   (area shape)))
         (random-point-in-shape shape)))

(defun stipple (shapes ratio)
  (mapcan (rcurry #'stipple-shape ratio) shapes))


;;;; Main ---------------------------------------------------------------------
(defun loom (seed filename filetype width height &key shapes ratio)
  (nest
    (with-seed seed)
    (flax.drawing:with-rendering (canvas filetype filename width height
                                         :background (hsv 0.09 0.05 0.975)))
    (randomly-initialize
      ((shapes (clamp 1 100 (random-gaussian-integer 6 2 #'rand)))))
    (progn
      (-<> (gen shapes)
        (stipple <> (/ (or ratio 100000) shapes))
        convert
        (flax.drawing:render canvas <>))
      (values shapes))))

;; (time (loom 11 "out" :png 800 800))
;; (time (loom 112 "out" :plot 800 800 :ratio 40000))
