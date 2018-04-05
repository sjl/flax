(in-package :flax.looms.007-stipple)


;;;; Convert ------------------------------------------------------------------
(defun convert (points)
  (iterate (for p :in points)
           (collect (flax.drawing:point (coord (vx p) (vy p))))))


;;;; Shapes -------------------------------------------------------------------
(defstruct (rectangle (:conc-name nil)) a b)
(defstruct (circle (:conc-name nil)) center radius)


(define-with-macro (rectangle :conc-name "") a b)
(define-with-macro (circle :conc-name "") center radius)


(defun random-coord ()
  (vec (rand 1.0) (rand 1.0)))

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
    (let ((x (vx c))
          (y (vy c)))
      (cons (vec (- x r) (- y r))
            (vec (+ x r) (+ y r))))))

(defun random-point-in-bounding-box (bounding-box)
  (destructuring-bind (a . b) bounding-box
    (let ((x1 (min (vx a) (vx b)))
          (x2 (max (vx a) (vx b)))
          (y1 (min (vy a) (vy b)))
          (y2 (max (vy a) (vy b))))
      (vec (random-range-inclusive x1 x2 #'rand)
           (random-range-inclusive y1 y2 #'rand)))))


;;;; Area ---------------------------------------------------------------------
(defgeneric area (shape))

(defmethod area ((shape rectangle))
  (with-rectangle (shape)
    (* (abs (- (vx a) (vx b)))
       (abs (- (vy a) (vy b))))))

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
  (<= (vdistance point (center shape))
      (radius shape)))

(defun canvas-contains-p (point)
  (and (<= 0 (vx point) 1)
       (<= 0 (vy point) 1)))

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

;; (time (loom 11 "out" :svg 800 800))
;; (time (loom 112 "out" :plot 800 800 :ratio 40000))
