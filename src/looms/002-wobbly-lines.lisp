(in-package :flax.looms.002-wobbly-lines)

;;;; Data ---------------------------------------------------------------------
(defvar *brush* nil)
(defvar *hue* nil)
(defvar *hue-increment* nil)
(defparameter *swing* 0.03)
(defparameter *background* (hsv 0 0 0.05))


;;;; Elements -----------------------------------------------------------------
(defstruct (line (:conc-name "")
                 (:constructor line (points)))
  (points (error "Required") :type vector))

(define-with-macro (line :conc-name "") points)


;;;; Element Conversion -------------------------------------------------------
(defun convert (line opacity)
  (list (flax.drawing::path (iterate (for p :in-whatever (points line))
                                     (collect (coord (vx p) (vy p))))
                            :color (hsv *hue* 0.9 1)
                            :opacity opacity)))


;;;; Generation ---------------------------------------------------------------
(defun initial (segments)
  (line
    (iterate
      (for x :from 0.0 :to (+ 1.0 least-positive-single-float) :by (/ 1.0 segments))
      (collect (vec x 0.5) :result-type 'vector))))


;;;; Tick ---------------------------------------------------------------------
(defun perturb-point (point)
  (incf (vy point) (random-range-inclusive (- *swing*) *swing* #'rand)))

(defun perturb-line (line)
  (map nil #'perturb-point (points line)))

(defun smooth-line (line)
  (iterate
    (with points = (points line))
    (with final = (1- (length points)))
    (for p :in-vector points :with-index i)
    (for y = (vy p))
    (for l = (or (unless (zerop i) (vy (aref points (1- i)))) y))
    (for r = (or (unless (= final i) (vy (aref points (1+ i)))) y))
    (zapf (vy p) (/ (+ % % l r) 4.0))))

(defun tick (line)
  (perturb-line line)
  (smooth-line line)
  (zapf *hue* (mod (+ % *hue-increment*) 1.0d0)))


;;;; Main ---------------------------------------------------------------------
(defun loom (seed filename filetype width height &key mode ticks verbose)
  (nest
    (with-seed seed)
    (flax.drawing:with-rendering (canvas filetype filename width height
                                         :padding 0.0
                                         :background *background*))
    (randomly-initialize
      ((ticks (round-to (random-range 3000 8000 #'rand) 1000))
       (mode (random-elt '(:opaque :transparent :fade) #'rand))))
    (let ((line (initial 300))
          (*hue* (random-range 0.0d0 1.0d0 #'rand))
          (*hue-increment* (/ (random-range 0.15d0 0.3d0 #'rand) ticks))))
    (progn
      (dotimes (tick ticks)
        (when (and verbose
                   (dividesp tick (/ (expt 10 (floor (log (1- ticks) 10))) 2)))
          (print tick))
        (when (and (eq filetype :png) (eq mode :fade) (dividesp tick 10))
          (flax.drawing:fade canvas *background* 0.04d0))
        (flax.drawing:render canvas (convert line (if (eq mode :transparent)
                                                    (/ 95.0d0 ticks)
                                                    1.0d0)))
        (tick line))
      (values mode ticks))))


;; (time (loom 133 "out" :svg 800 300))
