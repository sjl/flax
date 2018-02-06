(in-package :flax.looms.002-wobbly-lines)

;;;; Data ---------------------------------------------------------------------
(defvar *brush* nil)
(defvar *hue* nil)
(defvar *hue-increment* nil)


;;;; Elements -----------------------------------------------------------------
(defstruct (line (:conc-name "")
                 (:constructor line (points)))
  (points (error "Required") :type vector))

(define-with-macro (line :conc-name "") points)


;;;; Element Conversion -------------------------------------------------------
(defun convert (line total-ticks)
  (list (flax.drawing::path (coerce (points line) 'list)
                            :color (hsv *hue* 1 1)
                            :opacity (/ 95.0d0 total-ticks))))


;;;; Generation ---------------------------------------------------------------
(defun initial (segments)
  (line
    (iterate
      (for x :from 0.0 :to (+ 1.0 least-positive-single-float) :by (/ 1.0 segments))
      (collect (coord x 0.5) :result-type 'vector))))


;;;; Tick ---------------------------------------------------------------------
(defun perturb-line (line)
  (map nil (lambda (c)
             (incf (y c) (random-range-inclusive -0.025 0.025 #'rand)))
       (points line)))

(defun smooth-line (line)
  (iterate
    (with points = (points line))
    (with final = (1- (length points)))
    (for c :in-vector points :with-index i)
    (for y = (y c))
    (for l = (or (unless (zerop i) (y (aref points (1- i)))) y))
    (for r = (or (unless (= final i) (y (aref points (1+ i)))) y))
    (zapf (y c) (/ (+ % % l r) 4.0))))

(defun tick (line)
  (perturb-line line)
  (smooth-line line)
  (zapf *hue* (mod (+ % *hue-increment*) 1.0d0)))


;;;; Main ---------------------------------------------------------------------
(defun loom (seed ticks filename width height)
  (with-seed seed
    (flax.drawing:with-rendering (image filename width height :padding 0.0)
      (let ((line (initial 300))
            (*hue* (random-range 0.0d0 1.0d0 #'rand))
            (*hue-increment* (/ (random-range 0.15d0 0.3d0 #'rand) ticks)))
        (dotimes (tick ticks)
          (when (dividesp tick (/ (expt 10 (floor (log (1- ticks) 10))) 2))
            (print tick))
          (flax.drawing:render image (convert line ticks))
          (tick line))))))


;; (time (loom nil 1000 "out.png" 3000 500))
