(in-package :flax.looms.006-tracing-lines)

;;;; Config -------------------------------------------------------------------
(defparameter *spread* 0.0020)


;;;; Convert ------------------------------------------------------------------
(defun convert-point (point x y)
  (coord x (+ y point)))

(defun convert-line (line y)
  (flax.drawing:path
    (iterate (for point :in-vector line)
             (for x :from 0.0 :by (/ (1- (length line))))
             (collect (convert-point point x y)))
    :color (hsv 0 0 1)))

(defun convert-lines (lines)
  (iterate (for line :in lines)
           (for y :from 0.0 :by (/ (length lines)))
           (collect (convert-line line y))))


;;;; Generate -----------------------------------------------------------------
(defun make-initial-line (points)
  (make-array points :initial-element 0.0))

(defun perturb (point)
  (random-around point *spread* #'rand))

(defun wrapping-aref (array i)
  (aref array (mod i (length array))))

(defun average (sequence)
  (iterate (for x :in-whatever sequence)
           (averaging x)))

(defun next-line (line)
  (iterate (for i :index-of-vector line)
           (collect (random-gaussian
                      (average (subseq line
                                       (max 0 (- i 2))
                                       (min (1- (length line)) (+ i 1))))
                      *spread* #'rand)
                    :result-type 'vector)))

(defun generate-lines (points lines)
  (iterate
    (repeat lines)
    (for line :first (make-initial-line points) :then (next-line line))
    (collect line)))


;;;; Main ---------------------------------------------------------------------
(defun loom (seed filename filetype width height)
  (nest
    (with-seed seed)
    (flax.drawing:with-rendering (canvas filetype filename width height
                                         :background (hsv 0 0 0.05)))
    (let* ((points (round-to (random-range 100 150 #'rand) 10))
           (lines (round-to (random-range 80 140 #'rand) 10))
           (*spread* (/ 0.15 lines))))
    (progn
      (flax.drawing:render canvas (convert-lines (generate-lines points lines)))
      (list points lines))))

;; (time (loom nil "out" :svg 800 800))
