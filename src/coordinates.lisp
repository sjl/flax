(in-package :flax.coordinates)

(defstruct (coord (:conc-name "")
                  (:constructor make-coord (x y)))
  (x (error "Required") :type single-float)
  (y (error "Required") :type single-float))

(defun coord (x y)
  (make-coord (coerce x 'single-float)
              (coerce y 'single-float)))

(defun distance (c1 c2)
  (+ (square (- (x c2) (x c1)))
     (square (- (y c2) (y c1)))))

(defun clerp (from to n)
  (coord (lerp (x from) (x to) n)
         (lerp (y from) (y to) n)))

(defun coord-to-cons (c)
  (cons (x c) (y c)))
