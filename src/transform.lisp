(in-package :flax.transform)

(defun id ()
  (meye 3))

(defun scale (m x y)
  (m* (mat x 0 0
           0 y 0
           0 0 1)
      m))

(defun rotate (m angle)
  (m* (mat (cos angle)     (sin angle) 0
           (- (sin angle)) (cos angle) 0
           0               0           1)
      m))

(defun translate (m x y)
  (m* (mat 1 0 x
           0 1 y
           0 0 1)
      m))

(defun place (m corner1 corner2 &key (padding 0.0))
  (let* ((fw (abs (- (vx corner1) (vx corner2))))
         (fh (abs (- (vy corner1) (vy corner2))))
         (pw (* padding fw))
         (ph (* padding fh))
         (w (- fw pw pw))
         (h (- fh ph ph))
         (x (+ (min (vx corner1) (vx corner2)) pw))
         (y (+ (min (vy corner1) (vy corner2)) ph)))
    (translate (scale m w h) x y)))


(defmacro transformation (&rest transforms)
  `(_ (id)
     ,@(iterate (for (name . body) :in transforms)
                (collect `(,name _ ,@body)))))


(defgeneric ntransform (object transformation))

(defmethod ntransform ((vector vec3) transformation)
  (nm* transformation vector)
  vector)

(defmethod ntransform ((magnitude float) transformation)
  (with-fast-matref (m transformation 3)
    (let* ((a (m 0 0))
           (b (m 0 1))
           (c (m 1 0))
           (d (m 1 1))
           (scale (sqrt (/ (+ (square (+ a b))
                              (square (+ c d)))
                           2.0))))
      (* magnitude scale))))

(defmethod ntransform ((sequence sequence) transformation)
  (map-into sequence (rcurry #'ntransform transformation) sequence))


(defmacro ntransformf (place transformation)
  ;; im lazy
  `(setf ,place (ntransform ,place ,transformation)))
