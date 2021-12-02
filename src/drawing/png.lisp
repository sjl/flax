(in-package :flax.drawing)

;;;; Utils --------------------------------------------------------------------
(deftype image ()
  '(simple-array (double-float 0.0d0 1.0d0) (* * 3)))

(deftype index ()
  `(integer 0 (,array-dimension-limit)))

(deftype row-buffer ()
  '(simple-array (integer 0 255) (*)))


(defun make-image (width height color)
  (let ((image (make-array (list height width 3)
                 :element-type 'double-float
                 :initial-element 1.0d0)))
    (with-color (color r g b)
      (dotimes (row height)
        (dotimes (col width)
          (setf (aref image row col 0) r
                (aref image row col 1) g
                (aref image row col 2) b))))
    image))

(defun-inline normalize-alpha (alpha)
  (declare (optimize speed)
           (type fixnum alpha))
  (/ (min 255 (abs alpha)) 255.0d0))

(defun put-pixel (image color opacity x y alpha)
  (declare (optimize speed)
           (type image image)
           (type color color)
           (type index x y)
           (type (double-float 0.0d0 1.0d0) opacity)
           (type fixnum alpha))
  (let ((pixel-alpha (* opacity (normalize-alpha alpha))))
    (zapf (aref image y x 0) (lerp % (flax.colors::r color) pixel-alpha)
          (aref image y x 1) (lerp % (flax.colors::g color) pixel-alpha)
          (aref image y x 2) (lerp % (flax.colors::b color) pixel-alpha))
    (values)))


;;;; Canvas -------------------------------------------------------------------
(defclass* png-canvas (canvas)
  (image state))

(defmethod make-canvas ((type (eql :png)) &key height width background padding)
  (make-instance 'png-canvas
    :height height
    :width width
    :image (make-image width height background)
    :padding padding))


;;;; Rectangles ---------------------------------------------------------------
(defmethod draw ((canvas png-canvas) (rect rectangle))
  (with-coordinates canvas
      ((ax ay (a rect))
       (bx by (b rect))
       (r (round-corners rect)))
    (_ (paths:make-rectangle-path ax ay bx by :round r)
      (vectors:update-state (state canvas) _))))


;;;; Circles ------------------------------------------------------------------
(defmethod draw ((canvas png-canvas) (circ circle))
  (with-coordinates canvas
      ((x y (center circ))
       (r (radius circ)))
    (_ (paths:make-circle-path x y r)
      (vectors:update-state (state canvas) _))))


;;;; Points -------------------------------------------------------------------
(defmethod draw ((canvas png-canvas) (p point))
  (with-coordinates canvas
      ((x y (location p)))
    (_ (paths:make-circle-path x y 2)
      (vectors:update-state (state canvas) _))))


;;;; Paths --------------------------------------------------------------------
(defun pair-to-vec (pair)
  (vec (car pair) (cdr pair)))

(defun vec-to-pair (vec)
  (cons (vx vec) (vy vec)))

(defun reflect-control (control loc)
  (let* ((l (pair-to-vec loc))
         (c (pair-to-vec control))
         (cv (v- c l)))
    (vec-to-pair (v+ l (v- cv)))))

(defun fill-missing-control-points (points)
  (iterate
    ;; Unfortunately cl-vectors doesn't seem to have anything like the nice
    ;; convenient omit-the-starting-control-point-for-a-smooth-curve feature of
    ;; SVG, so we'll have to implement it ourselves.
    (with previous-ctrl2)
    (for point :in points)
    (for (p ctrl1 ctrl2) = point)
    (for previous-p :previous p)
    (cond
      (ctrl2 (collect point))
      (ctrl1 (psetf ctrl1 (reflect-control previous-ctrl2 previous-p)
                    ctrl2 ctrl1)
             (collect (list p ctrl1 ctrl2)))
      (t (collect point)))
    (setf previous-ctrl2 ctrl2)))

(defun convert-point (canvas point)
  (destructuring-bind (x . y) (coord-to-pair canvas point)
    (paths:make-point x y)))

(defun convert-points (canvas points)
  (mapcar-curried #'convert-point canvas points))

(defun make-vector-path (points)
  (destructuring-bind (first-point &rest remaining-points) points
    (let ((p (paths:create-path :open-polyline)))
      (paths:path-reset p (first first-point))
      (dolist (next-point remaining-points)
        (destructuring-bind (loc &rest control-points) next-point
          (paths:path-extend p (paths:make-bezier-curve control-points) loc)))
      p)))

(defmethod draw ((canvas png-canvas) (p path))
  (_ (points p)
    (mapcar-curried #'convert-points canvas _)
    fill-missing-control-points
    make-vector-path
    (paths:stroke-path _ 1)
    (vectors:update-state (state canvas) _)))


;;;; Triangles ----------------------------------------------------------------
(defmethod draw ((canvas png-canvas) (tri triangle))
  (with-coordinates canvas
      ((ax ay (a tri))
       (bx by (b tri))
       (cx cy (c tri)))
    (_ (list (cons ax ay)
               (cons bx by)
               (cons cx cy)
               (cons ax ay))
      paths:make-simple-path
      (paths:stroke-path _ 1)
      (vectors:update-state (state canvas) _))))


;;;; Rendering ----------------------------------------------------------------
(defmethod render-object ((canvas png-canvas) object)
  (setf (state canvas) (aa:make-state))
  (draw canvas object)
  (aa:cells-sweep/rectangle
    (state canvas) 0 0 (width canvas) (height canvas)
    (curry #'put-pixel (image canvas) (color object) (opacity object))))


;;;; Files --------------------------------------------------------------------
(defun-inline prepare-sample (value)
  (declare (optimize speed)
           (type (double-float 0.0d0 1.0d0) value))
  (round (* 255.0d0 value)))

(defun fill-row (image row buffer)
  (declare (optimize speed)
           (type image image)
           (type index row)
           (type row-buffer buffer))
  (iterate
    (declare (iterate:declare-variables))
    (with width = (length buffer))
    (for (the fixnum i) :from (* row width))
    (for (the fixnum j) :from 0 :below width)
    (setf (aref buffer j)
          (prepare-sample (row-major-aref image i)))))

(defmethod write-file ((canvas png-canvas) filename)
  (let ((width (width canvas))
        (height (height canvas))
        (image (image canvas)))
    (let ((png (make-instance 'zpng:pixel-streamed-png
                 :color-type :truecolor
                 :width width
                 :height height))
          (buffer (make-array (* width 3) :element-type '(integer 0 255))))
      (with-open-file (stream filename
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
        (zpng:start-png png stream)
        (dotimes (row height)
          (fill-row image row buffer)
          (zpng:write-row buffer png))
        (zpng:finish-png png)))))


;; todo fix this
(defun fade (canvas color alpha)
  (declare (optimize speed)
           (type color color)
           (type (double-float 0.0d0 1.0d0) alpha))
  (nest (let ((image (image canvas)))
          (declare (type image image)))
        (with-color (color r g b))
        (dotimes (row (array-dimension image 0)))
        (dotimes (col (array-dimension image 1)))
        (zapf (aref image row col 0) (lerp % r alpha)
              (aref image row col 1) (lerp % g alpha)
              (aref image row col 2) (lerp % b alpha))))
