(in-package :flax.drawing)

;;;; Parameters ---------------------------------------------------------------
(defparameter *black* (rgb 0 0 0))
(defparameter *white* (rgb 1 1 1))


;;;; Canvas -------------------------------------------------------------------
(defclass* (canvas :conc-name "") ()
  ((width :type (integer 0))
   (height :type (integer 0))
   (padding :type (single-float 0.0 0.5) :initform 0.03)
   (output-transformation :type mat3)))

(defun recompute-output-transformation (canvas)
  (let* ((fw (coerce (width canvas) 'single-float))
         (fh (coerce (height canvas) 'single-float))
         (p (padding canvas))
         (pw (* p fw))
         (ph (* p fh))
         (w (- fw pw pw))
         (h (- fh ph ph)))
    (setf (output-transformation canvas)
          (transformation
            (scale w h)
            (translate pw ph)))))

(defmethod initialize-instance :after ((canvas canvas) &key)
  (recompute-output-transformation canvas))

(define-with-macro (canvas :conc-name "") width height)

(defgeneric make-canvas (type &key &allow-other-keys))


;;;; Utils --------------------------------------------------------------------
(defun-inline homogenize (v)
  (vec3 (vx v) (vy v) 1))

(defun convert-coordinate (canvas coordinate)
  (let ((c (m* (output-transformation canvas) coordinate)))
    (values (vx3 c) (vy3 c))))

(defun convert-magnitude (canvas magnitude)
  (ntransform magnitude (output-transformation canvas)))


(defmacro with-coordinate (canvas-symbol binding &body body)
  (ecase (length binding)
    (2 (destructuring-bind (magnitude-symbol value) binding
         `(let ((,magnitude-symbol (convert-magnitude ,canvas-symbol ,value)))
            ,@body)))
    (3 (destructuring-bind (x-symbol y-symbol value) binding
         `(multiple-value-bind (,x-symbol ,y-symbol)
            (convert-coordinate ,canvas-symbol ,value)
            ,@body)))))

(defmacro with-coordinates (canvas bindings &body body)
  (once-only (canvas)
    `(nest
       ,@(mapcar (lambda (binding)
                   `(with-coordinate ,canvas ,binding))
                 bindings)
       (progn ,@body))))


(defun coord-to-string (c)
  (format nil "(~A, ~A)" (vx c) (vy c)))

(defun coord-to-pair (canvas c)
  (with-coordinates canvas ((x y c))
    (cons x y)))


;;;; Drawables ----------------------------------------------------------------
(defclass* (drawable :conc-name "") ()
  ((opacity :type (double-float 0.0d0 1.0d0))
   (color :type color)))

(defgeneric draw (canvas drawing-object))


;;;; Paths --------------------------------------------------------------------
(defclass* (path :conc-name "") (drawable)
  ((points :type list)))

(defun path (points &key (opacity 1.0d0) (color *black*))
  (make-instance 'path
    :points (mapcar #'homogenize points)
    :color color
    :opacity (coerce opacity 'double-float)))

(defmethod print-object ((o path) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~{~A~^ -> ~}"
            (mapcar #'coord-to-string (points o)))))

(defmethod ntransform ((path path) transformation)
  (dolist (p (points path))
    (ntransform p transformation))
  path)


;;;; Triangles ----------------------------------------------------------------
(defclass* (triangle :conc-name "") (drawable)
  ((a :type vec3)
   (b :type vec3)
   (c :type vec3)))

(defun triangle (a b c &key (opacity 1.0d0) (color *black*))
  (make-instance 'triangle :a (homogenize a) :b (homogenize b) :c (homogenize c)
                 :color color
                 :opacity (coerce opacity 'double-float)))

(defmethod print-object ((o triangle) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "(~D, ~D) (~D, ~D) (~D, ~D)"
            (vx (a o))
            (vy (a o))
            (vx (b o))
            (vy (b o))
            (vx (c o))
            (vy (c o)))))

(defmethod ntransform ((triangle triangle) transformation)
  (ntransform (a triangle))
  (ntransform (b triangle))
  (ntransform (c triangle))
  triangle)


;;;; Rectangles ---------------------------------------------------------------
(defclass* (rectangle :conc-name "") (drawable)
  ((a :type vec3)
   (b :type vec3)
   (round-corners :type float :initform 0.0)))

(defun rectangle (a b &key (opacity 1.0d0) (color *black*) round-corners)
  (make-instance 'rectangle :a (homogenize a) :b (homogenize b)
    :color color
    :opacity (coerce opacity 'double-float)
    :round-corners round-corners))

(defmethod print-object ((o rectangle) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "(~D, ~D) (~D, ~D)"
            (vx (a o))
            (vy (a o))
            (vx (b o))
            (vy (b o)))))

(defun compute-corner-rounding (canvas rect)
  (if-let ((rounding (round-corners rect)))
    (with-canvas (canvas)
      (* rounding
         (* (- 1.0 (* 2 (padding canvas)))
            (min height width))))
    0))

(defmethod ntransform ((rectangle rectangle) transformation)
  (ntransform (a rectangle))
  (ntransform (b rectangle))
  (callf (round-corners rectangle) #'ntransform)
  rectangle)


;;;; Circles ------------------------------------------------------------------
(defclass* (circle :conc-name "") (drawable)
  ((center :type vec3)
   (radius :type single-float)))

(defun circle (center radius &key (opacity 1.0d0) (color *black*))
  (make-instance 'circle :center (homogenize center) :radius radius
    :color color
    :opacity (coerce opacity 'double-float)))

(defmethod print-object ((o circle) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "(~D, ~D) radius ~D"
            (vx (center o))
            (vy (center o))
            (radius o))))

(defmethod ntransform ((circle circle) transformation)
  (ntransform (center circle))
  (callf (radius circle) #'ntransform)
  circle)


;;;; Points -------------------------------------------------------------------
(defclass* (point :conc-name "") (drawable)
  ((location :type vec3)))

(defun point (location &key (opacity 1.0d0) (color *black*))
  (make-instance 'point :location (homogenize location)
    :color color
    :opacity (coerce opacity 'double-float)))

(defmethod print-object ((o point) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "(~D, ~D)"
            (vx (location o))
            (vy (location o)))))

(defmethod ntransform ((point point) transformation)
  (ntransform (location point))
  point)


;;;; Text ---------------------------------------------------------------------
(defclass* (text :conc-name "") (drawable)
  ((pos :type vec3)
   (font :type string)
   (size :type single-float)
   (align :type keyword)
   (content :type string)))

(defun text (position size font content
             &key (opacity 1.0d0) (color *black*) (align :left))
  (make-instance 'text
    :pos (homogenize position) :size size :font font :content content
    :align align
    :color color
    :opacity (coerce opacity 'double-float)))

(defmethod print-object ((o text) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~S (~D, ~D)"
            (content o)
            (vx (pos o))
            (vy (pos o)))))

(defmethod ntransform ((text text) transformation)
  (ntransform (pos text))
  (callf (size text) #'ntransform)
  text)


;;;; Rendering ----------------------------------------------------------------
(defgeneric render-object (canvas object))

(defun render (canvas objects)
  (map nil (curry #'render-object canvas) objects))


;;;; File Writing -------------------------------------------------------------
(defgeneric write-file (canvas filename))


;;;; File Extensions ----------------------------------------------------------
(defgeneric file-extension (type))

(defmethod file-extension (type)
  (string-downcase (symbol-name type)))


;;;; Toplevel -----------------------------------------------------------------
(defun full-filename (filename canvas-type)
  (format nil "~A.~A" filename (file-extension canvas-type)))

(defmacro with-rendering
    ((canvas-symbol canvas-type filename width height &key
                    (padding 0.03)
                    (background '(rgb 1 1 1)))
     &body body)
  (once-only (canvas-type)
    `(progn
       #+sbcl (sb-ext:gc :full t)
       (let ((,canvas-symbol (make-canvas ,canvas-type
                                          :height ,height
                                          :width ,width
                                          :padding ,padding
                                          :background ,background)))
         (multiple-value-prog1 ,@body
           (write-file ,canvas-symbol (full-filename ,filename ,canvas-type)))))))


;;;; Usage --------------------------------------------------------------------

;;;; Implementations ----------------------------------------------------------
;;; To implement a new type of canvas, you'll need to:
;;;
;;; * Add a new subclass of canvas.
;;; * Implement make-canvas.
;;; * Implement all the drawing methods for the various shapes.
;;; * Implement render-object (which should call draw and maybe do other stuff).
;;; * Implement write-file.
