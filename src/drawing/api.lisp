(in-package :flax.drawing)

;;;; Parameters ---------------------------------------------------------------
(defparameter *black* (rgb 0 0 0))
(defparameter *white* (rgb 1 1 1))


;;;; Canvas -------------------------------------------------------------------
(defclass* canvas ()
  ((width :type (integer 1))
   (height :type (integer 1))
   (padding :type (single-float 0.0 0.5) :initform 0.03)
   (output-transformation :type mat3)))

(defun recompute-output-transformation (canvas)
  (setf (output-transformation canvas)
        (transformation
          (place (vec 0 0)
                 (vec (coerce (width canvas) 'single-float)
                      (coerce (height canvas) 'single-float))
                 :padding (padding canvas)))))

(defmethod initialize-instance :after ((canvas canvas) &key)
  (recompute-output-transformation canvas))

(define-with-macro canvas width height)

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

(defun coords-to-pairs (canvas cs)
  (loop :for c :in cs :collect (coord-to-pair canvas c)))


;;;; Drawables ----------------------------------------------------------------
(defclass* drawable ()
  ((opacity :type (double-float 0.0d0 1.0d0))
   (color :type color)))

(defgeneric draw (canvas drawing-object))


;;;; Paths --------------------------------------------------------------------
(defclass* path (drawable)
  ((points :type list)))

(defun normalize-point (point)
  (if (listp point)
    point
    (list point)))

(defun normalize-points (points)
  (mapcar #'normalize-point points))

(defun path (points &key (opacity 1.0d0) (color *black*))
  (make-instance 'path
    :points (mapcar-curried #'mapcar #'homogenize (normalize-points points))
    :color color
    :opacity (coerce opacity 'double-float)))

(defmethod print-object ((o path) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~{~A~^ -> ~}"
            (mapcar (compose #'coord-to-string #'first) (points o)))))

(defmethod ntransform ((path path) transformation)
  (dolist (ps (points path))
    (dolist (p ps)
      (ntransform p transformation)))
  path)


;;;; Triangles ----------------------------------------------------------------
(defclass* triangle (drawable)
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
  (ntransform (a triangle) transformation)
  (ntransform (b triangle) transformation)
  (ntransform (c triangle) transformation)
  triangle)


;;;; Rectangles ---------------------------------------------------------------
(defclass* rectangle (drawable)
  ((a :type vec3)
   (b :type vec3)
   (round-corners :type float :initform 0.0)))

(defun rectangle (a b &key (opacity 1.0d0) (color *black*) round-corners)
  (make-instance 'rectangle :a (homogenize a) :b (homogenize b)
    :color color
    :opacity (coerce opacity 'double-float)
    :round-corners (or round-corners 0.0)))

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
  (ntransform (a rectangle) transformation)
  (ntransform (b rectangle) transformation)
  (zapf (round-corners rectangle) (ntransform % transformation))
  rectangle)


;;;; Circles ------------------------------------------------------------------
(defclass* circle (drawable)
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
  (ntransform (center circle) transformation)
  ;; For non-aspect-ratio-preserving transformations, we want to keep circles
  ;; as circles, but ensure they fit within the new bounding box.  So we take
  ;; the smaller of the two possible radius transformations.
  (let ((a (vec 0 0 1))
        (b (vec 1 1 1)))
    (ntransform a transformation)
    (ntransform b transformation)
    (let ((c (v- a b)))
      (mulf (radius circle) (min (abs (vx c)) (abs (vy c))))))
  circle)


;;;; Points -------------------------------------------------------------------
(defclass* point (drawable)
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
  (ntransform (location point) transformation)
  point)


;;;; Glyph --------------------------------------------------------------------
(defclass* glyph (drawable)
  ((pos :type vec3)
   (width :type single-float)
   (ch :type character)
   (paths :type list)))

(defun glyph (position width character &key (opacity 1.0d0) (color *black*))
  (make-instance 'glyph
    :pos (homogenize position)
    :width (coerce width 'single-float)
    :ch character
    :color color
    :opacity (coerce opacity 'double-float)))

(defun recompute-glyph-paths (glyph)
  (let ((paths (letter-paths (ch glyph)))
        (size (* 2 (width glyph))))
    (ntransform paths (transformation
                        (scale size size)
                        (translate (vx (pos glyph))
                                   (vy (pos glyph)))))
    (setf (paths glyph) paths)))

(defmethod initialize-instance :after ((glyph glyph) &key)
  (recompute-glyph-paths glyph))

(defmethod print-object ((o glyph) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~A ~A" (ch o) (pos o))))

(defmethod ntransform ((glyph glyph) transformation)
  (ntransform (pos glyph) transformation)
  (ntransformf (width glyph) transformation)
  (ntransformf (paths glyph) transformation)
  ;; (recompute-glyph-paths glyph)
  glyph)

(defmethod draw (canvas (glyph glyph))
  (map-curried #'draw canvas (paths glyph)))


;;;; Text ---------------------------------------------------------------------
(defclass* text (drawable)
  ((pos :type vec3)
   (letter-width :type single-float)
   (letter-spacing :type single-float)
   (content :type string)
   (glyphs :type list)))

(defun rebuild-glyphs (text)
  (setf (glyphs text)
        (iterate
          (with pos = (pos text))
          (with y = (vy (pos text)))
          (with space = (+ (letter-width text) (letter-spacing text)))
          (with scale = (/ (letter-width text) 0.5))
          (for ch :in-string (content text))
          (for pch :previous ch)
          (for x :from (vx pos) :by space)
          (incf x (* (kern pch ch) scale))
          (collect (glyph (vec x y) (letter-width text) ch
                          :opacity (opacity text)
                          :color (color text))))))

(defun text (position letter-width content &key (letter-spacing 0.0) (opacity 1.0d0) (color *black*))
  (make-instance 'text
    :pos (homogenize position)
    :letter-width (coerce letter-width 'single-float)
    :letter-spacing (coerce letter-spacing 'single-float)
    :content content
    :color color
    :opacity (coerce opacity 'double-float)))

(defmethod initialize-instance :after ((text text) &key)
  (rebuild-glyphs text))


(defmethod print-object ((o text) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~S ~A"
            (content o)
            (pos o))))

(defmethod draw (canvas (text text))
  (map-curried #'draw canvas (glyphs text)))

(defmethod ntransform ((text text) transformation)
  (ntransform (pos text) transformation)
  (ntransformf (letter-width text) transformation)
  (rebuild-glyphs text)
  text)


;;;; Rendering ----------------------------------------------------------------
(defgeneric render-object (canvas object))

(defun render (canvas objects)
  (map-curried #'render-object canvas objects))


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
