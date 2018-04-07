(in-package :flax.drawing)

;;;; Parameters ---------------------------------------------------------------
(defparameter *padding* 0.03)
(defparameter *black* (rgb 0 0 0))
(defparameter *white* (rgb 1 1 1))


;;;; Canvas -------------------------------------------------------------------
(defclass* (canvas :conc-name "") ()
  (width height))

(define-with-macro (canvas :conc-name "") width height)

(defgeneric make-canvas (type &key &allow-other-keys))


;;;; Utils --------------------------------------------------------------------
(defun convert-coordinate (value dimension)
  (map-range (- *padding*) (1+ *padding*)
             0 dimension
             value))

(defun convert-magnitude (canvas magnitude)
  (let ((dim (min (height canvas) (width canvas))))
    (lerp 0 (- dim (* 2 *padding* dim)) magnitude)))


(defmacro with-coordinates (canvas bindings &body body)
  (once-only (canvas)
    (with-gensyms (width height)
      (labels ((parse-coord-binding (binding)
                 (with-gensyms (coord)
                   (destructuring-bind (x-symbol y-symbol value) binding
                     `((,coord ,value)
                       (,x-symbol (convert-coordinate (vx ,coord) ,width))
                       (,y-symbol (convert-coordinate (vy ,coord) ,height))))))
               (parse-magnitude-binding (binding)
                 (destructuring-bind (magnitude-symbol value) binding
                   `((,magnitude-symbol (convert-magnitude ,canvas ,value)))))
               (parse-binding (binding)
                 (ecase (length binding)
                   (2 (parse-magnitude-binding binding))
                   (3 (parse-coord-binding binding)))))
        `(with-canvas (,canvas ,width ,height)
           (let* ,(mapcan #'parse-binding bindings)
             ,@body))))))


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
    :points points
    :color color
    :opacity (coerce opacity 'double-float)))

(defmethod print-object ((o path) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~{~A~^ -> ~}"
            (mapcar #'coord-to-string (points o)))))


;;;; Triangles ----------------------------------------------------------------
(defclass* (triangle :conc-name "") (drawable)
  ((a :type vec2)
   (b :type vec2)
   (c :type vec2)))

(defun triangle (a b c &key (opacity 1.0d0) (color *black*))
  (make-instance 'triangle :a a :b b :c c
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


;;;; Rectangles ---------------------------------------------------------------
(defclass* (rectangle :conc-name "") (drawable)
  ((a :type vec2)
   (b :type vec2)
   (round-corners :type float :initform 0.0)))

(defun rectangle (a b &key (opacity 1.0d0) (color *black*) round-corners)
  (make-instance 'rectangle :a a :b b
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
         (* (- 1.0 *padding* *padding*)
            (min height width))))
    0))


;;;; Circles ------------------------------------------------------------------
(defclass* (circle :conc-name "") (drawable)
  ((center :type vec2)
   (radius :type single-float)))

(defun circle (center radius &key (opacity 1.0d0) (color *black*))
  (make-instance 'circle :center center :radius radius
    :color color
    :opacity (coerce opacity 'double-float)))

(defmethod print-object ((o circle) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "(~D, ~D) radius ~D"
            (vx (center o))
            (vy (center o))
            (radius o))))


;;;; Points -------------------------------------------------------------------
(defclass* (point :conc-name "") (drawable)
  ((location :type vec2)))

(defun point (location &key (opacity 1.0d0) (color *black*))
  (make-instance 'point :location location
    :color color
    :opacity (coerce opacity 'double-float)))

(defmethod print-object ((o point) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "(~D, ~D)"
            (vx (location o))
            (vy (location o)))))


;;;; Text ---------------------------------------------------------------------
(defclass* (text :conc-name "") (drawable)
  ((pos :type vec2)
   (font :type string)
   (size :type single-float)
   (align :type keyword)
   (content :type string)))

(defun text (position size font content
             &key (opacity 1.0d0) (color *black*) (align :left))
  (make-instance 'text
    :pos position :size size :font font :content content
    :align align
    :color color
    :opacity (coerce opacity 'double-float)))

(defmethod print-object ((o text) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~S (~D, ~D)"
            (content o)
            (vx (pos o))
            (vy (pos o)))))


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
                                          :background ,background))
             (*padding* ,padding))
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
