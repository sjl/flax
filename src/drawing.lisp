(in-package :flax.drawing)

;;;; Utils --------------------------------------------------------------------
(defparameter *padding* 0.03)
(defparameter *black* (rgb 0 0 0))

(defun convert-coord (value dimension)
  (map-range (- *padding*) (1+ *padding*)
             0 dimension
             value))

(defmacro with-coordinates (image bindings &body body)
  (with-gensyms (width height)
    `(destructuring-bind (,width ,height) (array-dimensions ,image)
       (let* ,(iterate (for (x-symbol y-symbol coord) :in bindings)
                       (for c = (gensym "coord"))
                       (appending
                         (list `(,c ,coord)
                               `(,x-symbol (convert-coord (x ,c) ,width))
                               `(,y-symbol (convert-coord (y ,c) ,height)))))
         ,@body))))


;;;; Drawing Protocol ---------------------------------------------------------
(defgeneric draw (image state drawing-object))

(defclass drawable ()
  ((opacity :type (double-float 0.0d0 1.0d0) :accessor opacity :initarg :opacity)
   (color :type color :accessor color :initarg :color)))


;;;; Paths --------------------------------------------------------------------
(defclass path (drawable)
  ((points :type list :accessor points :initarg :points)))

(defun path (points &key (opacity 1.0d0) (color *black*))
  (make-instance 'path
    :points points
    :color color
    :opacity opacity))

(defun coord-to-string (c)
  (format nil "(~A, ~A)" (x c) (y c)))

(defun coord-to-pair (image c)
  (with-coordinates image ((x y c))
    (cons x y)))

(defmethod print-object ((o path) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~{~A~^ -> ~}"
            (mapcar #'coord-to-string (points o)))))

(defmethod draw (image state (p path))
  (-<> (points p)
    (mapcar (curry #'coord-to-pair image) <>)
    paths:make-simple-path
    (paths:stroke-path <> 1)
    (vectors:update-state state <>)))


;;;; Triangles ----------------------------------------------------------------
(defclass triangle (drawable)
  ((a :type coord :accessor a :initarg :a)
   (b :type coord :accessor b :initarg :b)
   (c :type coord :accessor c :initarg :c)))

(defun triangle (a b c &key (opacity 1.0d0) (color *black*))
  (make-instance 'triangle :a a :b b :c c :color color :opacity opacity))

(defmethod print-object ((o triangle) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "(~D, ~D) (~D, ~D) (~D, ~D)"
            (x (a o))
            (y (a o))
            (x (b o))
            (y (b o))
            (x (c o))
            (y (c o)))))

(defmethod draw (image state (tri triangle))
  (with-coordinates image
      ((ax ay (a tri))
       (bx by (b tri))
       (cx cy (c tri)))
    (-<> (list (cons ax ay)
               (cons bx by)
               (cons cx cy)
               (cons ax ay))
      paths:make-simple-path
      (paths:stroke-path <> 1)
      (vectors:update-state state <>))))


;;;; Glue ---------------------------------------------------------------------
(deftype image ()
  '(simple-array color (* *)))

(deftype prepared-image ()
  '(simple-array (simple-array (integer 0 255) (3)) (* *)))

(deftype index ()
  `(integer 0 (,array-dimension-limit)))


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
  (let ((pixel (aref image x y)))
    (declare (type color pixel))
    (blend! pixel color (* opacity (normalize-alpha alpha)))
    (values)))


(defun-inline prepare-channel (value)
  (declare (optimize speed)
           (type (double-float 0.0d0 1.0d0) value))
  (round (* 255.0d0 value)))

(defun-inline prepare-pixel (pixel)
  (declare (optimize speed)
           (type color pixel))
  (with-color (pixel r g b)
    (list (prepare-channel r)
          (prepare-channel g)
          (prepare-channel b)
          255)))


(defun make-initialized-array (dimensions function &rest make-array-args)
  (let ((result (apply #'make-array dimensions make-array-args)))
    (do-array (v result)
      (setf v (funcall function)))
    result))

(defun make-image (width height)
  (make-initialized-array (list width height)
                          (curry #'rgb 1 1 1)))


(defun write-file (image filename)
  (destructuring-bind (width height) (array-dimensions image)
    (let ((png (make-instance 'zpng:pixel-streamed-png
                 :color-type :truecolor-alpha
                 :width width
                 :height height)))
      (with-open-file (stream filename
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
        (zpng:start-png png stream)
        (dotimes (y height)
          (dotimes (x width)
            (zpng:write-pixel (prepare-pixel (aref image x y)) png)))
        (zpng:finish-png png)))))


(defun blit (image object)
  (let ((state (aa:make-state)))
    (draw image state object)
    (destructuring-bind (width height) (array-dimensions image)
      (aa:cells-sweep/rectangle
        state 0 0 width height
        (curry #'put-pixel image (color object) (opacity object))))))


(defun render (image objects)
  (map nil (curry #'blit image) objects))

(defmacro with-rendering
    ((image-symbol filename width height &key (padding 0.03))
     &body body)
  `(let ((,image-symbol (make-image ,width ,height))
         (*padding* ,padding))
     (sb-ext:gc :full t)
     ,@body
     (write-file ,image-symbol ,filename)
     (values)))
