(in-package :flax.drawing)

;;;; Utils --------------------------------------------------------------------
(defparameter *padding* 0.03)

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
  ((opacity :type (single-float 0.0 1.0) :accessor opacity :initarg :opacity)))


;;;; Paths --------------------------------------------------------------------
(defclass path (drawable)
  ((points :type list :accessor points :initarg :points)))

(defun path (points &key (opacity 1.0))
  (make-instance 'path
    :points points
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

(defun triangle (a b c &key (opacity 1.0))
  (make-instance 'triangle :a a :b b :c c :opacity opacity))

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
  '(simple-array t (* *)))

(deftype index ()
  `(integer 0 (,array-dimension-limit)))


(defun-inline normalize-alpha (alpha)
  (declare (optimize speed)
           (type fixnum alpha))
  (/ (min 255 (abs alpha)) 255.0))

(defun-inline blend (old new alpha)
  (declare (optimize speed)
           (type (single-float 0.0 1.0) old new alpha))
  (lerp old new alpha))

(defun put-pixel (image opacity x y alpha)
  (declare (optimize speed)
           (type image image)
           (type index x y)
           (type (single-float 0.0 1.0) opacity)
           (type fixnum alpha))
  (zapf (aref image x y)
        (blend % 0.0 (* opacity (normalize-alpha alpha)))))

(defun-inline mutate-array (array function)
  (dotimes (i (array-total-size array))
    (setf (row-major-aref array i)
          (funcall function (row-major-aref array i)))))

(defun-inline scale-color (value)
  (declare (type (single-float 0.0 1.0) value))
  (round (* 255.0 value)))

(defun prepare-image (image)
  (declare (optimize speed)
           (type image image))
  (mutate-array image #'scale-color)
  image)

(defun make-grayscale-image (width height)
  (make-array (list width height) :initial-element 1.0))

(defun write-file (image filename)
  (trivial-ppm:write-to-file filename (prepare-image image)
                             :if-exists :supersede
                             :format :pgm))


(defun blit (image object)
  (let ((state (aa:make-state)))
    (draw image state object)
    (destructuring-bind (width height) (array-dimensions image)
      (aa:cells-sweep/rectangle state 0 0 width height
                                (curry #'put-pixel image (opacity object))))))


(defun render (image objects)
  (map nil (curry #'blit image) objects))

(defmacro with-rendering
    ((image-symbol filename width height &key (padding 0.03))
     &body body)
  `(let ((,image-symbol (make-grayscale-image ,width ,height))
         (*padding* ,padding))
     ,@body
     (write-file ,image-symbol ,filename)
     (values)))
