(in-package :flax.drawing)

;;;; Utils --------------------------------------------------------------------
(defparameter *padding* 0.03)
(defparameter *black* (rgb 0 0 0))

(defun convert-coord (value dimension)
  (map-range (- *padding*) (1+ *padding*)
             0 dimension
             value))

(defmacro with-coordinates (image bindings &body body)
  (with-gensyms (width height channels)
    `(destructuring-bind (,height ,width ,channels) (array-dimensions ,image)
       (declare (ignore ,channels))
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


;;;; Rectangles ---------------------------------------------------------------
(defclass rectangle (drawable)
  ((a :type coord :accessor a :initarg :a)
   (b :type coord :accessor b :initarg :b)
   (round-corners :type (or null integer)
                  :accessor round-corners
                  :initarg :round-corners)))

(defun rectangle (a b &key (opacity 1.0d0) (color *black*) round-corners)
  (make-instance 'rectangle :a a :b b
    :color color
    :opacity opacity
    :round-corners round-corners))

(defmethod print-object ((o rectangle) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "(~D, ~D) (~D, ~D)"
            (x (a o))
            (y (a o))
            (x (b o))
            (y (b o)))))

(defmethod draw (image state (rect rectangle))
  (with-coordinates image
      ((ax ay (a rect))
       (bx by (b rect)))
    (-<> (paths:make-rectangle-path ax ay bx by
                                    :round (* (round-corners rect)
                                              (* (- 1.0 *padding* *padding*)
                                                 (min (array-dimension image 0)
                                                      (array-dimension image 1)))))
      ;; paths:make-simple-path
      ;; (paths:stroke-path <> 1)
      (vectors:update-state state <>))))


;;;; Glue ---------------------------------------------------------------------
(deftype image ()
  '(simple-array (double-float 0.0d0 1.0d0) (* * 3)))

(deftype index ()
  `(integer 0 (,array-dimension-limit)))

(deftype row-buffer ()
  '(simple-array (integer 0 255) (*)))


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


(defun-inline prepare-sample (value)
  (declare (optimize speed)
           (type (double-float 0.0d0 1.0d0) value))
  (round (* 255.0d0 value)))


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

(defun write-file (image filename)
  (destructuring-bind (height width channels) (array-dimensions image)
    (declare (ignore channels))
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


(defun render-object (image object)
  (let ((state (aa:make-state)))
    (draw image state object)
    (destructuring-bind (height width channels) (array-dimensions image)
      (declare (ignore channels))
      (aa:cells-sweep/rectangle
        state 0 0 width height
        (curry #'put-pixel image (color object) (opacity object))))))

(defun render (image objects)
  (map nil (curry #'render-object image) objects))

(defun fade (image color alpha)
  (declare (optimize speed)
           (type image image)
           (type color color)
           (type (double-float 0.0d0 1.0d0) alpha))
  (nest (with-color (color r g b))
        (dotimes (row (array-dimension image 0)))
        (dotimes (col (array-dimension image 1)))
        (zapf (aref image row col 0) (lerp % r alpha)
              (aref image row col 1) (lerp % g alpha)
              (aref image row col 2) (lerp % b alpha))))

(defmacro with-rendering
    ((image-symbol filename width height &key
                   (padding 0.03)
                   (background '(rgb 1 1 1)))
     &body body)
  `(progn
     (sb-ext:gc :full t)
     (let ((,image-symbol (make-image ,width ,height ,background))
           (*padding* ,padding))
       (prog1 ,@body
         (write-file ,image-symbol ,filename)))))

