(in-package :flax.drawing)

;;;; Utils --------------------------------------------------------------------
(defconstant +padding+ 0.03)

(defun convert-coord (value dimension)
  (map-range (- +padding+) (1+ +padding+)
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


;;;; Lines --------------------------------------------------------------------
(defclass line ()
  ((a :type coord :accessor a :initarg :a)
   (b :type coord :accessor b :initarg :b)))

(defun line (a b)
  (make-instance 'line :a a :b b))

(defmethod print-object ((o line) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "(~D, ~D) to (~D, ~D)"
            (x (a o))
            (y (a o))
            (x (b o))
            (y (b o)))))


(defmethod draw (image state (l line))
  (with-coordinates image
      ((ax ay (a l))
       (bx by (b l)))
    (-<> (list (cons ax ay)
               (cons bx by))
      paths:make-simple-path
      (paths:stroke-path <> 1)
      (vectors:update-state state <>))))


;;;; Triangles ----------------------------------------------------------------
(defclass triangle ()
  ((a :type coord :accessor a :initarg :a)
   (b :type coord :accessor b :initarg :b)
   (c :type coord :accessor c :initarg :c)))

(defun triangle (a b c)
  (make-instance 'triangle :a a :b b :c c))

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
(defun alpha-to-black (alpha)
  (- 255 (min 255 (abs alpha))))

(defun put-pixel (image x y alpha)
  (zapf (aref image x y)
        ;; (round (* (alpha-to-black alpha) %))
        (min % (alpha-to-black alpha))
        ))


(defun make-grayscale-image (width height)
  (make-array (list width height)
    :element-type '(integer 0 255)
    :initial-element 255))

(defun write-file (image filename)
  (trivial-ppm:write-to-file filename image :if-exists :supersede :format :pgm))


(defun blit (image state)
  (destructuring-bind (width height) (array-dimensions image)
    (aa:cells-sweep/rectangle state 0 0 width height (curry #'put-pixel image))))

(defun render (objects filename width height)
  (format t "Rendering ~D objects~%" (length objects))
  (finish-output)
  ;; #+sbcl (sb-ext:gc :full t)
  (let ((image (make-grayscale-image width height)))
    (dolist (o objects)
      (let ((state (aa:make-state)))
        (draw image state o)
        (blit image state)))
    (write-file image filename))
  ;; #+sbcl (sb-ext:gc :full t)
  (values))

