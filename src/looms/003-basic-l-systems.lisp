(in-package :flax.looms.003-basic-l-systems)

;;;; L-Systems ----------------------------------------------------------------
(defclass lsystem ()
  ((axiom :type list :accessor axiom :initarg :axiom)
   (productions :type hash-table :accessor productions :initarg :productions)))

(defun make-lsystem (axiom productions)
  (make-instance 'lsystem
    :axiom (ensure-list axiom)
    :productions (iterate (with result = (make-hash-table))
                          (for (symbol . word) :in productions)
                          (setf (gethash symbol result)
                                (ensure-list word))
                          (finally (return result)))))

(defun run-lsystem (lsystem axiom iterations mutate callback)
  (recursively ((word (or axiom (axiom lsystem)))
                (iteration 0))
    (when callback
      (funcall callback iteration word))
    (if (= iterations iteration)
      word
      (recur (funcall mutate (mappend (rcurry #'gethash (productions lsystem)) word))
             (1+ iteration)))))

(defmacro define-lsystem (name axiom &rest productions)
  (let ((var (symb '* name '*)))
    `(progn
       (defparameter ,var (make-lsystem ',axiom ',productions))
       (defun ,name (iterations &key mutate callback axiom)
         (run-lsystem ,var axiom iterations mutate callback)))))


(define-lsystem anabaena-catenula ar
  (ar . (al br))
  (al . (bl ar))
  (br . ar)
  (bl . al))


(defun cull (word)
  (iterate
    (with chance = (map-range 0 150
                              0 0.8
                              (length word)))
    (for symbol :in word)
    (if-first-time
      (collect symbol)
      (unless (randomp chance #'rand)
        (collect symbol)))))


;;;; Drawing ------------------------------------------------------------------
(defparameter *cell-unit* 0.007)
(defparameter *aspect-ratio* 9/8)
(defparameter *cell-width*  (* *cell-unit* *aspect-ratio*))
(defparameter *cell-height* (* *cell-unit* (/ *aspect-ratio*)))
(defparameter *horizontal-padding* (/ *cell-width* 2))
(defparameter *vertical-padding* (/ *cell-height* 1.5))
(defparameter *brush* (rgb 1.000 0.920 0.850))
(defparameter *background* (rgb 0.337 0.196 0.063))

(defun symbol-width (symbol)
  (ecase symbol
    ((al ar) (* 2 *cell-width*))
    ((bl br) *cell-width*)))

(defun word-width (word)
  (+ (reduce #'+ word :key #'symbol-width)
     (* (1- (length word)) *horizontal-padding*)))

(defun convert-symbol (symbol x y)
  (flax.drawing:rectangle
    (coord x y)
    (coord (+ x (symbol-width symbol))
           (+ y *cell-height*))
    :color *brush*
    :round-corners (/ *cell-unit* 2)))

(defun convert (word iteration)
  (let ((y (* iteration (+ *cell-height* *vertical-padding*)))
        (width (word-width word)))
    (iterate
      (with x = (- 0.5 (/ width 2)))
      (for symbol :in word)
      (collect (convert-symbol symbol x y))
      (incf x (+ (symbol-width symbol) *horizontal-padding*)))))


(defun maximum-words ()
  (truncate 1.0 (+ *cell-height* *vertical-padding*)))


;;;; Main ---------------------------------------------------------------------
(defun random-anabaena-catenula-axiom (length)
  (gimme length (random-elt '(ar al br bl) #'rand)))

(defun loom (seed filename filetype width height)
  (nest
    (with-seed seed)
    (flax.drawing:with-rendering
        (canvas filetype filename width height :background *background*))
    (anabaena-catenula (maximum-words)
                       :axiom (random-anabaena-catenula-axiom
                                (random-range-inclusive 1 6 #'rand))
                       :mutate #'cull
                       :callback (lambda (iteration word)
                                   (flax.drawing:render canvas (convert word iteration))))))



;; (time (loom-anabaena-catenula nil "out" :svg 800 800))
