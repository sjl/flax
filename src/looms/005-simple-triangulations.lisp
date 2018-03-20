(in-package :flax.looms.005-simple-triangulations)

;; https://mattdesl.svbtle.com/pen-plotter-1

(defparameter *point-size* 0.003)

(defun convert-point (point)
  (flax.drawing:circle point (random-gaussian *point-size*
                                              (* 0.15 *point-size*)
                                              #'rand)))

(defun convert-triangle (ratio tri)
  (when (randomp ratio #'rand)
    (destructuring-bind (a b c) tri
      (list (flax.drawing:triangle a b c)))))

(defun convert (points ratio)
  (append
    (map 'list #'convert-point points)
    (mapcan (curry #'convert-triangle ratio) (triangulate points))))

(defun triangulate (points)
  (mapcar (lambda (indexes)
            (map 'list (curry #'aref points) indexes))
          (lofi.tri:triangulate (map 'vector #'coord-to-cons points))))

(defun gauss ()
  (clamp 0.0 1.0 (random-gaussian 0.5 0.15 #'rand)))

(defun generate-point-uniform ()
  (coord (rand 1.0) (rand 1.0)))

(defun generate-point-gaussian ()
  (coord (gauss) (gauss)))

(defun generate-point-gaussian-vertical ()
  (coord (rand 1.0) (gauss)))

(defun generate-point-gaussian-horizontal ()
  (coord (gauss) (rand 1.0)))

(defun generate (generator n)
  (iterate (repeat n)
           (collect (funcall generator) 
                    :result-type 'vector)))

(defun loom (seed points filename filetype width height &key ratio)
  (losh::clear-gaussian-spare)
  (nest
    (with-seed seed)
    (flax.drawing:with-rendering (canvas filetype filename width height
                                         :background (hsv 0.09 0.05 0.975)))
    (destructuring-bind (generator generator-name)
        (random-elt '((generate-point-uniform "Uniform")
                      (generate-point-gaussian "Gaussian")
                      (generate-point-gaussian-vertical "Vertical Gaussian")
                      (generate-point-gaussian-horizontal "Horizontal Gaussian"))
                    #'rand))
    (let* ((triangulation-ratio (if (randomp 0.5 #'rand)
                                  1
                                  (random-range 0.1 0.3 #'rand)))
           (triangulation-ratio (or ratio triangulation-ratio))))
    (progn
      (flax.drawing:render canvas (convert (generate generator points)
                                           triangulation-ratio))
      (list generator-name triangulation-ratio))))


;; (time (loom 5 400 "out" :svg 800 800 :ratio nil))
