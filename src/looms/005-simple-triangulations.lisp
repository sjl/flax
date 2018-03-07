(in-package :flax.looms.005-simple-triangulations)

;; https://mattdesl.svbtle.com/pen-plotter-1

(defparameter *point-size* 0.004)

(defun convert-point (point)
  (flax.drawing:circle point *point-size*))

(defun convert-triangle (tri)
  (destructuring-bind (a b c) tri
    (flax.drawing:triangle a b c)))

(defun convert (points)
  (append
    (map 'list #'convert-point points)
    (map 'list #'convert-triangle (triangulate points))))

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

(defun loom (seed points filename filetype width height)
  (losh::clear-gaussian-spare)
  (with-seed seed
    (flax.drawing:with-rendering (canvas filetype filename width height
                                         :background (hsv 0.09 0.05 0.975))
      (destructuring-bind (generator generator-name)
          (random-elt '((generate-point-uniform "Uniform")
                        (generate-point-gaussian "Gaussian")
                        (generate-point-gaussian-vertical "Vertical Gaussian")
                        (generate-point-gaussian-horizontal "Horizontal Gaussian"))
                      #'rand)
        (flax.drawing:render canvas (convert (generate generator points)))
        generator-name))))


;; (time (loom nil (* 10 (random 100)) "out" :png 800 800))
