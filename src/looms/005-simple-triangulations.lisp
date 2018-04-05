(in-package :flax.looms.005-simple-triangulations)

;; https://mattdesl.svbtle.com/pen-plotter-1

(defparameter *point-size* 0.003)

(defun convert-point (point)
  (flax.drawing:circle (coord (vx point) (vy point))
                       (random-gaussian *point-size*
                                              (* 0.15 *point-size*)
                                              #'rand)))

(defun convert-triangle (ratio tri)
  (when (randomp ratio #'rand)
    (destructuring-bind (a b c) tri
      (list (flax.drawing:triangle (coord (vx a) (vy a))
                                   (coord (vx b) (vy b))
                                   (coord (vx c) (vy c)))))))

(defun convert (points ratio)
  (append
    (map 'list #'convert-point points)
    (mapcan (curry #'convert-triangle ratio) (triangulate points))))

(defun triangulate (points)
  (mapcar (lambda (indexes)
            (map 'list (curry #'aref points) indexes))
          (lofi.tri:triangulate (map 'vector (lambda (p)
                                               (cons (vx p) (vy p)))
                                     points))))

(defun gauss ()
  (clamp 0.0 1.0 (random-gaussian 0.5 0.15 #'rand)))

(defun generate-point-uniform ()
  (vec2 (rand 1.0) (rand 1.0)))

(defun generate-point-gaussian ()
  (vec2 (gauss) (gauss)))

(defun generate-point-gaussian-vertical ()
  (vec2 (rand 1.0) (gauss)))

(defun generate-point-gaussian-horizontal ()
  (vec2 (gauss) (rand 1.0)))

(defun generate (generator n)
  (iterate (repeat n)
           (collect (funcall generator) 
                    :result-type 'vector)))

(defun select-generator ()
  (random-elt '((generate-point-uniform "Uniform")
                (generate-point-gaussian "Gaussian")
                (generate-point-gaussian-vertical "Vertical Gaussian")
                (generate-point-gaussian-horizontal "Horizontal Gaussian"))
              #'rand))

(defun loom (seed filename filetype width height &key ratio points)
  (nest
    (with-seed seed)
    (flax.drawing:with-rendering (canvas filetype filename width height
                                         :background (hsv 0.09 0.05 0.975)))
    (destructuring-bind (generator generator-name) (select-generator))
    (randomly-initialize
      ((ratio (if (randomp 0.5 #'rand)
                1
                (random-range 0.05 0.2 #'rand)))
       (points (round-to (random-range-inclusive 100 1000 #'rand) 10))))
    (progn
      (-<> (generate generator points)
        (convert <> ratio)
        (flax.drawing:render canvas <>))
      (values generator-name points ratio))))


;; (time (loom 55 "out" :svg 800 800 ))
