(in-package :flax.looms.001-triangles)

;;;; Triangle Subdivision
;;;
;;; Based on http://www.tylerlhobbs.com/writings/triangle-subdivision


;;;; Utils --------------------------------------------------------------------
(defun round-to (number divisor)
  (* divisor (round number divisor)))


;;;; Elements -----------------------------------------------------------------
(defstruct (triangle (:conc-name nil))
  (a (vec 0 0) :type vec2)
  (b (vec 0 0) :type vec2)
  (c (vec 0 0) :type vec2))

(define-with-macro triangle a b c)

(defun triangle (a b c)
  (make-triangle :a a :b b :c c))


;;;; Element Conversion -------------------------------------------------------
(defun convert-triangle (triangle)
  (with-triangle (triangle)
    (flax.drawing:triangle a b c)))

(defun convert (universe)
  (mapcar #'convert-triangle universe))


;;;; Generation ---------------------------------------------------------------
(defun initial-triangles ()
  (list (triangle (vec 0 1)
                  (vec 1 1)
                  (vec 0 0))
        (triangle (vec 1 0)
                  (vec 1 1)
                  (vec 0 0))))


(defun split-triangle-evenly (triangle)
  (with-triangle (triangle)
    (let* ((n 1/2)
           (p (vec2 (lerp (vx b) (vx c) n)
                    (lerp (vy b) (vy c) n))))
      (list (triangle p b a)
            (triangle p a c)))))

(defun generate-universe-even (depth &aux (triangles (initial-triangles)))
  (do-repeat depth
    (zapf triangles (alexandria:mappend #'split-triangle-evenly %)))
  triangles)


(defun find-longest-side (triangle)
  (with-triangle (triangle)
    (let* ((ab (vdistance a b))
           (bc (vdistance b c))
           (ca (vdistance c a))
           (longest (max ab bc ca)))
      (cond
        ((= longest ab) (list c a b))
        ((= longest bc) (list a c b))
        ((= longest ca) (list b c a))
        (t (error "what?"))))))

(defun split-triangle-self-balancing (triangle)
  (destructuring-bind (a b c) (find-longest-side triangle)
    (let ((p (_ (random-gaussian 0.5 0.1 #'rand)
               (clamp 0.3 0.7 _)
               (round-to _ 1/100)
               (vlerp b c _))))
      (list (triangle p b a)
            (triangle p a c)))))

(defun generate-universe-balancing (depth)
  (gathering
    (labels ((should-stop-p (iteration)
               (or (= depth iteration)
                   (and (> iteration 6)
                        (randomp (map-range 0 depth
                                            0.0 0.05
                                            iteration)
                                 #'rand))))
             (recur (triangle &optional (iteration 0))
               (if (should-stop-p iteration)
                 (gather triangle)
                 (map nil (rcurry #'recur (1+ iteration))
                      (split-triangle-self-balancing triangle)))))
      (map nil #'recur (initial-triangles)))))


;;;; Main ---------------------------------------------------------------------
(defun loom (seed filename filetype width height &key depth)
  (nest
    (with-seed seed)
    (randomly-initialize ((depth (random-range-inclusive 14 19 #'rand))))
    (flax.drawing:with-rendering (canvas filetype filename width height))
    (progn
      (_ (generate-universe-balancing depth)
        convert
        (flax.drawing:render canvas _))
      (values depth))))


;; (declaim (optimize (speed 1)))

;; (time (loom nil "out" :png 800 800 :depth 16))
