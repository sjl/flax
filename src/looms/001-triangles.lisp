(in-package :flax.looms.001-triangles)

;;;; Triangle Subdivision
;;;
;;; Based on http://www.tylerlhobbs.com/writings/triangle-subdivision

;;;; Utils --------------------------------------------------------------------
(defun round-to (number divisor)
  (* divisor (round number divisor)))


;;;; Elements -----------------------------------------------------------------
(defstruct (triangle (:conc-name ""))
  (a (coord 0 0) :type coord)
  (b (coord 0 0) :type coord)
  (c (coord 0 0) :type coord))

(define-with-macro (triangle :conc-name "") a b c)

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
  (list (triangle (coord 0 1)
                  (coord 1 1)
                  (coord 0 0))
        (triangle (coord 1 0)
                  (coord 1 1)
                  (coord 0 0))))


(defun split-triangle-evenly (triangle)
  (with-triangle (triangle)
    (let* ((n 1/2)
           (p (coord (lerp (x b) (x c) n)
                     (lerp (y b) (y c) n))))
      (list (triangle p b a)
            (triangle p a c)))))

(defun generate-universe-even (depth &aux (triangles (initial-triangles)))
  (do-repeat depth
    (zapf triangles (mappend #'split-triangle-evenly %)))
  triangles)


(defun find-longest-side (triangle)
  (with-triangle (triangle)
    (let* ((ab (distance a b))
           (bc (distance b c))
           (ca (distance c a))
           (longest (max ab bc ca)))
      (cond
        ((= longest ab) (list c a b))
        ((= longest bc) (list a c b))
        ((= longest ca) (list b c a))
        (t (error "what?"))))))

(defun split-triangle-self-balancing (triangle)
  (destructuring-bind (a b c) (find-longest-side triangle)
    (let ((p (-<> (random-gaussian 0.5 0.1 #'rand)
               (clamp 0.3 0.7 <>)
               (round-to <> 1/100)
               (clerp b c <>))))
      (list (triangle p b a)
            (triangle p a c)))))

(defun generate-universe-balancing (depth seed)
  (losh::clear-gaussian-spare)
  (with-seed seed
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
        (map nil #'recur (initial-triangles))))))


;;;; Main ---------------------------------------------------------------------
(defun loom (seed depth filename width height)
  (flax.drawing:with-rendering (image filename width height)
    (flax.drawing:render image (convert (generate-universe-balancing depth seed)))))


;; (time (loom 12 18 "out.pnm" 3000 3000))
