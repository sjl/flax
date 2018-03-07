;;;; lofi-tri.lisp
;;; Code vendored from https://github.com/photex/lofi-tri
;;; TODO: Implement a divide & conquer algorithm at some point.

(defpackage #:lofi.tri
  (:use #:cl #:sb-cga)
  (:export #:circle
           #:triangle
           #:random-point
           #:random-point-array
           #:sort-by-x
           #:sort-by-y
           #:sort-by-z
           #:distance
           #:midpoint
           #:circumcircle
           #:center
           #:radius
           #:verts
           #:in-circumcircle?
           #:has-shared-verts?
           #:triangulate))

(in-package #:lofi.tri)

;;; "lofi-tri" goes here. Hacks and glory await!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structs

(defstruct circle
  (center nil :type vec)
  (radius 0.0 :type float)
  (radius-squared 0.0 :type float)
  (diameter 0.0 :type float))

(defstruct triangle
  (verts #() :type vector)
  (circumcircle nil :type circle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(defun random-point (&optional (state *random-state*))
  "Return an instance of sb-cga:vec initialized with random values."
  (apply #'vec (loop repeat 3
                  collect (random 1.0 state))))

(defun random-point-array (count &optional (state *random-state*))
  "Returns an array of <count> random-points."
  (let ((result (make-array count :fill-pointer 0)))
    (dotimes (i count)
      (vector-push (random-point state) result))
    result))

(defmacro sort-by (point-set index)
  "Sort the input point set by the value in the specified index."
  `(sort ,point-set #'< :key (lambda (p) (aref p ,index))))

(defmacro sort-by-x (point-set)
  "Sort the input point set by the value at 0"
  `(sort-by ,point-set 0))

(defmacro sort-by-y (point-set)
  "Sort the input point set by the value at 1"
  `(sort-by ,point-set 1))

(defmacro sort-by-z (point-set)
  "Sort the input point set by the value at 2"
  `(sort-by ,point-set 2))

(defun get-min-max (point-set)
  "Return the min and max vectors for the given point set. Effectively the bounding box."
  (let* ((first-point (aref point-set 0))
         (rest-points (subseq point-set 1))
         (minx (aref first-point 0)) (maxx (aref first-point 0))
         (miny (aref first-point 1)) (maxy (aref first-point 1))
         (minz (aref first-point 2)) (maxz (aref first-point 2)))
    (loop :for p :across rest-points :do
       (setf minx (min minx (aref p 0)) maxx (max maxx (aref p 0))
             miny (min miny (aref p 1)) maxy (max maxy (aref p 1))
             minz (min minz (aref p 2)) maxz (max maxz (aref p 2))))
    (values (vec minx miny minz) (vec maxx maxy maxz))))

(defun get-bounding-triangle-points (point-set &optional (fudge-factor 10))
  (multiple-value-bind (min max) (get-min-max point-set)
    (let ((dx (* fudge-factor (- (aref max 0) (aref min 0))))
          (dy (* fudge-factor (- (aref max 1) (aref min 1)))))
      (make-array 3 :initial-contents
                  (list (sb-cga:vec (- (aref min 0) dx) (- (aref min 1) (* dy 3)) 0.0)
                        (sb-cga:vec (- (aref min 0) dx) (+ (aref max 1) dy) 0.0)
                        (sb-cga:vec (+ (aref max 0) (* dx 3)) (+ (aref max 1) dy) 0.0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meat and potatos

(defun distance (v0 v1 &key 3d squared)
  "Calculate the distance between two vectors in 2D or 3D.
Will return the square root of the result unless :squared t"
  (declare (type vec v0 v1))
  (let* ((diff (vec- v0 v1))
         (result (apply #'+ (loop for i from 0 upto (if 3d 2 1)
                               collect (expt (aref diff i) 2)))))
    (if squared
        result
        (sqrt result))))

(defun midpoint (v0 v1)
  "Return a vector representing the midpoint between the two provided vectors."
  (declare (type vec v0 v1))
  (vec/ (vec+ v0 v1) 2.0))

(defun circumcircle (v0 v1 v2)
  "Returns a circle struct representing the circumcircle of the given 3 vertices"
  (let* ((v1-v0 (vec- v1 v0))
         (v2-v0 (vec- v2 v0))
         (v2-v1 (vec- v2 v1))
         (v1+v0 (vec+ v1 v0))
         (v2+v0 (vec+ v2 v0))
         (a (aref v1-v0 0))
         (b (aref v1-v0 1))
         (c (aref v2-v0 0))
         (d (aref v2-v0 1))
         (e (+ (* a (aref v1+v0 0))
               (* b (aref v1+v0 1))))
         (f (+ (* c (aref v2+v0 0))
               (* d (aref v2+v0 1))))
         (g (* 2.0 (- (* a (aref v2-v1 1))
                      (* b (aref v2-v1 0)))))
         (colinear? (< (abs g) +default-epsilon+))
         (cx 0.0) (cy 0.0) (dx 0.0) (dy 0.0))
    (if colinear?
        (let ((minx (min (aref v0 0) (aref v1 0) (aref v2 0)))
              (miny (min (aref v0 1) (aref v1 1) (aref v2 1)))
              (maxx (max (aref v0 0) (aref v1 0) (aref v2 0)))
              (maxy (max (aref v0 1) (aref v1 1) (aref v2 1))))
          (setf cx (/ (+ minx maxx) 2)
                cy (/ (+ miny maxy) 2)
                dx (- cx minx)
                dy (- cy miny)))
        ;; else
        (setf cx (/ (- (* d e) (* b f)) g)
              cy (/ (- (* a f) (* c e)) g)
              dx (- cx (aref v0 0))
              dy (- cy (aref v0 1))))
    (let* ((radius-squared (+ (* dx dx)
                              (* dy dy)))
           (radius (sqrt radius-squared)))
      (make-circle :center (vec cx cy 0.0)
                   :radius radius
                   :radius-squared radius-squared
                   :diameter (* radius 2)))))

(defun new-triangle (vi0 vi1 vi2 points)
  "Returns a new triangle."
  (let ((v0 (aref points vi0))
        (v1 (aref points vi1))
        (v2 (aref points vi2)))
    (make-triangle :verts (make-array 3 :initial-contents (list vi0 vi1 vi2))
                   :circumcircle (circumcircle v0 v1 v2))))

(defun in-circumcircle? (tri p)
  "Does point 'p' sit within the circumcircle of 'tri'?"
  (declare (type triangle tri) (type vec p))
  (let* ((circumcircle (slot-value tri 'circumcircle))
         (center (slot-value circumcircle 'center))
         (dist-squared (distance center p :squared t)))
    (<= dist-squared (slot-value circumcircle 'radius-squared))))

(defmacro edge= (a b)
  `(or (and (= (first ,a) (first ,b))
            (= (second ,a) (second ,b)))
       (and (= (first ,a) (second ,b))
            (= (second ,a) (first ,b)))))

(defun unique-edge? (edges a)
  (let ((instance-count (length (remove-if-not (lambda (b) (edge= a b)) edges))))
    (<= instance-count 1)))

(defun has-shared-verts? (a b)
  (declare (type triangle a b))
  (let* ((averts (slot-value a 'verts))
         (bverts (slot-value b 'verts))
         (av0 (aref averts 0))
         (av1 (aref averts 1))
         (av2 (aref averts 2))
         (bv0 (aref bverts 0))
         (bv1 (aref bverts 1))
         (bv2 (aref bverts 2)))
    (or (= bv0 av0) (= bv0 av1) (= bv0 av2)
        (= bv1 av0) (= bv1 av1) (= bv1 av2)
        (= bv2 av0) (= bv2 av1) (= bv2 av2))))

(defun add-vertex (vi triangles points)
  (let* ((edges ())
         (unaffected-tris ()))
    ;; For each triangle in the list we take the edges
    ;; of any triangle where vert is inside it's circumcircle
    ;; and append it to the edges list. Otherwise the triangle
    ;; is collected and stored in unaffected-tris
    (setf unaffected-tris
          (loop for tri in triangles
             if (in-circumcircle? tri (aref points vi))
             do (let* ((verts (slot-value tri 'verts))
                       (e0 (list (aref verts 0) (aref verts 1)))
                       (e1 (list (aref verts 1) (aref verts 2)))
                       (e2 (list (aref verts 2) (aref verts 0))))
                  (setf edges (append edges (list e0 e1 e2))))
             else collect tri))

    ;; Remove any edges that are duplicate so that the edge
    ;; list only contains the boundary edges.
    (setf edges (remove-if-not (lambda (edge)
                                 (unique-edge? edges edge))
                               edges))

    ;; Using the remaining edges and our input vert create
    ;; new triangles and return them appended to our unaffected-tris list
    (append unaffected-tris (loop for edge in edges
                               collect (let ((vi0 (first edge))
                                             (vi1 (second edge)))
                                         (new-triangle vi0 vi1 vi points))))))

(defun triangulate (points)
  (let* (;; sjl: let the input be something vanilla
         (ps (map 'vector (lambda (point)
                            (sb-cga:vec (car point) (cdr point) 0.0))
                  points))
         ;; Add the coords for a large bounding triangle to the point set
         (st-coords (get-bounding-triangle-points ps))
         (sti0 (length ps))
         (sti1 (1+ sti0))
         (sti2 (1+ sti1))
         (ps (concatenate 'vector ps st-coords))
         ;; Create the bounding triangle instance
         (supertri (new-triangle sti0 sti1 sti2 ps))
         ;; Initialize our triangle list
         (triangles (list supertri)))

    ;; For each point in the list we get an updated set
    ;; of triangles by retesselating using the new point
    (loop for i below (length ps)
          do (setf triangles (add-vertex i triangles ps)))

    ;; Remove any triangles that share points with the super triangle
    (mapcar (lambda (triangle)
              (slot-value triangle 'verts))
            (remove-if (lambda (triangle)
                         (has-shared-verts? supertri triangle))
                       triangles))))
