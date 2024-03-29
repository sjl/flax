(in-package :flax.drawing)

;;;; Utils --------------------------------------------------------------------
(defun web-color (color)
  (with-color (color r g b)
    (format nil "#~2,'0X~2,'0X~2,'0X"
            (round (map-range 0 1 0 255 r))
            (round (map-range 0 1 0 255 g))
            (round (map-range 0 1 0 255 b)))))


;;;; Canvas -------------------------------------------------------------------
(defclass* svg-canvas (canvas)
  (scene))

(defmethod make-canvas ((type (eql :svg)) &key height width background padding)
  (let ((scene (svg:make-svg-toplevel 'svg:svg-1.1-toplevel
                                      :height height :width width)))
    (svg:draw scene (:rect :x 0 :y 0 :width width :height height
                     :fill (web-color background)))
    (make-instance 'svg-canvas
      :height height
      :width width
      :scene scene
      :padding padding)))


;;;; Rectangles ---------------------------------------------------------------
(defmethod draw ((canvas svg-canvas) (rect rectangle))
  (with-coordinates canvas
      ((ax ay (a rect))
       (bx by (b rect))
       (r (round-corners rect)))
    (svg:draw (scene canvas) (:rect
                              :x (min ax bx)
                              :y (min ay by)
                              :rx r
                              :ry r
                              :width (abs (- ax bx))
                              :height (abs (- ay by))
                              :fill (web-color (color rect))
                              :fill-opacity (opacity rect)))))


;;;; Circles ------------------------------------------------------------------
(defmethod draw ((canvas svg-canvas) (circ circle))
  (with-coordinates canvas
      ((x y (center circ))
       (r (radius circ)))
    (svg:draw (scene canvas) (:circle :cx x :cy y :r r
                              :fill (web-color (color circ))
                              :fill-opacity (opacity circ)))))


;;;; Points -------------------------------------------------------------------
(defmethod draw ((canvas svg-canvas) (p point))
  (with-coordinates canvas
      ((x y (location p)))
    (svg:draw (scene canvas) (:circle :cx x :cy y :r 2.0
                              :fill (web-color (color p))
                              :fill-opacity (opacity p)))))


;;;; Paths --------------------------------------------------------------------
(defun points-to-pairs (canvas points)
  (loop :for ps :in points :collect (coords-to-pairs canvas ps)))

(defun process-path-point (path point &optional first)
  (destructuring-bind (loc &optional ctrl1 ctrl2) point
    (cond
      (first (svg:with-path path
               (svg:move-to (car loc) (cdr loc))))
      (ctrl2 (svg:with-path path
               (svg:curve-to (car ctrl1) (cdr ctrl1)
                             (car ctrl2) (cdr ctrl2)
                             (car loc) (cdr loc))))
      (ctrl1 (svg:with-path path
               (svg:smooth-curve-to (car ctrl1) (cdr ctrl1)
                                    (car loc) (cdr loc))))
      (t (svg:with-path path
           (svg:line-to (car loc) (cdr loc)))))))

(defun make-svg-path-data (canvas points)
  (destructuring-bind (first-point &rest remaining-points)
      (points-to-pairs canvas points)
    (let ((p (svg:make-path)))
      (process-path-point p first-point t)
      (loop :for next-point :in remaining-points
            :do (process-path-point p next-point))
      p)))

(defmethod draw ((canvas svg-canvas) (path path))
  (svg:draw (scene canvas)
            (:path :d (make-svg-path-data canvas (points path))
             :fill "none"
             :stroke (web-color (color path))
             :stroke-width 1
             :stroke-opacity (opacity path))))


;;;; Triangles ----------------------------------------------------------------
(defmethod draw ((canvas svg-canvas) (tri triangle))
  (with-coordinates canvas
      ((ax ay (a tri))
       (bx by (b tri))
       (cx cy (c tri)))
    (svg:draw (scene canvas) (:polygon
                              :points (svg::points (list (list ax ay)
                                                         (list bx by)
                                                         (list cx cy)))
                              :fill "none"
                              :stroke-width 1
                              :stroke-opacity (opacity tri)
                              :stroke (web-color (color tri))))))


;;;; Rendering ----------------------------------------------------------------
(defmethod render-object ((canvas svg-canvas) object)
  (draw canvas object))


;;;; Files --------------------------------------------------------------------
(defmethod write-file ((canvas svg-canvas) filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (svg:stream-out stream (scene canvas))))

