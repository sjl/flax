(in-package :flax.drawing)

;;;; Utils --------------------------------------------------------------------
(defun web-color (color)
  (with-color (color r g b)
    (format nil "#~2,'0X~2,'0X~2,'0X"
            (round (map-range 0 1 0 255 r))
            (round (map-range 0 1 0 255 g))
            (round (map-range 0 1 0 255 b)))))


;;;; Canvas -------------------------------------------------------------------
(defclass* (svg-canvas :conc-name "") (canvas)
  (scene))

(defmethod make-canvas ((type (eql :svg)) &key height width background)
  (let ((scene (svg:make-svg-toplevel 'svg:svg-1.1-toplevel
                                      :height height :width width)))
    (svg:draw scene (:rect :x 0 :y 0 :width width :height height
                     :fill (web-color background)))
    (make-instance 'svg-canvas
      :height height
      :width width
      :scene scene)))


;;;; Rectangles ---------------------------------------------------------------
(defmethod draw ((canvas svg-canvas) (rect rectangle))
  (with-coordinates canvas
      ((ax ay (a rect))
       (bx by (b rect)))
    (let ((rounding (compute-corner-rounding canvas rect)))
      (svg:draw (scene canvas) (:rect
                                :x (min ax bx)
                                :y (min ay by)
                                :rx rounding
                                :ry rounding
                                :width (abs (- ax bx))
                                :height (abs (- ay by))
                                :fill (web-color (color rect))
                                :fill-opacity (opacity rect))))))


;;;; Paths --------------------------------------------------------------------
(defun make-svg-path-data (canvas points)
  (destructuring-bind (first-point &rest remaining-points)
      (mapcar (curry #'coord-to-pair canvas) points)
    (let ((p (svg:make-path)))
      (svg:with-path p
        (svg:move-to (car first-point) (cdr first-point)))
      (dolist (point remaining-points)
        (svg:with-path p
          (svg:line-to (car point) (cdr point))))
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
                              :stroke-width 0.25
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

