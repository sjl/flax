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


;;;; Text ---------------------------------------------------------------------
(defmethod draw ((canvas svg-canvas) (text text))
  (with-coordinates canvas
      ((x y (pos text))
       (size (size text)))
    (svg:text (scene canvas)
              (:x x :y y
               :font-size size
               :font-family (font text)
               :text-anchor (string-downcase (align text)) ; dammit inkscape
               :fill (web-color (color text))
               :fill-opacity (opacity text))
              (content text))))


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

