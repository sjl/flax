(in-package :flax.drawing)

;;; A plot is an SVG without the square background.
;;; TODO: shell out to svgsort automatically?


(defclass* plot-canvas (svg-canvas) ())

(defmethod make-canvas ((type (eql :plot)) &key height width)
  (let ((scene (svg:make-svg-toplevel 'svg:svg-1.1-toplevel
                                      :height height :width width)))
    (make-instance 'plot-canvas
      :height height
      :width width
      :scene scene)))

(defmethod file-extension ((type (eql :plot)))
  "svg")


(defmethod draw ((canvas plot-canvas) (p point))
  (svg:draw (scene canvas)
            (:path :d (make-svg-path-data canvas (list (location p)
                                                       (location p)))
             :stroke-linecap "round"
             :fill "none"
             :stroke (web-color (color p))
             :stroke-width 1
             :stroke-opacity (opacity p))))

