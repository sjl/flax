(in-package :flax.colors)

(declaim (inline color make-color))

(deftype color-float ()
  '(double-float 0.0d0 1.0d0))

(defstruct (color (:conc-name "")
                  (:constructor make-color (r g b)))
  (r 0.0d0 :type color-float)
  (g 0.0d0 :type color-float)
  (b 0.0d0 :type color-float))

(define-with-macro (color :conc-name "") r g b)

(defun rgb (r g b)
  (make-color (coerce r 'double-float)
              (coerce g 'double-float)
              (coerce b 'double-float)))

(defun-inline hsv-to-rgb (h s v)
  (declare (optimize speed)
           (type color-float h s v))
  ;; https://en.wikipedia.org/wiki/HSL_and_HSV#From_HSV
  ;; look i don't know either mate i just transcribed the fuckin thing
  (let* ((h (* h 360.0d0)) ; convert 0-1 to 0-360
         (h% (/ h 60.0d0))
         (c (* v s))
         (x (* c (- 1.0d0 (abs (1- (mod h% 2))))))
         (m (- v c)))
    (multiple-value-bind (r g b)
        (cond
          ((<= h% 1.0d0) (values c x 0.0d0))
          ((<= h% 2.0d0) (values x c 0.0d0))
          ((<= h% 3.0d0) (values 0.0d0 c x))
          ((<= h% 4.0d0) (values 0.0d0 x c))
          ((<= h% 5.0d0) (values x 0.0d0 c))
          ((<= h% 6.0d0) (values c 0.0d0 x))
          (t (values 0.0d0 0.0d0 0.0d0)))
      (values (+ r m)
              (+ g m)
              (+ b m)))))

(defun hsv (h s v)
  (multiple-value-call #'make-color
    (hsv-to-rgb (coerce h 'double-float)
                (coerce s 'double-float)
                (coerce v 'double-float))))


(defun blend! (destination color alpha)
  (declare (optimize speed)
           (type color destination color)
           (type color-float alpha))
  (with-color (destination dr dg db)
    (with-color (color r g b)
      (setf dr (lerp dr r alpha)
            dg (lerp dg g alpha)
            db (lerp db b alpha))))
  (values))

