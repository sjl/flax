(in-package :flax.drawing)

(defgeneric letter-paths (character))

(defmethod letter-paths ((character (eql #\Space)))
  (list))

(defmethod letter-paths ((character (eql #\+)))
  ;;           p₁
  ;;           |
  ;;           |
  ;;    p₃ ----+---- p₄
  ;;           |
  ;;           |
  ;;           p₂
  (let ((p1 (vec 0.25 0.35))
        (p2 (vec 0.25 0.75))
        (p3 (vec 0.05 0.55))
        (p4 (vec 0.45 0.55)))
    (list (path (list p1 p2))
          (path (list p3 p4)))))

(defmethod letter-paths ((character (eql #\-)))
  (let ((p1 (vec 0.05 0.55))
        (p2 (vec 0.45 0.55)))
    (list (path (list p1 p2)))))

(defmethod letter-paths ((character (eql #\L)))
  ;;     p₁
  ;;      |
  ;;      |
  ;;      |
  ;;      |
  ;;    p₂|______ p₃
  (let ((p1 (vec 0.05 0.10))
        (p2 (vec 0.05 1.00))
        (p3 (vec 0.45 1.00)))
    (list (path (list p1 p2 p3)))))

(defmethod letter-paths ((character (eql #\R)))
  ;;     p₁___ p₃
  ;;      |   \
  ;;    p₆|___/ p₄
  ;;      |   \
  ;;      |    \
  ;;    p₂|     \ p₅
  (let ((p1 (vec 0.05 0.10))
        (p2 (vec 0.05 1.00))
        (p3 (vec 0.25 0.10))
        (p4 (vec 0.25 0.55))
        (p5 (vec 0.45 1.00))
        (p6 (vec 0.05 0.55)))
    (list (path (list p1 p2))
          (path (list p1 p3
                      (list p4
                            (vec 0.45 0.10)
                            (vec 0.45 0.55))
                      p5))
          (path (list p4 p6)))))

(defmethod letter-paths ((character (eql #\→)))
  (let ((p1 (vec 0.05 0.55))
        (p2 (vec 0.45 0.55))
        (p3 (vec 0.30 0.45))
        (p4 (vec 0.30 0.65)))
    (list (path (list p1 p2))
          (path (list p3 p2 p4)))))

(defmethod letter-paths ((character (eql #\()))
  (let ((p1 (vec 0.40 0.10))
        (p2 (vec 0.40 1.00)))
    (list (path (list p1
                      (list p2
                            (vec 0.05 0.25)
                            (vec 0.05 0.85)))))))

(defmethod letter-paths ((character (eql #\))))
  (let ((p1 (vec 0.10 0.10))
        (p2 (vec 0.10 1.00)))
    (list (path (list p1
                      (list p2
                            (vec 0.45 0.25)
                            (vec 0.45 0.85)))))))


(defgeneric kern (a b))

(defmethod kern ((a character) (b character))
  0.0)

(defmethod kern ((a null) b)
  0.0)

(defmethod kern ((a (eql #\L)) (b (eql #\+))) -0.15)
(defmethod kern ((a (eql #\L)) (b (eql #\-))) -0.15)
(defmethod kern ((a (eql #\L)) (b (eql #\→))) -0.15)
(defmethod kern ((a (eql #\L)) (b (eql #\())) -0.07)
(defmethod kern ((a (eql #\R)) (b (eql #\→))) -0.05)
(defmethod kern ((a (eql #\R)) (b (eql #\L)))  0.05)
(defmethod kern ((a (eql #\→)) (b (eql #\L)))  0.05)
(defmethod kern ((a (eql #\→)) (b (eql #\R)))  0.05)
(defmethod kern ((a (eql #\()) (b (eql #\-))) -0.05)
(defmethod kern ((a (eql #\()) (b (eql #\+))) -0.05)
(defmethod kern ((a (eql #\-)) (b (eql #\)))) -0.05)
(defmethod kern ((a (eql #\+)) (b (eql #\)))) -0.05)

