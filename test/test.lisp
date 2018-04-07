(ql:quickload '(:flax :losh))

(defun check (loom)
  (terpri)
  (losh:pr 'checking loom)
  (mapcar (lambda (output)
            (funcall loom nil "out" output 500 500)
            (losh:pr output 'OK))
          '(:png :svg :plot)))

(progn
  (check #'flax.looms.001-triangles:loom)
  (check #'flax.looms.002-wobbly-lines:loom)
  (check #'flax.looms.003-basic-l-systems:loom)
  (check #'flax.looms.004-turtle-curves:loom)
  (check #'flax.looms.005-simple-triangulations:loom)
  (check #'flax.looms.006-tracing-lines:loom)
  (check #'flax.looms.007-stipple:loom))
