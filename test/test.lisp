(ql:quickload '(:flax :losh))

(defun check (interactive loom)
  (terpri)
  (losh:pr 'checking loom)
  (mapcar (lambda (output)
            (funcall loom nil "out" output 500 500)
            (losh:pr output 'OK))
          '(:png :plot :svg))
  (when interactive
    (break "Finished run of loom ~A" loom)))

(defun check-all (&key interactive)
  (check interactive #'flax.looms.001-triangles:loom)
  (check interactive #'flax.looms.002-wobbly-lines:loom)
  (check interactive #'flax.looms.003-basic-l-systems:loom)
  (check interactive #'flax.looms.004-turtle-curves:loom)
  (check interactive #'flax.looms.005-simple-triangulations:loom)
  (check interactive #'flax.looms.006-tracing-lines:loom)
  (check interactive #'flax.looms.007-stipple:loom))

(check-all)
