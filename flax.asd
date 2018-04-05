(asdf:defsystem :flax
  :description "Weaving art from seeds."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (

               :sb-cga ; for lofi-tri only
               :cl-pcg
               :cl-svg
               :cl-vectors
               :chancery
               :iterate
               :losh
               :zpng
               :3d-vectors
               :3d-matrices

               )

  :serial t
  :components
  ((:module "vendor" :serial t
    :components ((:file "quickutils-package")
                 (:file "quickutils")
                 (:module "lofi-tri"
                  :components ((:file "lofi.tri")))))
   (:file "package")
   (:module "src" :serial t
    :components
    ((:file "base")
     (:file "coordinates")
     (:file "colors")
     (:module "drawing" :serial t
      :components ((:file "api")
                   (:file "png")
                   (:file "svg")
                   (:file "plot")))
     (:module "looms" :serial nil
      :components
      ((:file "001-triangles")
       (:file "002-wobbly-lines")
       (:file "003-basic-l-systems")
       (:file "004-turtle-curves")
       (:file "005-simple-triangulations")
       (:file "006-tracing-lines")
       (:file "007-stippling")))))))

