(asdf:defsystem :flax
  :description "Weaving art from seeds."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (:cl-pcg
               :cl-vectors
               :iterate
               :losh
               :zpng)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components
                ((:file "base")
                 (:file "coordinates")
                 (:file "colors")
                 (:file "drawing")
                 (:module "looms" :serial nil
                  :components
                  ((:file "001-triangles")
                   (:file "002-wobbly-lines")
                   (:file "003-basic-l-systems")
                   (:file "004-turtle-curves")))))))

