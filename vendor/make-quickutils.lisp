(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :mappend
               :once-only
               :rcurry
               :symb
               :with-gensyms

               )
  :package "FLAX.QUICKUTILS")
