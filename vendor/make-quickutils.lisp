(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :alist-hash-table
               :compose
               :curry
               :ensure-list
               :mappend
               :once-only
               :rcurry
               :symb
               :with-gensyms

               )
  :package "FLAX.QUICKUTILS")
