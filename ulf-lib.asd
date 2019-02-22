;; ULF Inferface and Manipulation Library.
;; Started ~2018-11-19

(asdf:defsystem :ulf-lib
  :depends-on (:ttt :util :cl-strings :cl-ppcre)
  :components ((:file "package")
               (:file "suffix")
               (:file "ttt-lexical-patterns")
               (:file "ttt-phrasal-patterns")
               (:file "macro")
               (:file "preprocess")
               ))

