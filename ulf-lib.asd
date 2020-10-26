;; ULF Inferface and Manipulation Library.
;; Started ~2018-11-19

(asdf:defsystem :ulf-lib
  :depends-on (:ttt :cl-strings :cl-ppcre :cl-util :lisp-unit)
  :components ((:file "package")
               (:file "util")
               (:file "suffix")
               (:file "semtype")
               (:file "ttt-lexical-patterns")
               (:file "ttt-phrasal-patterns")
               (:file "gen-phrasal-patterns")
               (:file "search")
               (:file "macro")
               (:file "preprocess")
               (:file "composition"))
  :around-compile (lambda (next)
                    ; For debugging/development.
                    (proclaim '(optimize (debug 3) (safety 3) (space 1) (speed 1)))
                    ; For production.
                    ;(proclaim '(optimize (debug 0) (safety 1) (space 1) (speed 3)))
                    (funcall next)))

