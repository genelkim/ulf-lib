;; ULF Inferface and Manipulation Library.
(asdf:defsystem :ulf-lib
  :name "Episodic Logic-Unscoped Logical Form (ULF) Interface and Manipulation Library"
  :serial t
  :version "1.0.0"
  :description "A library for basic interfacing and manipulations of Episodic logic, unscoped logical formulas (ULFs)."
  :author "Gene Louis Kim <gkim21@cs.rochester.edu>"
  :license "MIT"
  :depends-on (:ttt :cl-strings :cl-ppcre :gute)
  :components ((:file "package")
               (:file "util")
               (:file "suffix")
               (:file "syntactic-features/feature-definition-declarations")
               (:file "syntactic-features/syntactic-features")
               (:file "syntactic-features/reader")
               (:file "semtype")
               (:file "syntactic-features/feature-definition-definitions")
               (:file "ttt-lexical-patterns")
               (:file "ttt-phrasal-patterns")
               (:file "gen-phrasal-patterns")
               (:file "search")
               (:file "macro")
               (:file "preprocess")
               (:file "composition")
               (:file "lang-util"))
  :around-compile (lambda (next)
                    ; For debugging/development.
                    ;(proclaim '(optimize (debug 3) (safety 3) (space 1) (speed 1)))
                    ; For production.
                    (proclaim '(optimize (debug 0) (safety 1) (space 1) (speed 3)))
                    (funcall next))
  :in-order-to ((test-op (test-op :ulf-lib/tests))))

(asdf:defsystem :ulf-lib/tests
  :serial t
  :description "Tests for the ULF-LIB library"
  :author "Gene Louis Kim <gkim21@cs.rochester.edu>"
  :license "MIT"
  :depends-on (:ulf-lib :lisp-unit :gute)
  :components ((:file "test/package")
               (:file "test/composition")
               (:file "test/semtype")
               (:file "test/syntactic-features")
               (:file "test/ttt-phrasal-patterns"))
  :perform (test-op (o c) (symbol-call :ulf-lib/tests :run)))

