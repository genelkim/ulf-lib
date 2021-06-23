(defpackage #:ulf-lib/tests
  (:use #:cl #:lisp-unit #:ulf-lib #:gute)
  (:export #:run))

(in-package :ulf-lib/tests)

(defun run (&key tests tags
                 ;; lisp-unit verbosity parameters.
                 (print-failures t)
                 (print-errors t)
                 (print-summary t)
                 (summarize-results t))
  "Run all tests.
  
  Optional arguments:
    tests:  list of test names, defaults to running all tests
    tags:   list of test tags

  `tests` and `tags` should not both be set. The `tags` argument will be
  ignored if that is the case.
  "
  (let ((*print-failures* print-failures)
        (*print-errors* print-errors)
        (*print-summary* print-summary)
        (*summarize-results* summarize-results))
    ;; Run tests.
    (cond
      ;; Specified tests.
      (tests
        (when tags
          (warn (concatenate 'string
                             "Both the :tags and :tests keyword "
                             "arguments are given for ulf-lib/tests:run. "
                             "Ignoring the :tags argument...")))
        ; The gute dependency is available via the ulf-lib package.
        (in-intern (tests pkgtests :ulf-lib/tests)
          (lisp-unit:run-tests pkgtests :ulf-lib/tests)))
      ;; Specified tags.
      (tags (run-tags tags :ulf-lib/tests))
      ;; Default, all tests.
      (t (run-tests :all :ulf-lib/tests)))))

