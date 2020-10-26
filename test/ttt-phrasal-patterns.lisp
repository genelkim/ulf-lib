;;; Gene Louis Kim, 3-17-2020
;;; Unit tests for verifying the ttt-phrasal-patterns functions.

(in-package :ulf-lib/tests)

(define-test ttt-p-arg-mod
  "Tests for adding mod-a modifiers to p-arg."
  (:tag :p-arg-mod)
  (assert-equal '(SENT-MOD)
                (phrasal-ulf-type?
                  '(right.mod-a (before.ps (i.pro ((past move.v) it.pro))))))
  (assert-equal '(UNKNOWN)
                (phrasal-ulf-type?
                  '(right.mod-n (before.ps (i.pro ((past move.v) it.pro)))))))

