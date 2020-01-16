;;; Gene Louis Kim, 1-14-2020
;;; Unit tests for verifying the ulf-composition functions.

(in-package :ulf-lib)

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(setq *summarize-results* t)

(defun string-from-compose-types (ulf1 ulf2 &key (extended? nil))
  "Takes two atomic ULF expressions, composes the types and returns the string
  representation of the type."
  (let ((type1 (ulf-type-string? ulf1))
        (type2 (ulf-type-string? ulf2))
        ;(compose-fn (if extended? #'extended-compose-types! #'compose-types!))
        (compose-string-fn (if extended? #'extended-compose-type-string! #'compose-type-string!)))
    (funcall compose-string-fn type1 type2 
             ;:compose-fn compose-fn
             )))

(define-test basic-compose
  "Basic examples for ULF composition"
  (:tag :compose-basic)
  (assert-equal "D" (string-from-compose-types 'the.d 'man.n))
  (assert-equal nil (string-from-compose-types 'the.d 'the.d))
  (assert-equal "{(S=>2)|{(D=>(S=>2))_V|{({D|(D=>(S=>2))}=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V|({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V}}}}}" (string-from-compose-types 'help.v 'me.pro))
  (assert-equal nil (string-from-compose-types 'be.v 'me.pro))
  (assert-equal "(D=>(S=>2))_V" (string-from-compose-types 'be.v 'happy.a)))
    
(define-test basic-macro-compose
  "Basic examples for macro ULF composition"
  (:tag :compose-basic)
  (assert-equal "{(D=>(S=>2))_V_T|{{(D=>(D=>(S=>2)))_V_T|((D=>(S=>2))=>(D=>(S=>2)))_V_T}|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V_T|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V_T|{({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V_T|({D|(D=>(S=>2))}^5=>(D=>(S=>2)))_V_T}}}}}" (string-from-compose-types 'pres 'run.v :extended? t))
  (assert-equal nil (string-from-compose-types 'past 'happy.a :extended? t))
  (assert-equal nil (string-from-compose-types 'cf 'man.n :extended? t))
  (assert-equal "{+PREDS[N+[(D=>(S=>2))_N]]|(D=>(S=>2))_N}" (string-from-compose-types 'n+preds 'man.n :extended? t))
  (assert-equal "{+PREDS[NP+[D]]|D}" (string-from-compose-types 'np+preds 'he.pro :extended? t)))
    
