;;; Unit tests for basic semtype functionality.

(in-package :ulf-lib/tests)

(defun semtype-match?-str (str1 str2)
  "Takes two string representations of semtypes and checks if they are equal
  with the semtype-match? function."
  (ulf-lib::semtype-match?
    (ulf-lib::extended-str2semtype str1)
    (ulf-lib::extended-str2semtype str2)))

(defun semtype-equal?-str (str1 str2)
  (ulf-lib::semtype-equal?
    (ulf-lib::extended-str2semtype str1)
    (ulf-lib::extended-str2semtype str2)))

(define-test basic-semtype-match?
  "Testing the basics of semtype-match? function."
  (:tag :semtype)
  (assert-equality #'semtype-match?-str "D" "D")
  (assert-equality #'semtype-match?-str "(D=>D)" "(D=>D)")
  (assert-equality #'semtype-match?-str "(D=>D)_V" "(D=>D)_V")
  (assert-equality #'semtype-match?-str "(D=>D)" "(D=>D)_V")

  ;; Non-symmetric wrt. syntactic features.
  (assert-equality #'semtype-match?-str "(D=>D)%T" "(D=>D)%T")
  (assert-equality #'semtype-match?-str "(D=>D)" "(D=>D)%T,X")
  (assert-true (not (semtype-match?-str "(D=>D)%T,X" "(D=>D)"))))

(define-test exponent-semtype-match?
  "Testing semtype-match? with exponents."
  (:tag :semtype)
  (assert-equality #'semtype-match?-str "D^2" "(D=>D)")
  (assert-equality #'semtype-match?-str "(D=>D)" "D^2")
  (assert-equality #'semtype-match?-str "(D=>D)^2" "((D=>D)=>(D=>D))")
  (assert-equality #'semtype-match?-str "((D=>D)=>(D=>D))" "(D=>D)^2")

  (assert-equality #'semtype-match?-str "(D=>(D=>2))" "(D^2=>2)")
  (assert-equality #'semtype-match?-str "(D=>(D^2=>2))" "(D^3=>2)")

  (assert-true (not (semtype-match?-str "(D=>D)^2" "(D^2=>D^2)")))
  (assert-equality #'semtype-match?-str "(D=>D)^2" "((D=>D)=>(D=>D))")
  (assert-equality #'semtype-match?-str "(D=>D)^2" "((D=>D)=>D^2)")
  (assert-equality #'semtype-match?-str "(D=>(D=>(D=>D)))" "(D^2=>D^2)"))

(define-test optional-semtype-match?
  "Testing semtype-match? with options."
  (:tag :semtype)
  
  ;; TODO: add more negative examples

  ;; Optionals are symmetric.
  (assert-equality #'semtype-match?-str "{D|2}" "D")
  (assert-equality #'semtype-match?-str "D" "{D|2}")

  ;; Optionals with exponents.
  (assert-equality #'semtype-match?-str "{D|2}^2" "(D=>2)")
  (assert-equality #'semtype-match?-str "{D|2}^2" "(D=>D)")
  (assert-equality #'semtype-match?-str "{D|2}^2" "(2=>D)")
  (assert-equality #'semtype-match?-str "{D|2}^2" "(2=>2)")
  (assert-equality #'semtype-match?-str "{D|2}^2" "({D|2}=>{D|2})")
  (assert-equality #'semtype-match?-str "{D|2}^2" "{D^2|{2^2|{(D=>2)|(2=>D)}}}")

  ;; Optionals in argument exponents.
  (assert-equality #'semtype-match?-str "({D|2}^3=>S)" "(D=>(2=>(D=>S)))")
  (assert-equality #'semtype-match?-str "({D|2}^n=>S)" "(D=>(2=>(D=>S)))")
  (assert-equality #'semtype-match?-str "({D|2}^n=>S)" "(D=>S)")
  (assert-equality #'semtype-match?-str "({D|2}^n=>S)" "S")
  (let ((ulf::*semtype-max-exponent* 4))
    (assert-equality #'semtype-match?-str "({D|2}^n=>S)" "(D=>(D=>(D=>(D=>S))))")))


;; TODO: more extensive semtype-equal? tests.
(define-test basic-semtype-equal?
  "Testing the basics of the semtype-equal? function."
  (:tag :semtype)
  (assert-equality #'semtype-equal?-str "D" "D")
  (assert-equality #'semtype-equal?-str "(D=>D)" "(D=>D)")
  (assert-equality #'semtype-equal?-str "(D=>D)_V" "(D=>D)_V")
  (assert-true (not (semtype-equal?-str "(D=>D)" "(D=>D)_V")))

  ;; Non-symmetric wrt. syntactic features.
  (assert-equality #'semtype-equal?-str "(D=>D)%T" "(D=>D)%T")
  (assert-true (not (semtype-equal?-str "(D=>D)" "(D=>D)%T,X")))
  (assert-true (not (semtype-equal?-str "(D=>D)%T,X" "(D=>D)"))))

