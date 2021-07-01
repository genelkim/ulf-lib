;;; Unit tests for basic semtype functionality.

(in-package :ulf-lib/tests)

(defun semtype-equal?-str (str1 str2)
  "Takes two string representations of semtypes and checks if they are equal
  with the semtype-match? function."
  (ulf-lib::semtype-equal?
    (ulf-lib::extended-str2semtype str1)
    (ulf-lib::extended-str2semtype str2)))

(define-test basic-semtype-equal?
  "Testing the basics of semtype-equal? function."
  (:tag :semtype)
  ;; TODO: this name needs to be changed to semtype-match?
  
  (assert-equality #'semtype-equal?-str "D" "D")
  (assert-equality #'semtype-equal?-str "(D=>D)" "(D=>D)")
  (assert-equality #'semtype-equal?-str "(D=>D)_V" "(D=>D)_V")
  (assert-equality #'semtype-equal?-str "(D=>D)" "(D=>D)_V")

  ;; Non-symmetric wrt. syntactic features.
  (assert-equality #'semtype-equal?-str "(D=>D)%T" "(D=>D)%T")
  (assert-equality #'semtype-equal?-str "(D=>D)" "(D=>D)%T,X")
  (assert-true (not (semtype-equal?-str "(D=>D)%T,X" "(D=>D)"))))

(define-test exponent-semtype-equal?
  "Testing semtype-equal? with exponents."
  (:tag :semtype)
  (assert-equality #'semtype-equal?-str "D^2" "(D=>D)")
  (assert-equality #'semtype-equal?-str "(D=>D)" "D^2")
  (assert-equality #'semtype-equal?-str "(D=>D)^2" "((D=>D)=>(D=>D))")
  (assert-equality #'semtype-equal?-str "((D=>D)=>(D=>D))" "(D=>D)^2")

  (assert-equality #'semtype-equal?-str "(D=>(D=>2))" "(D^2=>2)")
  (assert-equality #'semtype-equal?-str "(D=>(D^2=>2))" "(D^3=>2)")

  (assert-true (not (semtype-equal?-str "(D=>D)^2" "(D^2=>D^2)")))
  (assert-equality #'semtype-equal?-str "(D=>D)^2" "((D=>D)=>(D=>D))")
  (assert-equality #'semtype-equal?-str "(D=>D)^2" "((D=>D)=>D^2)")
  (assert-equality #'semtype-equal?-str "(D=>(D=>(D=>D)))" "(D^2=>D^2)"))

(define-test optional-semtype-equal?
  "Testing semtype-equal? with options."
  (:tag :semtype)
  
  ;; TODO: add more negative examples

  ;; Optionals are symmetric.
  (assert-equality #'semtype-equal?-str "{D|2}" "D")
  (assert-equality #'semtype-equal?-str "D" "{D|2}")

  ;; Optionals with exponents.
  (assert-equality #'semtype-equal?-str "{D|2}^2" "(D=>2)")
  (assert-equality #'semtype-equal?-str "{D|2}^2" "(D=>D)")
  (assert-equality #'semtype-equal?-str "{D|2}^2" "(2=>D)")
  (assert-equality #'semtype-equal?-str "{D|2}^2" "(2=>2)")
  (assert-equality #'semtype-equal?-str "{D|2}^2" "({D|2}=>{D|2})")
  (assert-equality #'semtype-equal?-str "{D|2}^2" "{D^2|{2^2|{(D=>2)|(2=>D)}}}")

  ;; Optionals in argument exponents.
  (assert-equality #'semtype-equal?-str "({D|2}^3=>S)" "(D=>(2=(D=>S)))")
  (assert-equality #'semtype-equal?-str "({D|2}^n=>S)" "(D=>(2=(D=>S)))")
  (assert-equality #'semtype-equal?-str "({D|2}^n=>S)" "(D=>S)")
  (assert-equality #'semtype-equal?-str "({D|2}^n=>S)" "S")
  (assert-equality #'semtype-equal?-str "({D|2}^n=>S)" "(D=>(D=>(D=>(D=>S))))"))

