;; ULF composition functions
;; UNDER CONSTRUCTION

(in-package :ulf-lib)

; Big limitation: Cases where two types can be matched by assigning values to
; exponent variables cannot be handled. An example is combining (2=>2) with
; (D^(- N 1)=>2), where N needs to be set to 1.

;; Decrement a given exponent field. If e is a number, simply decrement. If not,
;; return the list '(- e 1)
(defun decr-exp (e)
  (if (numberp e) (- e 1) (list '- e 1)))

;; Compose a given operator and argument if possible.
;; Assumption (for now): Arg has no exponent. If it does, it is ignored.
(defun apply-operator! (op arg)
  (if (optional-type-p op)
    ; Operator is an optional
    (let ((a (apply-operator! (car (types op)) arg))
          (b (apply-operator! (cadr (types op)) arg)))
      (if (and a b)
        (make-instance 'optional-type
                       :types (list a b))
        (or a b)))

    ; Operator is not optional
    (if (atomic-type-p op)
      ; Atomic operator. Must have exponent != 1. That is, the operator must be of
      ; the form A^n <=> A=>(A=>(A=>...))
      (when (and (semtype-equal? op arg :ignore-exp T) (not (equal (ex op) 1)))
        (let ((temp (copy-semtype op)))
          (progn
            (setf (ex temp) (decr-exp (ex op)))
            temp)))
  
      ; Operator is not atomic
      (if (semtype-equal? (domain op) arg :ignore-exp T)
        ; Exact match between arg and domain (ignoring the exponent)
        (if (equal (ex (domain op)) 1)
          (copy-semtype (range op))
          (let ((temp (copy-semtype op)))
            (progn
              (setf (ex (domain temp)) (decr-exp (ex (domain temp))))
              temp)))
        ; Not an exact match, but could still be possible using variable assignments
        ; Possible idea to handle cases where leftmost atom can be made to
        ; disappear with a 0 exponent: if that is possible, wrap it in an
        ; optional without the leftmost atom
        NIL)))) ; TODO

;; Compose two types if possible and return the composed type. Also return the
;; order in which the types were composed.
(defun compose-types! (x y)
  (if (or (not x) (not y))
    (or x y)
    (let ((comp (apply-operator! x y)))
      (if comp
        (values comp (list x y))
        (progn
          (setf comp (apply-operator! y x))
          (when comp (values comp (list y x))))))))

;; Given two atomic ULFs, return the type formed after composing (if possible)
(defun compose-atomic-ulfs! (a b)
  (if (or (not (atom-semtype? a)) (not (atom-semtype? b)))
    (or (atom-semtype? a) (atom-semtype? b))
    (let ((comp (apply-operator! (atom-semtype? a) (atom-semtype? b))))
      (if comp
        (values comp (list a b))
        (progn
          (setf comp (apply-operator! (atom-semtype? b) (atom-semtype? a)))
          (when comp (values comp (list b a))))))))

;; Given a ULF, evaluate and return the type if possible. Currently assumes
;; left associativity if there are more than 2 items scoped together. This is
;; not ideal, and needs to be changed.
;; Note: Unknown ulf atoms are ignored.
(defun ulf-type? (ulf &key lambda-vars)
  (if (symbolp ulf)
    ; atomic ULF
    (if (atom-semtype? ulf)
      (atom-semtype? ulf)
      (when (member ulf lambda-vars) (str2semtype "D")))

    ; non-atomic ULF
    (if (equal (car ulf) 'lambda)
      ; ULF is of the form (lambda var (expr))
      (when (= (length ulf) 3)
        (make-instance 'semtype
                       :domain (str2semtype "D")
                       :range (ulf-type? (third ulf) :lambda-vars (cons (cadr ulf) lambda-vars))))
      ; ULF is neither a lambda nor an atom
      (if (= (length ulf) 1)
        (ulf-type? (car ulf) :lambda-vars lambda-vars)
        (compose-types! (ulf-type? (reverse (cdr (reverse ulf))) :lambda-vars lambda-vars)
                        (ulf-type? (car (last ulf)) :lambda-vars lambda-vars))))))

