;; ULF composition functions
;; UNDER CONSTRUCTION

(in-package :ulf-lib)

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
        NIL)))) ; TODO

