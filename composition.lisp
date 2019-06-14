;; ULF composition functions
;; UNDER CONSTRUCTION

(in-package :ulf-lib)

;; TODO: Deal with lambdas

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
(defun ulf-type? (ulf)
  (if (symbolp ulf)
    (atom-semtype? ulf)
    (if (= (length ulf) 1)
      (ulf-type? (car ulf))
      (compose-types! (ulf-type? (reverse (cdr (reverse ulf)))) (ulf-type? (car (last ulf)))))))
;      (compose-types! (ulf-type? (car ulf)) (ulf-type? (cdr ulf))))))

