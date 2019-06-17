;; ULF composition functions

(in-package :ulf-lib)

;; Compose a given operator and argument if possible.
;; Assumption (for now): Arg has no exponent. If it does, it is ignored.
(defun apply-operator! (op arg)
  (if (optional-type-p op)
    ; Operator is an optional type
    (let ((a (apply-operator! (car (types op)) arg))
          (b (apply-operator! (cadr (types op)) arg)))
      (if (and a b)
        (new-semtype NIL NIL 1 NIL NIL :options (list a b))
        (or a b)))

    ; Operator is not optional
    (if (atomic-type-p op)
      ; Atomic operator of the form A^n with n > 1
      (when (and (semtype-equal? op arg :ignore-exp T) (> (ex op) 1))
        (new-semtype (domain op) NIL (- (ex op) 1) (subscript op) (tense op)))

      ; Operator is not atomic
      (when (semtype-equal? (domain op) arg :ignore-exp T)
        (if (= (ex (domain op)) 1)
          (copy-semtype (range op))
          (let ((temp (copy-semtype op)))
            (progn
              (setf (ex (domain temp)) (- (ex (domain temp)) 1))
              temp)))))))

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
;        (make-instance 'semtype
;                       :domain (str2semtype "D")
;                       :range (ulf-type? (third ulf) :lambda-vars (cons (cadr ulf) lambda-vars))))
        (new-semtype (str2semtype "D") (ulf-type? (third ulf) :lambda-vars (cons (cadr ulf) lambda-vars)) 1 NIL NIL))
      ; ULF is neither a lambda nor an atom
      (if (= (length ulf) 1)
        (ulf-type? (car ulf) :lambda-vars lambda-vars)
        (compose-types! (ulf-type? (reverse (cdr (reverse ulf))) :lambda-vars lambda-vars)
                        (ulf-type? (car (last ulf)) :lambda-vars lambda-vars))))))

