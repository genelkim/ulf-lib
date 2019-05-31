;; ULF composition functions

(in-package :ulf-lib)

; NOTE TO SELF: Use (eval) to while doing variable assignments

; NOTE TO SELF: Make sure to ignore ULFs x s.t. (atom-semtype? x) = NIL
; TODO: figure out what to do with lambdas. (lambda x F): D => F (I think)

;; Apply an operator (semtype) to a semtype. Return NIL if not possible.
;; Exponent of arg must be a number (for now; although I don't know if we'll
;; ever end up in a situation where it isn't)
;; TODO: figure out a way to resolve things like (to go.v) where values need to
;; be assigned to variables in exponents.
(defun apply-operator! (op arg)
  (when (and (not (atomic-type-p op))
             (semtype-equal? (domain op) arg :ignore-exp T)
             (or (not (numberp (ex (domain op)))) (>= (ex (domain op)) (ex arg))))
    (if (not (numberp (ex (domain op))))
      ; exponent of domain of op is NAN
      (let ((new-domain (copy-semtype (domain op))))
        (setf (ex new-domain) (list '- (ex (domain op)) (ex arg)))
        (make-instance 'semtype
                       :domain new-domain
                       :range (copy-semtype (range op))
                       ;:ex ?? Operators won't end up with exponents right?
                       :subscript (subscript op)
                       :tense (tense op)))

      ; exponent of domain of op is a number
      (if (= (ex (domain op)) 1)
        (copy-semtype (range op)) ; if exponent of domain is 1, the result is just the range
        (let ((new-domain (copy-semtype (domain op))))
          (setf (ex new-domain) (- (ex (domain op)) (ex arg)))
          (make-instance 'semtype
                         :domain new-domain
                         :range (copy-semtype (range op))
                         :subscript (subscript op)
                         :tense (tense op)))))))

;; Compose two types if possible and return the composed type. Also return the
;; order in which the types were composed.
(defun compose-types! (x y)
  (let ((comp (apply-operator! x y)))
    (if comp
      (values comp (list x y))
      (values (apply-operator! y x) (list y x)))))

;; Compose two atomic ULFs if possible and return the composed type. Also return
;; the order in which the ULFs were composed. If there is more than one way in
;; which the ULFs can be composed (for example when there are alternative types
;; for a ULF) a list with all possibilities is returned along with a list of the
;; corresponding orders in which the ULFs were composed.
(defun compose-atomic-ulfs! (a b)
  (let ((x (atom-semtype? a)) (y (atom-semtype? b)))
    (if (not (and x y))
      (values (or x y) (list a b)) ; if either x and y are NIL, return the other one
      (progn ; if both x and y are not NIL
        (unless (listp x) (setf x (list x)))
        (unless (listp y) (setf y (list y)))
        (let ((temp
          (apply #'mapcar
            (cons #'list
              (remove-if-not (lambda (l) (car l))
                (loop for t1 in x
                  nconc (loop for t2 in y
                          collect (list (compose-types! t1 t2)
                                    (substitute-if b (lambda (s) (not (symbolp s)))
                                      (substitute-if a (lambda (s) (semtype-equal? s t1))
                                        (nth-value 1 (compose-types! t1 t2))))))))))))
          (if (cdr (car temp))
            (values (car temp) (cadr temp))
            (values (car (car temp)) (car (cadr temp)))))))))

