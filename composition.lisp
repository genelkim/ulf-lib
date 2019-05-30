;; ULF composition functions

(in-package :ulf-lib)

; NOTE TO SELF: Use (eval) to while doing variable assignments

; NOTE TO SELF: Make sure to ignore ULFs x s.t. (atom-semtype? x) = NIL
; TODO: figure out what to do with lambdas. (lambda x F): D => F (I think)

;; Compose two types, one of which is the operator and the other is an argument
;; if possible. Return NIL if not possible.
;; Exponent of arg must be a number (for now; although I don't know if we'll end
;; up with a situation where it isn't a number)
;; TODO: figure out a way to resolve (to go.v) and such (need to assign values
;; to variables)
(defun compose-types! (op arg)
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

