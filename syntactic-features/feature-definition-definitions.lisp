;;; The class with all of the syntactic feature definitions as well as
;;; top-level feature info.

(in-package :ulf-lib)

;;;
;;; Combinator definitions.
;;; See feature-definition-declarations.lisp for default and usage.
;;;
(defparameter *predicate-semtype* (str2semtype "({D|(D=>(S=>2))}^n=>(D=>(S=>2)))"))

(defun auxiliary-combinator-fn (base opr arg csq
                                &optional opr-semtype arg-semtype)
  "Feature combinator function for auxiliaries.
  If the operator consequent is not some form of predicate, return nil.
  Otherwise, use the base feature."
  (declare (ignore opr arg csq arg-semtype))
  (if (not (semtype-equal? *predicate-semtype* (range opr-semtype)))
    nil
    base))


;;;
;;; Feature Definitions.
;;;

;; The possible-values must be unique through all definitions since we use
;; those values directly for a compact representation of the syntactic feature
;; set.
;;
;; The convention for binary features (+/-) is to indicate the negation with a
;; ! prefix. Then nil is used for unspecified feature values.
;; TODO: fill in combinator-fns.
(setf *feature-defintions*
  (list
    ;; TENSE
    (make-instance
      'feature-defintion
      :name "TENSE"
      :possible-values '(t !t)
      :default-value '!t)
    ;; AUXILIARY
    (make-instance
      'feature-defintion
      :name "AUXILIARY"
      :combinator-fn #'auxiliary-combinator-fn
      :possible-values '(x !x)
      :default-value '!x)
    ;; PLURALITY
    (make-instance
      'feature-defintion
      :name "PLURALITY"
      :possible-values '(pl !pl)
      :default-value '!pl)
    ;; PERFECT
    (make-instance
      'feature-defintion
      :name "PERFECT"
      :possible-values '(pf !pf)
      :default-value '!pf)
    ;; PASSIVE
    (make-instance
      'feature-defintion
      :name "PASSIVE"
      :possible-values '(pv !pv)
      :default-value '!pv)
    ;; PROGRESSIVE
    (make-instance
      'feature-defintion
      :name "PROGRESSIVE"
      :possible-values '(pg !pg)
      :default-value '!pg)))
;; TODO: p-arg, lexical, *h, qt-attr, 's, etc.
;;       maybe macro features should be different?

;; Association list from syntactic feature values to the feature names.
(setf *syntactic-feature-values*
  ;; GK: For some reason Lisp gets mad when I try to access possible-values
  ;; directly in the loop macro, so I nested another level.
  (loop for elem-def in *feature-defintions*
        collect (loop for feat in (possible-values elem-def)
                      collect (cons feat (name elem-def)))
        into sublists
        finally (return (apply #'append sublists))))

