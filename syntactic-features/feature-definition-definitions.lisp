;;; The class with all of the syntactic feature definitions as well as
;;; top-level feature info.

(in-package :ulf-lib)

;;;
;;; Combinator definitions.
;;; See feature-definition-declarations.lisp for default and usage.
;;;
(defparameter *predicate-semtype* (str2semtype "({D|(D=>(S=>2))}^n=>(D=>(S=>2)))"))
(defparameter *sentence-semtype* (str2semtype "(S=>2)"))

(defun base-result-pattern-combinator-generator (result-pattern)
  "A function that generates combinator functions that return base if the
  resulting semtype of the combination matches the given pattern (defined
  as a semtype itself). Otherwise, returns the nil."
  (lambda (base opr arg csq &optional opr-semtype arg-semtype)
    (declare (ignore opr arg csq))
    (when base ; don't bother if base is nil. 
      (let ((res-semtype (compose-types! opr-semtype arg-semtype
                                         ; including synfeats would lead to an
                                         ; infinite recursion.
                                         :ignore-synfeats t)))
        (if (semtype-match? result-pattern res-semtype)
          base
          nil)))))

;; Simple combinator functions. 
(setf (fdefinition 'tense-combinator-fn)
      (base-result-pattern-combinator-generator
        (new-semtype nil nil 1 nil :options (list *sentence-semtype*
                                                  *predicate-semtype*))))
(setf (fdefinition 'auxiliary-combinator-fn)
      (base-result-pattern-combinator-generator *predicate-semtype*))
(setf (fdefinition 'plurality-combinator-fn)
      (base-result-pattern-combinator-generator *predicate-semtype*))
(setf (fdefinition 'perfect-combinator-fn)
      (base-result-pattern-combinator-generator *predicate-semtype*))
(setf (fdefinition 'passive-combinator-fn)
      (base-result-pattern-combinator-generator *predicate-semtype*))
(setf (fdefinition 'progressive-combinator-fn)
      (base-result-pattern-combinator-generator *predicate-semtype*))


;;;
;;; Feature Definitions.
;;;

;; The possible-values must be unique through all definitions since we use
;; those values directly for a compact representation of the syntactic feature
;; set.
;;
;; The convention for binary features (+/-) is to indicate the negation with a
;; ! prefix. Then nil is used for unspecified feature values.
(setf *feature-defintions*
  (list
    ;; TENSE
    (make-instance
      'feature-defintion
      :name "TENSE"
      :combinator-fn #'tense-combinator-fn
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
      :combinator-fn #'plurality-combinator-fn
      :possible-values '(pl !pl)
      :default-value '!pl)
    ;; PERFECT
    (make-instance
      'feature-defintion
      :name "PERFECT"
      :combinator-fn #'perfect-combinator-fn
      :possible-values '(pf !pf)
      :default-value '!pf)
    ;; PASSIVE
    (make-instance
      'feature-defintion
      :name "PASSIVE"
      :combinator-fn #'passive-combinator-fn
      :possible-values '(pv !pv)
      :default-value '!pv)
    ;; PROGRESSIVE
    (make-instance
      'feature-defintion
      :name "PROGRESSIVE"
      :combinator-fn #'progressive-combinator-fn
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

