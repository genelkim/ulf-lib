;;; The class with all of the syntactic feature definitions as well as
;;; top-level feature info.

(in-package :ulf-lib)

(defclass feature-defintion ()
  ((name
     :initarg :name
     :initform (error "Must supply a name for the feature definition.")
     :accessor name)
   (possible-values
     :initarg :possible-values
     :initform (error "Must supply possible feature values for the feature.")
     :accessor possible-values)
   ;; A combinator must take four feature symbol arguments and two optional
   ;; semtype arguments, same as the combine-features method in
   ;; syntactic-features.lisp.
   (combinator-fn
     :initarg :combinator-fn
     :initform nil
     :accessor combinator-fn)
   ;; Default value for when the feature is unspecified in non-antecedent
   ;; position. Must be one of the possible values.
   (default-value
     :initarg :default-value
     :initform (error "Must supply a default feature value.")
     :accessor default-value)))

;; The possible-values must be unique through all definitions since we use
;; those values directly for a compact representation of the syntactic feature
;; set.
;;
;; The convention for binary features (+/-) is to indicate the negation with a
;; ! prefix. Then nil is used for unspecified feature values.
;; TODO: fill in combinator-fns.
(defparameter *feature-defintions*
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
(defparameter *syntactic-feature-values*
  ;; GK: For some reason Lisp gets mad when I try to access possible-values
  ;; directly in the loop macro, so I nested another level.
  (loop for elem-def in *feature-defintions*
        collect (loop for feat in (possible-values elem-def)
                      collect (cons feat (name elem-def)))
        into sublists
        finally (return (apply #'append sublists))))

(defmethod get-combinator ((obj feature-defintion))
  (if (combinator-fn obj)
    (combinator-fn obj)
    #'default-combinator-fn))

(defmethod get-syntactic-feature-combinator ((name string))
  (get-combinator (find name *feature-defintions* :key #'name)))

(defun default-combinator-fn (base opr arg csq
                              &optional opr-semtype arg-semtype)
  "Default feature combinator function simply uses the base feature."
  (declare (ignore opr arg csq opr-semtype arg-semtype))
  base)

