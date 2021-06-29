;;; The class with all of the syntactic feature definitions as well as
;;; top-level feature info.

(in-package :ulf-lib)

(defclass syntactic-element-definition ()
  ((name
     :initarg :name
     :initform (error "Must supply a name for the element definition.")
     :accessor name)
   (possible-values
     :initarg :possible-values
     :initform (error "Must supply possible feature values for syntactic element.")
     :accessor possible-values)
   (composition-fn
     :initarg :composition-fn
     :accessor composition-fn)))

;; The possible-values must be unique through all definitions since we use
;; those values directly for a compact representation of the syntactic feature
;; set.
;; TODO: fill in composition functions.
(defparameter *syntactic-element-definitions*
  (list
    ;; AUXILIARY
    (make-instance
      'syntactic-element-definition
      :name "auxiliary"
      :possible-values '(x))
    ;; PLURALITY
    (make-instance
      'syntactic-element-definition
      :name "plurality"
      :possible-values '(pl))
    ;; PERFECT
    (make-instance
      'syntactic-element-definition
      :name "perfect"
      :possible-values '(pf))
    ;; PASSIVE
    (make-instance
      'syntactic-element-definition
      :name "passive"
      :possible-values '(pv))))

(defparameter *syntactic-feature-list*
  ;; GK: For some reason Lisp gets mad when I try to access possible-values
  ;; directly in the loop macro, so I nested another level.
  (loop for elem-def in *syntactic-element-definitions*
        collect (loop for feat in (possible-values elem-def)
                      collect (cons feat (name elem-def)))
        into sublists
        finally (return (apply #'append sublists))))

