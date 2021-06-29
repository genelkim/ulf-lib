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
   ;; A combinator must take three feature symbol arguments and two optional
   ;; semtype arguments, same as the combine-features method in
   ;; syntactic-features.lisp.
   (combinator-fn
     :initarg :combinator-fn
     :initform nil
     :accessor combinator-fn)))

;; The possible-values must be unique through all definitions since we use
;; those values directly for a compact representation of the syntactic feature
;; set.
;; TODO: fill in combinator-fns.
(defparameter *syntactic-element-definitions*
  (list
    ;; AUXILIARY
    (make-instance
      'syntactic-element-definition
      :name "AUXILIARY"
      :possible-values '(x))
    ;; PLURALITY
    (make-instance
      'syntactic-element-definition
      :name "PLURALITY"
      :possible-values '(pl))
    ;; PERFECT
    (make-instance
      'syntactic-element-definition
      :name "PERFECT"
      :possible-values '(pf))
    ;; PASSIVE
    (make-instance
      'syntactic-element-definition
      :name "PASSIVE"
      :possible-values '(pv))
    ;; PROGRESSIVE
    (make-instance
      'syntactic-element-definition
      :name "PROGRESSIVE"
      :possible-values '(pg))))
;; TODO: p-arg, lexical, *h, qt-attr, 's, etc.
;;       maybe macro features should be different?

(defparameter *syntactic-feature-list*
  ;; GK: For some reason Lisp gets mad when I try to access possible-values
  ;; directly in the loop macro, so I nested another level.
  (loop for elem-def in *syntactic-element-definitions*
        collect (loop for feat in (possible-values elem-def)
                      collect (cons feat (name elem-def)))
        into sublists
        finally (return (apply #'append sublists))))

(defmethod get-combinator ((obj syntactic-element-definition))
  (if (combinator-fn obj)
    (combinator-fn obj)
    #'default-combinator-fn))

(defmethod get-syntactic-feature-combinator ((name string))
  (get-combinator (find name *syntactic-element-definitions* :key #'name)))

(defun default-combinator-fn (base opr arg
                              &optional opr-semtype arg-semtype)
  (declare (ignore opr arg opr-semtype arg-semtype))
  base)


