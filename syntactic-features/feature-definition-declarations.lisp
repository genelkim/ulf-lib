;;; The class with all of the syntactic feature definitions, parameter
;;; declarations, and related functions and methods.
;;;
;;; See feature-defintion-defintions.lisp for the parameter values.
;;; This file is separated out because the parameter values require access to
;;; some functions that are in files loaded after this one.

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

;;;
;;; Parameters.
;;; See feature-defintion-defintions.lisp for values.
;;;

;; List of feature-definitions.
(defparameter *feature-defintions* nil)
;; Association list from syntactic feature values to feature names.
(defparameter *syntactic-feature-values* nil)

;;;
;;; Combinators.
;;; See feature-defintion-defintions.lisp for feature-specific combinators.
;;;

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

