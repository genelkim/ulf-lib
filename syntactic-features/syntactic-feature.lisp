;;; Syntactic feature class.
;;; Holds the set of syntactic features and has associated functions for
;;; accessing info for each feature.

(in-package :ulf-lib)

(defclass syntactic-features ()
  ;; A mapping from syntactic elements to feature values.
  ((feature-map
     :initarg :feature-map
     :accessor feature-map)))

(defmethod print-verbose ((obj syntactic-features)
                          &optional (out *standard-output*))
  "Print syntactic features in an informative manner, which may not be able to
  be read by the reader macro."
  (format out "#{")
  (flet ((print-feature (pair) (format out "~s:~s" (car pair) (cdr pair))))
    (when (feature-map obj)
      (print-feature (car (feature-map obj))))
    (loop for pair in (cdr (feature-map obj))
          do (format out "," pair)
          do (print-feature pair)))
  (format out "}"))

(defmethod print-object ((obj syntactic-features) out)
  "Print syntactic features in a way that it can be read back in by the reader
  macro."
  (format out "#{")
  (let ((feat-vals (mapcar #'cdr (feature-map obj))))
    (when feat-vals
      (format out "~s" (car feat-vals)))
    (loop for feat in (cdr feat-vals)
          do (format out ",~s" feat)))
  (format out "}"))

(defparameter *syntactic-feature-list*
  '((pl . plurality)
    (pf . perfect)
    (pv . passive)
    (x . auxiliary)))

(defun lookup-feat-element (featsym)
  "Looks up the corresponding syntactic element for the given feature symbol,
  if any. nil if not found."
  (cdr (assoc featsym *syntactic-feature-list*)))

