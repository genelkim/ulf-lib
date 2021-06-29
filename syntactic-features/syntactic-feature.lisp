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

(defmethod to-string ((obj syntactic-features))
  "Returns a string representation of the object that can be read back in by
  the reader macro."
  (let ((s (make-string-output-stream)))
    (print-object obj s)
    (get-output-stream-string s)))

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

(defun lookup-feat-element (featsym)
  "Looks up the corresponding syntactic element for the given feature symbol,
  if any. nil if not found."
  (cdr (assoc featsym *syntactic-feature-list*)))

(defmethod copy ((obj syntactic-features))
  (make-instance
    'syntactic-features
    :feature-map (copy-tree (feature-map obj))))

(defun syntactic-features-equal? (x y)
  "Checks whether the two syntactic-features class instances are the same. This
  is only true if the feature map is identical."
  (flet ((element-sort (a b) (string< (cdr a) (cdr b))))
    (equal (sort x #'element-sort)
           (sort y #'element-sort))))

(defmethod feature-value ((obj syntactic-features) element)
  "Finds the feature value for the given syntactic element. The element value
  can be a string or a symbol."
  (when (symbolp element)
    (setf element (symbol-name element)))
  (setf element (string-upcase element))
  (cdr (assoc element (feature-map obj) :test #'equal)))

(defmethod add-features ((obj syntactic-features) (new-features list))
  "Takes a list of new features and adds them to the obj class.
  This will delete any prior feature label for a given syntactic element."
  ;; look up pair for each and update map.
  (loop for feat in new-features
        for elem = (lookup-feat-element feat)
        if (not elem)
        do (error "No syntactic element for feature ~S~%" feat)
        if (assoc elem (feature-map obj) :test #'equal)
        do (setf (cdr (assoc elem (feature-map obj) :test #'equal))
                 feat)
        else do (setf (feature-map obj) (cons (cons elem feat)
                                              (feature-map obj))))
  ;; Return the modified object.
  obj)

