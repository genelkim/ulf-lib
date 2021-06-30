;;; Syntactic feature class.
;;; Holds the set of syntactic features and has associated functions for
;;; accessing info for each feature.

(in-package :ulf-lib)

(defclass syntactic-features ()
  ;; A mapping from feature names to feature values.
  ((feature-map
     :initarg :feature-map
     :accessor feature-map)))

;; TODO: compile this from default feature values for each feature.
(defparameter *default-syntactic-features*
  (make-instance 'syntactic-features
                 :feature-map nil))

(defmethod empty? ((obj syntactic-features))
  ;; Empty if there are no non-nil entries.
  (null (remove-if #'(lambda (pair) (null (cdr pair)))
                   (feature-map obj))))

(defmethod get-feature-names ((obj syntactic-features))
  (mapcar #'car (feature-map obj)))

(defmethod get-feature-values ((obj syntactic-features))
  (mapcar #'cdr (feature-map obj)))

(defmethod print-verbose ((obj syntactic-features)
                          &optional (out *standard-output*))
  "Print syntactic features in an informative manner, which may not be able to
  be read by the reader macro."
  (let ((*package* (find-package "ULF-LIB")))
    (format out "#{")
    (flet ((print-feature (pair) (format out "~s:~s" (car pair) (cdr pair))))
      (when (feature-map obj)
        (print-feature (car (feature-map obj))))
      (loop for pair in (cdr (feature-map obj))
            do (format out "," pair)
            do (print-feature pair)))
    (format out "}")))

(defmethod to-string ((obj syntactic-features))
  "Returns a string representation of the object that can be read back in by
  the reader macro."
  (let ((s (make-string-output-stream)))
    (print-object obj s)
    (get-output-stream-string s)))

(defmethod print-object ((obj syntactic-features) out)
  "Print syntactic features in a way that it can be read back in by the reader
  macro."
  (let ((*package* (find-package "ULF-LIB")))
    (format out "#{")
    (let ((feat-vals (remove-if #'null (mapcar #'cdr (feature-map obj)))))
      (when feat-vals
        (format out "~s" (car feat-vals)))
      (loop for feat in (cdr feat-vals)
            do (format out ",~s" feat)))
    (format out "}")))

(defun lookup-feature-name (featsym)
  "Looks up the corresponding feature name for the given feature value, if any.
  nil if not found."
  (cdr (assoc featsym *syntactic-feature-values*)))

(defmethod copy ((obj syntactic-features))
  (make-instance
    'syntactic-features
    :feature-map (copy-tree (feature-map obj))))

(defun syntactic-features-equal? (x y)
  "Checks whether the two syntactic-features class instances are the same. This
  is only true if the feature map is identical."
  (cond
    ;; TODO: see if we can remove these top-2 conditions and make this function a method.
    ((and (null x) (null y)) t)
    ((or (null x) (null y)) nil)
    ;; TODO: add option to allow default value discrepancies
    (t
      (flet ((element-sort (a b) (string< (cdr a) (cdr b))))
        (equal (sort (feature-map x) #'element-sort)
               (sort (feature-map y) #'element-sort))))))

(defun syntactic-features-match? (patt inst)
  "Checks whether the syntactic features instance `inst` fulfills the
  syntactice features pattern `patt`. The instance assumes default values for
  unspecified slots, whereas unspecified slots in the pattern are assumed to
  allow any value."
  (cond
    ;; TODO: see if we can remove these top-2 conditions and make this function a method.
    ((and (null patt) (null inst)) t)
    ((or (null patt) (null inst)) nil)
    (t
      ;; For each feature specification in `patt` check fulfillment by `inst`.
      (loop for (feat-name . patt-feat-val) in (feature-map patt)
            if (and patt-feat-val
                    ;; `patt` doesn't match `inst`
                    (not (eql patt-feat-val
                              (feature-value inst feat-name)))
                    ;; `patt` doesn't match default or `inst` is specified.
                    (or (feature-value inst feat-name)
                        (not (eql patt-feat-val
                                  (default-syntactic-feature-value feat-name)))))
            do (return-from syntactic-features-match? nil))
      ;; Everything matched.
      t)))

(defmethod feature-value ((obj syntactic-features) element)
  "Finds the feature value for the given feature name. The name can be a string
  or a symbol."
  (when (symbolp element)
    (setf element (symbol-name element)))
  (setf element (string-upcase element))
  (cdr (assoc element (feature-map obj) :test #'equal)))

(defun default-syntactic-feature-value (feat-name)
  (default-value (find-if #'(lambda (def) (equal feat-name (name def)))
                          *feature-defintions*)))

(defmethod add-feature-values ((obj syntactic-features) (new-features list))
  "Takes a list of new feature values and adds them to the obj class.
  This will delete any prior feature label for a given feature name."
  ;; look up pair for each and update map.
  (loop for feat in new-features
        for elem = (lookup-feature-name feat)
        if (not elem)
        do (error "No feature name for feature value ~S~%" feat)
        if (assoc elem (feature-map obj) :test #'equal)
        do (setf (cdr (assoc elem (feature-map obj) :test #'equal))
                 feat)
        else do (setf (feature-map obj) (cons (cons elem feat)
                                              (feature-map obj))))
  ;; Return the modified object.
  obj)

(defmethod update-feature-map ((obj syntactic-features) (new-feature-map list))
  "Takes a new feature map and updates the old one to include all of the
  entries in the new one. Any feature names that are not specified in
  `new-feature-map` remains unaffected."
  (loop for (feat-name . feat-val) in new-feature-map
        if (assoc feat-name (feature-map obj) :test #'equal)
        do (setf (cdr (assoc feat-name (feature-map obj) :test #'equal))
                 feat-val)
        else do (setf (feature-map obj) (cons (cons feat-name feat-val)
                                              (feature-map obj))))
  obj)

(defmethod combine-features ((base syntactic-features)
                             (opr-feats syntactic-features)
                             (arg-feats syntactic-features)
                             (csq-feats syntactic-features)
                             &optional opr-semtype arg-semtype)
  "Combines syntactic-features objects. Feature precedence:
    1. Consequent
    2. Combinator result
    3. Base

  Arguments:
    base:
      Default features based on external information.
    opr-feats:
      Syntactic features for the operator semtype.
    arg-feats:
      Syntactic features for the argument semtype.
    csq-feats:
      Syntactic features for the operator consequent.
    opr-semtype (optional):
      Semtype for the operator; required if any present features need access to
      operator semtype information.
    arg-semtype (optional):
      Semtype for the argument; required if any present features need access to
      argument semtype information.

  Returns:
    The resulting syntactic-features object, which is a modified instance of
    `base`."
  ;; For each relevant feature in base, opr-feats, or arg-feats call the
  ;; corresponding combination function and update base accordingly.
  (let* ((feat-names (remove-duplicates
                       (apply #'append
                              (mapcar #'get-feature-names
                                      (list base opr-feats arg-feats)))
                       :test #'equal))
         (new-feat-vals
           (loop for feat-name in feat-names
                 for feat-combinator = (get-syntactic-feature-combinator
                                         feat-name)
                 for csq-feat-value = (feature-value csq-feats feat-name)
                 ;; Feature combinator takes the base, opr, and arg feature values
                 ;; and the semtypes.
                 collect (cons feat-name
                               (funcall feat-combinator
                                        (feature-value base feat-name)
                                        (feature-value opr-feats feat-name)
                                        (feature-value arg-feats feat-name)
                                        (feature-value csq-feats feat-name)
                                        opr-semtype
                                        arg-semtype))))
         ;; Add in combinator results.
         (with-combs (update-feature-map base new-feat-vals)))
    ;; Overwrite anything so far with consequent features, filtering out
    ;; unspecified ones. The consequent must specify any guaranteed types.
    (update-feature-map with-combs
                        (remove-if #'(lambda (pair) (null (cdr pair)))
                                   (feature-map csq-feats)))))

