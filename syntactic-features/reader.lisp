;;; The reader macro for syntactic features.
;;; Uses the dispatch character #{ and closed with }.
;;;
;;; This is based on the reader macro walkthrough at
;;; https://lisper.in/reader-macros.

(in-package :ulf-lib)

(defconstant +left-brace+ #\{)
(defconstant +right-brace+ #\})
(defconstant +comma+ #\,)
(defconstant +colon+ #\:)

;; TODO: use named-readtables for read table management https://common-lisp.net/project/named-readtables/
(defvar *previous-readtables* nil)

(defmacro enable-ulf-type-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
     (push *readtable* *previous-readtables*)
     (setf *readtable* (copy-readtable))
     (set-dispatch-macro-character #\# #\{ 'read-syntactic-features)
     ;; Set } as a read delimiter.
     (set-macro-character +right-brace+ 'read-delimiter)))

(defmacro disable-ulf-type-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (pop *previous-readtables*))))

;; Move to gute?
(defun read-next-object (separator delimiter
                         &optional (input-stream *standard-input*))
  "Lisp object reader from https://lisper.in/reader-macros."
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
      (progn
        (discard-next-char)
        nil)
      (let* ((object (read input-stream t nil t))
             (next-char (peek-next-char)))
        (cond
          ((char= next-char separator) (discard-next-char))
          ((and delimiter (char= next-char delimiter)) nil)
          (t (error "Unexpected next char: ~S~%" next-char)))
        object))))

(defun read-next-syntactic-feature (separator delimiter
                                    &optional (input-stream *standard-input*))
  "Reads a syntactic feature from a feature set."
  ;; For now, we just read them as lisp objects since they should just be a
  ;; comma-separated set of symbols.
  (read-next-object separator delimiter input-stream))

(defun verify+combine-feats (infeats)
  "Given a list of feature values, verifies that the values are not
  conflicting, all correspond to defined feature names and translates them into
  the appropriate classes."
  (when (not (= (length infeats)
                (length (remove-duplicates infeats))))
    (error "Feature values must be unique: ~S~%" infeats))
  ;; For each feature,
  ;;  1. Look up the corresponding feature name
  ;;  2. Instantiate the syntactic feature set class with element-value pairs.
  ;; The feature name will be used to look up default value exceptions.
  (in-intern (infeats feats :ulf-lib)
  (make-instance
    'syntactic-features
    :feature-map            
    (loop for feat in feats
          for elem = (lookup-feature-name feat)
          if (not elem)
          do (error "No feature name for feature value ~S~%" feat) 
          else collect (cons elem feat)))))

(defun read-separator (stream char)
  (declare (ignore stream))
  (error "Separator ~S shouldn't be read alone" char))

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

(defun read-syntactic-features (stream char arg)
  "Top-level syntactic feature reader."
  (declare (ignore char arg))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character +comma+ 'read-separator)
    (loop
      for feat = (read-next-syntactic-feature +comma+
                                              +right-brace+
                                              stream)
      while feat
      collect feat into feats
      finally (return (verify+combine-feats feats)))))

