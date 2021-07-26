;; Preprocessing ULFs.
;; These functions should be called over strings contraining ULFs to perform
;; preprocessing that must be done before calling 'read'.

(in-package :ulf-lib)

(add-nickname "CL-PPCRE" "RE")

;; When Lisp reads in ULFs from the SQL database, backslashes for escaping Lisp
;; characters are themselves sometimes escaped.  This function is meant to undo
;; that.
;;  (unescape-backslashes "(\\" there.pro \\")" -> "(\" there.pro \")"
(defun unescape-backslashes (ulfstr)
  (str:replace-all "\\\\" "\\" ulfstr))

;; Performs the expected name preprocessing of adding a space beforehand. This
;; allows us to still be able to identify names that only include capitalized
;; letters.
;;  |John| -> | John|
;;
;; GK: Despite having written this formula, I don't know exactly how this
;; works... it's based off of the example in documentation for cl-ppcre.
;;    (regex-replace "(be)(nev)(o)(lent)"
;;                              "benevolent: adj. generous, kind"
;;                              #'(lambda (match &rest registers)
;;                                  (format nil "~A [~{~A~^.~}]" match registers))
;;                              :simple-calls t)
;;    "benevolent [be.nev.o.lent]: adj. generous, kind"
;;
;; The regular expression matches anything between two pipes and groups them into
;; the first pipe and the rest.
(defun add-prename-space (ulfstr)
  (re:regex-replace-all
    "(\\|)(\[\^\\|\]\+\\|)"
    ulfstr
    #'(lambda (match &rest registers)
        (declare (ignore match))
        (format nil "~{~A~^ ~}" registers))
    :simple-calls t))

;; This function takes a string and add parentheses on the left and right to
;; make the parentheses match for the purposes of reading in s-expression.
;; This means that parentheses within complex symbols and withing strings
;; will be ignored.
;;  e.g. "HI)" -> "(HI)"
;;       "( this \"(string1\" ))" -> "(( this \"(string1\" ))"
;;       ")" -> "()"
;; Some miscellaneous spaces may get introduced between symbols and brackets.
;; These spaces will not affect the resulting s-expression when read into Lisp.
;;
;; NB: This function assumes that the only reason why a read would fail is
;; that the parens aren't matching.  Note that this function will crash if
;; quotes or symbol markers (e.g. |) don't match.
(defun make-string-paren-match (str)
  (labels
    ;; Heavy-lifting helper function.
    ;; Acc keeps track of previous completely interpreted strings, which may
    ;; need to be wrapped in parentheses.
    ((helper (str acc lpcount)
       (declare (type simple-string str)
                (type fixnum lpcount))
       (multiple-value-bind (sexpr endidx)
         (handler-case (read-from-string str)
           (end-of-file () (values str -1))
           #+SBCL (sb-int:simple-reader-error (c) (declare (ignore c)) (values str -2))
           ;; Allegro common lisp gives a speical error when there's an extra right paren.
           #+ALLEGRO (excl::extra-right-paren-error (c) (values str -2)))
         ;; Body of multiple-value-bind.
         (declare (ignore sexpr))
         (declare (type fixnum endidx))
         (cond
           ;; If we got an end of file error and it's not empty, add a paren at
           ;; the end of the current string.
           ((and (= endidx -1) (not (equal "" (trim str))))
            (helper (concatenate 'string str ")") acc lpcount))
           ;; If we got a extra-right-paren-error, this means the paren was at
           ;; the beginning (otherwise, read-from-string just reports that we
           ;; didn't read the whole string).  So put the first character --
           ;; which should be a right-paren -- into acc, increment lpcount, and
           ;; recurse.
           ((= endidx -2)
            (let* ((trimmed (the string (trim str)))
                   (firstletter (subseq trimmed 0 1))
                   (restletters (subseq trimmed 1)))
              (assert (equal firstletter ")") (firstletter) (format nil "firstletter ~s" firstletter))
              (helper restletters (cons firstletter acc) (1+ lpcount))))
           ;; If we read the whole thing, we're done, return all of acc, str,
           ;; and left parens with space separation
           ((or (>= endidx (length str)) (equal "" (trim str)))
            (trim (str:join " "
                            (cons (str:repeat lpcount "(")
                                  (reverse (cons str acc))))))
           ;; If we stopped somewhere so include the current segment into acc
           ;; and recurse into the rest of the string.
           (t (helper (subseq str endidx)
                      (cons (subseq str 0 endidx)
                            acc)
                      lpcount)))))
     ) ; end of labels definitions.
    ;; Main body.
    (helper str nil 0)))


(defun all-string-preprocess (str)
  (funcall
    (compose #'make-string-paren-match
             #'add-prename-space
             #'unescape-backslashes)
    str))

;; Reads a ulf from a string.
;; if multi-label is nil, multi-sentence strings are just put together as if
;; they were separated by a semi-colon. Otherwise, the given label is places as
;; an operator.
(defun ulf-from-string (str &key (multi-label nil))
  (declare (type simple-string str))
  (let ((start 0)
        (ulf-segments nil))
    (loop while (< start (length str))
          do 
          (handler-case
            (multiple-value-bind (obj idx) (read-from-string str t nil :start start)
               (setf start idx)
               (push obj ulf-segments))
            ;; If we hit a read error, abort the loop.
            #+SBCL (sb-int:simple-reader-error (hre)
                     (declare (ignore hre))
                     (format t "Hit a sb-int:simple-reader-error on string: ~s~%At start: ~s~%" 
                             str start)
                     (setf start (length str)))))
    (cond 
      ((= 1 (length ulf-segments)) (car ulf-segments))
      ((null multi-label) (reverse ulf-segments))
      (t (cons multi-label (reverse ulf-segments))))))
  

