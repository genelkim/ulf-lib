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
  (cl-strings:replace-all ulfstr "\\\\" "\\"))

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

