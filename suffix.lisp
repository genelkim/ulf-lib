;;; Functions relating directly to the ULF type suffixes.

(in-package :ulf-lib)

;; Takes a symbol and returns whether the symbol has a .-delimited suffix.
(defun has-suffix? (s)
  (> (length (cl-strings:split (format nil "~s" s) "."))
     1))

;; Splits the symbol at the last "." and returns the two values.
(defun split-by-suffix (sym)
  (let* ((atoms (util:split-into-atoms sym))
         ;(dotpos (position '|.| atoms :from-end t)))
         ;; Intern instead of literal so that it gets interned into the
         ;; namespace of the caller.
         (dotpos (position (intern ".") atoms :from-end t)))
    (if dotpos
      (values (util:fuse-into-atom (util:slice atoms 0 dotpos))
              (util:fuse-into-atom (util:slice atoms (+ dotpos 1) (length atoms))))
      (values sym nil))))

;; Strips the suffix, marked with "." from a string.
;;  e.g. "man.n" -> "man"
;; If there are multiple periods in the string, only the substring after the
;; last period is stripped off.
(defun strip-suffix (s)
  (let* ((split (cl-strings:split s "."))
         (base-ret (cl-strings:join
                     (subseq split 0 (max 1 (1- (length split))))
                     :separator ".")))
    ;; If it's a name, but there is a split, add back the pipe at the end.
    (if (and (is-strict-name? (read-from-string s)) (> (length split) 1))
      (concatenate 'string base-ret "|")
      base-ret)))

;; Takes a word symbol and a suffix and merges them together.
(defun add-suffix (word suffix)
  (if (not suffix) (return-from add-suffix word))
  (util:fuse-into-atom
    (concatenate 'list
                 (util:split-into-atoms word)
                 (list #\.)
                 (util:split-into-atoms suffix))))

