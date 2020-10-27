;;; Functions relating directly to the ULF type suffixes.

(in-package :ulf-lib)

;; Takes a symbol and returns whether the symbol has a .-delimited suffix.
(defun has-suffix? (s)
  (> (length (the list (cl-strings:split (format nil "~s" s) ".")))
     1))

;; Splits the symbol at the last "." and returns the two values.
(defun split-by-suffix (sym)
  (if (not (symbolp sym)) (return-from split-by-suffix sym))
  (let* ((pkg (symbol-package sym))
         (atoms (gute:split-into-atoms sym))
         ;(dotpos (position '|.| atoms :from-end t)))
         ;; Intern instead of literal so that it gets interned into the
         ;; namespace of the caller.
         (dotpos (position (intern ".") atoms :from-end t)))
    (declare (type list atoms))
    (if (and dotpos 
             ;; Suffixes can't have whitespace in them
             (notany #'(lambda (x)
                         (member (the character x)
                                 gute::*trim-whitespace-chars*))
                     (subseq (coerce (symbol-name sym) 'list) dotpos)))
      (values (gute:fuse-into-atom (gute:slice atoms 0 dotpos) :pkg pkg)
              (gute:fuse-into-atom (gute:slice atoms (+ dotpos 1) (length atoms)) :pkg pkg))
      (values sym nil))))

;; Strips the suffix, marked with "." from a string.
;;  e.g. "man.n" -> "man"
;; If there are multiple periods in the string, only the substring after the
;; last period is stripped off.
(declaim (ftype (function (string) string) strip-suffix))
(defun strip-suffix (s)
  (let* ((split (cl-strings:split s "."))
         (base-ret (cl-strings:join
                     (subseq (the string split)
                             0
                             (max 1 (1- (length (the string split)))))
                     :separator ".")))
    (cond
      ;; If there's a space in the suffix, then don't strip.
      ((some #'(lambda (x)
                 (member (the character x) gute::*trim-whitespace-chars*))
             (car (last (coerce split 'list))))
       s)
      ;; If it's a name, but there is a split, add back the pipe at the end.
      ((and (is-strict-name? (read-from-string s)) (> (length split) 1))
       (concatenate 'string base-ret "|"))
      (t base-ret))))

;; Takes a word symbol and a suffix and merges them together.
;; Assumes that we retain the package of word.
(defun add-suffix (word suffix &key (pkg (symbol-package word)))
  (if (not suffix) (return-from add-suffix word))
  (gute:fuse-into-atom
    (concatenate 'list
                 (gute:split-into-atoms word)
                 (list #\.)
                 (gute:split-into-atoms suffix))
    :pkg pkg))

;; An association list of the semantic type name and the suffix extension for
;; lexical ULF items.
(defparameter *type-suffix-alist*
  '((noun . n)
    (adj . a)
    (adv-a . adv-a)
    (adv-e . adv-e)
    (adv-s . adv-s)
    (adv-f . adv-f)
    (mod-a . mod-a)
    (mod-n . mod-n)
    (pp . pp)
    (term . pro)
    (verb . v)
    (pred . pred)
    (det . d)
    (aux-v . aux-v)
    (aux-s . aux-s)
    (sent . sent)
    (funct . f)))
;; TODO: complete type suffix list.
;; TODO: unify this system so all the lex-X? are defined from the list:
;;    (defun lex-[type]? (x) (in-ulf-lib-suffix-check x [suffix]))
;; Also, make a hierarchy of types so we can choose the most specific, e.g.
;;  - pred
;;    - verb
;;    - noun
;;    - adjective
;;    - preposition


;; Returns the suffix for the type. If none found, it just returns the type,
;; but in the desired format.
(declaim (ftype (function (symbol &key (:callpkg (or package symbol null))
                                       (:form simple-string))
                          (or symbol string))
                suffix-for-type))
(defun suffix-for-type (x &key (callpkg nil) (form "symbol"))
  (assert (member form '("string" "symbol") :test #'equal))
  (let ((suffix (cdr (assoc (safe-intern x :ulf-lib) *type-suffix-alist*))))
    (if suffix
      ;; Cases where we found a type result.
      (cond
        ((equal form "string") (symbol-name suffix))
        (callpkg (nth-value 0 (safe-intern suffix callpkg)))
        (t suffix))
      ;; Not found.
      (cond
        ((equal form "string") (symbol-name x))
        (t x)))))

