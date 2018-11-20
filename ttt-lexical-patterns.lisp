;; ULF lexical patterns identified in TTT.

(in-package :ulf-lib)

(defparameter *tense* '(past pres cf))
(defparameter *coordinator* '(and or but because))
(defparameter *detformer* '(nquan fquan))

;; Check if *x* has the *suffix* extension.
(defun suffix-check (x suffix)
  (cl-user::match-re (concatenate 'string "^\(\\w\|\\d\|-\|\/\|\\.\)\+\\." suffix "$")
            (format nil "~s" x)))

(defun lex-noun? (x)
  (suffix-check x "N"))

(defun lex-function? (x)
  (suffix-check x "F"))

(defun lex-pronoun? (x)
  (suffix-check x "PRO"))

(defun lex-verb? (x)
  (suffix-check x "V"))

(defun lex-adjective? (x)
  (suffix-check x "A"))

;; Symbols with *.p suffix.
(defun lex-p? (x)
  (suffix-check x "P"))

(defun lex-p-arg? (x)
  (cl-user::match-re (concatenate 'string "^\(\\w\|\\d\|-\)\+.P\\-ARG$")
            (format nil "~s" x)))

(defun lex-ps? (x)
  (suffix-check x "PS"))

(defun lex-pq? (x)
  (suffix-check x "PQ"))

;; Any preposition.
(defun lex-prep? (x)
  (or (lex-p? x)
      (lex-ps? x)
      (lex-pq? x)))

(defun lex-rel? (x)
  (suffix-check x "REL"))

(defun lex-det? (x)
  (suffix-check x "D"))

(defun lex-coord? (x)
  (or
    (member x *coordinator*)
    (suffix-check x "CC")))

; Auxiliaries.
(defun lex-aux-s? (x)
  (suffix-check x "AUX-S"))
(defun lex-aux-v? (x)
  (suffix-check x "AUX-V"))
(defun lex-aux? (x)
  (or
    (lex-aux-s? x)
    (lex-aux-v? x)))

(defun lex-number? (x)
  (numberp x))

;; Matches a name predicate.
;; TODO: generalize to other extensions.
(defun lex-name-pred? (x)
  (cl-user::match-re "^\\|\[\^\\|\]\+\\.N\\|$" (format nil "~s" x)))

;; Matches a regular name.
(defun lex-name? (x)
  (and
    (cl-user::match-re "^\\|\[\^\\|\]\+\\|$" (format nil "~s" x))
    (not (lex-name-pred? x))
    ;; Special handling of quotes '\" == '|"|.
    (not (eq '\" x))))

; Adverbs
(defun lex-adv-a? (x)
  (suffix-check x "ADV-A"))
(defun lex-adv-s? (x)
  (suffix-check x "ADV-S"))
(defun lex-adv-e? (x)
  (suffix-check x "ADV-E"))
(defun lex-adv-f? (x)
  (suffix-check x "ADV-F"))
(defun lex-adv-formula? (x)
  (or
    (lex-adv-s? x)
    (lex-adv-e? x)
    (lex-adv-f? x)))
(defun lex-adv? (x)
  (or
    (lex-adv-a? x)
    (lex-adv-s? x)
    (lex-adv-e? x)
    (lex-adv-f? x)))

;; Expletives.
(defun lex-x? (x)
  (suffix-check x "X"))

;; Yes/no evaluations.
(defun lex-yn? (x)
  (suffix-check x "YN"))

;; Greetings.
(defun lex-gr? (x)
  (suffix-check x "GR"))

(defun lex-tense? (x)
  (member x *tense*))

(defun lex-detformer? (x)
  (member x *detformer*))

(defun litstring? (x)
  (stringp x))

(defun lex-equal? (x) (equal x '=))
(defun lex-set-of? (x) (equal x 'set-of))
(defun lex-macro? (x) (member x '(qt-attr sub rep n+preds np+preds voc voc-O)))

(defun lex-verbaux? (x)
  (or (lex-verb? x) (aux? x))) 

