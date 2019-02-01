;; ULF lexical patterns identified in TTT.

(in-package :ulf-lib)

(defparameter *tense* '(past pres cf))
(defparameter *coordinator* '(and or but because))
(defparameter *detformer* '(nquan fquan))

;; Ensures that the input symbol is in ulf-lib.
(defmacro in-ulf-lib ((x y) &body body)
  `(util:in-intern (,x ,y :ulf-lib)
                   ,@body))


;; Check if *x* has the *suffix* extension.
(defun suffix-check (x suffix)
  (cl-user::match-re (concatenate 'string "^\(\\w\|\\d\|-\|\/\|:\|\\.\)\+\\." suffix "$")
            (format nil "~s" x)))

(defun in-ulf-lib-suffix-check (x suffix)
  ;(in-ulf-lib (x y)
  (util:in-intern (x y *package*)
              (suffix-check y suffix)))

(defun lex-noun? (x)
  (in-ulf-lib-suffix-check x "N"))

(defun lex-function? (x)
  (in-ulf-lib-suffix-check x "F"))

(defun lex-pronoun? (x)
  (in-ulf-lib-suffix-check x "PRO"))

(defun lex-verb? (x)
  (in-ulf-lib-suffix-check x "V"))

(defun lex-adjective? (x)
  (in-ulf-lib-suffix-check x "A"))

;; Symbols with *.p suffix.
(defun lex-p? (x)
  (in-ulf-lib-suffix-check x "P"))

(defun lex-p-arg? (x)
  (util:in-intern (x y *package*)
              (cl-user::match-re (concatenate 'string
                                              "^\(\\w\|\\d\|-\)\+.P\\-ARG$")
                                 (format nil "~s" y))))

(defun lex-ps? (x)
  (in-ulf-lib-suffix-check x "PS"))

(defun lex-pq? (x)
  (in-ulf-lib-suffix-check x "PQ"))

;; Any preposition.
(defun lex-prep? (x)
  (or (lex-p? x)
      (lex-ps? x)
      (lex-pq? x)))

;; Predicate modifiers.
(defun lex-mod-a? (x)
  (in-ulf-lib-suffix-check x "MOD-A"))
(defun lex-mod-n? (x)
  (in-ulf-lib-suffix-check x "MOD-N"))


(defun lex-rel? (x)
  (in-ulf-lib-suffix-check x "REL"))

(defun lex-det? (x)
  (in-ulf-lib-suffix-check x "D"))

(defun lex-coord? (x)
  (in-ulf-lib (x y)
              (or
                (member y *coordinator*)
                (suffix-check y "CC"))))

; Auxiliaries.
(defun lex-aux-s? (x)
  (in-ulf-lib-suffix-check x "AUX-S"))
(defun lex-aux-v? (x)
  (in-ulf-lib-suffix-check x "AUX-V"))
(defun lex-aux? (x)
  (or
    (lex-aux-s? x)
    (lex-aux-v? x)))

(defun lex-number? (x)
  (numberp x))

;; Matches a name predicate.
;; TODO: generalize to other extensions.
(defun lex-name-pred? (x)
  (util:in-intern (x y *package*)
              (cl-user::match-re "^\\|\[\^\\|\]\+\\.N\\|$"
                                 (format nil "~s" y))))

;; TODO: merge with lex-name? in ulf-lib.
;; Returns t if s is strictly a ULF name, e.g. |John|, |Mary|, etc.
;; Returns false on name-like predicates (e.g. |Green River|.n).
;; Can take a symbol or a string.
;; TODO: separate into separate functions for symbol and string since the ULF can contain strings!
(defun is-strict-name? (x)
  (util:in-intern (x s *package*)
              (let* ((sstr (if (symbolp s) (util:sym2str s) s))
                     (chars (coerce sstr 'list)))
                (and (eql #\| (nth 0 chars))
                     (eql #\| (nth (1- (length chars)) chars))))))

;; Matches a regular name.
(defun lex-name? (x)
  (util:in-intern (x y *package*)
              (and
                (cl-user::match-re "^\\|\[\^\\|\]\+\\|$"
                                   (format nil "~s" y))
                (not (lex-name-pred? y))
                ;; Special handling of quotes '\" == '|"|.
                (not (eq '\" y)))))

; Adverbs
(defun lex-adv-a? (x)
  (in-ulf-lib-suffix-check x "ADV-A"))
(defun lex-adv-s? (x)
  (in-ulf-lib-suffix-check x "ADV-S"))
(defun lex-adv-e? (x)
  (in-ulf-lib-suffix-check x "ADV-E"))
(defun lex-adv-f? (x)
  (in-ulf-lib-suffix-check x "ADV-F"))
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
  (in-ulf-lib-suffix-check x "X"))

;; Yes/no evaluations.
(defun lex-yn? (x)
  (in-ulf-lib-suffix-check x "YN"))

;; Greetings.
(defun lex-gr? (x)
  (in-ulf-lib-suffix-check x "GR"))

(defun lex-tense? (x)
  (in-ulf-lib (x y) (member y *tense*)))

(defun lex-detformer? (x)
  (in-ulf-lib (x y) (member y *detformer*)))

(defun litstring? (x)
  (stringp x))

(defun lex-equal? (x)
  (in-ulf-lib (x y) (equal y '=)))
(defun lex-set-of? (x)
  (in-ulf-lib (x y) (equal y 'set-of)))
(defun lex-macro? (x)
  (in-ulf-lib (x y) (member y '(qt-attr sub rep n+preds np+preds voc voc-O))))

(defun lex-verbaux? (x)
  (or (lex-verb? x) (aux? x)))

;; Returns true if the token is one that was elided in the surface string.
;; In the current guidelines, this is a curly bracketed token.
;; e.g. {that}, {he}.pro
(defun lex-elided? (intoken)
  (in-ulf-lib (intoken token)
    (multiple-value-bind (word suffix) (split-by-suffix token)
      (let ((wchars (util:split-into-atoms word))
            (tchars (util:split-into-atoms token)))
        ;; Either the whole thing is wrapped in curly brackets or everything but
        ;; the suffix.
        (or
          (and (equal (intern "{") (first wchars))
               (equal (intern "}") (car (last wchars))))
          (and (equal (intern "{") (first tchars))
               (equal (intern "}") (car (last tchars)))))))))

