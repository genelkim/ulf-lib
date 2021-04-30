;; ULF lexical patterns identified in TTT.

(in-package :ulf-lib)

;; Give cl-ppcre a nickname.
;(defpackage cl-ppcre (:nicknames re))
(add-nickname "CL-PPCRE" "RE")

(defparameter *tense* '(past pres cf))
(defparameter *coordinator* '(and or but because))
(defparameter *detformer* '(nquan fquan))
(defparameter *atomsymbols* "\\[?\\{?\(\\w\|\\d\|-\|\/\|:\|\\.\|\\*\|\\[\|\\]\)\+\\}?")
(defparameter *innername-symbols* "\[\^\\|\]\+")

; Note: composition with subscripts/tenses is a little wonky right now
(defparameter *semtypes*
  (mapcar
    (lambda (x) (cons (regex-replace-all
                        "{N}"
                        (regex-replace-all
                          "{S}"
                          (car x)
                          *atomsymbols*)
                        *innername-symbols*)
                      (if (listp (cdr x))
                        (mapcar #'str2semtype (cdr x))
                        (str2semtype (cdr x)))))
    '(("{S}\\.PRO" . "D")  ; pronouns
      ("\\|{N}\\|" . "D")  ; names
      ("\[\\d\|\\.\]\+" . "D")  ; numbers
      ("{S}\\.P" . "(D=>(D=>(S=>2)))_p")
      ("{S}\\.PS" . "((S=>2)=>((S=>2)=>(S=>2)))")
      ("{S}\\.N" . "(D=>(S=>2))_n")
      ("{S}-OF\\.N" . "(D=>(D=>(S=>2)))_n")
      ("{S}\\.A" . "{(D=>(S=>2))_a|(D=>(D=>(S=>2)))_a}")
      ; Note: "be.v" is intentionally placed above "*.v" so that it gets mapped correctly.
      ; Be careful while moving this around so that "be.v" is mapped correctly.
      ("BE\\.V" . "((D=>(S=>2))=>(D=>(S=>2)))_v")
      ("{S}\\.V" . "({D|(D=>(S=>2))}^n=>(D=>(S=>2)))_v")
      ("{S}\\.D" . "({(D=>(S=>2))_n|(D=>(S=>2))_p}=>D)")
      ("{S}\\.ADV-A" . "((D=>(S=>2))_v=>(D=>(S=>2))_v)")
      ("{S}\\.(ADV-E|ADV-S|ADV-F)" . "((S=>2)=>(S=>2))")
      ("PLUR" . "((D=>(S=>2))_n=>(D=>(S=>2))_n)")
      ("K" . "((D=>(S=>2))_n=>D)")
      ("TO|KA" . "((D=>(S=>2))_v=>D)")
      ("KE" . "((S=>2)_u=>D)")
      ("THAT" . "((S=>2)_t=>D)")
      ("WHETHER|ANS-TO" . "((S=>2)_t=>D)")
      ("ADV-A" . "((D=>(S=>2))=>((D=>(S=>2))_v=>(D=>(S=>2))_v))")
      ("ADV-E|ADV-S|ADV-F" . "((D=>(S=>2))=>((S=>2)=>(S=>2)))")
      ("FQUAN|NQUAN" . "((D=>(S=>2))_a=>((D=>(S=>2))_n=>D))")
      ("SET-OF" . "(D^n=>(D=>(D=>D)))")
      ("NOT" . "((S=>2)=>(S=>2))")
      ("=" . "(D=>(D=>(S=>2)))")
      ("{S}\\.CC" . "((S=>2)^n=>(S=>2))")
      ("{S}\\.MOD-A" . "((D=>(S=>2))_a=>(D=>(S=>2))_a)")
      ("{S}\\.MOD-N" . "((D=>(S=>2))_n=>(D=>(S=>2))_n)")
      ("MOD-A" . "((D=>(S=>2))=>((D=>(S=>2))_a=>(D=>(S=>2))_a))")
      ("MOD-N" . "((D=>(S=>2))=>((D=>(S=>2))_n=>(D=>(S=>2))_n))")
      ("{S}\\.AUX-S" . "AUX")
      ("{S}\\.AUX-V" . "AUX")
      ("{S}\\.P-ARG" . "PARG")
      ("\\!" . "((S=>2)=>(S=>2))")
      ("\\?" . "((S=>2)=>(S=>2))")
      )))

;; Ensures that the input symbol is in *package*.
(defmacro in-cur-package ((x y) &body body)
  `(util:in-intern (,x ,y *package*)
                   ,@body))


;; Check if *x* has the *suffix* extension.
;; Allows it to be square-bracketed from TTT operator hiding.
(defun suffix-check (x suffix)
  (re:all-matches (concatenate 'string "^\\[?\\|? ?\\{?\(\\w\|\\d\|-\|\/\|:\|\\.\|\\*\|\\[\|\\]\)\+\\}?\\." suffix "\\|?\\]?$")
            (format nil "~s" x)))

(defun in-ulf-lib-suffix-check (x suffix)
  (util:in-intern (x y :ulf-lib)
    (suffix-check y suffix)))

(defun in-package-suffix-check (x suffix)
  (util:in-intern (x y *package*)
    (suffix-check y suffix)))

(defun lex-noun? (x)
  (in-package-suffix-check x "N"))

(defun lex-rel-noun? (inx)
  (if (atom inx)
    (in-ulf-lib (inx x)
      (multiple-value-bind (word suffix) (split-by-suffix x)
        (declare (ignore suffix)) ; suffix check handled in lex-noun? below.
        (let ((wchars (the list (cl-strings:chars (atom2str word)))))
          (and (lex-noun? inx)
               (> (length wchars) 3)
               (equal '(#\- #\O #\F) (last wchars 3))))))))

(defun lex-function? (x)
  (in-package-suffix-check x "F"))

(defun lex-pronoun? (x)
  (in-package-suffix-check x "PRO"))

(defun lex-verb? (x)
  (in-package-suffix-check x "V"))

(defun lex-adjective? (x)
  (in-package-suffix-check x "A"))

;; Symbols with *.p suffix.
(defun lex-p? (x)
  (in-package-suffix-check x "P"))

(defun lex-p-arg? (x)
  (util:in-intern (x y *package*)
              (re:all-matches (concatenate 'string
                                              "^\(\\w\|\\d\|-\)\+.P\\-ARG$")
                                 (format nil "~s" y))))

(defun lex-ps? (x)
  (in-package-suffix-check x "PS"))

(defun lex-pq? (x)
  (in-package-suffix-check x "PQ"))

;; Any preposition.
(defun lex-prep? (x)
  (or (lex-p? x)
      (lex-ps? x)
      (lex-pq? x)))

;; Lexical prepositional phrase (should not normally appear, but is sometimes
;; used for processing).
(defun lex-pp? (x)
  (in-package-suffix-check x "PP"))

;; Predicate modifiers.
(defun lex-mod-a? (x)
  (in-package-suffix-check x "MOD-A"))
(defun lex-mod-n? (x)
  (or (in-package-suffix-check x "MOD-N")
      (in-ulf-lib (x y) (member y '(plur)))))

(defun lex-rel? (x)
  (in-package-suffix-check x "REL"))

(defun lex-det? (x)
  (in-package-suffix-check x "D"))

(defun lex-coord? (x)
  (declare (optimize (speed 1)))
  (in-ulf-lib (x y)
              (or
                (member y *coordinator*)
                (suffix-check y "CC"))))

; Auxiliaries.
(defun lex-aux-s? (x)
  (in-package-suffix-check x "AUX-S"))
(defun lex-aux-v? (x)
  (in-package-suffix-check x "AUX-V"))
(defun lex-aux? (x)
  (or
    (lex-aux-s? x)
    (lex-aux-v? x)))

(defun lex-number? (x)
  (numberp x))

;; Matches a name predicate.
(defun name-suffix-check (x suffix)
  (re:all-matches (concatenate 'string
                               "^\\[?\\|\[\^\\|\]\+\\." suffix "\\|\\]?$")
                  (format nil "~s" x)))

(defun in-ulf-lib-named-suffix-check (x suffix)
  (util:in-intern (x y :ulf-lib)
    (name-suffix-check y suffix)))

(defun in-package-named-suffix-check (x suffix)
  (util:in-intern (x y *package*)
    (name-suffix-check y suffix)))

(defun lex-name-noun? (x)
  (in-package-named-suffix-check x "N"))

(defun lex-name-det? (x)
  (in-package-named-suffix-check x "D"))

(defun lex-name-adj? (x)
  (in-package-named-suffix-check x "A"))

(defun lex-name-prep? (x)
  (in-package-named-suffix-check x "P"))

(defun lex-name-pred? (x)
  (or (lex-name-noun? x)
      (lex-name-det? x)
      (lex-name-adj? x)
      (lex-name-prep? x)))


;; TODO: merge with lex-name? in ulf-lib.
;; Returns t if s is strictly a ULF name, e.g. |John|, |Mary|, etc.
;; Returns false on name-like predicates (e.g. |Green River|.n).
;; Can take a symbol or a string.
;; TODO: separate into separate functions for symbol and string since the ULF can contain strings!
(defun is-strict-name? (x)
  (util:in-intern (x s *package*)
    (let* ((sstr (if (not (stringp s)) (util:atom2str s) s))
           (chars (coerce sstr 'list)))
      (and (eql #\| (nth 0 chars))
           (eql #\| (nth (1- (length chars)) chars))))))

;; Matches a regular name.
(defun lex-name? (x)
  (util:in-intern (x y *package*)
    (util:in-intern (x z :ulf-lib)
      (and
        (re:all-matches "^\\|\[\^\\|\]\+\\|$"
                           (format nil "~s" y))
        (not (lex-name-pred? y))
        ;; Special handling of quotes '\" == '|"|.
        (not (eq '\" z))))))

; Adverbs
(defun lex-adv-a? (x)
  (in-package-suffix-check x "ADV-A"))
(defun lex-adv-s? (x)
  (or (in-package-suffix-check x "ADV-S")
      (in-ulf-lib (x y) (eql y 'not))))
(defun lex-adv-e? (x)
  (in-package-suffix-check x "ADV-E"))
(defun lex-adv-f? (x)
  (in-package-suffix-check x "ADV-F"))
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
  (in-package-suffix-check x "X"))

;; Yes/no evaluations.
(defun lex-yn? (x)
  (in-package-suffix-check x "YN"))

;; Greetings.
(defun lex-gr? (x)
  (in-package-suffix-check x "GR"))

;; Lexical sentence (for implicit sentences, e.g. {ref}.sent).
(defun lex-sent? (x)
  (in-package-suffix-check x "SENT"))

(defun lex-tense? (x)
  (declare (optimize (speed 1)))
  (in-ulf-lib (x y) (member y *tense*)))

(defun lex-detformer? (x)
  (declare (optimize (speed 1)))
  (in-ulf-lib (x y) (member y *detformer*)))

(defun litstring? (x)
  (stringp x))

(defun lex-equal? (x)
  (in-ulf-lib (x y) (equal y '=)))
(defun lex-set-of? (x)
  (in-ulf-lib (x y) (equal y 'set-of)))
(defun lex-macro? (x)
  (in-ulf-lib (x y) (member y '(qt-attr sub rep n+preds np+preds voc voc-O))))
(defun lex-macro-hole? (x)
  (in-ulf-lib (x y) (member y '(*h *p *qt *s *ref))))

(defun lex-verbaux? (x)
  (or (lex-verb? x) (aux? x)))

(defun lex-pasv? (x)
  (in-ulf-lib (x y) (eql y 'pasv)))

(defun lex-possessive-s? (x)
  (in-ulf-lib (x y)
    (or (eql y '|'S|)
        (equal '(quote s) y))))

; TODO(gene): fill this in further.
(defun lex-invertible-verb? (x)
  (in-ulf-lib (x y) (member y '(make.v have.v))))

;; Returns true if the token is one that was elided in the surface string.
;; In the current guidelines, this is a curly bracketed token.
;; e.g. {that}, {he}.pro
(defun lex-elided? (intoken)
  (in-ulf-lib (intoken token)
    (multiple-value-bind (word _) (split-by-suffix token)
      (declare (ignore _))
      (let ((wchars (util:split-into-atoms word))
            (tchars (util:split-into-atoms token)))
        ;; Either the whole thing is wrapped in curly brackets or everything but
        ;; the suffix.
        (or
          (and (equal (intern "{") (first wchars))
               (equal (intern "}") (car (last wchars))))
          (and (equal (intern "{") (first wchars))
               (equal (intern "}") (car (last tchars)))))))))

;; Returns t if 'token' is an atomic ULF element that has a corresponding token
;; in the surface string and return nil otherwise.  e.g.,
;;   man.n -> t
;;   that -> t
;;   tht -> nil
;;   k -> nil
;;   to -> t
;;   perf -> t
;;   {man}.n -> nil
;;   2 -> t
;;   "a" -> t
(defun surface-token? (intoken+)
  (in-ulf-lib (intoken+ token+)
    (let ((token (safe-intern token+ :ulf-lib)))
      (or (not (symbolp token))
          (and (has-suffix? token)
               (not (lex-elided? token))
               (not (lex-hole-variable? token)))
          (is-strict-name? token)
          (member token '(that not and or to most some all every whether if))))))


;; Returns true if the token is a hole variable:
;;  e.g. *h, *p, *s, *ref
;; Include those that have been augmented with other type information,
;;  e.g. *h.n, *h.pro, [*h.n], [*h].pro
(defun lex-hole-variable? (intoken)
  (in-ulf-lib (intoken token)
    (multiple-value-bind (word _) (split-by-suffix (unhide-ttt-ops token))
      (declare (ignore _))
      (let ((wchars (util:split-into-atoms (unhide-ttt-ops word))))
        ;; The first character is a *.
        (equal '* (first wchars))))))

;; Returns a string containing the semantic type of a given atomic ULF expression.
(defun atom-semtype? (expr)
  (cdr (assoc (util:atom2str expr) *semtypes*
              :test (lambda (x y) (string-equal (scan-to-strings y x) x)))))

;; TODO: move this into a separate util.lisp file where we'll put other utility
;; processing functions.
;; Makes an elided ulf token explicit (e.g. {he}.pro -> he.pro)
(defun make-explicit! (intoken)
  (let ((pkg (symbol-package intoken)))
    (in-ulf-lib (intoken token)
      (multiple-value-bind (word suffix) (split-by-suffix token)
        (let ((wchars (util:split-into-atoms word))
              (tchars (util:split-into-atoms token)))
          ;; Either the whole thing is wrapped in curly brackets or everything but
          ;; the suffix.
          (cond
            ;; Everything before suffix is in curly brackets.
            ((and (equal (intern "{") (first wchars))
                  (equal (intern "}") (car (last wchars))))
             (add-suffix (fuse-into-atom (subseq wchars 1 (1- (length wchars)))
                                         :pkg pkg)
                                         suffix))
            ;; The whole this is in curly brackets.
            ((and (equal (intern "{") (first wchars))
                  (equal (intern "}") (car (last tchars))))
             (add-suffix (fuse-into-atom (subseq wchars 1) :pkg pkg)
                         (fuse-into-atom (subseq tchars 0 (1- (length tchars))))))
            ;; Not elided just return.
            (t intoken)))))))

