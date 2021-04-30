;; ULF phrase patterns identified in TTT.

(in-package :ulf-lib)

;; Ensures that the input symbol is in ulf-lib and the output symbol is in
;; callpkg.
(defmacro inout-ulf-lib ((x y &key (callpkg nil)) &body body)
  `(gute:inout-intern (,x ,y :ulf-lib :callpkg ,callpkg)
                      ,@body))

(defparameter *ttt-noun*
  '(! lex-noun?
      lex-name-pred?
      (plur noun?)
      ;; Explicit predicate modifiers.
      (mod-n? noun?)
      (noun? mod-n?)
      ;; Implicit predicate modifiers.
      (adj? noun?)
      (noun? noun?)
      (lex-name? noun?)
      (term? noun?)
      ; (mother-of.n |John|)
      (lex-rel-noun? term?)
      (noun? p-arg?)
      (lex-function? term?)
      (n+preds noun? (+ pred?))
      ((+ noun?) lex-coord? noun?)
      (phrasal-sent-op? noun?)
      (n+post noun? (+ pred? term? adv? p-arg? unknown?))
      (= term?)

      ;; Fall back if arguments not correctly analyzed.
      (n+preds _+)
      ))

(defparameter *ttt-adj*
  '(! lex-adjective?
      ;; Implicit predicate modification.
      (adj? adj?)
      (noun? adj?)
      ;; Explicit predicate modification.
      ; GK: old version where adjectives could be modified by adv-a
      ;(adv-a? adj?)
      ;(adj? adv-a?)
      (mod-a? adj?)
      (adj? mod-a?)
      (poss-by term?)
      ;; Some adjectives take infinitives.
      (adj? (to verb?))
      ;; Some adjectives take two arguments.
      ;; "jewelry worth $400"
      (lex-adjective? term?)
      ;; Some adjectives take arguments with prepositions.
      (adj? p-arg?)
      ;; A single argument with unlimited additional modifiers.
      ((* mod-a?) adj? (* mod-a?) (! p-arg? (to verb?)) (* mod-a?))
      ((* mod-a?) lex-adjective? (* mod-a?) term? (* mod-a?))
      ((* mod-a?) adj? (+ mod-a?))
      ((+ mod-a?) adj? (* mod-a?))
      ;; Coordination.
      ((+ adj?) lex-coord? (+ adj?))
      (phrasal-sent-op? adj?)
      ;; Equal sign with term.
      (= term?)
      ))

;; Adverbials.

(defparameter *ttt-adv-a*
   '(! lex-adv-a?
      (adv-a pred?)
      ;; Below is not quite correct since some *.pq map to (adv-e ...), but for
      ;; the sake of this syntax checker it doesn't matter.
      lex-pq?
      ((+ adv-a?) lex-coord? (+ adv-a?))
      ))

(defparameter *ttt-adv-e*
   '(! lex-adv-e?
      (adv-e pred?)
      ((+ adv-e?) lex-coord? (+ adv-e?))
      ))

(defparameter *ttt-adv-s*
   '(! lex-adv-s?
      (adv-s pred?)
      ((+ adv-s?) lex-coord? (+ adv-s?))
      (|(| _+ |)|)))

(defparameter *ttt-adv-f*
   '(! lex-adv-f?
      (adv-f pred?)
      ((+ adv-f?) lex-coord? (+ adv-f?))
      ))

;; Predicate modifiers.
(defparameter *ttt-mod-a*
  '(! lex-mod-a?
      (mod-a pred?)))

(defparameter *ttt-mod-n*
  '(! lex-mod-n?
      (mod-n pred?)
      (nnp term?)))

;; Prepositional Phrase.
(defparameter *ttt-pp*
   '(! lex-pp?
       (lex-p? term?)
       ((+ pp?) lex-coord? (+ pp?))
       (phrasal-sent-op? pp?)
       ;; "just outside Boston" -- (just.mod-a (outside.p |Boston|))
       (mod-a? pp?)
       (pp? mod-a?)

       ;; Fall back, anything starting with *.p
       (lex-p? _+)
       ))

;; This needs its own category because it can't act generally as terms or
;; predicates.  It has to be an argument to something.
(defparameter *ttt-p-arg*
  '(! (lex-p-arg? term?)
      (lex-p-arg? pred?)
      (adv-s? p-arg?)))

;; Terms.
(defparameter *ttt-term*
  '(! lex-pronoun?
      lex-name?
      lex-number?
      lex-rel?
      (det? noun?)
      (lex-set-of? (+ term?))
      ((+ term?) lex-coord? (+ term?))
      ;; Reified
      (noun-reifier? noun?)
      (verb-reifier? verb?)
      (sent-reifier? sent?)
      (tensed-sent-reifier? tensed-sent?)
      ;; Domain specific syntax.
      (ds _! litstring?)
      ;; Possessive macro.
      (preposs-macro? noun?)
      ;; np+preds.
      (np+preds term? (+ pred?))
      ;; Object quoted expression.
      (|"| _+ |"|) ; same as (\" .. \"), but breaks the syntax highlighting less.

      ;; FALL BACK ANALYSIS.
      ;; Fall back if np+preds arguments not analyzed correctly.
      (np+preds _+)
      ((_!1 's) _!2)
      ;; Fall back for reifiers.  Assume any expression starting with a reifier
      ;; is a term.
      (noun-reifier? _+)
      (verb-reifier? _+)
      (sent-reifier? _+)
      (tensed-sent-reifier? _+)
      ;; Fall back on determiners and set-of generating terms..
      (lex-set-of? _+)
      (! (det? _+) ~ (* det?) lex-coord? (+ det?))

      ;; Internal plurality representation.
      (plur-term term?)

      ;; Rather than building a whole set of types corresponding to versions
      ;; with the hole contained, I'll just check it dynamically.
      [*h]
      [*s]
      [*p]
      [*qt]
      [*ref]))

(defparameter *ttt-verb*
  '(! lex-verb?
      (pasv lex-verb?)
      ((* adv-a?) verb? (+ term? pred? adv-a? p-arg? phrasal-sent-op?))
      (adv-a? (* phrasal-sent-op?) verb?)
      (aux? (* phrasal-sent-op?) verb?)
      ((* verb?) lex-coord? (+ verb?))
      (phrasal-sent-op? verb?)

      ;; FALL BACK ANALYSIS.
      ;; Fall back if arguments not analyzed correctly.
      (verb? _!)
      ))

(defparameter *ttt-pred*
  '(! verb? noun? adj? tensed-verb? pp?
      (lex-rel? pred?)
      (sub lex-rel? tensed-sent?)
      (sub (^* lex-rel?) tensed-sent?)
      relativized-sent?

      ;; FALL BACK ANALYSIS.
      ;; Fall back on arguments not analyzed correctly.
      (lex-rel? _!)
      (sub lex-rel? _!)
      (sub (^* lex-rel?) _!)
      (phrasal-sent-op? pred?)
      ))

(defparameter *ttt-aux*
  '(! lex-aux? perf prog))

(defparameter *ttt-tensed-aux*
  '(lex-tense? aux?))

(defparameter *ttt-tensed-verb*
  '(! (lex-tense? verb?)
      ((* adv-a?) tensed-verb? (+ term? pred? adv-a? p-arg? phrasal-sent-op?))
      (tensed-aux? (* phrasal-sent-op?) verb?)
      (adv-a? (* phrasal-sent-op?) tensed-verb?)
      ((* tensed-verb?) lex-coord? (+ tensed-verb?))
      (phrasal-sent-op? tensed-verb?)
      ))

(defparameter *ttt-det*
  '(! lex-det?
      (lex-detformer? adj?)
      ((* det?) lex-coord? (+ det?))))

(defparameter *ttt-sent*
  '(! (term? verb?)        ; subject verb
      ((+ sent?) lex-coord? (+ sent?)) ; multiple sentences
      (sent-mod? sent?)    ; sentence modifier, sentence
      (sent? sent-mod?)    ; sentence, sentence modifier
      (adv-a? term? verb?) ; action adverb, subject, verb
      (sent? sent-punct?)  ; sentence with punctuation
      (term? = term?)      ; equality
      (term? adj?)         ; not correct on the surface, but correct logically
      (term? noun?)        ; not correct on the surface, but correct logically
      lex-sent?
      ((? sent-mod?) sent? (+ sent? sent-mod?))    ; sentences bracketed together (for semi-colons for example)
      (sent-mod? sent?)
      lex-x?
      ;; Term substitution, (TODO: generalize all the sub handling in this to apply the sub and then analyze)
      (sub term? sent?)
      ))

(defparameter *ttt-tensed-sent*
  '(! ;; Simple sentence.
      (term? tensed-verb?)
      ;; Coordinated sentence.
      ((+ tensed-sent?) lex-coord? (+ tensed-sent?))
      (lex-coord? tensed-sent? (+ tensed-sent?))
      ;; Modified sentence (e.g. curried coordination).
      (sent-mod? tensed-sent?)
      ;; Postfixed sentence modification.
      (tensed-sent? sent-mod?)
      ;; Backwards sentence...
      (tensed-verb? adv-a? term?)
      ;; Punctuated sentence.
      (tensed-sent? sent-punct?)
      ;; Prepositionally coordinated sentences.
      (ps? tensed-sent?)
      (tensed-sent? ps?)
      ;; Inverted auxiliary sentence.
      ((lex-tense? aux?) term? verb?)
      ;; Inverted verb sentence.
      ((lex-tense? lex-invertible-verb?) term? term?)
      ((lex-tenes? be.v) term? pred?)
      ;; Phrasal utterances.
      (pu (! ~ sent? tensed-sent?))
      ;; Multiple sentences stuck together (e.g. some multi-sentence annotations).
      (tensed-sent? (+ tensed-sent?))
      ;; Expletives are treated as tensed sentences.
      lex-x?
      ;; Yes/no expressions are treated as tensed sentences.
      lex-yn?
      ;; Greetings.
      lex-gr?
      (gr _!)
      ;; Implicit sentence marked by single extension.
      lex-sent?
      ;; Term substitution, (TODO: generalize all the sub handling in this to apply the sub and then analyze)
      (sub term? tensed-sent?)
      ))

(defparameter *ttt-sent-mod*
  '(!1 (lex-coord? (!2 tensed-sent? sent?))
       ;; Prepositionally coordinated sentences.
       ps?
       adv-e?
       adv-s?
       adv-f?))

(defparameter *ttt-ps*
  '(!1 (lex-ps? tensed-sent?)
       (mod-a? ps?)))

(defparameter *ttt-preposs-macro*
  '(term? 's))

(defparameter *ttt-voc*
  '(!1
     (voc term?) (voc-O term?)
     ;; FALL BACK.
     (voc _!) (voc-O _1)))

;; Matches the TTT pattern (ttt) to the s-expression (expr) after hiding TTT
;; operators in expr.
(defun hidden-match-expr (ttt expr)
  (ttt::match-expr ttt (hide-ttt-ops expr)))

;; Matches a TTT pattern to a ULF expressions after hiding TTT operators in
;; expr and allowing for substitutions to be completed.
(defun hidden-maybe-sub-expr (ttt expr)
  (or (hidden-match-expr ttt expr)
      (hidden-match-expr ttt (apply-substitution-macros
                               expr
                               :calling-package :ulf-lib))))

(defun noun? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-noun* y)))
(defun adj? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-adj* y)))
(defun adv-a? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-adv-a* y)))
(defun adv-e? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-adv-e* y)))
(defun adv-s? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-adv-s* y)))
(defun adv-f? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-adv-f* y)))
(defun adv? (x) (in-ulf-lib (x y) (or (adv-a? y) (adv-e? y) (adv-s? y) (adv-f? y))))
(defun mod-a? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-mod-a* y)))
(defun mod-n? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-mod-n* y)))
(defun pp? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-pp* y)))
(defun term? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-term* y)))
(defun verb? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-verb* y)))
(defun pred? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-pred* y)))
(defun det? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-det* y)))
(defun aux? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-aux* y)))
(defun tensed-aux? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-tensed-aux* y)))
(defun tensed-verb? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-tensed-verb* y)))
(defun sent? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-sent* y)))
(defun tensed-sent? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-tensed-sent* y)))
(defun sent-mod? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-sent-mod* y)))
(defun ps? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-ps* y)))
(defun preposs-macro? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-preposs-macro* y)))
(defun p-arg? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-p-arg* y)))
(defun voc? (x) (in-ulf-lib (x y) (hidden-maybe-sub-expr *ttt-voc* y)))

;; Memoize the biggies.
(gute:memoize 'noun?)
(gute:memoize 'adj?)
(gute:memoize 'verb?)
(gute:memoize 'adv-a?)
(gute:memoize 'adv-e?)
(gute:memoize 'adv-s?)
(gute:memoize 'adv-f?)
(gute:memoize 'adv?)
(gute:memoize 'pp?)
(gute:memoize 'term?)
(gute:memoize 'pred?)

(defun sent-punct? (x)
  (in-ulf-lib (x y)
             (member y '(! ? .? [!] [?] [.?]))))

;; Reifiers.
(defun noun-reifier? (x)
  (in-ulf-lib (x y)
             (member y '(k))))
(defun tensed-sent-reifier? (x)
  (in-ulf-lib (x y)
             (member y '(that tht whether ans-to))))
(defun sent-reifier? (x)
  (in-ulf-lib (x y)
             (member y '(ke))))
(defun verb-reifier? (x)
  (in-ulf-lib (x y)
             (member y '(ka to gd))))

;; Operator forming type-shifters.
(defun advformer? (x)
  (in-ulf-lib (x y) (member y '(adv-a adv-e adv-s adv-f))))
(defun detformer? (x)
  (in-ulf-lib (x y) (member y '(fquan nquan))))
(defun modformer? (x)
  (in-ulf-lib (x y) (member y '(mod-a mod-n))))

(defun contains-relativizer (x)
  (in-ulf-lib (x y)
             (hidden-match-expr '(^* lex-rel?) y)))
(defun contains-relativizer? (x) (contains-relativizer x)) ;  TODO merge with above.

;; A relativized sentence is a tensed sentence with a relativizer in it.
(defun relativized-sent? (x)
  (in-ulf-lib (x y)
             (and (tensed-sent? y)
                  (contains-relativizer y))))

(defun mod-n-former? (x) (in-ulf-lib (x y) (member y '(mod-n))))
(defun mod-a-former? (x) (in-ulf-lib (x y) (member y '(mod-a))))

(defparameter *type-id-fns*
  (list (list #'noun? 'noun)
        (list #'adj? 'adj)
        (list #'lex-p? 'prep)
        (list #'adv-a? 'adv-a)
        (list #'adv-e? 'adv-e)
        (list #'adv-s? 'adv-s)
        (list #'adv-f? 'adv-f)
        (list #'mod-a? 'mod-a)
        (list #'mod-n? 'mod-n)
        (list #'mod-a-former? 'mod-a-former)
        (list #'mod-n-former? 'mod-n-former)
        (list #'pp? 'pp)
        (list #'term? 'term)
        (list #'verb? 'verb)
        (list #'pred? 'pred)
        (list #'det? 'det)
        (list #'aux? 'aux)
        (list #'tensed-aux? 'tensed-aux)
        (list #'tensed-verb? 'tensed-verb)
        (list #'sent? 'sent)
        (list #'tensed-sent? 'tensed-sent)
        (list #'lex-tense? 'tense)
        (list #'sent-punct? 'sent-punct)
        (list #'sent-mod? 'sent-mod)
        (list #'noun-reifier? 'noun-reifier)
        (list #'verb-reifier? 'verb-reifier)
        (list #'sent-reifier? 'sent-reifier)
        (list #'tensed-sent-reifier? 'tensed-sent-reifier)
        (list #'advformer? 'advformer)
        (list #'detformer? 'detformer)
        (list #'preposs-macro? 'preposs-macro)
        (list #'relativized-sent? 'rel-sent)
        (list #'p-arg? 'p-arg)
        (list #'voc? 'voc)
        ;; Purely lexical types.
        (list #'lex-equal? 'equal-sign)
        (list #'lex-set-of? 'set-of-op)
        (list #'lex-macro? 'macro-symbol)
        (list #'lex-ps? 'sent-prep)
        (list #'lex-coord? 'coordinator)
        (list #'lex-pasv? 'pasv)
        (list #'lex-possessive-s? 'possessive-s)
        ))

(defun type-shifter? (inx &key (callpkg nil))
  (inout-ulf-lib (inx x :callpkg callpkg)
    (or (noun-reifier? x)
        (verb-reifier? x)
        (sent-reifier? x)
        (tensed-sent-reifier? x)
        (mod-n-former? x)
        (mod-a-former? x)
        (advformer? x)
        (detformer? x))))

;; Hypothesizes the type of the given ULF formula.
(defun phrasal-ulf-type? (inx &key (callpkg nil))
  (inout-ulf-lib (inx x :callpkg callpkg)
    (let ((matched (remove-if-not
                     #'(lambda (pair)
                         (apply (the function (first pair)) (list x)))
                     *type-id-fns*)))
      (if matched
        (mapcar #'second matched)
        '(unknown)))))
(defun unknown? (inx &optional (callpkg nil))
  (inout-ulf-lib (inx x :callpkg callpkg)
    (equal '(unknown) (phrasal-ulf-type? x))))

;; Labels formula with the hypothesized types.
;; TODO: Merging this function with 'phrasal-ulf-type?' to get all types in one bottom
;;       up fashion would speed this up a lot.
(defun label-formula-types (rawf &key (callpkg nil))
  (inout-ulf-lib (rawf f :callpkg callpkg)
    (cond
      (;; Atom or quoted expression (which techinically is a cons).
       (or (atom f) 
           (and (consp f) (eq 'quote (car f)))) f)
      (t (list (cons 'types (phrasal-ulf-type? f))
               (mapcar #'label-formula-types f))))))

;; Condition to check if an element is a filitered sentence-level operator.
;; Basically all sentence-level operators that are written within the phrase in
;; the surface form.
(defun phrasal-sent-op? (x)
  (in-ulf-lib (x e)
             (or
               (adv-e? e)
               (adv-s? e)
               (adv-f? e)
               (member e '(not not.adv-e not.adv-s))
               (ps? e)
               ;; Weaker verion of (*.ps x) matching. Don't require it to be a
               ;; tensed sentence even though that's what's required in the
               ;; type.
               (and (listp x) (> (length x) 1)
                    (lex-ps? (first x))))))

