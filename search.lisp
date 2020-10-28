
(in-package :ulf-lib)

;; TODO: clean up the interning in these functions.

(defun marked-conjugated-vp-head? (inx)
  (util:in-intern (inx x :ulf-lib)
    (or (and (symbolp x)
             (multiple-value-bind (word suffix) (ulf:split-by-suffix x)
               (declare (ignore word))
               (eq suffix 'conjugated-vp-head)))
        (and (listp x) (= 2 (length x))
             (lex-tense? (first x)) (marked-conjugated-vp-head? (second x))))))

(defun search-vp-head (invp &key (sub nil) (callpkg *package*))
;``````````````````````
; Searches vp (a ULF VP) for the head, which is either the main verb or
; auxiliary/perf/prog acting over the VP. If sub is not nil, sub substitutes
; for the vp head.
;
; Returns the following values in a list
;   vp head
;   whether is was found
;   new vp
  (inout-intern (invp vp :ulf-lib :callpkg callpkg)
    (cond
      ;; Already marked conjguated VP head.
      ((marked-conjugated-vp-head? vp)
       (values vp t (if sub sub vp)))
      ;; Simple tensed or not, lexical or passivized verb.
      ((ttt:match-expr '(!1 lex-verbaux? pasv-lex-verb?
                            (ulf:lex-tense? (! lex-verbaux? pasv-lex-verb?)))
                       vp)
       (values vp t (if sub sub vp)))
      ;; Starts with a verb or auxiliary -- recurse into it.
      ((and (listp vp)
            (or (verb? (car vp)) (tensed-verb? (car vp)) (tensed-aux? (car vp))))
       (multiple-value-bind (hv found new-carvp) (search-vp-head (car vp)
                                                                 :sub sub
                                                                 :callpkg callpkg)
         (values hv found (cons new-carvp (cdr vp)))))
      ;; Starts with adv-a or phrasal sentence operator -- recurse into cdr.
      ((and (listp vp)
            (or (adv-a? (car vp)) (phrasal-sent-op? (car vp))))
       (multiple-value-bind (hv found new-cdrvp) (search-vp-head (cdr vp)
                                                                 :sub sub
                                                                 :callpkg callpkg)
         (values hv found (cons (car vp) new-cdrvp))))
      ;; Otherwise, it's not found.
      (t (values nil nil vp)))))


(defun find-vp-head (vp &key (callpkg *package*))
;````````````````````
; Finds the main verb in a ULF VP.
  (search-vp-head vp :callpkg callpkg))


(defun replace-vp-head (vp sub &key (callpkg *package*))
;```````````````````````
; Find the main verb and returns a new VP with the substitute value.
  (multiple-value-bind (_1 _2 newvp) (search-vp-head vp
                                                     :sub sub
                                                     :callpkg callpkg)
    (declare (ignore _1))
    (declare (ignore _2))
    newvp))

(defun search-np-head (innp &key (sub nil) (callpkg *package*))
; Searches np (a ULF NP) for the head noun. If sub is not nil, sub substitutes
; for the head noun.
;
; Returns the following values in a list
;   head noun
;   whether it was found
;   new np
; This function treats 'plur' as part of the noun.
  (inout-intern (innp np :ulf-lib :callpkg callpkg)
    (cond
      ;; Simple lexical or plural case.
      ((or (lex-noun? np) (lex-name? np))
       (values np t (if sub sub np)))
      ;; Basic pluralized case.
      ((and (listp np) (= (length np) 2) (equal 'plur (first np))
            (or (lex-noun? (second np)) (lex-name-pred? (second np))))
       (values np t (if sub sub np)))
      ;; Pluralized relational noun case.
      ((and (listp np) (= (length np) 2)
            (equal 'plur (first np)) (listp (second np)))
       (values (list 'plur (first (second np))) t
               (if sub (list sub (cdr (second np)))
                 np)))
      ;; Noun post-modification.
      ;;   (n+preds ...)
      ;;   (n+post ...)
      ((ttt:match-expr (list (list '! 'n+preds 'n+post)
                             'noun? '_+) np)
       (let ((macro (first np))
             (inner-np (second np))
             (post (cddr np)))
         (multiple-value-bind (hn found new-inner-np) (search-np-head inner-np :sub sub :callpkg callpkg)
           (values hn found (cons macro (cons new-inner-np post))))))
      ;; Noun premodification.
      ;;  (dog.n monster.n)
      ;;  (happy.a fish.n)
      ;;  ((mod-n happy.a) fish.n)
      ;;  (|Rochester| landscape.n)
      ((ttt:match-expr '((! mod-n? noun? adj? term?) noun?) np)
       (let ((modifier (first np))
             (inner-np (second np)))
         (multiple-value-bind (hn found new-inner-np) (search-np-head inner-np :sub sub :callpkg callpkg)
           (values hn found (list modifier new-inner-np)))))
      ;; Phrasal sent op.
      ;;   (definitely.adv-s table.n)
      ;;   (not thing.n)
      ((ttt:match-expr '(phrasal-sent-op? noun?) np)
       (multiple-value-bind (hn found new-inner-np) (search-np-head (second np) :sub sub :callpkg callpkg)
         (values hn found (list (first np) new-inner-np))))
      ;; Otherise, noun followed by other stuff.
      ;;   (collapse.n (of.p-arg (the.d empire.n)))
      ((and (listp np) (noun? (first np)))
       (multiple-value-bind (hn found new-inner-np) (search-np-head (first np) :sub sub :callpkg callpkg)
         (values hn found (cons new-inner-np (cdr np)))))
      ;; most-n.
      ((and (listp np) (= (length np) 3) (eq (first np) 'most-n))
       (multiple-value-bind (hn found new-inner-np) (search-np-head (third np) :sub sub :callpkg callpkg)
         (values hn found (list (first np) (second np) new-inner-np))))
      ;; If none of these, we can't find it.
      (t (values nil nil np)))))


(defun find-np-head (np &key (callpkg *package*))
;````````````````````
; Finds the main verb in a ULF np.
  (search-np-head np :callpkg callpkg))


(defun replace-np-head (np sub &key (callpkg *package*))
;```````````````````````
; Find the main verb and returns a new np with the substitute value.
  (multiple-value-bind (_1 _2 newnp) (search-np-head np
                                                     :sub sub
                                                     :callpkg callpkg)
    (declare (ignore _1))
    (declare (ignore _2))
    newnp))

(defun search-ap-head (inap &key (sub nil) (callpkg *package*))
;````````````````````````````````````````
; Searches the adjective phrase for the head.
;
; Returns the following values
;  head adjective
;  whether it was found
;  new adjective phrase
  (inout-intern (inap ap :ulf-lib :callpkg callpkg)
    (cond
      ;; Simple lexical case.
      ((lex-adjective? ap) (values ap t (if sub sub ap)))
      ;; Adjective premodification.
      ((ttt:match-expr '((! mod-a? adj? noun?) (* phrasal-sent-op?) adj?) ap)
       (let ((mods (butlast ap))
             (inner-ap (car (last ap))))
         (multiple-value-bind (ha found new-inner-ap) (search-ap-head inner-ap :sub sub :callpkg callpkg)
           (values ha found (append mods (list new-inner-ap))))))
      ;; Adjective post-modification/arguments.
      ((ttt:match-expr '(adj? (+ mod-a? term? p-arg? phrasal-sent-op?)) ap)
       (let ((inner-ap (car ap))
             (modargs (cdr ap)))
         (multiple-value-bind (ha found new-inner-ap) (search-ap-head inner-ap :sub sub :callpkg callpkg)
           (values ha found (cons new-inner-ap modargs)))))
      ;; Starting with phrasal sent ops.
      ((and (listp ap) (phrasal-sent-op? (car ap)))
       (multiple-value-bind (ha found new-cdrap) (search-ap-head (cdr ap) :sub sub :callpkg callpkg)
         (values ha found (cons (car ap) new-cdrap))))
      ;; Starts with an adjective.
      ((and (listp ap) (adj? (car ap)))
       (multiple-value-bind (ha found new-carap) (search-ap-head (car ap) :sub sub :callpkg callpkg)
         (values ha found (cons new-carap (cdr ap)))))
      ;; Otherwise, not found.
      (t (values nil nil ap)))))


(defun find-ap-head (ap &key (callpkg *package*))
;````````````````````
; Finds the main verb in a ULF ap.
  (search-ap-head ap :callpkg callpkg))


(defun replace-ap-head (ap sub &key (callpkg *package*))
;```````````````````````
; Find the main verb and returns a new ap with the substitute value.
  (multiple-value-bind (_1 _2 newap) (search-ap-head ap
                                                     :sub sub
                                                     :callpkg callpkg)
    (declare (ignore _1))
    (declare (ignore _2))
    newap))

