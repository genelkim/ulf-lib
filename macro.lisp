;;; Gene Louis Kim
;;; 11-19-2018
;;;
;;; ULF macro handling code.

(in-package :ulf-lib)

;;; Returns t if 'ulf' contains *h, nil otherwise.
(defun contains-hole (inulf &key (inholevar '*h))
  (util:in-intern (inulf ulf :ulf-lib)
    (util:in-intern (inholevar holevar :ulf-lib)
                     (cond
                       ((and (atom ulf) (eq holevar ulf)) t)
                       ((atom ulf) nil)
                       (t (or (contains-hole (car ulf) :inholevar holevar)
                              (contains-hole (cdr ulf) :inholevar holevar)))))))

;;; Applies all the sub macros in the given ULF.
;;; Returns a pair of values (success, results)
;;; If fail-on-bad-use is t, this function returns
;;; nil for 'success' and returns the level at which
;;; it failed in 'results'.
;;;
;;; If 'fail-on-bad-use' is nil, the system will always
;;; return true and naively apply the 'sub' macro.
;;;
;;; Examples of bad use:
;;;   'sub' does not have exactly 2 arguments
;;;   *h is not present in the second argument
(defun apply-sub-macro (inulf &key (fail-on-bad-use nil) (calling-package nil))
  (let
    ((initial-result
       (multiple-value-list
         (util:in-intern
           (inulf ulf :ulf-lib)
           (cond
             ((atom ulf) (values t ulf))
             ;; If sub and less than 2 arguments, fail.
             ((and (eq (first ulf) 'sub) (< (length ulf) 3))
              (return-from apply-sub-macro (values nil ulf)))
             ;; If sub, recurse into the second arg, then try to apply.
             ((eq (first ulf) 'sub)
              (if (and fail-on-bad-use (not (equal (length ulf) 3)))
                (return-from apply-sub-macro (values nil ulf)))

              (let* ((leftrec
                       (multiple-value-list
                         (apply-sub-macro (second ulf) :fail-on-bad-use fail-on-bad-use)))
                     (rightrec
                       (multiple-value-list
                         (apply-sub-macro (third ulf) :fail-on-bad-use fail-on-bad-use)))
                     (lrsuc (first leftrec))
                     (lrres (second leftrec))
                     (rrsuc (first rightrec))
                     (rrres (second rightrec)))
                (cond
                  ;; If the recursion failed, propagate results.
                  ((not lrsuc) (values lrsuc lrres))
                  ((not rrsuc) (values rrsuc rrres))
                  ;; If the recrusive result doesn't have a *h, return with failure.
                  ((and fail-on-bad-use (not (contains-hole rrres)))
                   (values nil (list (first ulf) lrres rrres)))
                  ;; Apply substitution and return result.
                  (t (values t (subst lrres '*h rrres))))))
             ;; Otherwise, just recursive into all.  If there's a failure, return
             ;; it. Otherwise, merge together.
             (t (let* ((recres (mapcar #'(lambda (x)
                                           (multiple-value-list
                                             (apply-sub-macro x :fail-on-bad-use fail-on-bad-use)))
                                       ulf))
                       (successes (mapcar #'first recres))
                       (fs (mapcar #'second recres)))
                  (if (reduce #'(lambda (x y) (and x y)) successes)
                    (values t fs) ; If all recursion succeeded, return the results with 't'.
                    ; Otherwise, find the first one that failed and return it.
                    (let ((failpos (position nil successes)))
                      (values nil (nth failpos fs))))))))))
     ) ; end of let definitions.

    (if calling-package
      (values (first initial-result)
              (util:intern-symbols-recursive (second initial-result) calling-package))
      (values (first initial-result) (second initial-result)))))


;; Assumes there's at most one occurrence of var in curulf.
(defun add-info-to-var (curulf var srculf)
  ;; TODO: For now just take the first type (we should use a
  ;; hierarchy of types to select the most specific one.
  (let* ((typ (first (ulf-type? srculf)))
         (typeadded (add-suffix var (suffix-for-type typ)))
         (replacement
           (cond
             ((plur-noun? srculf) (list 'plur typeadded))
             ((plur-term? srculf) (list 'plur-term typeadded))
             (t typeadded))))
    (subst replacement var curulf)))


;; Add types, pluralization, etc. to the variables *h for sub macros.
(defun add-info-to-sub-vars (inulf &key (calling-package nil))
  (let
    ((initial-result
       (util:in-intern (inulf ulf :ulf-lib)
         (labels
           (;; Main recursive function that does all the heavy lifting.
            (recfn (curulf)
              (cond
                ((atom curulf) curulf)
                ;; If sub has less than 2 arguments, just recurse.
                ((and (eq (first curulf) 'sub) (< (length curulf) 3))
                 (recfn (second curulf)))
                ;; If sub, recurse into both arguments, then add type to variable in
                ;; second argument.
                ((eq (first curulf) 'sub)
                 (let* ((leftrec (recfn (second curulf)))
                        (rightrec (recfn (third curulf))))
                   (list 'sub
                         leftrec
                         (add-info-to-var rightrec '*h leftrec))))
                ;; Otherwise, just recurse.
                (t (mapcar #'recfn curulf)))))

           ;; Main body of add-info-to-sub-vars.
           (recfn ulf)))))
    ;; Intern to calling package.
    (if calling-package
      (util:intern-symbols-recursive initial-result calling-package)
      initial-result)))


;; This assumes that there's at most one relativizer in curulf.
(defun add-info-to-relativizer (curulf srculf)
  (let* ((origrel (first (util:tree-find-if curulf #'lex-rel?)))
         (replacement
          ;; Right now, all we care about is plurality.
          (cond
            ((or (plur-term? srculf) (plur-noun? srculf))
             (list 'plur-term origrel))
            (t origrel))))
    (if origrel
      (subst replacement origrel curulf)
      curulf)))


;; Adds pluralization, etc. to the relativizers in relative clauses.
(defun add-info-to-relativizers (inulf &key (calling-package nil))
  (let
    ((initial-result
       (util:in-intern (inulf ulf :ulf-lib)
         (labels
           (;; Main recursive function.
            (recfn (curulf)
              (cond
                ((atom curulf) curulf)
                ;; If n+preds, n+post, np+preds, recurse into elements then take
                ;; apply the info of the first arg to all of the other args.
                ((and (member (first curulf) '(n+preds n+post np+preds))
                      (> (length curulf) 2))
                 (let* ((recvals (mapcar #'recfn curulf))
                        (macro (first recvals))
                        (headexpr (second recvals))
                        (postmods (cddr recvals)))
                   (cons macro
                         (cons headexpr
                               (mapcar #'(lambda (expr)
                                           (add-info-to-relativizer expr headexpr))
                                       postmods)))))
                ;; Otherwise, just recurse.
                (t (mapcar #'recfn curulf)))))
           ;; Main body.
           (recfn ulf)))))
    ;; Intern to calling package.
    (if calling-package
      (intern-symbols-recursive initial-result calling-package)
      initial-result)))


;;; Takes a sentence of the form
;;; ((<tense> verb/aux) NP VP ADV1 .. ADVn)
;;; and transforms it to
;;; (NP ((((<tense> verb/aux) VP) ADV1) ... ADVn))
;;;
;;; TODO: need to do something fancier, like identifying continuous adverbs
;;; from the end and wrapping those.  Otherwise, supply the arguments flat.
;;; For example: "Made he a table for John?"
;;;   (((past make.v) he.pro (a.d table.n) (for.p-arg |John|)) ?)
(defun uninvert-verbaux! (ulf)
  (if (< (length ulf) 3)
    (return-from uninvert-verbaux! nil))
  (let ((headva (first ulf))
        (np (second ulf))
        (vp (third ulf))
        (remain (cdddr ulf)))
    (list np
          (reduce #'list remain :initial-value (list headva vp)))))

;;; Rules to uninvert sentences with verb/auxiliary inversion, e.g. questions.
;;; This assumes that any interleaved sentence operators have been lifted out.
(defparameter *ttt-uninvert-verbaux*
  '(
    ;; If it's an inverted auxiliary, eagerly uninvert.
    (/ ((lex-tense? lex-aux?) term? _+)
       (uninvert-verbaux! ((lex-tense? lex-aux?) term? _+)))
    ;; If it's a verb, only uninvert if we're in a question.
    ;; TODO: this actually needs to be more sophisticated.  We can look at the
    ;; composition of the inverted and uninverted versions and choose the one
    ;; that is coherent.
    (/ (((lex-tense? lex-verb?) term? _+) [?])
       ((uninvert-verbaux! ((lex-tense? lex-verb?) term? _+)) [?]))))

;; Function to uninvert verbauxes from a given ULFs.
(defun uninvert-verbauxes (ulf)
  (let ((ttthidden (hide-ttt-ops ulf)))
    (util:unhide-ttt-ops
      (ttt:apply-rules *ttt-uninvert-verbaux*
                       ttthidden
                       :max-n 500
                       :trace nil))))

;; Lift adv-a that are mixed in with verb arguments.
(defparameter *ttt-lift-adv-a*
  '((/ ((! verb? tensed-verb?) _+1 adv-a? _*2)
       (adv-a? (! _+1 _*2)))
    (/ ((! verb? tensed-verb?) _*1 adv-a? _+2)
       (adv-a? (! _*1 _+2)))))
(defun lift-adv-a (ulf)
  (let ((ttthidden (hide-ttt-ops ulf)))
    (util:unhide-ttt-ops
      (ttt:apply-rules *ttt-lift-adv-a*
                       ttthidden))))


(defun apply-qt-attr-macro (inulf &key (calling-package nil))
  (labels
    ((rec-helper (ulf)
       ;(princln "In rec-helper")
       ;(princln ulf)
       (cond
         ((atom ulf) (values t ulf nil))
         ;; If possible to apply (\" (.. (qt-attr (.. *qt ..)) ..) \"),
         ;; recurse and try to apply.
         ((and (<= 3 (length ulf))
               (eq (first ulf) '|"|)
               (eq (car (last ulf)) '|"|))
          (multiple-value-bind (suc res qt-attr) (rec-helper
                                                   (if (= 3 (length ulf))
                                                     (second ulf)
                                                     (subseq ulf 1 (1- (length ulf)))))
            ;(princln "suc")
            ;(princln suc)
            ;(princln "res")
            ;(princln res)
            ;(princln "qt-attr")
            ;(princln qt-attr)
            (cond
              ;; Apply transformation.
              (qt-attr (values t
                               (subst (list '|"| res '|"|)
                                      '*qt
                                      qt-attr)
                               nil))
              ;; Otherwise just return
              (t (values suc (list '|"| res '|"|) qt-attr)))))
         ;; If starting with 'qt-attr, recurse, but then return recursive result
         ;; in the last slot and return nil for the main clause.
         ((and (= 2 (length ulf))
               (eq (first ulf) 'qt-attr)
               (contains-hole (second ulf) :inholevar '*qt))
          (multiple-value-bind (suc res qt-attr) (rec-helper (second ulf))
            (values suc nil res)))
         ;; TODO: handle malformed cases (e.g. (\" ... \") is more than length 3
         ;;                                    (qt-attr ..) is more than length 2)
         ;; Otherwise just recursive into everything, filter nil in recursive result.
         (t
           (let* ((recres (mapcar #'(lambda (x) (multiple-value-list (rec-helper x)))
                                  ulf))
                  (sucs (mapcar #'first recres))
                  (ress (remove-if #'null (mapcar #'second recres)))
                  (qt-attrs (mapcar #'third recres)))
             (values (every #'identity sucs) ress (some #'identity qt-attrs)))))))
       ;; TODO: return untransformed version on failure...
    (let
      ((initial-result (multiple-value-list (util:in-intern
                                              (inulf ulf :ulf-lib)
                                              (rec-helper ulf)))))
      (cond
        ;; Failure in qt-attr or qt-attr not , so just return input.
        ((or (not (first initial-result))
             (third initial-result))
         inulf)
        ;; Intern to given package.
        (calling-package  (values (first initial-result)
                                  (util:intern-symbols-recursive (second initial-result)
                                                                 calling-package)))
        ;; Keep ulf-lib pacakge.
        (t (values (first initial-result) (second initial-result)))))))

