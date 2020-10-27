;;; Gene Louis Kim
;;; 11-19-2018
;;;
;;; ULF macro handling code.

(in-package :ulf-lib)

;;; Returns t if 'ulf' contains *h, nil otherwise.
(defun contains-hole (inulf &key (inholevar '*h))
  (gute:in-intern (inulf ulf :ulf-lib)
    (gute:in-intern (inholevar holevar :ulf-lib)
                     (cond
                       ((and (atom ulf) (eq holevar ulf)) t)
                       ((atom ulf) nil)
                       (t (or (contains-hole (car ulf) :inholevar holevar)
                              (contains-hole (cdr ulf) :inholevar holevar)))))))


;;; General function for variable insertion macros. Given a macro name, variable
;;; symbol, a context selector function, and insertion selector function, returns
;;; a function that applies this macro to a given ulf.
;;; For example:
;;;  macro-name: 'sub
;;;  var-sym: '*h
;;;  context-selector-fn: #'third
;;;  insertion-selector-fn: #'second
;;;  bad-use-fn: (lambda (x) (not (and (listp x) (eq 'sub (first x)) (equal (length x) 3))))
;;; Returns a function that applies all 'sub macros.
(declaim (ftype (function (symbol symbol function function function)
                          function)
                var-insertion-macro))
(defun var-insertion-macro (macro-name var-sym
                                       context-selector-fn
                                       insertion-selector-fn
                                       bad-use-fn)
  (declare (type (function ((or list atom)) t) bad-use-fn))
  (labels
    (;; Helper function that does all the heavy lifting.
     (macro-expand-fn (ulf fail-on-bad-use)
       (cond
         ((atom ulf) (values t ulf))
         ;; If macro-name and not the right use arguments, fail.
         ;((and (eq (first ulf) macro-name) (< (length ulf) 3))
         ; (return-from macro-expand-fn (values nil ulf)))
         ;; If macro-name, recurse into all the args and try to apply..
         ((and (eq (first ulf) macro-name)
               fail-on-bad-use
               (funcall bad-use-fn ulf))
            (values nil ulf))
         ;; Macro name otherwise is will be applied.
         ((eq (first ulf) macro-name)
          ;; Recurse into rest and apply the macro using the
          ;; context-selector-fn and insertion-selector-fn.
          (let* ((recres
                   (mapcar #'(lambda (x) (multiple-value-list
                                           (macro-expand-fn x
                                                            fail-on-bad-use
                                                            )))
                           ulf))
                 (sucs (mapcar #'first recres))
                 (ress (mapcar #'second recres)))

            (cond
              ;; If the recursion failed, propagate results.
              ((not (every #'identity sucs))
               (values nil ress))
              ;; If the recrusive result doesn't have a hole, return with failure.
              ((and fail-on-bad-use
                    (not (some #'(lambda (x)
                                   (contains-hole x :inholevar var-sym))
                               ress)))
               (values nil ress))
              ;; Apply substitution and return result.
              (t (values t (subst (funcall insertion-selector-fn ress)
                                  var-sym
                                  (funcall context-selector-fn ress)))))))
         ;; Otherwise, just recursive into all.  If there's a failure, return
         ;; it. Otherwise, merge together.
         (t (let* ((recres (mapcar #'(lambda (x)
                                       (multiple-value-list
                                         (macro-expand-fn x
                                                          fail-on-bad-use
                                                          )))
                                   ulf))
                   (sucs (mapcar #'first recres))
                   (ress (mapcar #'second recres)))
              (if (every #'identity sucs)
                (values t ress) ; If all recursion succeeded, return the results with 't'.
                ; Otherwise, find the first one that failed and return it.
                (let ((failpos (position nil sucs)))
                  (values nil (nth failpos ress))))))))
     ) ; end labels defns
    ;; Return the macro expand fn.
    #'(lambda (inulf fail-on-bad-use calling-package)
       (let ((initial-result
               (multiple-value-list
                 (gute:in-intern
                   (inulf ulf :ulf-lib)
                   (macro-expand-fn ulf fail-on-bad-use)))))
         (if calling-package
           (values (first initial-result)
                   (gute:intern-symbols-recursive (second initial-result) calling-package))
           (values (first initial-result) (second initial-result)))))))


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
  (let ((sub-fn
          (var-insertion-macro
            'ulf-lib::sub 'ulf-lib::*h #'third #'second
            #'(lambda (x) (not (and (listp x) (eq 'ulf-lib::sub (first x)) (equal (length x) 3)))))))
    (funcall sub-fn inulf fail-on-bad-use calling-package)))

;;; Same as apply-sub-macro, but for the rep macro.
(defun apply-rep-macro (inulf &key (fail-on-bad-use nil) (calling-package nil))
  (let ((rep-fn
          (var-insertion-macro
            'ulf-lib::rep 'ulf-lib::*p #'second #'third
            #'(lambda (x) (not (and (listp x) (eq 'ulf-lib::rep (first x)) (equal (length x) 3)))))))
    (funcall rep-fn inulf fail-on-bad-use calling-package)))


;; Assumes there's at most one occurrence of var in curulf.
(defun add-info-to-var (curulf var srculf)
  ;; TODO: For now just take the first type (we should use a
  ;; hierarchy of types to select the most specific one.
  (let* ((typ (first (phrasal-ulf-type? srculf)))
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
       (gute:in-intern (inulf ulf :ulf-lib)
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
      (gute:intern-symbols-recursive initial-result calling-package)
      initial-result)))


;; This assumes that there's at most one relativizer in curulf.
(defun add-info-to-relativizer (curulf srculf)
  (let* ((origrel (first (gute:tree-find-if curulf #'lex-rel?)))
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
       (gute:in-intern (inulf ulf :ulf-lib)
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
;;; from the end and wrapping those. Otherwise, supply the arguments flat.
;;; For example: "Made he a table for John?"
;;;   (((past make.v) he.pro (a.d table.n) (for.p-arg |John|)) ?)
(defun uninvert-verbaux! (ulf)
  (declare (type list ulf))
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
       ((uninvert-verbaux! ((lex-tense? lex-verb?) term? _+)) [?]))
    ;; Special case for be.v, since it only takes predicates.
    (/ ((lex-tense? be.v) term? _+)
       (uninvert-verbaux! ((lex-tense? be.v) term? _+)))))

;; Function to uninvert verbauxes from a given ULFs.
;; calling-package is an option for the package of the symbols of the returned
;;   value. By default it will be :ulf-lib.
(defun uninvert-verbauxes (inulf &key (calling-package nil))
  (let
    ((initial-result
       (gute:in-intern (inulf ulf :ulf-lib)
         (let ((ttthidden (hide-ttt-ops ulf)))
           (unhide-ttt-ops
             (ttt:apply-rules *ttt-uninvert-verbaux*
                              ttthidden
                              :max-n 500
                              :trace nil))))))
    (if calling-package
      (gute:intern-symbols-recursive initial-result calling-package)
      initial-result)))

;; Lift adv-a that are mixed in with verb arguments.
(defparameter *ttt-lift-adv-a*
  '((/ ((! verb? tensed-verb?) _+1 adv-a? _*2)
       (adv-a? (! _+1 _*2)))
    (/ ((! verb? tensed-verb?) _*1 adv-a? _+2)
       (adv-a? (! _*1 _+2)))))
(defun lift-adv-a (ulf)
  (let ((ttthidden (hide-ttt-ops ulf)))
    (unhide-ttt-ops
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
          (multiple-value-bind (suc res) (rec-helper (second ulf))
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
      ((initial-result (multiple-value-list (gute:in-intern
                                              (inulf ulf :ulf-lib)
                                              (rec-helper ulf)))))
      (cond
        ;; Failure in qt-attr or qt-attr not , so just return input.
        ((or (not (first initial-result))
             (third initial-result))
         (values nil inulf))
        ;; Intern to given package.
        (calling-package  (values (first initial-result)
                                  (gute:intern-symbols-recursive (second initial-result)
                                                                 calling-package)))
        ;; Keep ulf-lib pacakge.
        (t (values (first initial-result) (second initial-result)))))))

