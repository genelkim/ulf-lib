;;; Gene Louis Kim 
;;; 11-19-2018
;;;
;;; ULF macro handling code.

(in-package :ulf-lib)

;;; Returns t if 'ulf' contains *h, nil otherwise.
(defun contains-hole (ulf)
  (cond
    ((and (atom ulf) (eq '*h ulf)) t)
    ((atom ulf) nil)
    (t (or (contains-hole (car ulf))
           (contains-hole (cdr ulf))))))

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
(defun apply-sub-macro (ulf &optional fail-on-bad-use)
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
                (apply-sub-macro (second ulf) fail-on-bad-use)))
            (rightrec 
              (multiple-value-list
                (apply-sub-macro (third ulf) fail-on-bad-use)))
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
    ;; it.  Otherwise, merge together.
    (t (let* ((recres (mapcar #'(lambda (x) 
                                  (multiple-value-list 
                                    (apply-sub-macro x fail-on-bad-use)))
                              ulf))
              (successes (mapcar #'first recres))
              (fs (mapcar #'second recres)))
         (if (reduce #'(lambda (x y) (and x y)) successes)
           ;; If all recursion succeeded, return the results with 't'.
           (values t fs)
           ;; Otherwise, find the first one that failed and return it.
           (let ((failpos (position nil successes)))
             (values nil (nth failpos fs))))))))


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

;; Function to uninvert verbauxes from a given ULFs.
(defun uninvert-verbauxes (ulf)
  (let ((ttthidden (hide-ttt-ops ulf)))
    (util:unhide-ttt-ops 
      (ttt:apply-rules *ttt-uninvert-verbaux*
                       ttthidden))))

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

