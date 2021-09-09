
(in-package :ulf-lib)

;; TODO: move this somewhere more generic
;; Returns true if the argument is a plural term.
;;  (the.d (plur *.n))
;;  (the.d (.... (plur *.n)))
;;  they.pro
;;  them.pro
;;  we.pro
(defun plur-term? (inx)
  (in-intern (inx x :ulf-lib)
    (cond
      ;; If an atom, it's just whether it's one of the selected pronouns.
      ((atom x)
       ;; TODO: deal with "ours is better"
       (member x '(they.pro them.pro we.pro us.pro you.pro these.pro those.pro
                            both.pro few.pro many.pro several.pro all.pro any.pro
                            most.pro none.pro some.pro ours.pro yours.pro
                            theirs.pro)))
      ;; For terms from nouns, either the quantifier forces a plural reading, e.g. many,
      ;; or we check the head noun for a plural operator.
      ((and (listp x) (term? x) (= (length x) 2)
            (or (det? (first x)) (noun-reifier? (first x)))
            (or (noun? (second x)) (pp? (second x))))
       ;; TODO: deal with examples like "all water is wet"
       (or (member (first x) '(these.d those.d both.d few.d many.d several.d))
           (plur-noun? (second x))
           (plur-partitive? (second x))))
      ;; Coordinated nouns or sets of terms are plural.
      ((and (listp x) (term? x) (> (length x) 2))
       (or (lex-set-of? (first x)) (lex-coord? (second x))
           ;; coordinator in the second to last position.
           (lex-coord? (nth (- (length x) 2) x))))
      ;; Term marked with 'plur-term (an internal computational marker).
      ((and (listp x) (= (length x) 2) (eql (first x) 'plur-term) (term? (second x)))
       t)
      ;; Otherwise, singular.
      (t nil))))


(defun plur-partitive? (inx)
  (in-intern (inx x :ulf-lib)
    (and (listp x) (= (length x) 2)
         (lex-p? (first x)) (plur-term? (second x)))))


(defun plur-noun? (inarg)
;````````````````````
; True if arg is a plural noun phrase. Basically if the np head is plural.
  (in-intern (inarg arg :ulf-lib)
    (or
      (and (listp arg) (= (length arg) 2)
           (eql 'plur (first arg)) (noun? (second arg)))
      (let ((hn (search-np-head arg :callpkg :ulf-lib)))
        (plur-lex-noun? hn)))))

(defun plur-lex-noun? (inarg)
;````````````````````
; True if arg is of the form (plur <lexical noun>) and false otherwise.
  (in-intern (inarg arg :ulf-lib)
    (and (listp arg) (= (length arg) 2)
         (eql 'plur (first arg)) (or (lex-noun? (second arg))
                                     (lex-name-pred? (second arg))))))

(defun pasv-lex-verb? (inarg)
;````````````````````
; True if arg is of the form (pasv <lexical verb>) and false otherwise.
  (in-intern (inarg arg :ulf-lib)
    (and (listp arg) (= (length arg) 2)
         (eql 'pasv (first arg))
         (lex-verb? (second arg)))))

(defun perf-lex-verb? (inarg)
;````````````````````
; True if arg is of the form (perf <lexical verb>) and false otherwise.
  (in-intern (inarg arg :ulf-lib)
    (and (listp arg) (= (length arg) 2)
         (eql 'perf (first arg))
         (lex-verb? (second arg)))))

(defun prog-lex-verb? (inarg)
;````````````````````
; True if arg is of the form (prog <lexical verb>) and false otherwise.
  (in-intern (inarg arg :ulf-lib)
    (and (listp arg) (= (length arg) 2)
         (eql 'prog (first arg))
         (lex-verb? (second arg)))))

