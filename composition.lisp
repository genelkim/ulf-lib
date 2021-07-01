;; ULF composition functions

(in-package :ulf-lib)

(defun merge-subscripts (ss1 ss2)
  "Merge the two subscripts, considering ss1 as the main subscript. If they are
  contradictory, take the ss1 value."
  (let*
    ((ss1chars (if (null ss1) nil (coerce (symbol-name ss1) 'list)))
     (ss2chars (if (null ss2) nil (coerce (symbol-name ss2) 'list)))
     (poses (coerce "NAVP" 'list))
     (new-val
       (cond
         ;; TODO: isn't the find-if below the same as the intersection? If so, just setf the intersection in the check...
         ((intersection poses ss1chars)
          (find-if #'(lambda (x) (member (the character x) poses)) ss1chars))
         ((intersection poses ss2chars)
          (find-if #'(lambda (x) (member (the character x) poses)) ss2chars))
         (t nil)))
     (filtered (remove-if #'null (list new-val))))
    (if (null filtered)
      nil
      (intern (coerce filtered 'string) :ulf-lib))))

;; TODO: incorporate this into tense synfeats stuff. Also make sure tense isn't included on atoms.
(defun merge-tenses (t1 t2)
  "Merge the two tenses, considering t1 as the main subscript. If they are
  contradictory, take the t1 value."
  (let*
    ((t1chars (if (null t1) nil (coerce (symbol-name t1) 'list)))
     (t2chars (if (null t2) nil (coerce (symbol-name t2) 'list)))
     (tense-val
       (cond
         ((member #\T t1chars) #\T)
         ((member #\U t1chars) #\U)
         ((member #\T t2chars) #\T)
         ((member #\U t2chars) #\U)
         (t nil)))
     (filtered (remove-if #'null (list tense-val))))
    (if (null filtered)
      nil
      (intern (coerce filtered 'string) :ulf-lib))))

(defun compose-synfeats! (op arg)
  "Composes the synfeats slots of the operator and argument semtypes. Returns
  the new synfeats slot value."
  ;; 1. Get the base synfeats based on the operator connective
  ;; 2. Call the syntactic-features class method for updating individual features.
  (let ((base-synfeats
          (cond
            ((eql '=> (connective op)) (copy (synfeats op)))
            ((eql '>> (connective op)) (copy (synfeats arg)))
            (t (error "Unknown connective for determining base synfeat: ~s~%"
                      (connective op))))))
    ; todo: switch op to opr.
    (combine-features base-synfeats
                      (synfeats op)
                      (synfeats arg)
                      (synfeats (range op))
                      op
                      arg)))

;; Compose a given operator and argument if possible.
;; Assumption (for now): Arg has no exponent. If it does, it is ignored.
;; Subscripts are propagated from op.
;; Synfeats are propagated from op if => and arg if >> with
;; exceptions per synfeat.
;; type-params are propagated from both.
(declaim (ftype (function (semtype) fixnum) ex))
(defun apply-operator! (op arg &key (recurse-fn #'apply-operator!))
  (let
    ((new-params (append (type-params op) (type-params arg)))
     (result
       (cond
         ((optional-type-p op)
          ; Operator is an optional type
          (let ((a (funcall recurse-fn (first (types op)) arg))
                (b (funcall recurse-fn (second (types op)) arg)))
            (if (and a b)
              (new-semtype nil nil 1 nil :options (list a b))
              (or a b))))
         ((optional-type-p arg)
          ; Argument is an optional type
          (let ((a (funcall recurse-fn op (first (types arg))))
                (b (funcall recurse-fn op (second (types arg)))))
            (if (and a b)
              (new-semtype nil nil 1 nil :options (list a b)) ; TODO: add a function new-optional-semtype
              (or a b))))
         ; Operator is not optional and atomic operator of the form A^n with n>1
         ((atomic-type-p op)
          (when (and (semtype-equal? op arg :ignore-exp T) (> (ex op) 1))
            (new-semtype (domain op) nil (- (ex op) 1) (subscript op))))
         ;; Operator is a non-atomic type with domain exponent n=1
         ((and (semtype-p op) (semtype-equal? (domain op) arg :ignore-exp T) (= (ex (domain op)) 1))
          (let ((result (copy-semtype (range op))))
            ;; Only add subscript when not atomic or (S=>2)
            (when (not (or (atomic-type-p result) (equal (semtype2str result) "(S=>2)")))
              (setf (subscript result)
                    (merge-subscripts (subscript (range op)) (subscript op))))
            ;; Update syntactic features.
            (setf (synfeats result)
                  (compose-synfeats! op arg))
            result))
         ;; Operator is non-atomic type with domain exponent n>1
         ((and (semtype-p op) (semtype-equal? (domain op) arg :ignore-exp T))
          (let ((result (copy-semtype op)))
            (setf (ex (domain result)) (- (ex (domain result)) 1))
            result)))))
    ; Update type params before returning, if not optional. All type params are
    ; assumed to be in non-optional types.
    (when (not (optional-type-p op))
      (add-semtype-type-params result new-params))
    result))

;; Compose two types if possible and return the composed type. Also return the
;; order in which the types were composed.
(declaim (ftype (function ((or semtype null) (or semtype null) &key (:op-apply-fn function))
                          (values (or semtype null) simple-string list))
                compose-types!))
(defun compose-types! (x y &key (op-apply-fn #'apply-operator!))
  (if (or (not x) (not y))
    (values (or x y) "none" nil)
    (let (comp)
      (cond
        ((setf comp (funcall op-apply-fn x y))
         (values comp "right" (list x y)))
        ((setf comp (funcall op-apply-fn y x))
         (values comp "left" (list y x)))
        (t
          (values nil "none" nil))))))

;; Given two atomic ULFs, return the type formed after composing (if possible)
(defun compose-atomic-ulfs! (a b)
  (if (or (not (atom-semtype? a)) (not (atom-semtype? b)))
    (or (atom-semtype? a) (atom-semtype? b))
    (let ((comp (apply-operator! (atom-semtype? a) (atom-semtype? b))))
      (if comp
        (values comp (list a b))
        (progn
          (setf comp (apply-operator! (atom-semtype? b) (atom-semtype? a)))
          (when comp (values comp (list b a))))))))

;; Given a ULF, evaluate and return the type if possible. Currently assumes
;; left associativity if there are more than 2 items scoped together. This is
;; not ideal, and needs to be changed.
;; Key argument 'lambda-vars' is used for internal recursion.
;; Note: Unknown ulf atoms are ignored.
(defun ulf-type? (ulf &key lambda-vars)
  (if (atom ulf)
    ; atomic ULF
    (progn
      (let
        ((semtype (cond
                    ((eql ulf '\") (extended-str2semtype "\""))
                    ((eql ulf '|'S|) (extended-str2semtype "POSTGEN1"))
                    ((atom-semtype? ulf) (atom-semtype? ulf))
                    ((member ulf lambda-vars) (str2semtype "D"))
                    ((lex-tense?  ulf) (extended-str2semtype "TENSE"))
                    ((lex-macro?  ulf) (extended-str2semtype (symbol-name ulf)))
                    ((eql ulf '*qt) (extended-str2semtype "D[*QT]"))
                    ((lex-macro-hole? ulf) (extended-str2semtype (symbol-name ulf)))
                    (t nil))))
        semtype))
    ; non-atomic ULF
    (if (equal (car ulf) 'lambda)
      ; ULF is of the form (lambda var (expr))
      (when (= (length ulf) 3)
        (new-semtype (str2semtype "D")
                     (ulf-type? (third ulf)
                                :lambda-vars (cons (cadr ulf) lambda-vars))
                     1
                     nil))
      ; ULF is neither a lambda nor an atom
      (if (= (length ulf) 1)
        (ulf-type? (car ulf) :lambda-vars lambda-vars)
        (extended-compose-types! (ulf-type? (reverse (cdr (reverse ulf)))
                                            :lambda-vars lambda-vars)
                                 (ulf-type? (car (last ulf))
                                            :lambda-vars lambda-vars))))))

;; Given a ULF, evaluate the type if possible and return a string representation
;; of the type.
(defun ulf-type-string? (inulf)
  (in-intern (inulf ulf :ulf-lib)
    (semtype2str (ulf-type? ulf))))

(defun str-ulf-type-string? (string-ulf)
  (ulf-type-string? (read-from-string string-ulf)))


(defun compose-type-string-builder (compose-fn str2semtype-fn)
  "Builds a string-based interface to a type composition function with that
  functions relevant string-to-semtype mapping function."
  #'(lambda (type1 type2)
      (when (and type1 type2)
        (multiple-value-bind
          (composed direction types)
          (funcall compose-fn
                   (funcall str2semtype-fn type1)
                   (funcall str2semtype-fn type2))
          (values (semtype2str composed)
                  direction
                  (mapcar #'semtype2str types))))))


;; Given two types are strings, compose them if possible and return the
;; resulting type as a string.
(defun compose-type-string! (type1 type2)
  (funcall (compose-type-string-builder #'compose-types!
                                        #'str2semtype)
           type1 type2))

(defparameter *unary-noun-semtype* (str2semtype "(D=>(S=>2))_n"))
(defparameter *unary-pred-semtype* (str2semtype "(D=>(S=>2))"))
(defparameter *unary-verb-semtype* (str2semtype "(D=>(S=>2))_v"))
(defparameter *unary-tensed-verb-semtype* (str2semtype "(D=>(S=>2))_v_t"))
(defparameter *general-verb-semtype* (str2semtype "({D|(D=>(S=>2))}^n=>(D=>(S=>2)))_v"))
(defparameter *general-untensed-verb-semtype* (str2semtype "({D|(D=>(S=>2))}^n=>(D=>(S=>2)))_v_!t"))
(defparameter *term-semtype* (str2semtype "D"))
(defparameter *sent-mod-semtype* (str2semtype "{((S=>2)=>(S=>2))|((S=>2)>>(S=>2))}"))
(defparameter *tensed-sent-semtype* (str2semtype "(S=>2)_t"))
(defparameter *auxiliary-semtype* (str2semtype "((D=>(S=>2))_v_!t,!x>>(D=>(S=>2))_v_!t,x)"))

(declaim (ftype (function (list) list) get-all-top-domains))
(defun get-all-top-domains (types)
  "Extracts all top-level domains from the list of types, recursing into
  optional-types.
  "
  (apply #'append
         (mapcar 
           #'(lambda (typ)
               (cond
                 ((optional-type-p typ)
                  (get-all-top-domains (types typ)))
                 (t (list (domain typ)))))
           types)))

(defun extended-apply-operator! (op arg &key (recurse-fn #'extended-apply-operator!))
  "Compose a given operator and argument if possible. Assumption (for now): Arg
  has no exponent. If it does, it is ignored. The strict EL type compositions
  are extended to include ULF macros and structural relaxations."
  (cond
    ; TENSE, N+PREDS, NP+PREDS, +PREDS, etc. can't be the operand type.
    ((and (atomic-type-p arg)
          (member (domain arg)
                  '(tense n+preds np+preds +preds qt-attr qt-attr1 sub sub1 rep
                          rep1 parg)))
     nil)
    ;;; TENSE
    ;;; TENSE + TYPE_V_U => TYPE_V_T
    ((and (atomic-type-p op) (eql (domain op) 'tense)
          (semtype-equal? *general-untensed-verb-semtype* arg
                          :ignore-exp t))
     (let ((tensed-semtype (copy-semtype arg)))
       (add-semtype-tense tensed-semtype 't)
       tensed-semtype))
    ;;; N+PREDS
    ;;; 1. n+preds + N_n >> {+preds[n+[N_n]]|N_n}
    ;;; 2. +preds[n+[T]] + N >> {+preds[n+[T]]|T}
    ;;; Stops when it (+preds) is used as the T (a variant of the unary noun).
    ; n+preds + N_n >> {+preds[n+[N_n]]|N_n}
    ((and (atomic-type-p op) (eql (domain op) 'n+preds))
     ;; TODO(gene): probably need semtype-sufficent? which doesn't check exact
     ;; equality, but whether an argument is sufficient for the slot type. For
     ;; example (D=>(S=>2))_n is not equal to (D=>(S=>2)), but it is a
     ;; sufficient argument.
     (when (semtype-equal? *unary-noun-semtype* arg :ignore-exp t)
       (let* ((n+-semtype (new-semtype 'n+ nil 1 nil :type-params (list arg)))
              (+preds-semtype (new-semtype '+preds nil 1 nil :type-params (list n+-semtype))))
         ;;Optional semtype of either continuing to act as +preds, or as the internal noun.
         (new-semtype nil nil 1 nil :options (list +preds-semtype arg)))))
    ;;; NP+PREDS
    ;;; 1. np+preds + D >> {+preds[n+[D]]|D}
    ;;; 2. +preds[n+[T]] + N >> {+preds[n+[T]]|T}
    ;;; Stops when it (+preds) is used as the T (a variant of the unary noun).
    ; np+preds + D >> {+preds[n+[D]]|D}
    ((and (atomic-type-p op) (eql (domain op) 'np+preds))
     (when (semtype-equal? *term-semtype* arg :ignore-exp t)
       (let* ((np+-semtype (new-semtype 'np+ nil 1 nil :type-params (list arg)))
              (+preds-semtype (new-semtype '+preds nil 1 nil :type-params (list np+-semtype))))
         ;; Optional semtype of either continuing to act as +preds, or as the internal noun.
         (new-semtype nil nil 1 nil :options (list +preds-semtype arg)))))
    ; +preds[n+[T]] + N >> {+preds[n+[T]]|T}(N+PREDS&NP+PREDS)
    ((and (atomic-type-p op) (eql (domain op) '+preds))
     (when (semtype-equal? *unary-pred-semtype* arg :ignore-exp t)
       (let* ((op-params (type-params op))
              (n+-params (remove-if-not #'(lambda (x) (eql (domain x) 'n+)) op-params))
              (np+-params (remove-if-not #'(lambda (x) (eql (domain x) 'np+)) op-params))
              (other-params (remove-if #'(lambda (x) (member (domain x) '(n+ np+))) op-params))
              (arg-params (type-params arg))
              (new-params (append (cdr n+-params) (cdr np+-params) other-params arg-params))
              inner-semtype updated-inner-semtype updated-+preds-semtype)
         (assert (not (and n+-params np+-params))
                 (n+-params np+-params op arg)
                 "Can't be in both n+pred and np+preds. n+-params: ~s~%np+-params: ~s~%op: ~s~%arg: ~s~%"
                 n+-params np+-params op arg)
         ;; Update inner-semtype with new params.
         (setf inner-semtype (if n+-params
                               (first (type-params (first n+-params)))
                               (first (type-params (first np+-params)))))
         (setf updated-inner-semtype (copy-semtype inner-semtype))
         (setf (type-params updated-inner-semtype)
               (append (type-params updated-inner-semtype) new-params))
         (setf updated-+preds-semtype (copy-semtype op))
         (setf (type-params updated-+preds-semtype)
               (append (type-params updated-+preds-semtype) new-params))
         (new-semtype nil nil 1 nil :options (list updated-+preds-semtype updated-inner-semtype)))))
    ;;; QT-ATTR
    ;;; The whole process of this is as follows
    ;;; Type(*qt): D[*qt],   Type(qt-attr): qt-attr
    ;;; 1. (say.v *qt)                            Type((say.v *qt)) = Compose(Type(say.v),D)[*qt]
    ;;; 2. (qt-attr (say.v *qt))                  qt-attr + T[*qt] >> qt-attr1[T[*qt]]
    ;;; 3. (yes! (qt-attr (say.v *qt)))           qt-attr1[T1[*qt]] + T2 >> T2[qt-attr1[T1[*qt]]]
    ;;; 4. (\" (yes! (qt-attr (say.v *qt))))      \" + T2[qt-attr1[T1[*qt]]] >> qt-attr2[T1[*qt]]
    ;;; 5. (\" (yes! (qt-attr (say.v *qt))) \")   qt-attr2[T1[*qt]] + \" >> T1
    ;;; Step 1 is handled just with *qt being D[*qt]
    ; qt-attr + T[*qt] >> qt-attr1[T[*qt]]
    ((and (atomic-type-p op) (eql (domain op) 'qt-attr))
     (let ((type-params (get-semtype-type-params arg)))
       (when (find '*qt (get-all-top-domains type-params))
         (new-semtype 'qt-attr1 nil 1 nil :type-params (list arg)))))
    ; qt-attr1[T1[*qt]] + T2 >> T2[qt-attr1[T1[*qt]]]
    ((and (atomic-type-p op) (eql (domain op) 'qt-attr1))
     (let ((result (copy-semtype arg)))
       (add-semtype-type-params result (list op))
       result))
    ; \" + T2[qt-attr1[T1[*qt]]] >> qt-attr2[T1[*qt]]
    ((and (atomic-type-p op) (eql (domain op) '\")
          (some #'(lambda (param) (eql (domain param) 'qt-attr1))
                (get-semtype-type-params arg)))
     (let* ((qt-attr1-param
              (find-if #'(lambda (param) (eql (domain param) 'qt-attr1))
                       (get-semtype-type-params arg))))
       (when (not qt-attr1-param)
         (error "We should NEVER get here! Included to help compiler."))
       (new-semtype 'qt-attr2 nil 1 nil
                    :type-params (type-params qt-attr1-param))))
    ;;; qt-attr2[T1[*qt]] + \" >> T1
    ((and (atomic-type-p op) (eql (domain op) 'qt-attr2)
          (atomic-type-p arg) (eql (domain arg) '\"))
     (assert (= 1 (length (the list (type-params op)))) (op arg)
             "Didn't expect to have multiple type params for qt-attr2.~%op: ~s~%arg: ~s~%"
             (semtype2str op) (semtype2str arg))
     (let* ((result (copy-semtype (first (type-params op))))
            (qt-removed-type-params
              (remove-if #'(lambda (param) (eql (domain param) '*qt))
                         (get-semtype-type-params result))))
       (set-semtype-type-params result qt-removed-type-params)
       result))
    ;;; SUB
    ;;; 1a. sub + T >> sub1[T]
    ;;; 1b. T + *h >> Range(T)[*h[Dom(T)]]
    ;;; 2. sub1[T1] + T2[*h[T3]] >> T2, iff T1 can be the arg of T3.
    ; sub + T >> sub1[T]
    ((and (atomic-type-p op) (eql (domain op) 'sub))
     (new-semtype 'sub1 nil 1 nil :type-params (list (copy-semtype arg))))
    ; Hole variables.
    ; T + *h << Range(T)[*h[Dom(T)]]
    ; T + *p >> Range(T)[*h[Dom(T)]]
    ((and (not (atomic-type-p op)) (not (optional-type-p op))
          (atomic-type-p arg) (member (domain arg) '(*h *p)))
     (let ((op-dom (copy-semtype (domain op)))
           (op-ran (copy-semtype (range op)))
           (arg-copy (copy-semtype arg)))
       (add-semtype-type-params arg-copy (list op-dom))
       (add-semtype-type-params op-ran (list arg-copy))
       op-ran))
    ; sub1[T1] + T2[*h[T3]] >> T2, iff T1 can be the arg of T3.
    ((and (atomic-type-p op) (eql (domain op) 'sub1))
     (let ((*h-params (remove-if-not #'(lambda (x) (eql (domain x) '*h))
                                     (type-params arg)))
           (other-params (remove-if #'(lambda (x) (eql (domain x) '*h))
                                    (type-params arg))))
       ; Check that T1 and T3 exists and that they match.
       (when (and (not (null (type-params op)))
                  (not (null *h-params))
                  (not (null (type-params (first *h-params))))
                  (semtype-equal? (first (type-params op))
                                  (first (type-params (first *h-params)))
                                  :ignore-exp t))
         (assert (= 1 (length (the list (type-params op)))))
         (assert (= 1 (length *h-params)))
         (assert (= 1 (length (the list (type-params (first *h-params))))))
         (let ((arg-copy (copy-semtype arg)))
           (set-semtype-type-params arg-copy
                                    (mapcar #'copy-semtype other-params))
           arg-copy))))
    ;;; REP
    ;;; 1a. T + *p >> Range(T)[*h[Dom(T)]] (handled above in the SUB section)
    ;;; 1b. rep + T1[*p[T2]] >> rep1[T1[*p[T2]]]
    ;;; 2. rep1[T1[*p[T2]]] + T3 >> T1, iff T3 can be the arg of T2.
    ; rep + T1[*p[T2]] >> rep1[T1[*p[T2]]]
    ((and (atomic-type-p op)
          (eql (domain op) 'rep)
          (not (optional-type-p arg)))
     (when (null (type-params arg))
       (return-from extended-apply-operator! nil))
     (let ((*p-params (remove-if-not #'(lambda (x) (eql (domain x) '*p))
                                     (type-params arg))))
       (when (and (not (null *p-params))
                  (not (null (type-params (first *p-params)))))
         (assert (= 1 (length *p-params)))
         (new-semtype 'rep1 nil 1 nil
                      :type-params (list (copy-semtype arg))))))
    ; rep1[T1[*p[T2]]] + T3 >> T1, iff T3 can be the arg of T2.
    ((and (atomic-type-p op) (eql (domain op) 'rep1))
     (let* ((t1-fn #'(lambda (tp)
                       (some #'(lambda (tptp) (eql (domain tptp) '*p))
                             (type-params tp))))
            (t2-fn #'(lambda (t1tp) (eql (domain t1tp) '*p)))
            (t1 (first (remove-if-not t1-fn (type-params op))))
            (t2 (first (type-params (first (remove-if-not t2-fn
                                                          (type-params t1)))))))
       (when (semtype-equal? t2 arg :ignore-exp t)
         (let ((retval (copy-semtype t1))
               (new-type-params (append (remove-if t1-fn (type-params op))
                                        (remove-if t2-fn (type-params t1))
                                        (type-params arg))))
           (set-semtype-type-params retval new-type-params)
           retval))))
    ;;; 'S
    ;;; 1. D + POSTGEN1 >> POSTGEN2
    ;;; 2. POSTGEN2 + (D=>(S=>2))_N >> D
    ; 1. D + POSTGEN1 >> POSTGEN2
    ((and (atomic-type-p arg)
          (semtype-equal? *term-semtype* op)
          (eql (domain arg) 'postgen1))
     (new-semtype 'postgen2 nil 1 nil
                  :type-params (append (type-params op) (type-params arg))))
    ; 2. POSTGEN2 + (D=>(S=>2))_N >> D
    ((and (atomic-type-p op)
          (eql (domain op) 'postgen2)
          (semtype-equal? *unary-noun-semtype* arg :ignore-exp t))
     (let ((term-st (copy-semtype *term-semtype*)))
       (setf (type-params term-st)
             (append (type-params op) (type-params arg)))
       term-st))
    ;;; AUX
    ;;; [NO LONGEr USED] 1. AUX + (D=>(S=>2))_V_U >> (D=>(S=>2))_V_U_X
    ;;; 2. TENSE + AUX => TAUX
    ;;; 3. TAUX + (D=>(S=>2))_V [no T or X] >> (D=>(S=>2))_V_T_X
    ; 1. AUX + (D=>(S=>2))_V [no T or X] >> (D=>(S=>2))_V_X
    ;((and (atomic-type-p op)
    ;      (eql (domain op) 'aux)
    ;      (null (tense arg))
    ;      (null (feature-value (synfeats arg) 'auxiliary))
    ;      (semtype-equal? *unary-verb-semtype* arg))
    ; (copy-semtype *unary-verb-semtype*
    ;               :c-type-params (type-params arg)
    ;               :c-synfeats (add-feature-values (copy (synfeats arg)) '(x))))
    ; 2. TENSE + AUX => TAUX
    ((and (atomic-type-p op) (eql (domain op) 'tense)
          (semtype-equal? *auxiliary-semtype* arg))
     (new-semtype 'taux nil 1 nil))
    ; 3. TAUX + (D=>(S=>2))_V [no T or X] >> (D=>(S=>2))_V_T_X
    ((and (atomic-type-p op)
          (eql (domain op) 'taux)
          (not (eql 't (feature-value (synfeats arg) 'tense)))
          (null (feature-value (synfeats arg) 'auxiliary))
          (semtype-equal? *unary-verb-semtype* arg))
     (copy-semtype *unary-verb-semtype*
                   :c-type-params (type-params arg)
                   :c-synfeats (add-feature-values (copy (synfeats arg)) '(x t))))
    ;;; PARG
    ;;; 1. PARG + T => PARG1[T]
    ;;; 2a. T1_V + PARG1[T2] => T1_V(T2) {application}
    ;;; 2b. T1_{N,A} + PARG1[T2] => T1_{N,A}
    ; 1. PARG + T => PARG[T]
    ((and (atomic-type-p op) (eql (domain op) 'parg))
     (new-semtype 'parg1 nil 1 nil :type-params (list (copy-semtype arg))))
    ; 2a. T1_V + PARG1[T2] => T1_V(T2) {application}
    ((and (semtype-equal? *general-verb-semtype* op)
          (atomic-type-p arg) (eql (domain arg) 'parg1)
          (extended-compose-types! op (first (type-params arg))))
     (extended-compose-types! op (first (type-params arg))))
    ; 2b. T1_{N,A} + PARG1[T2] => T1_{N,A}
    ((and (member (subscript op) '(n a))
          (atomic-type-p arg) (eql (domain arg) 'parg1))
     (copy-semtype op))
    ;; Fall back to EL compositional functionality.
    (t (apply-operator! op arg :recurse-fn recurse-fn))))


(defun left-right-apply-operator! (op arg &key (recurse-fn #'left-right-apply-operator!))
  "A further relaxation of `extended-apply-operator!` which generalizes the
  type system to allow left-to-right composition even when there is infixing,
  inversions, and sentence modifiers."
  (cond
    ;;; SUBJECT(term) + VP
    ; Treat it like it's VP + TERM
    ; TODO(gene): Ideally, we would force it to be a sentence, but we won't
    ; worry about that for now.
    ((and (semtype-equal? *term-semtype* op)
          (semtype-equal? *general-verb-semtype* arg))
     ;(format t "1!!~%")
     (apply-operator! arg op))
    ;;; ADV-S + * >> *
    ; This will over generate for non-paired SENT-MODs
    ; TODO(gene): generate a type that turns back to * after combining with COMPLEX
    ; TODO(gene): find a way to distinguish between adv-s, adv-e, and other sent-mods.
    ((semtype-equal? *sent-mod-semtype* op) arg)
    ;;; * + SENT-MOD >> *
    ((semtype-equal? *sent-mod-semtype* arg) op)
    ;;; TSENT + !/? -> TSENT
    ; TODO(gene): this is currently subsumed by the rule above of * + SENT-MOD >> *. We should eventually distinguish these so that we don't allows punctuation to appear anywhere in that way the most sent-mod operators do.
    ;;; Inverted TAUX (ITAUX)
    ;;; 1. TAUX + TERM >> ITAUX
    ;;; 2. ITAUX + (D=>(S=>2))_V [no T or X] >> (S=>2)_T
    ; 1. TAUX + TERM >> ITAUX
    ((and (atomic-type-p op) (eql (domain op) 'taux)
          (semtype-equal? *term-semtype* arg))
     (new-semtype 'itaux nil 1 nil))
    ; 2. ITAUX + (D=>(S=>2))_V [no T or X] >> (S=>2)_T
    ((and (atomic-type-p op)
          (eql (domain op) 'itaux)
          (not (eql 't (feature-value (synfeats arg) 'tense)))
          (null (feature-value (synfeats arg) 'auxiliary))
          (semtype-equal? *unary-verb-semtype* arg)
          (not (semtype-equal? *unary-tensed-verb-semtype* arg)))
     (copy-semtype *tensed-sent-semtype*
                   :c-type-params (type-params arg)
                   :c-synfeats (add-feature-values (copy (synfeats arg)) '(t))))
    ;; Fall back to extended-apply-operator! when special cases are not
    ;; relevant.
    (t (extended-apply-operator! op arg :recurse-fn recurse-fn))))

(defun extended-compose-types! (type1 type2)
  "Compose two types if possible and return the composed type. Also return the
  order in which the types were composed. The strict EL type compositions are
  extended to include ULF macros and structural relaxations."
  (compose-types! type1 type2 :op-apply-fn #'extended-apply-operator!))


(defun extended-compose-type-string! (type1 type2)
  "Given two types as strings, compose them if possible and return the
  resulting type as a string. The strict EL type compositions are extended to
  include ULF macros and structural relaxations."
  (funcall (compose-type-string-builder #'extended-compose-types!
                                        #'extended-str2semtype)
           type1 type2))


(defun list-extended-compose-type-string! (type1 type2)
  "Same as extended-compose-type-string! but returns everything in a list
  rather than a multiple values.
  "
  (multiple-value-list (extended-compose-type-string! type1 type2)))


(defun left-right-compose-types! (type1 type2)
  "left-right version of compose-types!
  "
  (compose-types! type1 type2 :op-apply-fn #'left-right-apply-operator!))


(defun left-right-compose-type-string! (type1 type2)
  "left-right version of compose-type-string!
  "
  (funcall (compose-type-string-builder #'left-right-compose-types!
                                        #'extended-str2semtype)
           type1 type2))


(defun list-left-right-compose-type-string! (type1 type2)
  "List interface of left-right-compose-type-string!
  "
  (multiple-value-list (left-right-compose-type-string! type1 type2)))
  
