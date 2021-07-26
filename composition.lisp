;; ULF composition functions

(in-package :ulf-lib)

(defun merge-suffixes (ss1 ss2)
  "Merge the two suffixes, considering ss1 as the main suffix. If they are
  contradictory, take the ss1 value."
  (let*
    ((ss1chars (if (null ss1) nil (coerce (symbol-name ss1) 'list)))
     (ss2chars (if (null ss2) nil (coerce (symbol-name ss2) 'list)))
     (poses (coerce "NAVP" 'list))
     (new-val
       (cond
         ((intersection poses ss1chars)
          (find-if #'(lambda (x) (member (the character x) poses)) ss1chars))
         ((intersection poses ss2chars)
          (find-if #'(lambda (x) (member (the character x) poses)) ss2chars))
         (t nil)))
     (filtered (remove-if #'null (list new-val))))
    (if (null filtered)
      nil
      (intern (coerce filtered 'string) :ulf-lib))))

(defun compose-synfeats! (opr arg)
  "Composes the synfeats slots of the operator and argument semtypes. Returns
  the new synfeats slot value."
  ;; 1. Get the base synfeats based on the operator connective
  ;; 2. Call the syntactic-features class method for updating individual features.
  (let ((base-synfeats
          (cond
            ((eql '=> (connective opr)) (copy (synfeats opr)))
            ((eql '>> (connective opr)) (copy (synfeats arg)))
            (t (error "Unknown connective for determining base synfeat: ~s~%"
                      (connective opr))))))
    (combine-features base-synfeats
                      (synfeats opr)
                      (synfeats arg)
                      (synfeats (range opr))
                      opr
                      arg)))

;; Compose a given operator and argument if possible.
;; Assumption (for now): Arg has no exponent. If it does, it is ignored.
;; suffixes are propagated from op.
;; Synfeats are propagated from op if => and arg if >> with
;; exceptions per synfeat.
;; type-params are propagated from both.
(declaim (ftype (function (semtype) fixnum) ex))
(defun apply-operator! (raw-opr raw-arg
                        &key (recurse-fn #'apply-operator!)
                             ignore-synfeats)
  (let*
    ((new-params (append (type-params raw-opr) (type-params raw-arg)))
     (opr (unroll-exponent-step raw-opr))
     (arg (unroll-exponent-step raw-arg))
     ; we can now assume all domain and top-level exponents are = 1.
     (result
       (cond
         ((optional-type-p opr)
          ; Operator is an optional type
          (let ((a (funcall recurse-fn (first (types opr)) arg))
                (b (funcall recurse-fn (second (types opr)) arg)))
            (if (and a b)
              (new-semtype nil nil 1 nil :options (list a b))
              (or a b))))
         ((optional-type-p arg)
          ; Argument is an optional type
          (let ((a (funcall recurse-fn opr (first (types arg))))
                (b (funcall recurse-fn opr (second (types arg)))))
            (if (and a b)
              (new-semtype nil nil 1 nil :options (list a b)) ; TODO: add a function new-optional-semtype
              (or a b))))
         ; Operator is not optional and atomic operator of the form A^n with n>1
         ((atomic-type-p opr)
          (when (and (semtype-match? opr arg) (> (ex opr) 1))
            (new-semtype (domain opr) nil (- (ex opr) 1) (suffix opr))))
         ;; Operator is a non-atomic type with domain exponent n=1
         ((and (semtype-p opr) (semtype-match? (domain opr) arg) (= (ex (domain opr)) 1))
          (let ((result (copy-semtype (range opr))))
            ;; Only add suffix when not atomic or (S=>2)
            (when (not (or (atomic-type-p result) (equal (semtype2str result) "(S=>2)")))
              (set-suffix result
                             (merge-suffixes (suffix (range opr)) (suffix opr))))
            ;; Update syntactic features.
            (when (not ignore-synfeats)
              (set-synfeats result (compose-synfeats! opr arg)))
            result)))))
    ; Update type params before returning, if not optional. All type params are
    ; assumed to be in non-optional types.
    (when (not (optional-type-p opr))
      (add-semtype-type-params result new-params))
    result))

;; Compose two types if possible and return the composed type. Also return the
;; order in which the types were composed.
(declaim (ftype (function ((or semtype null) (or semtype null)
                           &key (:opr-apply-fn function) (:ignore-synfeats boolean))
                          (values (or semtype null) simple-string list))
                compose-types!))
(defun compose-types! (x y &key (opr-apply-fn #'apply-operator!) ignore-synfeats)
  (if (or (not x) (not y))
    (values (or x y) "none" nil)
    (let (comp)
      (cond
        ((setf comp (funcall opr-apply-fn x y :ignore-synfeats ignore-synfeats))
         (values comp "right" (list x y)))
        ((setf comp (funcall opr-apply-fn y x :ignore-synfeats ignore-synfeats))
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
(defparameter *unary-tensed-verb-semtype* (str2semtype "(D=>(S=>2))_v%t"))
(defparameter *general-verb-semtype* (str2semtype "({D|(D=>(S=>2))}^n=>(D=>(S=>2)))_v"))
(defparameter *general-untensed-verb-semtype* (str2semtype "({D|(D=>(S=>2))}^n=>(D=>(S=>2)))_v%!t"))
(defparameter *term-semtype* (str2semtype "D"))
(defparameter *sent-mod-semtype* (str2semtype "{((S=>2)=>(S=>2))|((S=>2)>>(S=>2))}"))
(defparameter *tensed-sent-semtype* (str2semtype "(S=>2)%t"))
(defparameter *auxiliary-semtype* (str2semtype "((D=>(S=>2))_v%!t,!x>>(D=>(S=>2))_v%!t,x)"))

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

(defun extended-apply-operator! (opr arg
                                 &key (recurse-fn #'extended-apply-operator!)
                                      ignore-synfeats)
  "Compose a given operator and argument if possible. Assumption (for now): Arg
  has no exponent. If it does, it is ignored. The strict EL type compositions
  are extended to include ULF macros and structural relaxations."
  (cond
    ; N+PREDS, NP+PREDS, +PREDS, etc. can't be the operand type.
    ((and (atomic-type-p arg)
          (member (domain arg)
                  '(n+preds np+preds +preds qt-attr qt-attr1 sub sub1 rep
                          rep1 parg)))
     nil)
    ;;; N+PREDS
    ;;; 1. n+preds + N_n >> {+preds[n+[N_n]]|N_n}
    ;;; 2. +preds[n+[T]] + N >> {+preds[n+[T]]|T}
    ;;; Stops when it (+preds) is used as the T (a variant of the unary noun).
    ; n+preds + N_n >> {+preds[n+[N_n]]|N_n}
    ((and (atomic-type-p opr) (eql (domain opr) 'n+preds))
     ;; TODO(gene): probably need semtype-sufficent? which doesn't check exact
     ;; equality, but whether an argument is sufficient for the slot type. For
     ;; example (D=>(S=>2))_n is not equal to (D=>(S=>2)), but it is a
     ;; sufficient argument.
     (when (semtype-match? *unary-noun-semtype* arg :ignore-exp t)
       (let* ((n+-semtype (new-semtype 'n+ nil 1 nil :type-params (list arg)))
              (+preds-semtype (new-semtype '+preds nil 1 nil :type-params (list n+-semtype))))
         ;;Optional semtype of either continuing to act as +preds, or as the internal noun.
         (new-semtype nil nil 1 nil :options (list +preds-semtype arg)))))
    ;;; NP+PREDS
    ;;; 1. np+preds + D >> {+preds[n+[D]]|D}
    ;;; 2. +preds[n+[T]] + N >> {+preds[n+[T]]|T}
    ;;; Stops when it (+preds) is used as the T (a variant of the unary noun).
    ; np+preds + D >> {+preds[n+[D]]|D}
    ((and (atomic-type-p opr) (eql (domain opr) 'np+preds))
     (when (semtype-match? *term-semtype* arg :ignore-exp t)
       (let* ((np+-semtype (new-semtype 'np+ nil 1 nil :type-params (list arg)))
              (+preds-semtype (new-semtype '+preds nil 1 nil :type-params (list np+-semtype))))
         ;; Optional semtype of either continuing to act as +preds, or as the internal noun.
         (new-semtype nil nil 1 nil :options (list +preds-semtype arg)))))
    ; +preds[n+[T]] + N >> {+preds[n+[T]]|T}(N+PREDS&NP+PREDS)
    ((and (atomic-type-p opr) (eql (domain opr) '+preds))
     (when (semtype-match? *unary-pred-semtype* arg :ignore-exp t)
       (let* ((opr-params (type-params opr))
              (n+-params (remove-if-not #'(lambda (x) (eql (domain x) 'n+)) opr-params))
              (np+-params (remove-if-not #'(lambda (x) (eql (domain x) 'np+)) opr-params))
              (other-params (remove-if #'(lambda (x) (member (domain x) '(n+ np+))) opr-params))
              (arg-params (type-params arg))
              (new-params (append (cdr n+-params) (cdr np+-params) other-params arg-params))
              inner-semtype updated-inner-semtype updated-+preds-semtype)
         (assert (not (and n+-params np+-params))
                 (n+-params np+-params opr arg)
                 "Can't be in both n+pred and np+preds. n+-params: ~s~%np+-params: ~s~%opr: ~s~%arg: ~s~%"
                 n+-params np+-params opr arg)
         ;; Update inner-semtype with new params.
         (setf inner-semtype (if n+-params
                               (first (type-params (first n+-params)))
                               (first (type-params (first np+-params)))))
         (setf updated-inner-semtype (copy-semtype inner-semtype))
         (setf (type-params updated-inner-semtype)
               (append (type-params updated-inner-semtype) new-params))
         (setf updated-+preds-semtype (copy-semtype opr))
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
    ((and (atomic-type-p opr) (eql (domain opr) 'qt-attr))
     (let ((type-params (get-semtype-type-params arg)))
       (when (find '*qt (get-all-top-domains type-params))
         (new-semtype 'qt-attr1 nil 1 nil :type-params (list arg)))))
    ; qt-attr1[T1[*qt]] + T2 >> T2[qt-attr1[T1[*qt]]]
    ((and (atomic-type-p opr) (eql (domain opr) 'qt-attr1))
     (let ((result (copy-semtype arg)))
       (add-semtype-type-params result (list opr))
       result))
    ; \" + T2[qt-attr1[T1[*qt]]] >> qt-attr2[T1[*qt]]
    ((and (atomic-type-p opr) (eql (domain opr) '\")
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
    ((and (atomic-type-p opr) (eql (domain opr) 'qt-attr2)
          (atomic-type-p arg) (eql (domain arg) '\"))
     (assert (= 1 (length (the list (type-params opr)))) (opr arg)
             "Didn't expect to have multiple type params for qt-attr2.~%opr: ~s~%arg: ~s~%"
             (semtype2str opr) (semtype2str arg))
     (let* ((result (copy-semtype (first (type-params opr))))
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
    ((and (atomic-type-p opr) (eql (domain opr) 'sub))
     (new-semtype 'sub1 nil 1 nil :type-params (list (copy-semtype arg))))
    ; Hole variables.
    ; T + *h << Range(T)[*h[Dom(T)]]
    ; T + *p >> Range(T)[*h[Dom(T)]]
    ((and (not (atomic-type-p opr)) (not (optional-type-p opr))
          (atomic-type-p arg) (member (domain arg) '(*h *p)))
     (let ((opr-dom (copy-semtype (domain opr)))
           (opr-ran (copy-semtype (range opr)))
           (arg-copy (copy-semtype arg)))
       (add-semtype-type-params arg-copy (list opr-dom))
       (add-semtype-type-params opr-ran (list arg-copy))
       opr-ran))
    ; sub1[T1] + T2[*h[T3]] >> T2, iff T1 can be the arg of T3.
    ((and (atomic-type-p opr) (eql (domain opr) 'sub1))
     (let ((*h-params (remove-if-not #'(lambda (x) (eql (domain x) '*h))
                                     (type-params arg)))
           (other-params (remove-if #'(lambda (x) (eql (domain x) '*h))
                                    (type-params arg))))
       ; Check that T1 and T3 exists and that they match.
       (when (and (not (null (type-params opr)))
                  (not (null *h-params))
                  (not (null (type-params (first *h-params))))
                  (semtype-match? (first (type-params opr))
                                  (first (type-params (first *h-params)))
                                  :ignore-exp t))
         (assert (= 1 (length (the list (type-params opr)))))
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
    ((and (atomic-type-p opr)
          (eql (domain opr) 'rep)
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
    ((and (atomic-type-p opr) (eql (domain opr) 'rep1))
     (let* ((t1-fn #'(lambda (tp)
                       (some #'(lambda (tptp) (eql (domain tptp) '*p))
                             (type-params tp))))
            (t2-fn #'(lambda (t1tp) (eql (domain t1tp) '*p)))
            (t1 (first (remove-if-not t1-fn (type-params opr))))
            (t2 (first (type-params (first (remove-if-not t2-fn
                                                          (type-params t1)))))))
       (when (semtype-match? t2 arg :ignore-exp t)
         (let ((retval (copy-semtype t1))
               (new-type-params (append (remove-if t1-fn (type-params opr))
                                        (remove-if t2-fn (type-params t1))
                                        (type-params arg))))
           (set-semtype-type-params retval new-type-params)
           retval))))
    ;;; 'S
    ;;; 1. D + POSTGEN1 >> POSTGEN2
    ;;; 2. POSTGEN2 + (D=>(S=>2))_N >> D
    ; 1. D + POSTGEN1 >> POSTGEN2
    ((and (atomic-type-p arg)
          (semtype-match? *term-semtype* opr)
          (eql (domain arg) 'postgen1))
     (new-semtype 'postgen2 nil 1 nil
                  :type-params (append (type-params opr) (type-params arg))))
    ; 2. POSTGEN2 + (D=>(S=>2))_N >> D
    ((and (atomic-type-p opr)
          (eql (domain opr) 'postgen2)
          (semtype-match? *unary-noun-semtype* arg :ignore-exp t))
     (let ((term-st (copy-semtype *term-semtype*)))
       (setf (type-params term-st)
             (append (type-params opr) (type-params arg)))
       term-st))
    ;;; PARG
    ;;; 1. PARG + T => PARG1[T] ; remove lexical
    ;;; 2a. T1_V + PARG1[T2] => T1_V(T2) {application}
    ;;; 2b. T1_{N,A} + PARG1[T2] => T1_{N,A}
    ; 1. PARG + T => PARG[T] ; remove lexical
    ((and (atomic-type-p opr) (eql (domain opr) 'parg))
     (let ((stored-type (copy-semtype arg)))
       (add-feature-values (synfeats stored-type) '(!lex)) ; delexicalize
       (new-semtype 'parg1 nil 1 nil :type-params (list stored-type))))
    ; 2a. T1_V + PARG1[T2] => T1_V(T2) {application} ; delexicalized
    ((and (semtype-match? *general-verb-semtype* opr)
          (atomic-type-p arg) (eql (domain arg) 'parg1)
          (extended-compose-types! opr (first (type-params arg))))
     (extended-compose-types! opr (first (type-params arg))))
    ; 2b. T1_{N,A} + PARG1[T2] => T1_{N,A} ; delexicalized
    ((and (member (suffix opr) '(n a))
          (atomic-type-p arg) (eql (domain arg) 'parg1))
     (copy-semtype
       opr
       :c-synfeats (add-feature-values (copy (synfeats opr)) '(!lex))))
    ;; Fall back to EL compositional functionality.
    (t (apply-operator! opr arg :recurse-fn recurse-fn))))


(defun left-right-apply-operator! (opr arg
                                   &key (recurse-fn #'left-right-apply-operator!)
                                        ignore-synfeats)
  "A further relaxation of `extended-apply-operator!` which generalizes the
  type system to allow left-to-right composition even when there is infixing,
  inversions, and sentence modifiers."
  (cond
    ;;; SUBJECT(term) + VP
    ; Treat it like it's VP + TERM
    ; TODO(gene): Ideally, we would force it to be a sentence, but we won't
    ; worry about that for now.
    ((and (semtype-match? *term-semtype* opr)
          (semtype-match? *general-verb-semtype* arg))
     ;(format t "1!!~%")
     (apply-operator! arg opr))
    ;;; ADV-S + * >> *
    ; This will over generate for non-paired SENT-MODs
    ; TODO(gene): generate a type that turns back to * after combining with COMPLEX
    ; TODO(gene): find a way to distinguish between adv-s, adv-e, and other sent-mods.
    ((semtype-match? *sent-mod-semtype* opr) arg)
    ;;; * + SENT-MOD >> *
    ((semtype-match? *sent-mod-semtype* arg) opr)
    ;; Fall back to extended-apply-operator! when special cases are not
    ;; relevant.
    (t (extended-apply-operator! opr arg :recurse-fn recurse-fn))))

(defun extended-compose-types! (type1 type2 &key ignore-synfeats)
  "Compose two types if possible and return the composed type. Also return the
  order in which the types were composed. The strict EL type compositions are
  extended to include ULF macros and structural relaxations."
  (compose-types! type1 type2
                  :opr-apply-fn #'extended-apply-operator!
                  :ignore-synfeats ignore-synfeats))


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


(defun left-right-compose-types! (type1 type2 &key ignore-synfeats)
  "left-right version of compose-types!
  "
  (compose-types! type1 type2
                  :opr-apply-fn #'left-right-apply-operator!
                  :ignore-synfeats ignore-synfeats))


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
  
