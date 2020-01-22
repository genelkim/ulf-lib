;; ULF composition functions

(in-package :ulf-lib)

(defun  merge-subscripts (ss1 ss2)
  "Merge the two subscripts, considering ss1 as the main subscript. If they are
  contradictory, take the ss1 value.
  "
  (let*
    ((ss1chars (if (null ss1) nil (coerce (symbol-name ss1) 'list)))
     (ss2chars (if (null ss2) nil (coerce (symbol-name ss2)'list)))
     (poses (coerce "NAVP" 'list))
     (new-val
       (cond
         ((intersection poses ss1chars) (find-if #'(lambda(x) (member x poses)) ss1chars))
         ((intersection poses ss2chars) (find-if #'(lambda(x) (member x poses)) ss2chars))
         (t nil)))
     (filtered (remove-if #'null (list new-val))))
    (if (null filtered)
      nil
      (intern (coerce filtered 'string)))))

(defun merge-tenses (t1 t2)
  "Merge the two tenses, considering t1 as the main subscript. If they are
  contradictory, take the t1 value.
  "
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
      (intern (coerce filtered 'string)))))

;; Compose a given operator and argument if possible.
;; Assumption (for now): Arg has no exponent. If it does, it is ignored.
;; Any subscripts and tenses are propagated from op.
;; type-params are propagated from both.
(defun apply-operator! (op arg &key (recurse-fn #'apply-operator!))
  (let
    ((new-params (append (type-params op) (type-params arg)))
     (result
       (cond
         ((optional-type-p op)
          ; Operator is an optional type
          (let ((a (funcall recurse-fn (car (types op)) arg))
                (b (funcall recurse-fn (cadr (types op)) arg)))
            (if (and a b)
              (new-semtype  nil nil 1 nil nil :options (list a b))
              (or a b))))
         ; Operator is not optional and atomic operator of the form A^n with n>1
         ((atomic-type-p op)
          (when (and (semtype-equal? op arg :ignore-exp T) (> (ex op) 1))
            (new-semtype  (domain op) nil (- (ex op) 1) (subscript op) (tense op))))
         ;; Operator is a non-atomic type with domain exponent n=1
         ((and (semtype-p op) (semtype-equal? (domain op) arg :ignore-exp T) (= (ex (domain op)) 1))
          (let ((result (copy-semtype (range op))))
            ;; Only add subscript when not atomic or (S=>2)
            (when (not (or (atomic-type-p result) (equal (semtype2str result) "(S=>2)")))
              (setf (subscript result)
                    (merge-subscripts (subscript (range op)) (subscript op))))
            ;; Only add tense when not atomic.
            (when (not (atomic-type-p result))
              (setf (tense result)
                    (merge-tenses (tense (range op)) (tense op))))
            result))
         ;; Operator is non-atomic type with domain exponent n>1
         ((and (semtype-p op) (semtype-equal? (domain op) arg :ignore-exp T))
          (let ((result (copy-semtype op)))
            (setf (ex (domain result)) (- (ex (domain result)) 1))
            result)))))
    ; Update type params before returning.
    (add-semtype-type-params result new-params)
    result))

;; Compose two types if possible and return the composed type. Also return the
;; order in which the types were composed.
(defun compose-types! (x y &key (op-apply-fn #'apply-operator!))
  (if (or (not x) (not y))
    (or x y)
    (let ((comp (funcall op-apply-fn x y)))
      (if comp
        (values comp (list x y))
        (progn
          (setf comp (funcall op-apply-fn y x))
          (when comp (values comp (list y x))))))))

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
        (new-semtype (str2semtype "D") (ulf-type? (third ulf) :lambda-vars (cons (cadr ulf) lambda-vars)) 1 nil nil))
      ; ULF is neither a lambda nor an atom
      (if (= (length ulf) 1)
        (ulf-type? (car ulf) :lambda-vars lambda-vars)
        (extended-compose-types! (ulf-type? (reverse (cdr (reverse ulf))) :lambda-vars lambda-vars)
                        (ulf-type? (car (last ulf)) :lambda-vars lambda-vars))))))

;; Given a ULF, evaluate the type if possible and return a string representation
;; of the type.
(defun ulf-type-string? (ulf)
  (semtype2str (ulf-type? ulf)))

(defun str-ulf-type-string? (string-ulf)
  (ulf-type-string? (read-from-string string-ulf)))

;; Given two types are strings, compose them if possible and return the
;; resulting type as a string.
(defun compose-type-string! (type1 type2)
  (semtype2str (compose-types! (str2semtype type1) (str2semtype type2))))

(defparameter *unary-noun-semtype* (str2semtype "(D=>(S=>2))_n"))
(defparameter *unary-pred-semtype* (str2semtype "(D=>(S=>2))"))
(defparameter *unary-verb-semtype* (str2semtype "(D=>(S=>2))_v"))
(defparameter *general-verb-semtype* (str2semtype "({D|(D=>(S=>2))}^n=>(D=>(S=>2)))_v"))
(defparameter *term-semtype* (str2semtype "D"))

(defun extended-apply-operator! (op arg)
  "Compose a given operator and argument if possible. Assumption (for now): Arg
  has no exponent. If it does, it is ignored. The strict EL type compositions
  are extended to include ULF macros and structural relaxations.
  "
  (cond
    ; TENSE, N+PREDS, NP+PREDS, +PREDS, etc. can't be the operand type.
    ((and (atomic-type-p arg) 
          (member (domain arg) 
                  '(tense n+preds np+preds +preds qt-attr qt-attr1 sub sub1 rep rep1)))
     nil)
    ;;; TENSE
    ;;; TENSE + TYPE_V => TYPE_TV
    ((and (atomic-type-p op) (eql (domain op)'tense))
     (if (semtype-equal? arg *general-verb-semtype* :ignore-exp t)
       (let ((tensed-semtype (copy-semtype arg)))
         (add-semtype-tense tensed-semtype t)
         tensed-semtype)))
    ;;; N+PREDS
    ;;; 1. n+preds + N_n >> {+preds[n+[N_n]]|N_n}
    ;;; 2. +preds[n+[T]] + N >> {+preds[n+[T]]|T}
    ;;; Stops when it (+preds) is used as the T (a variant of the unary noun).
    ; n+preds + N_n >> {+preds[n+[N_n]]|N_n}
    ((and (atomic-type-p op) (eql (domain op) 'n+preds))
     ;;TODO(gene): probably need semtype-sufficent? which doesn't chcek exact
     ;;equality,butwhetheranargumentissufficientfortheslottype.For
     ;;example(D=>(S=>2))_nisnotequalto(D=>(S=>2)),butitisa
     ;;sufficientargument.
     (if (semtype-equal? arg *unary-noun-semtype* :ignore-exp t)
       (let* ((n+-semtype (new-semtype 'n+ nil 1 nil nil :type-params (list arg)))
              (+preds-semtype (new-semtype '+preds nil 1 nil nil :type-params (list n+-semtype))))
         ;;Optionalsemtypeofeithercontinuingtoactas+preds,orastheinternalnoun.
         (new-semtype nil nil 1 nil nil :options (list +preds-semtype arg)))))
    ;;; NP+PREDS
    ;;; 1. np+preds + D >> {+preds[n+[D]]|D}
    ;;; 2. +preds[n+[T]] + N >> {+preds[n+[T]]|T}
    ;;; Stops when it (+preds) is used as the T (a variant of the unary noun).
    ; np+preds + D >> {+preds[n+[D]]|D}
    ((and (atomic-type-p op) (eql (domain op) 'np+preds))
     (if (semtype-equal? arg *term-semtype* :ignore-exp t)
       (let* ((np+-semtype (new-semtype 'np+ nil 1 nil nil :type-params (list arg)))
              (+preds-semtype (new-semtype '+preds nil 1 nil nil :type-params (list np+-semtype))))
         ;; Optional semtype of either continuing to act as +preds, or as the internal noun.
         (new-semtype  nil nil 1 nil nil :options (list +preds-semtype arg)))))
    ; +preds[n+[T]] + N >> {+preds[n+[T]]|T}(N+PREDS&NP+PREDS)
    ((and (atomic-type-p op) (eql (domain op)'+preds))
     (if (semtype-equal? arg *unary-pred-semtype* :ignore-exp t)
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
         (setf inner-semtype (if n+-params (first n+-params) (first np+-params)))
         (setf updated-inner-semtype (copy-semtype inner-semtype))
         (setf (type-params updated-inner-semtype)
               (append (type-params updated-inner-semtype) new-params))
         (setf updated-+preds-semtype (copy-semtype op))
         (setf (type-params updated-+preds-semtype) new-params)
         (new-semtype nil nil 1 nil nil :options (list updated-+preds-semtype updated-inner-semtype)))))
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
       (when (find '*qt (mapcar #'domain type-params))
         (new-semtype 'qt-attr1 nil 1 nil nil :type-params (list arg)))))
    ; qt-attr1[T1[*qt]] + T2 >> T2[qt-attr1[T1[*qt]]]
    ((and (atomic-type-p op) (eql (domain op) 'qt-attr1))
     (let ((result (copy-semtype arg)))
       (add-semtype-type-params result op)
       result))
    ; \" + T2[qt-attr1[T1[*qt]]] >> qt-attr2[T1[*qt]]
    ((and (atomic-type-p op) (eql (domain op) '\")
          (some #'(lambda (param) (eql (domain param) 'qt-attr1))
                (get-semtype-type-params arg)))
     (let* ((qt-attr1-param 
              (find-if #'(lambda (param) (eql (domain param) 'qt-attr1))
                       (get-semtype-type-params arg))))
     (new-semtype 'qt-attr2 nil 1 nil nil :type-params (type-params qt-attr1-param))))
    ;;; qt-attr2[T1[*qt]] + \" >> T1
    ((and (atomic-type-p op) (eql (domain op) 'qt-attr2)
          (atomic-type-p arg) (eql (domain arg) '\"))
     (assert (= 1 (length (type-params op))) (op arg)
             "Didn't expect to have multiple type params for qt-attr2.~%op: ~s~%arg: ~s~%"
             (semtype2str op) (semtype2str arg))
     (let ((result (copy-semtype (first (type-params op))))
           (qt-removed-type-params
             (remove-if #'(lambda (param) (eql (domain param) '*qt))
                        (get-semtype-type-params op))))
       (set-type-params result qt-removed-type-params)
       result))

    ;; Fall back to EL compositional functionality.
    (t (apply-operator! op arg :recurse-fn #'extended-apply-operator!))))

(defun extended-compose-types! (type1 type2)
  "Composetwotypesifpossibleandreturnthecomposedtype.Alsoreturnthe
  orderinwhichthetypeswerecomposed.ThestrictELtypecompositionsare
  extendedtoincludeULFmacrosandstructuralrelaxations.
  "
  (compose-types! type1 type2 :op-apply-fn #'extended-apply-operator!))


(defun extended-compose-type-string!(type1 type2)
  "Giventwotypesasstrings,composethemifpossibleandreturnthe
  resultingtypeasastring.ThestrictELtypecompositionsareextendedto
  includeULFmacrosandstructuralrelaxations.
  "
  (semtype2str (extended-compose-types! (extended-str2semtype type1)
                                        (extended-str2semtype type2))))

