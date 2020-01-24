;;; Gene Louis Kim, 1-14-2020
;;; Unit tests for verifying the ulf-composition functions.

(in-package :ulf-lib)

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(setq *summarize-results* t)

(defun semtype-str-equal (str1 str2)
  "Takes to string representations of semtypes and checks if they are equal,
  expanding out options into the same format."
  (strict-semtype-equal
    (extended-str2semtype str1)
    (extended-str2semtype str2)))

(defun string-from-compose-types (ulf1 ulf2 &key (extended? nil))
  "Takes two atomic ULF expressions, composes the types and returns the string
  representation of the type."
  (let ((type1 (ulf-type-string? ulf1))
        (type2 (ulf-type-string? ulf2))
        (compose-string-fn (if extended? 
                             #'extended-compose-type-string!
                             #'compose-type-string!)))
    (funcall compose-string-fn type1 type2 )))

(define-test basic-compose
  "Basic examples for ULF composition"
  (:tag :compose-basic)
  (assert-equal "D" (string-from-compose-types 'the.d 'man.n))
  (assert-equal nil (string-from-compose-types 'the.d 'the.d))
  (assert-equal "{(S=>2)|{(D=>(S=>2))_V|{({D|(D=>(S=>2))}=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V|({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V}}}}}"
                (string-from-compose-types 'help.v 'me.pro))
  (assert-equal nil (string-from-compose-types 'be.v 'me.pro))
  (assert-equal "(D=>(S=>2))_V" (string-from-compose-types 'be.v 'happy.a)))
    
(define-test basic-macro-compose
  "Basic examples for macro ULF composition"
  (:tag :compose-basic)
  ; tense
  (assert-equal "{(D=>(S=>2))_V_T|{{(D=>(D=>(S=>2)))_V_T|((D=>(S=>2))=>(D=>(S=>2)))_V_T}|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V_T|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V_T|{({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V_T|({D|(D=>(S=>2))}^5=>(D=>(S=>2)))_V_T}}}}}" (string-from-compose-types 'pres 'run.v :extended? t))
  (assert-equal nil (string-from-compose-types 'past 'happy.a :extended? t))
  (assert-equal nil (string-from-compose-types 'cf 'man.n :extended? t))
  ; *+preds
  (assert-equal "{+PREDS[N+[(D=>(S=>2))_N]]|(D=>(S=>2))_N}" (string-from-compose-types 'n+preds 'man.n :extended? t))
  (assert-equal "{+PREDS[NP+[D]]|D}" (string-from-compose-types 'np+preds 'he.pro :extended? t))
  ; sub/rep
  (assert-equal "SUB1[D]" (string-from-compose-types 'sub 'he.pro :extended? t))
  (assert-equal nil (string-from-compose-types 'rep 'man.n :extended? t))
  (assert-equality 
    #'semtype-str-equal
    "D[*P[{(D=>(S=>2))_N|(D=>(S=>2))_P}]]"
    (string-from-compose-types 'the.d '*p :extended? t))
  (assert-equality
    #'semtype-str-equal
    "REP1[D[*P[{(D=>(S=>2))_N|(D=>(S=>2))_P}]]]"
    (string-from-compose-types 'rep '(the.d *p) :extended? t))
  ; qt-attr
  (assert-equal nil (string-from-compose-types 'qt-attr 'her.pro :extended? t))
  (assert-equal "{(S=>2)[*QT]|{(D=>(S=>2))_V[*QT]|{({D|(D=>(S=>2))}=>(D=>(S=>2)))_V[*QT]|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V[*QT]|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V[*QT]|({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V[*QT]}}}}}"
                (string-from-compose-types 'say.v '*qt :extended? t))
  (assert-equal "QT-ATTR1[{(S=>2)[*QT]|{(D=>(S=>2))_V[*QT]|{{(D=>(D=>(S=>2)))_V[*QT]|((D=>(S=>2))=>(D=>(S=>2)))_V[*QT]}|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V[*QT]|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V[*QT]|({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V[*QT]}}}}}]"
                (string-from-compose-types 'qt-attr '(say.v *qt) :extended? t)))

(define-test tense-compose
  "A more exhaustive set of tests for tense composition."
  (:tag :specific-compose)
  (assert-true t)
  )

(define-test +preds-compose
  "A more exhaustive set of tests for n+preds and np+preds."
  (:tag :specific-compose)
  (assert-true t)
  )

(define-test qt-attr-compose
  "A more exhaustive set of tests for qt-attr."
  (:tag :specific-compose)
  ;; The whole process of this is as follows
  ;; Type(*qt): D[*qt],   Type(qt-attr): qt-attr
  ;; (say.v *qt)                            Type((say.v *qt)) = Compose(Type(say.v),D)[*qt]
  ;; (qt-attr (say.v *qt))                  qt-attr + T[*qt] >> qt-attr1[T[*qt]]
  ;; (yes! (qt-attr (say.v *qt)))           qt-attr1[T1[*qt]] + T2 >> T2[qt-attr[T1[*qt]]]
  ;; (\" (yes! (qt-attr (say.v *qt))))      \" + T2[qt-attr[T1[*qt]]] >> qt-attr2[T1[*qt]]
  ;; (\" (yes! (qt-attr (say.v *qt))) \")   qt-attr2[T1[*qt]] + \" >> T1
  (assert-equal "\"" (ulf-type-string? '\"))
  (assert-equal "D[*QT]" (ulf-type-string? '*qt))
  (assert-equal "(D=>(S=>2))[*QT]" (string-from-compose-types '= '*qt :extended? t))
  (assert-equal "QT-ATTR1[(D=>(S=>2))[*QT]]"
                (string-from-compose-types 'qt-attr '(= *qt) :extended? t))
  (assert-equal "D[QT-ATTR1[(D=>(S=>2))[*QT]]]"
                (string-from-compose-types '(qt-attr (= *qt)) 'it.pro :extended? t))
  (assert-equal "QT-ATTR2[(D=>(S=>2))[*QT]]"
                (string-from-compose-types '\" '((qt-attr (= *qt)) it.pro) :extended? t))
  (assert-equal "(D=>(S=>2))"
                (string-from-compose-types (list '|"| '((qt-attr (= *qt)) it.pro)) '|"|
                                           :extended? t)))

