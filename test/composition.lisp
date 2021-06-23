;;; Gene Louis Kim, 1-14-2020
;;; Unit tests for verifying the ulf-composition functions.

(in-package :ulf-lib/tests)

;; Out-of-package versions of internal function calls.
(defun extended-compose-type-string! (int1 int2)
  (gute:in-intern (int1 t1 :ulf-lib)
    (gute:inout-intern (int2 t2 :ulf-lib :callpkg :ulf-lib/tests)
      (ulf-lib::extended-compose-type-string! t1 t2))))
(defun left-right-compose-type-string! (int1 int2)
  (gute:in-intern (int1 t1 :ulf-lib)
    (gute:inout-intern (int2 t2 :ulf-lib :callpkg :ulf-lib/tests)
      (ulf-lib::left-right-compose-type-string! t1 t2))))


(defun semtype-str-equal (str1 str2)
  "Takes to string representations of semtypes and checks if they are equal,
  expanding out options into the same format."
  (ulf-lib::strict-semtype-equal
    (ulf-lib::extended-str2semtype str1)
    (ulf-lib::extended-str2semtype str2)))

(defun string-from-compose-types (ulf1 ulf2 &key (ext nil))
  "Takes two atomic ULF expressions, composes the types and returns the string
  representation of the type.

  ext
    nil         - Uses default composition
    'extended   - Uses extended variant
    'left-right - Uses left-right variant
  "
  (let ((type1 (ulf-type-string? ulf1))
        (type2 (ulf-type-string? ulf2))
        (compose-string-fn
          (case ext
            (extended #'extended-compose-type-string!)
            (left-right #'left-right-compose-type-string!)
            ((nil) #'compose-type-string!)
            (otherwise (error "Unknown composition variant: ~s~%" ext)))))
    (funcall compose-string-fn type1 type2)))

(define-test basic-compose
  "Basic examples for ULF composition"
  (:tag :compose-basic)
  (assert-equal "D" (string-from-compose-types 'the.d 'man.n))
  (assert-equal nil (string-from-compose-types 'the.d 'the.d))
  (assert-equal
    "{(S=>2)|{(D=>(S=>2))_V|{({D|(D=>(S=>2))}=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V|({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V}}}}}"
    (string-from-compose-types 'help.v 'me.pro))
  (assert-equal nil (string-from-compose-types 'be.v 'me.pro))
  (assert-equal "(D=>(S=>2))_V" (string-from-compose-types 'be.v 'happy.a)))

(define-test basic-macro-compose
  "Basic examples for macro ULF composition"
  (:tag :compose-basic)
  ; tense
  (assert-equal "{(D=>(S=>2))_V_T|{{(D=>(D=>(S=>2)))_V_T|((D=>(S=>2))=>(D=>(S=>2)))_V_T}|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V_T|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V_T|{({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V_T|({D|(D=>(S=>2))}^5=>(D=>(S=>2)))_V_T}}}}}" (string-from-compose-types 'pres 'run.v :ext 'extended))
  (assert-equal nil (string-from-compose-types 'past 'happy.a :ext 'extended))
  (assert-equal nil (string-from-compose-types 'cf 'man.n :ext 'extended))
  ; *+preds
  (assert-equal "{+PREDS[N+[(D=>(S=>2))_N]]|(D=>(S=>2))_N}" (string-from-compose-types 'n+preds 'man.n :ext 'extended))
  (assert-equal "{+PREDS[NP+[D]]|D}" (string-from-compose-types 'np+preds 'he.pro :ext 'extended))
  ; sub/rep
  (assert-equal "SUB1[D]" (string-from-compose-types 'sub 'he.pro :ext 'extended))
  (assert-equal nil (string-from-compose-types 'rep 'man.n :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "D[*P[{(D=>(S=>2))_N|(D=>(S=>2))_P}]]"
    (string-from-compose-types 'the.d '*p :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "REP1[D[*P[{(D=>(S=>2))_N|(D=>(S=>2))_P}]]]"
    (string-from-compose-types 'rep '(the.d *p) :ext 'extended))
  ; qt-attr
  (assert-equal nil (string-from-compose-types 'qt-attr 'her.pro :ext 'extended))
  (assert-equal "{(S=>2)[*QT]|{(D=>(S=>2))_V[*QT]|{({D|(D=>(S=>2))}=>(D=>(S=>2)))_V[*QT]|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V[*QT]|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V[*QT]|({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V[*QT]}}}}}"
                (string-from-compose-types 'say.v '*qt :ext 'extended))
  (assert-equal "QT-ATTR1[{(S=>2)[*QT]|{(D=>(S=>2))_V[*QT]|{{(D=>(D=>(S=>2)))_V[*QT]|((D=>(S=>2))=>(D=>(S=>2)))_V[*QT]}|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V[*QT]|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V[*QT]|({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V[*QT]}}}}}]"
                (string-from-compose-types 'qt-attr '(say.v *qt) :ext 'extended))
  ; 's
  (assert-equality
    #'semtype-str-equal
    "POSTGEN2"
    (string-from-compose-types '|Gene| '|'S| :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "D"
    (string-from-compose-types '(|Gene| |'S|) 'dog.n :ext 'extended)))

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
  (assert-equal "(D=>(S=>2))[*QT]" (string-from-compose-types '= '*qt :ext 'extended))
  (assert-equal "QT-ATTR1[(D=>(S=>2))[*QT]]"
                (string-from-compose-types 'qt-attr '(= *qt) :ext 'extended))
  (assert-equal "D[QT-ATTR1[(D=>(S=>2))[*QT]]]"
                (string-from-compose-types '(qt-attr (= *qt)) 'it.pro :ext 'extended))
  (assert-equal "QT-ATTR2[(D=>(S=>2))[*QT]]"
                (string-from-compose-types '\" '((qt-attr (= *qt)) it.pro) :ext 'extended))
  (assert-equal "(D=>(S=>2))"
                (string-from-compose-types (list '|"| '((qt-attr (= *qt)) it.pro)) '|"|
                                           :ext 'extended)))

(define-test aux-compose
  "Tests for auxiliaries."
  (:tag :aux-compose)
  ;; This is very similar to tense.
  ;; AUX + (D=>(S=>2))_V (no T, X) >> (D=>(S=>2))_V_X
  ;; TENSE + AUX => TAUX
  ;; TAUX + (D=>(S=>2))_V (no T, X) >> (D=>(S=>2))_V_T_X
  (assert-equality
    #'semtype-str-equal
    "(D=>(S=>2))_V_X"
    (string-from-compose-types 'do.aux-s 'run.v :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "TAUX"
    (string-from-compose-types 'past 'do.aux-s :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "(D=>(S=>2))_V_T_X"
    (string-from-compose-types '(past do.aux-s) 'run.v :ext 'extended))
  (assert-equal nil (string-from-compose-types
                      'do.aux-s '(do.aux-s run.v) :ext 'extended))
  (assert-equal nil (string-from-compose-types
                      'past '(past do.aux-s) :ext 'extended))
  (assert-equal nil (string-from-compose-types
                      '(past do.aux-s) '(do.aux-s run.v) :ext 'extended)))

(define-test p-arg-compose
  "Tests for p-arg types."
  (:tag :p-arg-compose)
  ;; PARG + T >> PARG[T]
  ;; T1_{v,n,a} + PARG[T2] >> T1(T2) {application}
  (assert-equality #'equal "PARG" (ulf-type-string? 'with.p-arg))
  (assert-equality
    #'semtype-str-equal
    "PARG1[D]"
    (string-from-compose-types 'in.p-arg 'that.pro :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    (format nil "PARG1[~a]" (ulf-type-string? 'chicken.n))
    (string-from-compose-types 'as.p-arg 'chicken.n :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "{(S=>2)|{(D=>(S=>2))_V|{({D|(D=>(S=>2))}=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V|({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V}}}}}"
    (string-from-compose-types 'sleep.v '(in.p-arg that.pro) :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "{(D=>(S=>2))_V|{({D|(D=>(S=>2))}=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V|({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V}}}}"
    (string-from-compose-types 'dress.v '(as.p-arg chicken.n) :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "{(D=>(S=>2))_A|(D=>(D=>(S=>2)))_A}"
    (string-from-compose-types 'liked.a '(by.p-arg (the.d audience.n)) :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "(D=>(S=>2))_N"
    (string-from-compose-types 'sale.n '(of.p-arg (his.d car.n)) :ext 'extended))
  (assert-equal nil (string-from-compose-types 'in.p '(in.p-arg that.pro)))
  (assert-equal nil (string-from-compose-types 'the.d '(of.p-arg (k force.n))))
  (assert-equal nil (string-from-compose-types 'him.pro '(in.p-arg that.pro)))
  (assert-equal nil (string-from-compose-types 'quickly.adv-a '(at.p-arg her.pro))))


(define-test sentential-punctuation-compose
  "Tests for ! and ?."
  (:tag :sentential-punctuation-compose)
  (assert-equal "((S=>2)=>(S=>2))" (ulf-type-string? '!))
  (assert-equal "((S=>2)=>(S=>2))" (ulf-type-string? '?))
  (assert-equality
    #'semtype-str-equal
    "(S=>2)"
    (string-from-compose-types '(i.pro (go.v there.pro)) '! :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "(S=>2)"
    (string-from-compose-types '(i.pro (go.v there.pro)) '? :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "(S=>2)_T"
    (string-from-compose-types '! '(i.pro ((past go.v) there.pro)) :ext 'extended))
  (assert-equality
    #'semtype-str-equal
    "(S=>2)_T"
    (string-from-compose-types '? '(i.pro ((past go.v) there.pro)) :ext 'extended)))

(define-test comp-p-arg-mod
  "Tests for adding mod-a modifiers to p-arg."
  (:tag :comp-p-arg-mod)
  ; NB(gene): I'm not sure we want this. Instead we should just use
  ; right.adv-s. *.ps are very much not adjective-like. Perhaps we could use
  ; right.mod-p when we introduce it.
  (assert-equality
    #'semtype-str-equal
    "((S=>2)=>((S=>2)=>(S=>2)))"
    (string-from-compose-types 'right.mod-a '(before.ps (i.pro ((past move.v) it.pro)))))
  (assert-equal nil
    (string-from-compose-types 'right.mod-n '(before.ps (i.pro ((past move.v) it.pro))))))

(define-test subject-vp
  "Subject verb phrase composition for extended typing."
  (:tag :subject-vp :left-right)
  (multiple-value-bind
    (typestr dir original)
    (string-from-compose-types 'he.pro 'run.v :ext 'left-right)
    (declare (ignore original))
    (assert-equal "right" dir
                  (ulf-type-string? 'he.pro) (ulf-type-string? 'run))
    (assert-equality
      #'semtype-str-equal
      "{(S=>2)|{(D=>(S=>2))_V|{({D|(D=>(S=>2))}=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V|({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V}}}}}"
      typestr))
  (multiple-value-bind
    (typestr dir original)
    (string-from-compose-types 'he.pro 'run.v :ext 'extended)
    (declare (ignore original))
    (assert-equal "left" dir
                  (ulf-type-string? 'he.pro) (ulf-type-string? 'run))
    (assert-equality
      #'semtype-str-equal
      "{(S=>2)|{(D=>(S=>2))_V|{({D|(D=>(S=>2))}=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^2=>(D=>(S=>2)))_V|{({D|(D=>(S=>2))}^3=>(D=>(S=>2)))_V|({D|(D=>(S=>2))}^4=>(D=>(S=>2)))_V}}}}}"
      typestr))
  (multiple-value-bind
    (typestr dir original)
    (string-from-compose-types 'it.pro '((pres do.aux-s) see.v) :ext 'left-right)
    (declare (ignore original))
    (assert-equal "right" dir
                  (ulf-type-string? 'it.pro) (ulf-type-string? '((pres do.aux-s) see.v)))
    (assert-equality #'semtype-str-equal "(S=>2)_T" typestr))
  (multiple-value-bind
    (typestr dir original)
    (string-from-compose-types 'it.pro '((pres do.aux-s) see.v) :ext 'extended)
    (declare (ignore original))
    (assert-equal "left" dir
                  (ulf-type-string? 'it.pro) (ulf-type-string? '((pres do.aux-s) see.v)))
    (assert-equality #'semtype-str-equal "(S=>2)_T" typestr)))


(define-test free-sent-mod
  (:tag :free-sent-mod :left-right)
  (assert-equality #'semtype-str-equal "((S=>2)=>(S=>2))" (ulf-type-string? 'not))
  (assert-equality #'semtype-str-equal "((S=>2)=>(S=>2))" (ulf-type-string? 'not.adv-s))
  (assert-equality #'semtype-str-equal "((S=>2)=>(S=>2))" (ulf-type-string? 'always.adv-f))
  (assert-equality #'semtype-str-equal "((S=>2)=>(S=>2))" (ulf-type-string? 'today.adv-e))
  (assert-equality #'semtype-str-equal "((S=>2)=>(S=>2))" (ulf-type-string? '(when.ps (i.pro (past sleep.v)))))
  (let ((ulf-segments
          '(he.pro
             run.v
             dog.n
             (past run.v)
             (past do.aux-s)
             (k dog.n)
             ((past see.v) him.pro)
             (i.pro ((past see.v) him.pro))))
        (sent-mods
          '(not
             not.adv-s
             always.adv-f
             today.adv-e
             (when.ps (i.pro (past sleep.v))))))
    (loop for ulf-segment in ulf-segments
          do
          (loop for sent-mod in sent-mods
                do
                (progn
                  (multiple-value-bind
                    (typestr dir original)
                    (string-from-compose-types sent-mod ulf-segment :ext 'left-right)
                    (declare (ignore original))
                    ; TODO(gene): this direction shouold only work for adv-s if we were being precise.
                    (assert-equality #'semtype-str-equal
                                     (ulf-type-string? ulf-segment)
                                     typestr)
                    (assert-equal "right" dir))
                  (multiple-value-bind
                    (typestr dir original)
                    (string-from-compose-types ulf-segment sent-mod :ext 'left-right)
                    (declare (ignore original))
                    (assert-equality #'semtype-str-equal
                                     (ulf-type-string? ulf-segment)
                                     typestr)
                    (assert-equal "right" dir)))))))

(define-test itaux
  (:tag :itaux :left-right)
  (assert-equality
    #'semtype-str-equal
    "ITAUX"
    (string-from-compose-types '(past do.aux-s) 'him.pro :ext 'left-right))
  ; TODO(gene): write a generalization of ulf-type? for left-right type.
  ; for now, we will just bypass it under the assumption that the test above
  ; this one passed.
  (assert-equality
    #'semtype-str-equal
    "(S=>2)_T"
    (left-right-compose-type-string! "ITAUX" (ulf-type-string? 'run.v)))
  (assert-equal nil (string-from-compose-types
                      '(past do.aux-s) 'dog.n
                      :ext 'left-right))
  (assert-equal nil (left-right-compose-type-string!
                      "ITAUX" (ulf-type-string? '(pres run.v))))
  (assert-equal nil (left-right-compose-type-string!
                      "ITAUX" (ulf-type-string? 'him.pro)))
  (assert-equal nil (left-right-compose-type-string!
                      "ITAUX" (ulf-type-string? '((pres may.aux-s) see.v)))))

