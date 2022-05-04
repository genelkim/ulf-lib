
(in-package :ulf-lib/tests)

(defun str2str-compose-types (str1 str2)
  (ulf-lib::semtype2str
    (ulf-lib::compose-types!
      (ulf-lib::extended-str2semtype str1)
      (ulf-lib::extended-str2semtype str2))))

(define-test basic-synfeats-composition
  "Tests regarding the syntactic feature compositions."
  (:tag :synfeats :compose)

  ;; Tense only propagates to predicates and sentences.
  (assert-equality
    #'semtype-equal?-str
    "(S=>2)%T"
    (str2str-compose-types "(D=>(S=>2))%T"
                           "D"))
  (assert-equality
    #'semtype-equal?-str
    "2"
    (str2str-compose-types "(D=>2)%T"
                           "D"))
  (assert-equality
    #'semtype-equal?-str
    "(D=>2)"
    (str2str-compose-types "(D=>(D=>2))%T"
                           "D"))

  ;; Plurality and passive, progressive only propagates to predicates.
  ;; Auxiliary, perfect, and progressive propagates to sentences.
  (loop for feat in '(x pl pf pv pg)
        do (assert-equality
             #'semtype-equal?-str
             (format nil "(D=>(S=>2))%~s" feat)
             (str2str-compose-types
               (format nil "(D=>(D=>(S=>2)))%~s" feat)
               "D"))
        if (member feat '(pl pv))
        do (assert-equality
             #'semtype-equal?-str
             "(S=>2)"
             (str2str-compose-types
               (format nil "(D=>(S=>2))%~s" feat)
               "D"))
        else do (assert-equality
                  #'semtype-equal?-str
                  (format nil "(S=>2)%~s" feat)
                  (str2str-compose-types
                    (format nil "(D=>(S=>2))%~s" feat)
                    "D"))))

(define-test multiple-synfeats-composition
  "Tests syntactic feature compositions with multiple features mixing."
  (:tag :synfeats :compose)

  ;; Plurality and passive, progressive only propagates to predicates.
  ;; Auxiliary, perfect, and progressive propagates to sentences.
  (loop for feat1 in '(x pl pf pv pg)
        do (loop for feat2 in '(x pl pf pv pg)
                 when (not (eql feat1 feat2))
                 do (assert-equality
                      #'semtype-equal?-str
                      (format nil "(D=>(S=>2))%~s,~s" feat1 feat2)
                      (str2str-compose-types
                        (format nil "(D=>(D=>(S=>2)))%~s,~s" feat1 feat2)
                        "D"))
                 ;; Both only propagate to predictes
                 when (and (not (eql feat1 feat2))
                           (member feat1 '(pl pv))
                           (member feat2 '(pl pv)))
                 do (assert-equality
                      #'semtype-equal?-str
                      "(S=>2)"
                      (str2str-compose-types
                        (format nil "(D=>(S=>2))%~s,~s" feat1 feat2)
                        "D"))
                 ;; Both propagate to sentences
                 when (and (not (eql feat1 feat2))
                           (not (member feat1 '(pl pv)))
                           (not (member feat2 '(pl pv))))
                 do (assert-equality
                      #'semtype-equal?-str
                      (format nil "(S=>2)%~s,~s" feat1 feat2)
                      (str2str-compose-types
                        (format nil "(D=>(S=>2))%~s,~s" feat1 feat2)
                        "D"))
                 ;; Only feat1 propagate to sentences
                 when (and (not (eql feat1 feat2))
                           (not (member feat1 '(pl pv)))
                           (member feat2 '(pl pv)))
                 do (assert-equality
                      #'semtype-equal?-str
                      (format nil "(S=>2)%~s" feat1)
                      (str2str-compose-types
                        (format nil "(D=>(S=>2))%~s,~s" feat1 feat2)
                        "D"))
                 ;; Only feat2 propagate to sentences
                 when (and (not (eql feat1 feat2))
                           (member feat1 '(pl pv))
                           (not (member feat2 '(pl pv))))
                 do (assert-equality
                      #'semtype-equal?-str
                      (format nil "(S=>2)%~s" feat2)
                      (str2str-compose-types
                        (format nil "(D=>(S=>2))%~s,~s" feat1 feat2)
                        "D")))))

(define-test ulf-synfeats-composition
  "Tests syntactic feature compositions using ULF expressions."
  (:tag :synfeats :compose)

  ;; Try it with ulf expressions.
  (assert-equality
    #'semtype-match?-str
    "(D=>(S=>2))_V%T"
    (string-from-compose-types '(past be.v) 'green.a))
  (assert-equality
    #'semtype-match?-str
    "(S=>2)%T"
    (string-from-compose-types 'it.pro '((past be.v) green.a)))
  (assert-equality
    #'semtype-match?-str
    "(D=>(S=>2))_V%PG"
    (string-from-compose-types 'prog '(be.v green.a)))
  (assert-equality
    #'semtype-match?-str
    "(S=>2)"
    (string-from-compose-types 'it.pro '(prog (be.v green.a)))))

(define-test tensed-synfeats-composition
  "Tests syntactic feature compositions that interact with tense."
  (:tag :synfeats :compose)

  (loop for (feat . ulf) in '((x . do.aux-v)
                              (pg . prog)
                              (pf . perf))
        do (progn
             ;; Auxiliary
             (assert-equality
               #'semtype-match?-str
               (format nil "(D=>(S=>2))_V%!T,~s" feat)
               (string-from-compose-types ulf 'go.v))
             (assert-equality
               #'semtype-match?-str
               (format nil "(D=>(S=>2))_V%T,~s" feat)
               (string-from-compose-types (list 'pres ulf) 'go.v))
             (assert-equal
               nil
               (string-from-compose-types ulf '(pres go.v)))
             (assert-equal
               nil
               (string-from-compose-types (list 'pres ulf) '(pres go.v))))))

(define-test aspectual-operators-composition
  "Tests the combinations of various aspectual operators."
  (:tag :synfeats :compose)

  (assert-equality
    #'semtype-match?-str
    "(D=>(S=>2))_V%PF,PG"
    (string-from-compose-types 'perf '(prog go.v)))
  (assert-equality
    #'semtype-match?-str
    "(D=>(S=>2))_V%PG,X"
    (string-from-compose-types 'may.aux-v '(prog go.v)))
  (assert-equality
    #'semtype-match?-str
    "(D=>(S=>2))_V%PF,X"
    (string-from-compose-types 'may.aux-v '(perf go.v)))
  (assert-equality
    #'semtype-match?-str
    "(D=>(S=>2))_V%PG,PF,X"
    (string-from-compose-types 'may.aux-v '(perf (prog go.v))))
  (assert-equal nil (string-from-compose-types 'prog '(perf go.v)))
  (assert-equal nil (string-from-compose-types 'prog '(do.aux-v go.v)))
  (assert-equal nil (string-from-compose-types 'perf '(do.aux-v go.v))))

(define-test passive-composition
  "Tests for passive operator."
  (:tag :synfeats :compose)
  (assert-equality
    #'semtype-match?-str
    "({D|(D=>(S=>2))}^n=>(D=>(D=>(S=>2))))_v%pv"
    (string-from-compose-types 'pasv 'go.v))
  (assert-equal nil (string-from-compose-types 'pasv '(pasv go.v)))
  (assert-equal nil (string-from-compose-types 'pasv '(see.v him.pro))))

