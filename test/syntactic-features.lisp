
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

  ;; Auxiliary, plurality, perfect, passive, progressive only propagates to
  ;; predicates.
  (loop for feat in '(x pl pf pv pg)
        do (assert-equality
             #'semtype-equal?-str
             (format nil "(D=>(S=>2))%~s" feat)
             (str2str-compose-types
               (format nil "(D=>(D=>(S=>2)))%~s" feat)
               "D"))
        do (assert-equality
             #'semtype-equal?-str
             "(S=>2)"
             (str2str-compose-types
               (format nil "(D=>(S=>2))%~s" feat)
               "D"))))

(define-test multiple-synfeats-composition
  "Tests syntactic feature compositions with multiple features mixing."
  (:tag :synfeats :compose)

  ;; Auxiliary, plurality, perfect, passive, progressive only propagates to
  ;; predicates.
  (loop for feat1 in '(x pl pf pv pg)
        do (loop for feat2 in '(x pl pf pv pg)
                 when (not (eql feat1 feat2))
                 do (progn
                      (assert-equality
                        #'semtype-equal?-str
                        (format nil "(D=>(S=>2))%~s,~s" feat1 feat2)
                        (str2str-compose-types
                          (format nil "(D=>(D=>(S=>2)))%~s,~s" feat1 feat2)
                          "D"))
                      (assert-equality
                        #'semtype-equal?-str
                        "(S=>2)"
                        (str2str-compose-types
                          (format nil "(D=>(S=>2))%~s,~s" feat1 feat2)
                          "D"))))))

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

  ;; Auxiliary
  (assert-equality
    #'semtype-match?-str
    "(D=>(S=>2))_V%!T,X"
    (string-from-compose-types 'do.aux-v 'go.v))
  (assert-equality
    #'semtype-match?-str
    "(D=>(S=>2))_V%T,X"
    (string-from-compose-types '(pres do.aux-v) 'go.v))
  (assert-equal
    nil
    (string-from-compose-types 'do.aux-v '(pres go.v)))
  (assert-equal
    nil
    (string-from-compose-types '(pres do.aux-v) '(pres go.v)))

  ;; Progressive
  (assert-equality
    #'semtype-match?-str
    "(D=>(S=>2))_V%!T,PG"
    (string-from-compose-types 'prog 'go.v))
  (assert-equality
    #'semtype-match?-str
    "(D=>(S=>2))_V%T,PG"
    (string-from-compose-types '(pres prog) 'go.v))
  (assert-equal
    nil
    (string-from-compose-types 'prog '(pres go.v)))
  (assert-equal
    nil
    (string-from-compose-types '(pres prog) '(pres go.v))))

