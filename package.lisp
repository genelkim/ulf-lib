;; ULF Interface and Manipulation Library.
;; Started ~2018-11-19

(in-package :cl-user)

(defpackage :ulf-lib
  (:nicknames :ulf)
  (:use :cl :ttt :util)
  (:export
    ;; macro.lisp
    :uninvert-verbauxes
    :apply-sub-macro
    :lift-adv-a

    ;; ttt-lexical-patterns.lisp
    :lex-noun?
    :lex-function?
    :lex-pronoun?
    :lex-verb?
    :lex-adjective?
    :lex-p?
    :lex-p-arg?
    :lex-ps?
    :lex-pq?
    :lex-prep?
    :lex-rel?
    :lex-det?
    :lex-coord?
    :lex-aux-s?
    :lex-aux-v?
    :lex-aux?
    :lex-number?
    :lex-name-pred?
    :lex-name?
    :lex-adv-a?
    :lex-adv-s?
    :lex-adv-e?
    :lex-adv-f?
    :lex-adv-formula?
    :lex-x?
    :lex-yn?
    :lex-tense?
    :lex-detformer?
    :litstring?
    :lex-equal?
    :lex-set-of?
    :lex-macro?
    :lex-verbaux?

    ;; ttt-phrasal-patterns.lisp
    :noun?  
    :adj?  
    :adv-a?  
    :adv-e?  
    :adv-s?  
    :adv-f?  
    :adv?  
    :pp?  
    :term?  
    :verb?  
    :pred?  
    :det?  
    :aux?  
    :tensed-aux? 
    :tensed-verb? 
    :sent?  
    :tensed-sent? 
    :sent-mod?  
    :preposs-macro?
    :p-arg?  
    :voc?  
    :sent-punct?
    :noun-reifier?
    :tensed-sent-reifier?
    :sent-reifier?
    :verb-reifier?
    :advformer?
    :detformer?
    :phrasal-sent-op? 
    :relativized-sent?
    ))

