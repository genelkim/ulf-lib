;; ULF Interface and Manipulation Library.
;; Started ~2018-11-19

(in-package :cl-user)

(defpackage :ulf-lib
  (:nicknames :ulf)
  (:use :cl :cl-user :ttt :cl-strings :cl-ppcre :util)
  (:shadow :split)
  (:export
    ;; suffix.lisp
    :has-suffix?
    :split-by-suffix
    :strip-suffix
    :add-suffix
    :suffix-for-type

    ;; macro.lisp
    :uninvert-verbauxes
    :apply-sub-macro
    :lift-adv-a
    :uninvert-verbaux!
    :add-info-to-sub-vars
    :add-info-to-relativizers

    ;; preprocess.lisp
    :unescape-backslashes
    :add-prename-space

    ;; ttt-lexical-patterns.lisp
    :lex-noun?
    :lex-rel-noun?
    :lex-function?
    :lex-pronoun?
    :lex-verb?
    :lex-adjective?
    :lex-p?
    :lex-p-arg?
    :lex-ps?
    :lex-pq?
    :lex-prep?
    :lex-mod-a?
    :lex-mod-n?
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
    :lex-elided?
    :is-strict-name?
    :lex-hole-variable?

    ;; ttt-phrasal-patterns.lisp
    :noun?
    :adj?
    :adv-a?
    :adv-e?
    :adv-s?
    :adv-f?
    :adv?
    :mod-a?
    :mod-n?
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
    :modformer?
    :phrasal-sent-op?
    :contains-relativizer
    :relativized-sent?
    :ulf-type?
    :label-formula-types

    ;; gen-phrasal-patterns.lisp
    :plur-term?
    :plur-noun?
    :plur-partitive?
    :plur-lex-noun?
    :pasv-lex-verb?

    ;; search.lisp
    :search-vp-head
    :find-vp-head
    :replace-vp-head
    :search-np-head
    :find-np-head
    :replace-np-head
    :search-ap-head
    :find-ap-head
    :replace-ap-head

    ;; type symbols (in ttt-phrasal-patterns.lisp)
    ;; TODO: write util function for printing while ignoring the symbol
    ;; namespace so that we don't need to export all this...
    :noun
    :adj
    :adv-a
    :adv-e
    :adv-s
    :adv-f
    :mod-a
    :mod-n
    :pp
    :term
    :verb
    :pred
    :det
    :aux
    :aux
    :tensed-aux
    :tensed-verb
    :sent
    :tensed-sent
    :tense
    :sent-punct
    :sent-mod
    :noun-reifier
    :verb-reifier
    :sent-reifier
    :tensed-sent-reifier
    :advformer
    :detformer
    :preposs-macro
    :rel-sent
    :p-arg
    :equal-sign
    :set-of-op
    :macro-symbol
    :unknown
    :types
    ))

