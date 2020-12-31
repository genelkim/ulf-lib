;;; Language utilities.
;;; Utilities such as converting between related forms of words in a ULF representation.

(in-package :ulf-lib)

(defun pronoun2possdet (inpro &key pkg)
  "Generates a dependent possessive determiner corresponding to the given
  pronoun. Raises an error if the input is not pronoun. For archaic 'thou'
  always returns thy.d. If this is before a vowel, it should be thine.d.

  This handles all forms of the pronouns, subject/object/reflexive/independent
  possessive.
  
  The return value will not necessary be an atom since 'one' gets an apostrophe
  's'.
  
  The keyword argument 'pkg' is the output package into which the symbols will
  be interned."
  (inout-intern (inpro pro :ulf-lib :callpkg pkg)
    (let ((word (split-by-suffix pro)))
      (cond
        ((member word '(i me mine)) 'my.d)
        ((member word '(we us ourself ourselves ours)) 'our.d)
        ((member word '(you yourself yourselves yours ye)) 'your.d)
        ((member word '(thou thee thyself thine)) 'thy.d)
        ((member word '(he him himself his)) 'his.d)
        ((member word '(she her herself hers)) 'her.d)
        ((member word '(it itself its)) 'its.d)
        ((member word '(they them themselves theirs)) 'their.d)
        ((member word '(one oneself)) '(one.pro 's))
        ((member word '(who whom whose)) 'whose.d)
        (otherwise (error "Unknown pronoun for generating a dependent possessive determiner."))))))

(defun term2possdet (interm &key pkg)
  "Generates a dependent possessive determiner corresponding to the given ULF term.
  For pronouns, particular new symbol is generated. For most term, an apostrophe 's'
  is added.
  
  The keyword argument 'pkg' is the output package into which the symbols will
  be interned."
  (inout-intern (interm term :ulf-lib :callpkg pkg)
    (cond
      ((and (atom term)
            (eql 'pro (nth-value 1 (split-by-suffix term))))
       (pronoun2possdet term))
      (t (list term '|'S|)))))

