# ulf-lib
Library for interfacing with and manipulating unscoped episodic logical forms (ULF).  

## Dependencies
- SBCL
- Quicklisp
- TTT (get a copy at https://github.com/genelkim/ttt)
- https://github.com/genelkim/cl-util
- All other dependencies are available through Quicklisp

## Installation
1. Install quicklisp by following instructions at https://www.quicklisp.org/beta/
2. Then place the other depenedencies listed above in a folder accessible to Quicklisp or ASDF (which underlies quicklisp).  How to do this in a couple ways is described by the following Stack Overflow answer https://stackoverflow.com/a/11265601.

## Running the Code
This is really meant to be a library, but to check the basic functionality of any of the functions, you can load the file load.lisp and enter the package :ulf-lib.  For example,
```
$ sbcl
$ (load "load.lisp")
$ ...[loading messages]...
$ (in-package :ulf-lib)
$ (phrasal-ulf-type? 'man.n)
(NOUN PRED)
$ (phrasal-ulf-type? '|John|)
(TERM)
$ (phrasal-ulf-type? '(the.d (angry.a man.n)))
(TERM)
$ (phrasal-ulf-type? '(see.v the.d man.n))
(UNKNOWN)
$ (apply-sub-macro '((sub what.pro ((past do.aux-s) he.pro (say.v *h))) ?))
(((PAST DO.AUX-S) HE.PRO (SAY.V WHAT.PRO)) ?)
```
