
(in-package :ulf-lib)

;; Ensures that the input symbol is in ulf-lib.
(defmacro in-ulf-lib ((x y) &body body)
  `(gute:in-intern (,x ,y :ulf-lib)
                   ,@body))

