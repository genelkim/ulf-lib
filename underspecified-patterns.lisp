;; ULF patterns for underspecified types.
;;
;; These were introduced with Len's constituency tree-to-ulf parsers, which
;; sometimes could not fully resolve a type. Here are a few examples:
;;  .aux (could be aux-v or aux-s)
;;  .adv (could be adv-a, adv-s, adv-e, adv-f)
;;  fin (could be pres, past, or cf)
;;  .mod (could be mod-a or mod-n)

(in-package :ulf-lib)

(defun construct-alternative-type-strings (type-strings)
  "Constructs a new type string that allows all of the given types as options."
  (cond
    ((= 1 (length type-strings)) (first type-strings))
    ((= 2 (length type-strings)) 
     (str:concat "{" (first type-strings) 
                 "|" (second type-strings) "}"))
    (t (str:concat "{" (first type-strings) "|"
                   (construct-alternative-type-strings (cdr type-strings))
                   "}"))))

(defparameter *underspecified-semtypes*
  (mapcar
    ;; Expand out shorthand in key and convert list of symbols to optional
    ;; semtype of alternatives.
    #'(lambda (x)
        (let* ((key1 (regex-replace-all "{S}" (car x) *atomsymbols*))
               (finalkey (regex-replace-all "{N}" key1 *innername-symbols*)))
        (cons finalkey
              (let ((fn (gute:compose #'semtype2str
                                      #'atom-semtype?))
                    (args (cdr x)))
                (str2semtype
                  (construct-alternative-type-strings
                    (mapcar fn (cdr x))))))))
    ;; Declarative shorthand
    '(("{S}\\.AUX" . (aux.aux-v aux.aux-s))
      ("{S}\\.ADV" . (adv.adv-a adv.adv-s adv.adv-e adv.adv-f))
      ("{S}\\.MOD" . (mod.mod-a mod.mod-n))
      ("FIN" . (pres past cf)))))

(defmacro with-underspecified-types (&body body)
  "Runs the body while allowing underspecified semtypes.
  
  Within the scope of the macro, the variable *semtypes* is modified so any
  function that directly interacts with this will be affected. Afterward, the
  prior *semtypes* steate will be reinstated so any changes to the variable
  within this scope will be lost.
  
  This macro is thread-safe."
  `(let* ((*semtypes* (append *semtypes* *underspecified-semtypes*)))
     ,@body))

