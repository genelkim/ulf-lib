;; Data structures for representing ULF semantic types

(in-package :ulf-lib)

;; Class representing a ULF semantic type
(defclass semtype ()
  ((domain
     :initarg :domain
     :accessor domain)
   (range
     :initarg :range
     :initform nil
     :accessor range)
   (exponent
     :initarg :ex
     :initform 1
     :accessor ex)
   (subscript
     :initarg :subscript
     :initform nil
     :accessor subscript)
   (tense
     :initarg :tense
     :initform nil
     :accessor tense)
   ; Internal type parameters, needed for some macros to carry over information.
   (type-params
     :initarg :type-params
     :initform nil
     :accessor type-params)))

;; Subclass for atomic types
(defclass atomic-type (semtype)
  ())

;; Type to represent "{A|B}"
;; Currently only supports two options
(defclass optional-type (semtype)
  ((types
     :initarg :types
     :accessor types)))

;; Check if a given object is a semtype
(defun semtype-p (s)
  (typep s 'semtype))

;; Check if a given object is an atomic-type.
(defun atomic-type-p (s)
  (typep s 'atomic-type))

;; Check if a given object is an optional-type.
(defun optional-type-p (s)
  (typep s 'optional-type))

(defun add-semtype-tense (semtype tense)
  "Adds tense to a semtype. Recurses into optional-type, since it shouldn't
  directly carry tense."
  (cond
    ((optional-type-p semtype)
     (mapcar #'(lambda (st) (add-semtype-tense st tense)) (types semtype)))
    ((null semtype) nil)
    ;; Atomic types don't get tenses (e.g. D, S, 2).
    ((atomic-type-p semtype) nil)
    (t (setf (tense semtype) tense))))

(defun add-semtype-type-params (semtype type-params)
  "Adds type-params to a semtype. Recurses into optional-type, since it
  shouldn't directly carry type-params."
  (cond
    ((optional-type-p semtype)
     (mapcar #'(lambda (st) (add-semtype-type-params st type-params)) (types semtype)))
    ((null semtype) nil)
    (t (setf (type-params semtype) (append (type-params semtype) type-params)))))

(defun set-semtype-type-params (semtype type-params)
  "Like add-semtype-type-params, but overwrites, rather than appending to existing
  type params."
  (cond
    ((optional-type-p semtype)
     (mapcar #'(lambda (st) (set-semtype-type-params st type-params)) (types semtype)))
    ((null semtype) nil)
    (t (setf (type-params semtype) type-params))))

(defun get-semtype-type-params (semtype)
  "Gets the type-params from a semtype. Recurses into optional-type, but only in
  one branch since all final branches should have the same type-params."
  (cond
    ((null semtype) nil)
    ((listp semtype) (get-semtype-type-params (first semtype)))
    ((optional-type-p semtype) (get-semtype-type-params (types semtype)))
    (t (type-params semtype))))

;; Create a new ULF type as an instance of the appropriate class.
;; If :options is specified, an optional type is created.
;; If ran is NIL an atomic type is created.
;; Rigorous sanity checking is NOT done, so unexpected inputs might cause
;; unexpected outputs.
;;
;; Types with a variable for an exponent are expanded out into a chain of
;; optionals where the variable value lies between 0 and 6. For example, A^n would
;; become {A^0|{A^1|{A^2|...}}}.
(defun new-semtype (dom ran exponent sub ten &key options type-params)
  (progn
    (setf dom (copy-semtype dom))
    (setf ran (copy-semtype ran))
    (setf options (mapcar #'copy-semtype options))
    (setf type-params (mapcar #'copy-semtype type-params))
    (if (not (numberp exponent))
      ; The exponent is not a number
      (if (listp exponent)
        ; If the exponent is a list, we are currently recursing to form a chain of
        ; optionals
        (if (= (length exponent) 1)
          (new-semtype dom ran (car exponent) sub ten :options options :type-params type-params)
          (new-semtype NIL NIL 1 NIL NIL
                       :options (list (new-semtype dom ran (car exponent) sub ten :options options :type-params type-params)
                                      (new-semtype dom ran (cdr exponent) sub ten :options options :type-params type-params))))
        ; If the exponent is not a number or a list, treat it as a variable and
        ; start recursion to form a chain of optionals
        (new-semtype dom ran '(0 1 2 3 4 5) sub ten :options options :type-params type-params))

      ; Unless the exponent is 0 (in which case the type is NIL), create the type
      (unless (= exponent 0)
        (if options
          ; Create optional type
          (let ((result (make-instance 'optional-type
                                       :types options
                                       :ex exponent
                                       :subscript sub
                                       :tense ten)))
            (add-semtype-type-params result type-params)
            result)

          ; If the type isn't an optional, check if range is non-NIL
          (if ran
            ; Range is not NIL
            (if (and (optional-type-p dom) (= (ex dom) 1))
              ; The domain is optional. Push the range in. For example, {A|B}=>C
              ; would become {(A=>C)|(B=>C)}. This is convenient for composition
              ; functions.
              (make-instance 'optional-type
                             :types (list (new-semtype (car (types dom)) ran 1 sub ten :type-params type-params)
                                          (new-semtype (cadr (types dom)) ran 1 sub ten :type-params type-params)))

              ; The domain is not optional
              (if dom
                ; Create new semtype
                (make-instance 'semtype
                               :domain dom
                               :range ran
                               :ex exponent
                               :subscript sub
                               :tense ten
                               :type-params type-params)
                ; If the domain is NIL, return the range
                (progn
                  (setf (ex ran) exponent)
                  (setf (subscript ran) sub)
                  (setf (tense ran) ten)
                  ran)))

            ; Range is NIL; the type is atomic.
            (make-instance 'atomic-type
                           :domain dom
                           :ex exponent
                           :subscript sub
                           :tense ten
                           :type-params type-params)))))))

;; Check if two semantic types are equal. If one of the types is an optional
;; type then return true of either of the two options match. If both types are
;; optional they must contain the same options (order doesn't matter).
;; If :ignore-exp is not NIL then exponents of the two types aren't checked
;; If :ignore-exp is 'r then ignore exponents recursively
;; Tenses and subscripts are checked if both types have a specified
;; tense/subscript.
;; Tenses and subscripts on optionals are ignored.
;;
;; TODO(gene): see note below
;; Note: This function is more of a "compatibility checker" than a function to
;; check actual equality. I'll probably rename this to something better later.
(defun semtype-equal? (x y &key ignore-exp)
  (if (or (optional-type-p x) (optional-type-p y))
    ;; At least one optional
    (if (and (optional-type-p x) (optional-type-p y))
      ;; Both optional
      (when (if ignore-exp T (equal (ex x) (ex y)))
        (let ((A (car (types x))) (B (cadr (types x))) (C (car (types y))) (D (cadr (types y))))
          (or (and (semtype-equal? A C :ignore-exp (when (equal ignore-exp 'r) 'r))
                   (semtype-equal? B D :ignore-exp (when (equal ignore-exp 'r) 'r)))
              (and (semtype-equal? A D :ignore-exp (when (equal ignore-exp 'r) 'r))
                   (semtype-equal? B C :ignore-exp (when (equal ignore-exp 'r) 'r))))))

      (if (optional-type-p x)
        ;; x optional; y not optional
        (or (and (or (= (ex y) (* (ex (car (types x))) (ex x))) ignore-exp)
                 (semtype-equal? y (car (types x)) :ignore-exp (if (equal ignore-exp 'r) 'r T)))
            (and (or (= (ex y) (* (ex (cadr (types x))) (ex x))) ignore-exp)
                 (semtype-equal? y (cadr (types x)) :ignore-exp (if (equal ignore-exp 'r) 'r T))))
        ;; y optional; x not optional
        (or (and (or (= (ex x) (* (ex (car (types y))) (ex y))) ignore-exp)
                 (semtype-equal? x (car (types y)) :ignore-exp (if (equal ignore-exp 'r) 'r T)))
            (and (or (= (ex x) (* (ex (cadr (types y))) (ex y))) ignore-exp)
                 (semtype-equal? x (cadr (types y)) :ignore-exp (if (equal ignore-exp 'r) 'r T))))))

    ;; No optionals
    (when (and (if ignore-exp T (equal (ex x) (ex y)))
               (equal (type-of x) (type-of y))
               (if (and (subscript x) (subscript y)) (equal (subscript x) (subscript y)) T)
               (equal (tense x) (tense y)))
      (if (atomic-type-p x)
        (equal (domain x) (domain y))
        (and (semtype-equal? (domain x) (domain y) :ignore-exp (when (equal ignore-exp 'r) 'r))
             (semtype-equal? (range x) (range y) :ignore-exp (when (equal ignore-exp 'r) 'r)))))))

;; Make a new semtype identical to the given type
;; Key word options allow overwriting specific fields if appropriate.
(defun copy-semtype (x &key c-dom c-ran c-ex c-subscript c-tense c-types c-type-params)
  (if (or (semtype-p x) (atomic-type-p x) (optional-type-p x))
    (if (atomic-type-p x)
      (make-instance 'atomic-type
                     :domain (if c-dom c-dom (domain x))
                     :ex (if c-ex c-ex (ex x))
                     :subscript (if c-subscript c-subscript (subscript x))
                     :tense (if c-tense c-tense (tense x))
                     :type-params (if c-type-params c-type-params (type-params x)))
      (if (optional-type-p x)
        (make-instance 'optional-type
                       :types (if c-types
                                c-types
                                (list (copy-semtype (car (types x)))
                                      (copy-semtype (cadr (types x)))))
                       :ex (if c-ex c-ex (ex x))
                       :subscript (if c-subscript c-subscript (subscript x))
                       :tense (if c-tense c-tense (tense x))
                       :type-params (if c-type-params c-type-params (type-params x)))
        (make-instance 'semtype
                     :domain (if c-dom c-dom (copy-semtype (domain x)))
                     :range (if c-ran c-ran (copy-semtype (range x)))
                     :ex (if c-ex c-ex (ex x))
                     :subscript (if c-subscript c-subscript (subscript x))
                     :tense (if c-tense c-tense (tense x))
                     :type-params (if c-type-params c-type-params (type-params x)))))
    x))

;; Convert a semtype to a string. The string it returns can be read back into a
;; type using str2semtype.
(defun semtype2str (s)
  (when (null s)
    (return-from semtype2str nil))
  (let ((type-params-str
          (format nil "[~a]"
                  (cl-strings:join (mapcar #'semtype2str (type-params s)) :separator ",")))
        base)
    (setf base
          (cond
            ((atomic-type-p s)
             ; Atomic type
             (format nil "~a~a~a~a"
                     (domain s)
                     (if (subscript s) (format nil "_~a" (subscript s)) "")
                     (if (tense s) (format nil "_~a" (tense s)) "")
                     (if (= (ex s) 1) "" (format nil "^~a" (ex s)))))
            ((optional-type-p s)
             ; Optional type
             (format nil "{~a|~a}~a"
                     (semtype2str (car (types s)))
                     (semtype2str (cadr (types s)))
                     (if (= (ex s) 1) "" (format nil "^~a" (ex s)))))
            ((semtype-p s)
             ; Not optional or atomic
             (format nil "(~a=>~a)~a~a~a"
                     (semtype2str (domain s))
                     (semtype2str (range s))
                     (if (subscript s) (format nil "_~a" (subscript s)) "")
                     (if (tense s) (format nil "_~a" (tense s)) "")
                     (if (= (ex s) 1) "" (format nil "^~a" (ex s)))))
            (t nil)))
    (if (equal type-params-str "[]")
      base
      (format nil "~a~a" base type-params-str))))

;; Split a string of the form ([domain]=>[range]) into [domain] and [range].
;; Helper for str2semtype.
(defun split-semtype-str (s)
  (let ((level 0) (i 1))
    (loop
      (when (equal (char s i) #\()
        (setf level (+ level 1)))
      (when (equal (char s i) #\))
        (setf level (- level 1)))
      (when (and (equal (char s i) #\=) (= level 0))
        (return i))
      (setf i (+ i 1)))
    (list (subseq s 1 i) (subseq s (+ i 2) (- (length s) 1)))))

;; Split a string of the form {A|B} into A and B. Helper for str2semtype.
(defun split-opt-str (s)
  (let ((level 0) (i 1))
    (loop
      (when (equal (char s i) #\{)
        (setf level (+ level 1)))
      (when (equal (char s i) #\})
        (setf level (- level 1)))
      (when (and (equal (char s i) #\|) (= level 0))
        (return i))
      (setf i (+ i 1)))
    (list (subseq s 1 i) (subseq s (+ i 1) (- (length s) 1)))))

(defun split-type-param-str (tpstr)
  "Splits a type param list string into the individual type strings.
  These are comma separated, but since the types are recursively defined, only
  split on commas that are at the top-level square bracketing.
  "
  (when (null tpstr)
    (return-from split-type-param-str nil))
  (let ((params nil)
        (cur-chars nil)
        (bracket-depth 0))
    (loop for c across tpstr do
          (cond
            ; Comma at top-level, add word and reset cur-chars.
            ((and (eql c #\,) (= bracket-depth 0))
             (push (coerce (reverse cur-chars) 'string) params)
             (setf cur-chars nil))
            ; Open bracket, add depth.
            ((eql c #\[) (1+ bracket-depth)
                         (push c cur-chars))
            ; Close bracket, subtract depth.
            ((eql c #\]) (1- bracket-depth)
                         (push c cur-chars))
            ; Otherwise, just add to current characters.
            (t (push c cur-chars))))
    (when (not (null cur-chars))
      (push (coerce (reverse cur-chars) 'string) params))
    (assert (= bracket-depth 0) (bracket-depth tpstr params)
            "Bracket depth not 0 at the end!~%bracket-depth: ~s~%tpstr: ~s~%params: ~s~%"
            bracket-depth tpstr params)
    (reverse params)))

;; Convert a string into a semantic type.
;; Strings must be of the form ([domain]=>[range]) or a single character, where
;; [domain] and [range] are valid strings of the same form.
;; Some single character subscripts are supported, denoted by a _ followed
;; by the character. Single digit/character exponents are also supported
;; denoted by a ^ followed by the digit/character. Exponents must occur after
;; any subscripts.
(defun str2semtype (s &key (recurse-fn #'str2semtype))
  (declare (type function recurse-fn))
  (let (; ([domain]=>[range])_[ut|navp]^n\[[type1],[type2],..\]
        (non-atom-regex "^(\\(.*\\))(_(([UT])|([NAVP])))?(\\^([A-Z]|[2-9]))?(\\[(.*)\\])?$")
        ; {A|B}[[type1],[type2],..\]
        (optional-regex "^(\\{.*\\})(_(([UT])|([NAVP])))?(\\^([A-Z]|[2-9]))?(\\[(.*)\\])?$")
        (atomic-regex "^([A-Z0-9\-\*]+)(_(([UT])|([NAVP])))?(\\^([A-Z]|[2-9]))?(\\[(.*)\\])?$"))
    (setf s (string-upcase s))
    (if (equal (char s 0) #\()
      ; NON ATOMIC ([domain]=>[range])_[ut|navp]^n\[[type1],[type2],..\]
      (let ((match (nth-value 1 (cl-ppcre:scan-to-strings non-atom-regex s))))
        (new-semtype (funcall recurse-fn (car (split-semtype-str (svref match 0))))
                     (funcall recurse-fn (cadr (split-semtype-str (svref match 0))))
                     (if (svref match 6) (read-from-string (svref match 6)) 1)
                     (if (svref match 4) (read-from-string (svref match 4)) nil)
                     (if (svref match 3) (read-from-string (svref match 3)) nil)
                     :type-params (mapcar recurse-fn (split-type-param-str (svref match 8)))))

      ; ATOMIC or OPTIONAL
      (if (equal (char s 0) #\{)
        ; OPTIONAL {A|B}[[type1],[type2],..\]
        (let ((match (nth-value 1 (cl-ppcre:scan-to-strings optional-regex s))))
            (new-semtype NIL NIL
                         (if (svref match 6) (read-from-string (svref match 6)) 1)
                         (if (svref match 4) (read-from-string (svref match 4)) nil)
                         (if (svref match 3) (read-from-string (svref match 3)) nil)
                         :options (list (funcall recurse-fn (car (split-opt-str (svref match 0))))
                                        (funcall recurse-fn (cadr (split-opt-str (svref match 0)))))
                         :type-params (mapcar recurse-fn (split-type-param-str (svref match 8)))))
          ; ATOMIC
          (let ((match (nth-value 1 (cl-ppcre:scan-to-strings atomic-regex s))))
            ;(format t "s: ~s~%" s)
            ;(format t "match1: ~s~%" (read-from-string (svref match 0)))
            (new-semtype (read-from-string (svref match 0)) NIL
                         (if (svref match 6) (read-from-string (svref match 6)) 1)
                         (if (svref match 4) (read-from-string (svref match 4)) nil)
                         (if (svref match 3) (read-from-string (svref match 3)) nil)
                         :type-params (mapcar recurse-fn (split-type-param-str (svref match 8)))))))))

;; Convert a string into a semantic type, extended to handle cases in ULF that
;; do not strictly follow EL compositions. Strings must be of the form
;; ([domain]=>[range]), a single character, or one of the designated special
;; symbols for selected ULF macros and phenomena. [domain] and [range] are
;; valid strings of the same form. Some single character subscripts are
;; supported, denoted by a _ followed by the character. Single digit/character
;; exponents are also supported denoted by a ^ followed by the digit/character.
;; Exponents must occur after any subscripts.
(defun extended-str2semtype (s)
  (let* ((str (string-upcase s))
         (sym (intern str *package*)))
    (cond
      ;; Non-atomic type.
      ((equal (char str 0) #\() (str2semtype str :recurse-fn #'extended-str2semtype))
      ;; Optional type.
      ((equal (char str 0) #\{) (str2semtype str :recurse-fn #'extended-str2semtype))
      ;; Tense.
      ((equal str "TENSE") (new-semtype 'tense nil 1 nil nil))
      ;; Quotes.
      ((equal str "\"") (new-semtype '\" nil 1 nil nil))
      ;; Any of the macros.
      ((lex-macro? sym) (new-semtype sym nil 1 nil nil))
      ;; Any of the extended macros.
      ((member str '("QT-ATTR1" "QT-ATTR2" "SUB1" "REP1" "POSTGEN1" "POSTGEN2") :test #'equal)
       (new-semtype sym nil 1 nil nil))
      ;; Macro hole variables (*p, *h, *ref, *s, etc.)
      ((lex-macro-hole? sym) (new-semtype sym nil 1 nil nil))
      ;; Non-special atomic type.
      (t (str2semtype s :recurse-fn #'extended-str2semtype)))))

; TODO(gene): change this to semtype-equal? to semtype-sufficient? and this to semtype-equal?
; TODO(gene): test this
(defun strict-semtype-equal (s1 s2)
  "Determines whether two semtypes are equivalent. For optional types, order
  doesn't matter and allows flattening of options."
  (labels
    (; Runs a list of accessor-test pair for computing equality on specific
     ; components of two elements.
     (acc-test (t1 t2)
       #'(lambda (accessor-test)
           (let ((accfn (first accessor-test))
                 (testfn (second accessor-test)))
             (funcall testfn (funcall accfn t1) (funcall accfn t2)))))
     ; Runs equality on two lists of semtypes, ignoring order.
     (set-no-option-equal (set1 set2)
       (and (= (length set1) (length set2))
            (every #'(lambda (set1-elem)
                       (member set1-elem set2 :test #'no-option-equal))
                   set1)))
     ; Determines equality for two semtypes that don't contain any options.
     (no-option-equal (t1 t2)
       (cond
         ((or (optional-type-p t1) (optional-type-p t2))
          (error "No optional types allowed in no-option-equal"))
         ((or (and (atomic-type-p t1) (not (atomic-type-p t2)))
              (and (not (atomic-type-p t1)) (atomic-type-p t2)))
          (error "Types don't match for t1 and t2 in no-option-equal"))
         ; Base case.
         ((and (not (semtype-p t1)) (not (semtype-p t2))) (equal t1 t2))
         ((or (not (semtype-p t1)) (not (semtype-p t2))) nil)
         ; Atomic (only has domain)
         ((atomic-type-p t1)
          (every (acc-test t1 t2)
                 (list (list #'domain #'no-option-equal)
                       (list #'ex #'equal) 
                       (list #'subscript #'equal) 
                       (list #'tense #'equal) 
                       (list #'type-params #'set-no-option-equal))))
         ; Non-atomic (has domain and range)
         (t
          (every (acc-test t1 t2)
                 (list (list #'domain #'no-option-equal)
                       (list #'range #'no-option-equal)
                       (list #'ex #'equal) 
                       (list #'subscript #'equal) 
                       (list #'tense #'equal) 
                       (list #'type-params #'set-no-option-equal))))))
     ) ; end of label definitions.
    ; labels body
    ; If we get to an atomic label, just check the labels.
    (let ((flat-s1 (types (flatten-options s1)))
          (flat-s2 (types (flatten-options s2))))
      ; Check that the lengths are the same and that for every option in s1,
      ; the same option exists in s2.
      (set-no-option-equal flat-s1 flat-s2))))


(defun flatten-type-params (s)
  "Flattens the type-params of the given semtype. This function is co-recursive
  with flatten-options which flattens the main options."
  (let* 
     (; Expand out each type-param into its list of possibilities.
      (type-param-choice-lst 
        (mapcar (compose #'types #'flatten-options) 
                (type-params s)))
      ; Build an option list with every combination of type-param choices
      (type-param-choices (cartesian-product type-param-choice-lst))
      (new-options (mapcar #'(lambda (tp-choice)
                               (copy-semtype s :c-type-params tp-choice))
                           type-param-choices)))
     (cond
       ((= 1 (length new-options)) (first new-options))
       ; Build the optional semtype with these options and return.
       (t (new-semtype nil nil 1 nil nil :options new-options)))))

(defun flatten-options (s)
  "Flattens a semtype so that all options are represented with a single
  top-level option type. In a sense, this is un-factorizing the semtype.
  This will always return an optional type even if there is only one option."
  ; First, flatten the type-params if necessary.
  (setf s (if (type-params s) 
            (flatten-type-params s)
            s))
  (cond
    ;; Optional type, recurse on all and flatten into a single optional type.
    ((optional-type-p s)
     (let ((new-options (apply #'append (mapcar (compose #'types #'flatten-options) 
                                                (types s)))))
       (new-semtype nil nil 1 nil nil :options new-options)))
    ;; Atomic type, return a copy of it wrapped in an optional type.
    ((atomic-type-p s)
     (new-semtype nil nil 1 nil nil :options (list (copy-semtype s))))
    ;; Non-atomic, non-optional type, recurse into the domain and range and generate an optional
    ;; type of all combinations of the recursed types.
    (t (let ((flat-dom (flatten-options (domain s)))
             (flat-ran (flatten-options (range s)))
             new-options)
         (assert (optional-type-p flat-dom))
         (assert (optional-type-p flat-ran))
         (setf new-options
               (loop for cur-dom in (types flat-dom)
                     append (loop for cur-ran in (types flat-ran)
                                  collect (copy-semtype s :c-dom cur-dom
                                                          :c-ran cur-ran))))
         (new-semtype nil nil 1 nil nil :options new-options)))))

(defun binarize-flat-options (s)
  "Takes a flat option type and makes it a right-leaning binary tree of options."
  (labels
    ((helper (options)
       (cond
         ((< (length options) 3)
          (new-semtype nil nil 1 nil nil :options options))
         (t (new-semtype nil nil 1 nil nil
                         :options (list (first options)
                                        (binarize-flat-options (rest options))))))))
    ; Top-level call.
    (helper (types s))))

