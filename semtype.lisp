;; Data structures for representing ULF semantic types

(in-package :ulf-lib)

;; Maximum semtype exponent for variable values.
;; e.g. D^n=>2 can be generated as 2, (D=>2), (D=>(D=>2)), etc. until the
;; maximum number.
;; TODO: write functions to reload in parameters (e.g. *semtypes*, etc.) after setting this so that it gets propagated properly.
(defparameter *semtype-max-exponent* 3)


;; A MORE COMPACT REPRESENTATIONS
;;
;; 1. Add a different connective, which assumes that unspecified,
;;    non-contradicted features are preserved. For example, instead of
;;    {((S=>2)%!t=>(S=>2)%!t)|((S=>2)_t=>(S=>2)%t)}, for a sentence modifier,
;;    preserving tense we can have ((S=>2)>>(S=>2)).
;;
;;    In comparison, the => connective must specify any restrictive feature in
;;    the antecedent (even if default) and all non-default features in the
;;    consequent. For example, ((S=>2)_!t=>D) takes an untensed sentence, with
;;    any other feature settings and results in a default entity (e.g. not a
;;    p-arg).
;;
;;    At the top-level, all non-default features must be specified, as if it
;;    were the consequent of truth. (T=>\phi).
;;
;;    Basically, underspecification of syntactic features is interpreted
;;      a) in the antecedent, as allowing anything;
;;      b) in the consequent of >>, as taking the antecedent value; and
;;      c) in the consequent of =>, as taking the parent values.
;;
;;    => := basic antecendent/consequent
;;    >> := argument feature-preserving antecedent/consequent
;;    
;;    Shorthand operator
;;    %> := synfeat modification (retains all other information and distributes
;;          accordingly)
;;          This is always reducible to a >> with synfeat changes applied
;;          before and after distributions. The semantic content and
;;          unspecified synfeats remain unchanged.
;;          This shorthand operator is preprocessed out and will not appear in
;;          system generated semtypes.
;;      See tense entry (PRES|PAST|CF) in ttt-lexical patterns for example.
;;
;; 2. ! prefix for any feature for the negation
;;

;; TODO Make sure *h and *p can have duplicates, or have counts.

;; Class representing a ULF semantic type
(defclass semtype ()
  ((connective
     :initarg :connective
     :initform '=>
     :accessor connective)
   (domain
     :initarg :domain
     :accessor domain)
   (range
     :initarg :range
     :initform nil
     :accessor range)
   (exponent
     :initarg :ex
     :initform 1
     :accessor ex
     :type fixnum)
   (suffix ; only n,v,a,p
     :initarg :suffix
     :initform nil
     :accessor suffix)
   ; Internal type parameters, needed for some macros to carry over information.
   (type-params
     :initarg :type-params
     :initform nil
     :accessor type-params
     :type list)
   ; Miscellaneous ordered syntactic features.
   (synfeats
     :initarg :synfeats
     :initform *default-syntactic-features*
     :accessor synfeats)))

;; Subclass for atomic types
(defclass atomic-type (semtype)
  ())

;; Type to represent "{A|B}"
;; Currently only supports two options
(defclass optional-type (semtype)
  ((types
     :initarg :types
     :accessor types
     :type list)))

;; Check if a given object is a semtype
(defun semtype-p (s)
  (typep s 'semtype))

;; Check if a given object is an atomic-type.
(defun atomic-type-p (s)
  (typep s 'atomic-type))

;; Check if a given object is an optional-type.
(defun optional-type-p (s)
  (typep s 'optional-type))

(defmethod print-object ((obj semtype) out)
  "Prints out semtype in #<SEMTYPE [string]> where [string] can be read in
   using str2semtype or extended-str2semtype."
  (print-unreadable-object (obj out :type t)
    (format out (semtype2str obj))))

(defun add-semtype-tense (semtype tense)
  "Adds tense to a semtype. Recurses into optional-type, since it shouldn't
   directly carry tense."
  (cond
    ((optional-type-p semtype)
     (mapcar #'(lambda (st) (add-semtype-tense st tense)) (types semtype)))
    ((null semtype) nil)
    ;; Atomic types don't get tenses (e.g. D, S, 2).
    ((atomic-type-p semtype) nil)
    (t (add-feature-values (synfeats semtype) (list tense)))))

(defun add-semtype-type-params (semtype type-params)
  "Adds type-params to a semtype. Recurses into optional-type, since it
   shouldn't directly carry type-params."
  (cond
    ((optional-type-p semtype)
     (mapcar #'(lambda (st) (add-semtype-type-params st type-params)) (types semtype)))
    ((null semtype) nil)
    (t (setf (type-params semtype) (append (type-params semtype) type-params)))))

(defmethod set-suffix ((obj semtype) suf)
  "Sets the suffix to a semtype. Recurses into optional-type since it doesn't
   directly carry suffixes. Ignored if called on an atomic type."
  (cond
    ((optional-type-p obj)
     (mapcar #'(lambda (child) (set-suffix child suf)) (types obj)))
    ((and (atomic-type-p obj) (not (null suf)))
     (warn "Atomic semtypes cannot take suffixes."))
    (t
     (setf (suffix obj) suf))))

(defmethod set-synfeats ((obj semtype) sf)
  "Adds synfeats to a semtype, recursive into optional-type since it doesn't
   directly carry synfeats."
  (cond
    ((optional-type-p obj)
     (mapcar #'(lambda (child) (set-synfeats child (copy sf))) (types obj)))
    (t
     (setf (synfeats obj) sf))))

(defun set-semtype-type-params (semtype type-params)
  "Like add-semtype-type-params, but overwrites, rather than appending to existing
   type params."
  (cond
    ((optional-type-p semtype)
     (mapcar #'(lambda (st) (set-semtype-type-params st type-params)) (types semtype)))
    ((null semtype) nil)
    (t (setf (type-params semtype) type-params))))

;; Use t instead of semtype for argument check because of the following
;; compiler note
;;  note: can't open-code test of unknown type SEMTYPE
(declaim (ftype (function (t) list) get-semtype-type-params))
(defun get-semtype-type-params (semtype)
  "Gets the type-params from a semtype. Recurses into optional-type, but only in
   one branch since all final branches should have the same type-params."
  ;; Locally declare specific function type of generic function.
  (declare (ftype (function (t) list) types type-params))
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
(defun new-semtype (dom ran exponent suf
                        &key options (synfeats *default-syntactic-features*)
                        type-params (conn '=>))
  ;; Fixnum and list are the only allowed number and sequence types,
  ;; respectively for exponent.
  (declare (type (or fixnum list (not (or number sequence)))
                 exponent))
  ;; Locally declare specific relevant type for generic 'ex' function.
  ;; Use t instead of semtype for argument check because of the following
  ;; compiler note
  ;;  note: can't open-code test of unknown type SEMTYPE
  (declare (ftype (function (t) fixnum) ex))
  (when (or (symbolp dom) (listp dom))
    (setf dom (intern-symbols-recursive dom :ulf-lib)))
  (when (or (symbolp ran) (listp ran))
    (setf ran (intern-symbols-recursive ran :ulf-lib)))
  (when (or (symbolp suf) (listp suf))
    (setf suf (intern-symbols-recursive suf :ulf-lib)))
  (setf dom (copy-semtype dom))
  (setf ran (copy-semtype ran))
  (setf options (mapcar #'copy-semtype options))
  (setf type-params (mapcar #'copy-semtype type-params))
  (setf synfeats (if synfeats (copy synfeats) nil))
  (cond
    ;; The exponent is not a number. If the exponent is a list, we are
    ;; currently recursing to form a chain of optionals.
    ;; Base case for optional chain.
    ((and (not (numberp exponent)) (listp exponent) (= (length exponent) 1))
     (new-semtype dom ran (car exponent) suf
                  :options options
                  :synfeats synfeats
                  :type-params type-params
                  :conn conn))
    ;; Recursive case for optional chain.
    ((and (not (numberp exponent)) (listp exponent))
     (new-optional-semtype
       (list (new-semtype dom ran (car exponent) suf
                          :options options
                          :synfeats synfeats
                          :type-params type-params
                          :conn conn)
             (new-semtype dom ran (cdr exponent) suf
                          :options options
                          :synfeats synfeats
                          :type-params type-params
                          :conn conn))))
    ;; If the exponent is not a number or a list, treat it as a variable and
    ;; start recursion to form a chain of optionals
    ((not (numberp exponent))  
     (new-semtype dom ran
                  (loop for exp-x from 0 upto *semtype-max-exponent* collect exp-x)
                  suf
                  :options options
                  :synfeats synfeats
                  :type-params type-params
                  :conn conn))
    ;; If exponent is 0, type is NIL.
    ((= exponent 0) nil)
    ;; Create non-0 exponent optional type
    (options    
      (let ((result (make-instance 'optional-type
                                   :types options
                                   :ex exponent
                                   :suffix suf
                                   :synfeats synfeats)))
        (add-semtype-type-params result type-params)
        result))
    ;; If the type isn't an optional and range is NIL; the type is atomic.
    ((null ran)
     (make-instance 'atomic-type
                    :domain dom
                    :ex exponent
                    :suffix suf
                    :synfeats synfeats
                    :type-params type-params
                    :connective conn))
    ;; Range is not NIL and the domain is optional. Push the range in. For
    ;; example, ({A|B}=>C) would become {(A=>C)|(B=>C)}. This is convenient for
    ;; composition functions.
    ((and (optional-type-p dom) (= (ex dom) 1))
     (make-instance 'optional-type
                    :types (list (new-semtype (car (types dom)) ran 1 suf
                                              :synfeats synfeats
                                              :type-params type-params
                                              :conn conn)
                                 (new-semtype (cadr (types dom)) ran 1 suf
                                              :synfeats synfeats
                                              :type-params type-params
                                              :conn conn))))
    ; The domain is not optional
    (dom
      ; Create new semtype
      (make-instance 'semtype
                     :domain dom
                     :range ran
                     :ex exponent
                     :suffix suf
                     :synfeats synfeats
                     :type-params type-params
                     :connective conn))
    ; If the domain is NIL, return the range with suffix values set.
    (t
      (progn
        (setf (ex ran) exponent)
        (setf (suffix ran) suf)
        (setf (synfeats ran) synfeats)
        ran))))

(defun new-optional-semtype (options)
  "Makes a new optional semtype.

   Optional semtypes have no domain, range, exponent, suffix, type parameters,
   or suffix. These all must be propagated into the individual options.
   Externally, optional types may have exponents, but in interpretaiton into the
   internal classes, we expand those exponents out into option realizations."
  (new-semtype nil nil 1 nil :options options))

(defun unroll-exponent-step (st)
  "Undoes a single step of exponent compression. If `st` has an exponent
   >1 or its domain has an exponent >1, this function generates a equivalent
   semtype where the expoenent is decremented and a new domain is introduced.

   Examples:
   A           ----> A
   A^4         ----> (A=>A^3)
   (A^3=>B^2)  ----> (A=>(A^2=>B^2))"
  (let (new-dom new-ran)
    (cond
      ;; Top-level exponent >1.
      ;; Move syntactic and type-param info to the domain and range.
      ((> (ex st) 1)
       (setf new-dom (copy-semtype st :c-ex 1))
       (setf new-ran (copy-semtype st :c-ex (1- (ex st))))
       (new-semtype new-dom new-ran 1 nil))
      ;; Domain exponent >1.
      ;; Retain all the syntactic and type-param info at top-level,
      ;; just move an argument out.
      ((and (not (atomic-type-p st))
            (not (optional-type-p st))
            (> (ex (domain st)) 1))
       (setf new-dom (copy-semtype (domain st) :c-ex 1))
       (setf new-ran
             (new-semtype (copy-semtype (domain st)
                                        :c-ex (1- (ex (domain st))))
                          (copy-semtype (range st))
                          1 nil))
       (copy-semtype st :c-dom new-dom :c-ran new-ran :c-conn '=>))
      (t
       st))))

;; Check if y matches with the semantic type pattern x. If one of the types is
;; an optional type then return true if any option-pair matches.
;;
;; For options, checks if there is a single compatible match.
;; For suffixes, if both have suffix specification, checks for equality
;;   otherwise doesn't care.
;; For synfeats, call syntactic-features-match? with x as the pattern.
;; If :ignore-exp is not NIL then exponents of the two types aren't checked
;; If :ignore-exp is 'r then ignore exponents recursively
(defun semtype-match? (raw-x raw-y &key ignore-exp)
  (declare (ftype (function (t) fixnum) ex))
  ;; Expand out one level of exponents if relevant.
  (labels
    ((>>-synfeat-diff (st)
       "Computes the synfeat difference between the domain and range for a
        semtype with >> connective.

       Returns a list of two syntactic-features class instances.
        1. domain synfeats where the range value is specified and differs.
        2. range synfeats where the domain value is specified and differs."
       (let ((keys (union (get-feature-names (synfeats (domain st)))
                          (get-feature-names (synfeats (range st)))))
             (new-dom-sf (copy (synfeats (domain st))))
             (new-ran-sf (copy (synfeats (range st)))))
         (loop for key in keys
               for dval = (feature-value new-dom-sf key)
               for rval = (feature-value new-ran-sf key)
               when (not (and dval rval (not (equal dval rval))))
               do (del-feature-value new-dom-sf key)
               and do (del-feature-value new-ran-sf key))
         (list new-dom-sf new-ran-sf)))
     (>>-synfeat-match? (xst yst)
       "Computes whether the syntactic features of two synfeats match when they
        have the >> connective.

        If connective is >> but a synfeat switch is specified, it must be
        specified in both since unspecified cases will never lead to the
        switch due to the synfeat propagation of >>."
       (let ((xdiff (>>-synfeat-diff xst))
             (ydiff (>>-synfeat-diff yst)))
         (and (syntactic-features-equal? (first xdiff) (first ydiff))
              (syntactic-features-equal? (second xdiff) (second ydiff))))))

    (let ((x (if ignore-exp raw-x (unroll-exponent-step raw-x)))
          (y (if ignore-exp raw-y (unroll-exponent-step raw-y)))
          (rec-ignore-exp (when (equal ignore-exp 'r) 'r)))
      ;; Now we can assume exponent = 1 for the domain and at the top-level.
      (cond
        ;; One is optional.
        ;; Make option lists and see if any pair of options work.
        ((or (optional-type-p x) (optional-type-p y))
         (let ((x-options (if (optional-type-p x) (types x) (list x)))
               (y-options (if (optional-type-p y) (types y) (list y))))
           (loop for x-option in x-options
                 if (loop for y-option in y-options
                          if (semtype-match? x-option
                                             y-option
                                             :ignore-exp rec-ignore-exp)
                          return t)
                 return t)))

        ;; No optionals
        (t
         (and
           ;; Suffixes.
           ;; If both are specified, must match.
           (if (and (suffix x) (suffix y))
               (equal (suffix x) (suffix y))
               t)
           ;; Syntactic features.
           (syntactic-features-match? (synfeats x) (synfeats y))
           (if (or (atomic-type-p x) (atomic-type-p y))
               ;; If atomic, both must be and match domain values.
               (and (atomic-type-p x)
                    (atomic-type-p y)
                    (equal (domain x) (domain y)))
               ;; If not atomic, domain, range, and connective must match.
               (and (semtype-match? (domain x) (domain y)
                                    :ignore-exp ignore-exp)
                    (semtype-match? (range x) (range y)
                                    :ignore-exp ignore-exp)
                    (equal (connective x) (connective y))
                    ;; If connective is >> but a synfeat switch is specified, it
                    ;; must be specified in both since unspecified cases will
                    ;; never lead to the switch.
                    (if (equal '>> (connective x))
                        (>>-synfeat-match? x y)
                        t)))))))))

;; Make a new semtype identical to the given type
;; Key word options allow overwriting specific fields if appropriate.
;; Keyword options where nil is a specific value (e.g. false) a keyword symbol
;; :null is used to indicate no value provided, which defaults to using the
;; source semtype value.
(defun copy-semtype (x &key c-dom c-ran c-ex
                       (c-suffix :null)
                       (c-types :null)
                       (c-synfeats :null)
                       (c-type-params :null)
                       (c-conn :null))
  (let ((result
          (cond
            ((atomic-type-p x)
             (make-instance 'atomic-type
                            :domain (if c-dom c-dom (domain x))
                            :ex (if c-ex c-ex (ex x))))
            ((optional-type-p x)
             (make-instance 'optional-type
                            :types (if (not (eql :null c-types))
                                       c-types
                                       (list (copy-semtype (first (types x)))
                                             (copy-semtype (second (types x)))))
                            :ex (if c-ex c-ex (ex x))))
            ((semtype-p x)
             (make-instance 'semtype
                            :domain (if c-dom c-dom (copy-semtype (domain x)))
                            :range (if c-ran c-ran (copy-semtype (range x)))
                            :ex (if c-ex c-ex (ex x))))
            (t (return-from copy-semtype x)))))
    (labels
      ((nnull-backup (nnull backup)
         (if (not (eql :null nnull)) nnull (funcall backup x))))
      ;; Fill in not null slots.
      (setf (suffix result) (nnull-backup c-suffix #'suffix))
      (setf (type-params result) (nnull-backup c-type-params #'type-params))
      (setf (connective result) (nnull-backup c-conn #'connective))
      (setf (synfeats result)
            ;; Need to copy synfeats object.
            (nnull-backup c-synfeats #'(lambda (st)
                                         (if (synfeats st)
                                             (copy (synfeats st))
                                             nil))))
      result)))

;; Convert a semtype to a string. The string it returns can be read back into a
;; type using str2semtype.
(defun semtype2str (s)
  (let ((*package* (find-package "ULF-LIB"))) ; avoid package prefixes in symbols.
    (when (null s)
      (return-from semtype2str nil))
    (let ((type-params-str
            (format nil "[~a]"
                    (str:join ";" (mapcar #'semtype2str (type-params s)))))
          ;; Synfeats without the macro dispatch symbol.
          (synfeat-str (str:concat
                         (if (suffix s)
                             (format nil "_~s" (suffix s))
                             "")
                         (if (and (synfeats s) (not (empty? (synfeats s))))
                             (format nil "%~a"
                                     ;; Remove #{ and } from syntactic features.
                                     (str:substring
                                       2 -1 (to-string (synfeats s))))
                             "")))
          (exponents
            (if (= (the fixnum (ex s)) 1) "" (format nil "^~a" (ex s))))
          base)
      (setf base
            (cond
              ((atomic-type-p s)
               ; Atomic type
               (format nil "~a~a~a"
                       (domain s)
                       synfeat-str
                       exponents))
              ((optional-type-p s)
               ; Optional type
               (format nil "{~a|~a}~a"
                       (semtype2str (first (types s)))
                       (semtype2str (second (types s)))
                       exponents))
              ((semtype-p s)
               ; Not optional or atomic
               (format nil "(~a~a~a)~a~a"
                       (semtype2str (domain s))
                       (connective s)
                       (semtype2str (range s))
                       synfeat-str
                       exponents))
              (t nil)))
      (if (equal type-params-str "[]")
          base
          (format nil "~a~a" base type-params-str)))))

;; Split a string of the form ([domain]=>[range]) or ([domain]>>[range]) into
;; [domain], [range], [connective]. Helper for str2semtype.
(declaim (ftype (function (simple-string) list) split-semtype-str))
(defun split-semtype-str (s)
  (let ((level 0) (i 1))
    (declare (type fixnum level i))
    (loop
      (when (eql (char s i) #\()
        (setf level (+ level 1)))
      (when (eql (char s i) #\))
        (setf level (- level 1)))
      (when (and (= level 0)
                 (> (length s) (+ i 2))
                 (or (and (eql (char s i) #\=) (eql (char s (1+ i)) #\>))   ; =>
                     (and (eql (char s i) #\>) (eql (char s (1+ i)) #\>))   ; >>
                     (and (eql (char s i) #\%) (eql (char s (1+ i)) #\>)))) ; %>
        (return i))
      (setf i (+ i 1)))
    (list (subseq s 1 i)
          (subseq s (+ i 2) (- (length s) 1))
          (subseq s i (+ i 2)))))

;; Split a string of the form {A|B} into A and B. Helper for str2semtype.
(declaim (ftype (function (simple-string) list) split-opt-str))
(defun split-opt-str (s)
  (let ((level 0) (i 1))
    (declare (type fixnum level i))
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
   These are semi-colon separated, but since the types are recursively defined,
   only split on semi-colons that are at the top-level square bracketing."
  (declare (type (or simple-string null) tpstr))
  (when (null tpstr)
    (return-from split-type-param-str nil))
  (let ((params nil)
        (cur-chars nil)
        (bracket-depth 0))
    (declare (type list params cur-chars)
             (type fixnum bracket-depth))
    (loop for c across tpstr do
          (cond
            ; Semi-colon at top-level, add word and reset cur-chars.
            ((and (eql c #\;) (= bracket-depth 0))
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

(defun process-out-synfeat-connective (base-semtype new-synfeats type-params)
  "Processes out the special synfeat connective, %>, which assumes that the
   antecedent and consequent semantic types match except for the explicit
   synfeat changes listed in the consequent of %>.

   We simply convert A%>S into {a1>>a1%S|{a2>>a2%S|{...}}} for the a1, a2, ...
   flattened versions of A. This ensures that after applying this rule, the
   semantic type in fact stays the same even if the original semantic type had a
   bunch of alternatives. We don't want all alternatives to be possible in the
   consequent for each of the antecedent alternatives."
  ;; Steps
  ;; 1. Flatten out the options
  ;; For each option
  ;;  2. Make a copy
  ;;  3. Overwrite the synfeats of the copy with new synfeats
  ;;  4. Create the new type with the base as antecedent and copy as consequent
  ;; Then
  ;; 5. Merge into a new single option type
  ;; 6. Binarize
  (let
    ((new-types
       ;; Flatten, overwrite synfeats, and create new type.
       (loop for st in (types (flatten-options base-semtype))
             for copy-st = (copy-semtype
                             st :c-synfeats (copy *default-syntactic-features*))
             for new-synfeat = (update-syntactic-features (synfeats copy-st)
                                                          new-synfeats)
             collect (new-semtype st copy-st 1 nil
                                  :type-params type-params
                                  :conn '>>)))
     flat-type)
    ;; Merge and binarize.
    (setf flat-type (new-optional-semtype new-types))
    (binarize-flat-options flat-type)))


;; Convert a string into a semantic type.
;; Strings must be of the form ([domain]=>[range]) or a single character, where
;; [domain] and [range] are valid strings of the same form.
;; Some single character suffixes are supported, denoted by a _ followed by the
;; character. Comma-separated syntactic features are supported after %. Single
;; digit/character exponents are also supported denoted by a ^ followed by the
;; digit/character. Exponents must occur after any suffixes and syntactic
;; features.
(defun str2semtype (s &key (recurse-fn #'str2semtype))
  (declare (type function recurse-fn))
  (let* ((subscript-regex (concatenate 'string
                                       "(_([NAVP]))?"        ; POS
                                       "(%([^>\\^\\[]+))?")) ; synfeats
         (superscript-regex "(\\^([A-Z]|[2-9]))?")
         (type-param-regex "(\\[(.*)\\])?")
         ; ([domain]=>[range])_[ut|navp]^n\[[type1],[type2],..\]
         (non-atom-regex (concatenate 'string
                                      "^(\\(.*\\))" 
                                      subscript-regex
                                      superscript-regex
                                      type-param-regex
                                      "$"))
         ; {A|B}[[type1],[type2],..\]
         (optional-regex (concatenate 'string
                                      "^(\\{.*\\})"
                                      subscript-regex
                                      superscript-regex
                                      type-param-regex
                                      "$"))
         (atomic-regex (concatenate 'string
                                    "^([A-Z0-9\-\+\*]+)"
                                    subscript-regex
                                    superscript-regex
                                    type-param-regex
                                    "$"))

         ;; Compute match indices.
         ;; Each section generates 2 matches and first one isn't included in labels.
         (suf-idx 2)
         (syn-idx 4)
         (exp-idx 6)
         (tp-idx 8)
         (synfeat-parse-fn #'(lambda (str)
                               (enable-ulf-type-syntax)
                               (let ((result (read-from-string
                                               (format nil "#{~a}" str))))
                                 (disable-ulf-type-syntax)
                                 result))))

    (setf s (string-upcase s))
    (cond
      ; NON ATOMIC ([domain][connective][range])_[ut]#[synfeats]^n\[[type1],[type2],..\]
      ((equal (char s 0) #\()
       (let* ((match (nth-value 1 (cl-ppcre:scan-to-strings non-atom-regex s)))
              (split-segments (split-semtype-str (svref match 0)))
              (conn (intern (third split-segments) :ulf-lib)))
         (if (eql '%> conn)
             ;; If connective is %> then we need to process the antecedent first
             ;; and apply the consequent synfeats accordingly.
             ;; This will never have top-level exponents, suffixes, or synfeats of its own. 
             (process-out-synfeat-connective
               (funcall recurse-fn (first split-segments))        ; get antecedents
               (funcall synfeat-parse-fn (second split-segments)) ; consequent is just synfeats
               (mapcar recurse-fn (split-type-param-str (svref match tp-idx)))) ; type params

             ;; For other connectives, simply find the components in the string.
             (new-semtype (funcall recurse-fn (first split-segments))
                          (funcall recurse-fn (second split-segments))
                          (if (svref match exp-idx) (read-from-string (svref match exp-idx)) 1)
                          (if (svref match suf-idx) (read-from-string (svref match suf-idx)) nil)
                          :synfeats (funcall synfeat-parse-fn (svref match syn-idx))
                          :type-params (mapcar recurse-fn (split-type-param-str (svref match tp-idx)))
                          :conn conn))))

      ; OPTIONAL {A|B}[[type1],[type2],..\]
      ((equal (char s 0) #\{)
       (let* ((match (nth-value 1 (cl-ppcre:scan-to-strings optional-regex s)))
              (options (split-opt-str (svref match 0))))
         (new-semtype NIL NIL
                      (if (svref match exp-idx) (read-from-string (svref match exp-idx)) 1)
                      (if (svref match suf-idx) (read-from-string (svref match suf-idx)) nil)
                      :options (list (funcall recurse-fn (first options))
                                     (funcall recurse-fn (second options)))
                      :synfeats (funcall synfeat-parse-fn (svref match syn-idx))
                      :type-params (mapcar recurse-fn (split-type-param-str (svref match tp-idx))))))
      ; ATOMIC
      (t
       (let ((match (nth-value 1 (cl-ppcre:scan-to-strings atomic-regex s))))
         (new-semtype (read-from-string (svref match 0)) NIL
                      (if (svref match exp-idx) (read-from-string (svref match exp-idx)) 1)
                      (if (svref match suf-idx) (read-from-string (svref match suf-idx)) nil)
                      :type-params (mapcar recurse-fn (split-type-param-str (svref match tp-idx)))))))))

;; Convert a string into a semantic type, extended to handle cases in ULF that
;; do not strictly follow EL compositions. Strings must be of the form
;; ([domain]=>[range]), a single character, or one of the designated special
;; symbols for selected ULF macros and phenomena. [domain] and [range] are
;; valid strings of the same form. Some single character suffixes are
;; supported, denoted by a _ followed by the character. Single digit/character
;; exponents are also supported denoted by a ^ followed by the digit/character.
(defun extended-str2semtype (s)
  (let* ((str (string-upcase s))
         (sym (intern str :ulf-lib)))
    (cond
      ;; Non-atomic type.
      ((equal (char str 0) #\() (str2semtype str :recurse-fn #'extended-str2semtype))
      ;; Optional type.
      ((equal (char str 0) #\{) (str2semtype str :recurse-fn #'extended-str2semtype))
      ;; p-arg
      ((equal str "PARG") (new-semtype 'parg nil 1 nil))
      ;; Quotes.
      ((equal str "\"") (new-semtype '\" nil 1 nil))
      ;; Any of the macros.
      ((lex-macro? sym) (new-semtype sym nil 1 nil))
      ;; Any of the extended macros.
      ((member str '("QT-ATTR1" "QT-ATTR2" "SUB1" "REP1" "POSTGEN1" "POSTGEN2" "+PREDS") :test #'equal)
       (new-semtype sym nil 1 nil))
      ;; Macro hole variables (*p, *h, *ref, *s, etc.)
      ((lex-macro-hole? sym) (new-semtype sym nil 1 nil))
      ;; Non-special atomic type.
      (t (str2semtype s :recurse-fn #'extended-str2semtype)))))

(defun semtype-equal? (s1 s2)
  "Determines whether two semtypes are equivalent. For optional types, order
   doesn't matter and allows flattening of options."
  (labels
    (; Runs a list of accessor-test pair for computing equality on specific
     ; components of two elements.
     (acc-test (t1 t2)
       #'(lambda (accessor-test)
           (declare (type list accessor-test))
           (let ((accfn (the function (first accessor-test)))
                 (testfn (the function (second accessor-test))))
             (funcall testfn (funcall accfn t1) (funcall accfn t2)))))
     ; Runs equality on two lists of semtypes, ignoring order.
     (set-no-option-equal (set1 set2)
       (declare (type list set1 set2))
       (and (= (length set1) (length set2))
            (every #'(lambda (set1-elem)
                       (member set1-elem set2 :test #'no-option-equal))
                   set1)))
     ; Determines equality for two semtypes that don't contain any options.
     (no-option-equal (raw-t1 raw-t2)
       ;; Pull out exponents.
       (let ((t1 (unroll-exponent-step raw-t1))
             (t2 (unroll-exponent-step raw-t2)))
         (cond
           ((or (optional-type-p t1) (optional-type-p t2))
            (error "No optional types allowed in no-option-equal"))
           ;; Base cases.
           ;; One of the arguments are atomic.
           ((or (and (atomic-type-p t1) (not (atomic-type-p t2)))
                (and (not (atomic-type-p t1)) (atomic-type-p t2)))
            nil)
           ; Atomic (only has domain)
           ((and (atomic-type-p t1) (atomic-type-p t2))
            (every (acc-test t1 t2)
                   (list (list #'domain #'equal)
                         (list #'suffix #'equal)
                         (list #'synfeats #'syntactic-features-equal?)
                         (list #'type-params #'set-no-option-equal))))
           ; Non-atomic (has domain and range)
           (t
            (every (acc-test t1 t2)
                   (list (list #'domain #'no-option-equal)
                         (list #'range #'no-option-equal)
                         (list #'suffix #'equal)
                         (list #'synfeats #'syntactic-features-equal?)
                         (list #'type-params #'set-no-option-equal)
                         (list #'connective #'equal)))))))
     ) ; end of label definitions.
    ;; Use t instead of semtype for argument check because of the following
    ;; compiler note
    ;;  note: can't open-code test of unknown type SEMTYPE
    (declare (ftype (function (t t) function)
               acc-test))
    ; labels body
    ; If we get to an atomic label, just check the labels.
    (let ((flat-s1 (remove-duplicates (types (flatten-options s1))
                                      :test #'semtype-equal?))
          (flat-s2 (remove-duplicates (types (flatten-options s2))
                                      :test #'semtype-equal?)))
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
      (t (new-optional-semtype new-options)))))

(defun flatten-options (raw-s)
  "Flattens a semtype so that all options and concrete exponents are
   represented with a single top-level option type. In a sense, this is
   un-factorizing the semtype. This will always return an optional type even if
   there is only one option. Will return nil if there's a 0 exponent."
  ; Remove top-level and domain-level exponent.
  (let ((s (unroll-exponent-step raw-s)))
    ; Flatten the type-params if necessary.
    (setf s (if (type-params s)
                (flatten-type-params s)
                s))
    (cond
      ;; Concrete =0 exponent type, return nil.
      ((= 0 (ex s)) nil)

      ;; Optional type, recurse on all and flatten into a single optional type.
      ((optional-type-p s)
       (let ((new-options (apply #'append (mapcar (compose #'(lambda (opt) (if opt (types opt) nil))
                                                           #'flatten-options)
                                                  (types s)))))
         (if new-options
             (new-optional-semtype new-options))))
      ;; Atomic type, return a copy of it wrapped in an optional type.
      ((atomic-type-p s)
       (new-optional-semtype (list (copy-semtype s))))
      ;; Non-atomic, non-optional type, recurse into the domain and range and generate an optional
      ;; type of all combinations of the recursed types.
      (t (let ((flat-dom (flatten-options (domain s)))
               (flat-ran (flatten-options (range s)))
               new-options)
           (setf new-options
                 (cond
                   ((null flat-dom) flat-ran)
                   ((null flat-ran) flat-dom)
                   (t
                    (assert (optional-type-p flat-dom))
                    (assert (optional-type-p flat-ran))
                    (loop for cur-dom in (types flat-dom)
                          append (loop for cur-ran in (types flat-ran)
                                       collect (copy-semtype s :c-dom cur-dom
                                                             :c-ran cur-ran))))))
           (if new-options
               (new-optional-semtype new-options)))))))

(defun binarize-flat-options (s)
  "Takes a flat option type and makes it a right-leaning binary tree of options."
  (labels
    ((helper (options)
       (declare (type list options))
       (cond
         ((< (length options) 3) (new-optional-semtype options))
         (t (new-optional-semtype (list (first options)
                                        (helper (rest options))))))))
    ; Top-level call.
    (helper (types s))))

