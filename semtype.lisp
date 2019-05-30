;; Data structure for representing ULF semantic types

(in-package :ulf-lib)

; Possible idea: represent 'n' in exp as -10, with -9 = n+1, -11 = n-1, and so on
; Might be easier for functions in composition.lisp but worse for readability
; Although the print function would make it prettier, which might make it OK.

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
     :accessor tense)))

;; Subclass for atomic types
(defclass atomic-type (semtype)
  ())

;; Check if a given semantic type is an atomic type.
(defun atomic-type-p (s)
  (equal (type-of s) 'atomic-type))

;; Check if two semantic types are equal
;; If :ignore-exp is not NIL then the exponents of the two semtypes aren't checked
(defun semtype-equal? (x y &key ignore-exp)
  (when (and (if ignore-exp T (equal (ex x) (ex y)))
             (equal (subscript x) (subscript y))
             (equal (tense x) (tense y))
             (equal (atomic-type-p x) (atomic-type-p y)))
    (if (atomic-type-p x)
      (equal (domain x) (domain y))
      (and (semtype-equal? (domain x) (domain y)) (semtype-equal? (range x) (range y))))))

;; Make a new semtype identical to the given type
(defun copy-semtype (x)
  (if (atomic-type-p x)
    (make-instance 'atomic-type
                   :domain (domain x)
                   :ex (ex x)
                   :subscript (subscript x)
                   :tense (tense x))
    (make-instance 'semtype
                   :domain (copy-semtype (domain x))
                   :range (copy-semtype (range x))
                   :ex (ex x)
                   :subscript (subscript x)
                   :tense (tense x))))

;; Turn an exponent of a semtype into a string
;; An exponent is always of the form (+/- A B) or just a number/symbol
(defun exp2str (x)
  (if (listp x)
    (format nil "(~a~a~a)" (exp2str (second x)) (car x) (exp2str (third x)))
    (format nil "~a" x)))

;; Print a given semantic type
(defun print-semtype (s)
  (if (atomic-type-p s)
    (progn
      (format t "~a" (domain s))
      (unless (equal (ex s) 1) (format t "^~a" (exp2str (ex s)))))
    (progn
      (format t "(")
      (print-semtype (domain s))
      (format t "=>")
      (print-semtype (range s))
      (format t ")")
      (when (subscript s) (format t  "_~a" (subscript s)))
      (when (tense s) (format t "_~a" (tense s)))
      (unless (equal (ex s) 1) (format t "^~a" (exp2str (ex s)))))))

;; Split a string of form ({domain}=>{range}) into {domain} and {range}
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

;; Convert a string into a semantic type.
;; Strings must be of the form ({domain}=>{range}) or just a single character.
;; {domain} and {range} must also be valid strings of the same form.
;; Only supports single digits or single letters as exponents
(defun str2semtype (s)
  (if (equal (char s 0) #\()
    ; NON ATOMIC ({domain}=>{range})_{[ut|navp]}^n
    (let ((match (nth-value 1 (cl-ppcre:scan-to-strings
                                "(\\(.*\\))(_(([ut])|([navp])))?(\\^([a-z]|[2-9]))?$"
                                s))))
      (make-instance 'semtype
                     :domain (str2semtype (car (split-semtype-str (svref match 0))))
                     :range (str2semtype (cadr (split-semtype-str (svref match 0))))
                     :ex (if (svref match 6) (read-from-string (svref match 6)) 1)
                     :subscript (if (svref match 4) (read-from-string (svref match 4)) nil)
                     :tense (if (svref match 3) (read-from-string (svref match 3)) nil)))

    ; ATOMIC
    (let ((match (nth-value 1 (cl-ppcre:scan-to-strings "([A-Z]|[0-9])(\\^([a-z]|[2-9]))?" s))))
      (make-instance 'atomic-type
                     :domain (read-from-string (svref match 0))
                     :ex (if (svref match 2) (read-from-string (svref match 2)) 1)))))
