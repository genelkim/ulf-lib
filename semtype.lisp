;; Data structure for representing ULF semantic types

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
   (tense?
     :initarg :tense?
     :initform nil
     :accessor tense?)))

;; Check if a given semantic type is an atomic type.
;; Atomic semantic types have a symbol domain and a nil range.
(defun semtype-atom? (s)
  (and (or (symbolp (domain s)) (integerp (domain s))) (not (range s))))

;; Print a given semantic type
(defun print-semtype (s)
  (if (semtype-atom? s)
    (progn
      (format t "~a" (domain s))
      (when (> (ex s) 1) (format t "^~a" (ex s))))
    (progn
      (format t "(")
      (print-semtype (domain s))
      (format t "=>")
      (print-semtype (range s))
      (format t ")")
      (when (subscript s) (format t  "_~a" (subscript s)))
      (when (tense? s) (format t "_~a" (tense? s)))
      (unless (equal (ex s) 1) (format t "^~a" (ex s))))))

;; Split a string of form ({domain}=>{range}) into {domain} and {range}
(defun split-semtype-str (s)
  (setf level 0)
  (setf i 1)
  (loop
    (when (equal (char s i) #\()
      (setf level (+ level 1)))
    (when (equal (char s i) #\))
      (setf level (- level 1)))
    (when (and (equal (char s i) #\=) (= level 0))
      (return i))
    (setf i (+ i 1)))
  (list (subseq s 1 i) (subseq s (+ i 2) (- (length s) 1))))

;; Convert a string into a semantic type.
;; Strings must be of the form ({domain}=>{range}) or just a single character.
;; {domain} and {range} must also be valid strings of the same form.
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
                     :tense? (if (svref match 3) (read-from-string (svref match 3)) nil)))

    ; ATOMIC
    (let ((match (nth-value 1 (cl-ppcre:scan-to-strings "([A-Z]|[0-9])(\\^([a-z]|[2-9]))?" s))))
        (make-instance 'semtype
                       :domain (read-from-string (svref match 0))
                       :ex (if (svref match 2) (read-from-string (svref match 2)) 1)))))

