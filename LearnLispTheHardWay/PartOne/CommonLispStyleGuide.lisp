;; Symbol names should be descriptive, short, typed in all lowercase, with words separated by a single hyphen:

(defun my-addition-function (&rest rest)
  (apply #'+ rest))
;; => MY-ADDITION-FUNCTION

(deftype my-integer-type ()
  '(and integer
        (satisfies plusp)))
;; => MY-INTEGER-TYPE

(defvar my-hash-table (make-hash-table :test 'equal))
;; => MY-HASH-TABLE

(defvar my-alist '(("one" . 1)
                   ("two" . 2)
                   ("three" . 3)))
;; => MY-ALIST

;; Note that symbol names created from strings should be in all-caps:
(intern "MY-NEW-INTERNED-SYMBOL")
;; => MY-NEW-INTERNED-SYMBOL
   NIL
;; or you'll have to reference it in surrounding hbars as a literal, case-sensitive symbol:
(intern "my-funky-interned-symbol")
;; => |my-funky-interned-symbol|
   NIL

;; Global variables, i.e., variables declared as top-level forms with defvar or defparameter, 
;; are named using “earmuffs” because they are dynamic and special, such as the following built into Common Lisp:

*package*
;; => #<PACKAGE "COMMON-LISP-USER">
*print-base*
;; => 10
;; In the case of earmuffs, these are stylistic. They are not parsed as syntactic tokens by the Lisp reader, 
;; they are simply read as part of the symbol name.

;; Another stylistic convention for symbol names uses a pair of plus-signs to wrap symbol-names of constants:

(defconstant +my-new-constant+ 1.0)

;; Package internal symbols are sometimes named with a prepended percent-sign, 
;; and not exported with the package API. These are similar in purpose to private functions, 
;; methods, and variables in other programming languages—only they can always be accessed by using the full symbol name:

(defpackage #:my-new-package
  (:use :cl)
  (:export #:mad-adder))

(in-package :my-new-package)

;; Do some wonky stuff with a package-internal function
(defun %madder (x)
  (declare (integer x))
  (apply #'+ (loop for i from 1 upto x
                   collect (* x i))))

;; Write an exported interface to your package internal function
(defun mad-adder (x)
  "Call %MADDER with integer argument X."
  (%madder x))

(in-package :cl-user)

(my-new-package:mad-adder 10)

(my-new-package::%madder 10)

;; Predicate functions, i.e., boolean tests, typically end with a suffixed “p”. 
;; If it is a multi-word symbol already separated by dashes, you append the suffix as “-p” 
;; (dash-p); while if it is a single word or mnemonic symbol name, the “p” can be appended without a dash.

(bit-vector-p #*01010001)
=> T
(integerp 10)
=> T


;; bad style
(defun a-badly-formatted-function (x y z)
  (progn
    (setq x (+ x x))
    (setq y (* y y))
    (mod
      (+ z z)
      (+ x y)
      )
    )
  )

;; the right way
(defun a-pretty-function (x y z)
  "Function definitions need docstrings."
  (declare (integer x y z))
  (let ((x2 (+ x x))
        (y2 (* y y)))
    (mod (* z z) (+ x2 y2))))

;; IF is a special operator, and doesn't have a body expression
(if t t nil)

;; but you'll normally need to split the form for clarity; see how the three parameters line up?
(if t
    (format t "Then: True~%")
    (format t "Else: False~%"))

;; WHEN does have a body expression though, so its body isn't lined up with the test-form parameter
(when t
  (format t "This is true, too!"))

;; a badly formatted class definition
(defclass march-hare ()
  ((name        :type string  :initarg :name        :initform "Haigha"  :accessor name)
   (tea-time-p  :type boolean :initarg :tea-time-p  :initform t         :accessor tea-time-p)
   (tie         :type string  :initarg :tie         :initform "bow-tie" :accessor tie)))

;; the right way
(defclass march-hare ()
  ((name :type string :initarg :name :initform "Haigha" :accessor name
         :documentation "The name of the March Hare.")
   (tea-time-p :type boolean :initarg :tea-time-p :initform t :accessor tea-time-p
               :documentation "Whether or not it's tea-time.")
   (tie :type string :initarg :tie :initform "bow-tie" :accessor tie
        :documentation "The style of tie the March Hare is wearing."))
  (:documentation "'The March Hare will be much the most interesting, and perhaps as this is May it won't be raving mad---at least not so mad as it was in March.' -- Lewis Carroll"))

;;;; four preceding semi-colons for file headers, copyright and license information

;;; three preceding semi-colons for descriptive text

;; two preceding semi-colons for commentary on the immediately following text

; one preceding semi-colon for in-line comments, inside code blocks

