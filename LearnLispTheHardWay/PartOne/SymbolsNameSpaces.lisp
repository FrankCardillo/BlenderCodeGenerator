;; Common Lisp has separate namespaces for Functions and Variables in any given environment.

(defvar foo 1)

(defun foo (foo)
  (+ foo foo))

(foo foo)
;; => 2

;; Dynamic scoping is special and explicit in Common Lisp; lexical scoping is more intuitive and 
;; implicit—in other words, you have to specifically declare a symbol to be special to use its dynamic binding 
;; from within a lexical scope where the symbol could be lexically bound and assigned as a different variable, 
;; while many forms introduce an implicit lexical scope. For this reason there is a naming convention for 
;; top-level, dynamic variables, called “earmuffs”

;; top-level, dynamic variables can be declared with DEFVAR or DEFPARAMETER
(defvar *my-dynamic-var* "I'm special!")
;; => *MY-DYNAMIC-VAR*
;; notice that the variable names are qualified with a pair of asterisks? These are called earmuffs.
(defparameter *my-extra-special-dynamic-var* "I'm special, too!")
;; => *MY-EXTRA-SPECIAL-DYNAMIC-VAR*
;; one obvious way to introduce a lexical scope is with a LET form for binding and assigning lexical variables:
(let ((one 1)
      (two 2)
      (three 3))
  (+ one two three))
;; => 6
;; now let's put them both together
(defvar *one* 1)
;; => *ONE*
(let ((one 1.0))
  (+ one *one*))
;; => 2.0

;; Another excellent feature for taming the raw power of Common Lisp is its package system, 
;; which allow you to specify custom read-tables for your environment. When you define a package, 
;; you have to explicitly import symbols you want available in the package namespace—even the 
;; symbols of the Common Lisp language itself; you can import all of a package's exported symbols 
;; into your new package at once with the :use keyword expression in the body of your package definition.

(in-package :cl-user)

(defpackage #:my-new-package
  (:nicknames #:newpack)
  (:use :cl :cl-user)
  (:export #:mad-adder))

(in-package :my-new-package)

(defvar *my-private-var* "I'm not exported from the package")

(defun mad-adder (n &rest rest)
  "An addition function for MY-NEW-PACKAGE."
  (apply #'+ n rest))


;; When using members of a package

;; this:
(newpack:mad-adder 1)
;; is the same as:
(my-new-package:mad-adder 1)
;; if a symbol isn't exported, however, you have to use two colons between the package and symbol
newpack::*my-private-var*