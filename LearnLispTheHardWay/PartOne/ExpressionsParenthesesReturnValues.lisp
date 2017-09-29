(+ 1 1)
;; => 2  as expected, because it's a valid S-Expression
;; {+ 1 1}
;; this won't evaluate, because the Lisp reader doesn't recognize braces

;; first lets define a couple variables
(defvar *test-list-a* (list 1 2 3))
(defvar *test-list-b* (list 'd 'e 'f))
;; append returns a new list from its arguments
(append *test-list-a* *test-list-b*)
;; => (1 2 3 D E F)
;; you can see that the original lists haven't changed
*test-list-a*
;; => (1 2 3)
*test-list-b*
;; => (D E F)
;; but now lets do a destructive operation, NCONC (ie, in-place list concatenation)
(nconc *test-list-a* *test-list-b*)
;; => (1 2 3 D E F)
;; the variable's binding and assignment haven't changed, but the last cons-cell
;; now points to *test-list-b* instead of terminating at NIL
*test-list-a*
;; => (1 2 3 D E F)

;; this is a typical anonymous function call; the last form in its body is (+ x x)
;; so the function call returns (+ 2 2) => 4
((lambda (x) (+ x x)) 2)
;; => 4
;; in this function, the return result of (+ x x) is not assigned so it is essentially
;; lost; the function body moves on to the next form, (* x x), which is the last form
;; of this function body. So the function call only returns (* 10 10) => 100
((lambda (x) (+ x x) (* x x)) 10)
;; => 100
;; in this function, we capture the return values of both (+ x x) and (* x x), as the
;; lexical variables SUM and PRODUCT; using VALUES, we can return multiple values from
;; a form instead of just one
((lambda (x) (let ((sum (+ x x)) (product (* x x))) (values sum product))) 10)
;; => 20 100
;; but calling VALUES without anything gives us... an expression with no return result!
(values)
;; => ; No value

;; these are some self-evaluating objects:
;; strings---
"a string"
;; characters---
#\greek_small_letter_lamda
;; numbers
42
#x2A
;; bit-vectors---
#*1001

;; this:
'(a b c)
;; is the same as this:
'(a
  b
  c)
;; and this:
(list 'a 'b 'c)