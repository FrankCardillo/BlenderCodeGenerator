(list)
;; => NIL
(list 'a)
;; => (A)
(list 'a nil)
;; => (A NIL)
(cons 'a nil)
;; => (A)

;; lists are built on top of cons cells

;; this:
(list 'a 'b 'c)
;; is the same as this:
(cons 'a (cons 'b (cons 'c nil)))
;; while this:
(list 'a 'nil)
;; is the same as this:
(cons 'a (cons nil nil))

;; can have the end of a chain of cons cells point to another value

;; this:
'(a . b)
;; is the same as this:
(cons 'a 'b)

;; A list of dot-notation pairs like this is called an association list, 
;; or alist for short. They are one of many structures available in Lisp 
;; for storing key/value pairs, and have a good API.

'((a . b)
  (c . d)
  (e . f))

;; this creates three cons-cells, the quoted symbols 'A, 'B, and 'C each in the CAR of their own Cons-Cell
(list 'a 'b 'c)
;; it would be the same as typing this:
(cons 'a (cons 'b (cons 'c nil)))
;; or this:
'(a . (b . (c . nil)))
;; or this:
'(a b c . nil)
;; or simply this:
'(a b c)