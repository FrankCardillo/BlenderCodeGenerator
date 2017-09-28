94

'a-quoted-symbol

#\greek_small_letter_lamda

"This is a string"

nil

'() ;; evaluates to nil

() ;; evaluates to nil

;; That is because nil is defined as the empty list, 
;; which itself is treated atomically as it doesn't require any consing. 
;; But there's other trickery going on behind the scenes here too—strings, 
;; for example, while treated as Atoms, are actually sequences of character literals
;; lists are also a sub-type of sequences.

t

;; These are all atoms. They are not cons cells

;; To really understand what's going on here, you have to remember that there's 
;; an object hierarchy—every object in Lisp descends from t—and again, everything is 
;; an object. The symbol nil is a special case, because it's the only member of the system 
;; class null. Only nil and its alternate representation, the empty list, represents logical 
;; falsity. Every other object is truthy. You don't have to return t to say that a function 
;; returns true, because any value other than nil is logically truthy, but it is useful when 
;; you need to return true and don't have any specific value to return.

'()

'(this is a list of symbols)

(quote (this is another list of symbols))

'another-quoted-symbol3


;; lists as forms (code not data)
(+ 10 20 (* 30 2))

(princ "Hello, from Lisp!")

(loop for item in '(this list will get printed in titlecase) do (format t "~@(~A~) " item))