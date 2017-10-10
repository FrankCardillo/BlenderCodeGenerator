;; Polish Prefix notation, the operator comes first:
(operator . (list of parameters))

(+ 1 2 3 4 5)
;; => 15

;; In Lisp, you don't have to write the addition operator, +, over and over between each 
;; number you wish to add together. You are passing parameters to a function, and the 
;; function knows to collect the parameters as a set and Sum them. That's the big 
;; conceptual difference between Lisp and other languages—when the operator comes first, 
;; you are telling the computer what you want instead of what to do and how to do it. 
;; So in the example above, you are telling Lisp you want the Sum of the set of integers 
;; from 1 to 5 inclusive, not telling it to add 1 to 2, then add the result to 3, then add 
;; the result to 4, and then add that result to 5, to get the integer 15. Do you see the difference?

