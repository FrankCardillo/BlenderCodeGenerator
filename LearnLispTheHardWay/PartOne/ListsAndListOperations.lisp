;; In the spirit of Lambda Calculus and a pure functional heritage, the Lisp-family of programming 
;; languages gets its name from its purpose—LISt Processing. Even though Common Lisp is not a 
;; purely-functional programming language, its fundamental syntax has not deviated from this 
;; heritage; as you should remember from Chapter 1.1, Cons-Cells are one of two essential forms of 
;; Expressions in Lisp, and they are represented using List syntax; and since the other—Atoms—are, 
;; by definition, self-evaluating, everything interesting that you can do in Lisp is effectively a 
;; List operation. And these are divided into consing and non-consing operations.

;; This is important to remember—it's the reason there are so many parentheses in Lisp source-code. 
;; Every form surrounded by parentheses is a list. The only difference between code and data, 
;; syntactically speaking, is that code is read and evaluated whereas data is only read; you can 
;; switch to “data mode” by quoting an expression, but quoting isn't a free pass. Lisp still expects 
;; to be able to Read the forms you've quoted as valid forms, so the syntax is just as important to 
;; data as it is to code.

;; This is also the key to Lisp's homoiconicity—the same syntax is used to represent both Code and Data, 
;; and as a result you can treat Code as Data, and Data as Code. As far as Lisp is concerned, there is 
;; no difference between the two.

;; But Lists are also a proper type in Common Lisp, that descends from sequences.

;; ========================================================================================================

;; Cons-Cells are the smallest compound data structure in Lisp. A Cons-Cell is effectively a pair of pointers. 
;; You can tell if what you're looking at is a Cons-Cell by using the predicate consp.

* (consp 5)
NIL

* (consp "a")
NIL

* (consp 'a)
NIL

* (consp (cons 'a 'b))
T

;; One way to create a Cons-Cell is using the cons function.

* (cons 'a 'b)
(A . B)
;; They can hold any type of data, not just symbols.

* (cons 1 2)
(1 . 2)

* (cons "one" "two")
("one" . "two")

;; You can see that when we cons two atoms together, we get back a Dotted Pair. 
;; This is a readable representation of Cons-Cells. That is, you can use it directly, 
;; rather than calling cons.

* (cons 'a 'b)
(A . B)

* '(a . b)
(A . B)
;; These two representations are equivalent.

* (equal (cons 'a 'b) '(a . b))
T

;; Cons-Cells need not contain homogenous data.

* (cons 'a 2)
(A . 2)

* (cons 1 "two")
(1 . "two")

* (cons "a" 'b)
("a" . B)

;; Using Cons-Cells as building blocks would be kind of pointless if we couldn't 
;; get their components back out. To get the value of the first slot in a Cons-Cell, we use the car function.

* (cons 'a 'b)
(A . B)

* (car (cons 'a 'b))
A
;; Similarly, we can get the value from the second slot in a Cons-Cell using cdr.

* (cons 1 2)
(1 . 2)

* (cdr (cons 1 2))
2

;; cons, car and cdr are purely functional. Which means they never mutate their arguments.

* (defvar *a* (cons 1 2))
*A*

* *a*
(1 . 2)

* (cdr *a*)
2

* *a*
(1 . 2)

* (cons 3 (cdr *a*))
(3 . 2)

* *a*
(1 . 2)
;; It is an error to use car and cdr on something other than a Cons-Cell.

* (car 1)
; Evaluation aborted on #<TYPE-ERROR expected-type: LIST datum: 1>

* (cdr 'a)
; Evaluation aborted on #<TYPE-ERROR expected-type: LIST datum: A>.
;; This includes other compound values such as strings and vectors

* (car "a")
; Evaluation aborted on #<TYPE-ERROR expected-type: LIST datum: "a">.

* (cdr #(1 2))
; Evaluation aborted on #<TYPE-ERROR expected-type: LIST datum: #<(SIMPLE-VECTOR 2) {1007D4C76F}>>.
;; but not the empty list, also represented as NIL

* (car nil)
NIL

* (cdr nil)
NIL

* (car ())
NIL

* (cdr ())
NIL

;; A list is either the empty list, or a chain of Cons-Cells ending with the empty list.

* (listp nil)
T

* (listp (cons 5 nil))
T
;; If you cons something onto the empty list, you get the list of that thing.

* (cons 5 nil)
(5)
;; We can exploit the Cons-Cells' ability to contain heterogenous data in order to represent linked lists or trees.

* (cons 3 (cons 2 (cons 1 nil)))
(3 2 1)

;; Another way to create lists is using the list function.

* (list 3 2 1)
(3 2 1)
;; The expression (list a b ...) is effectively shorthand for the expression (cons a (cons b ...)), 
;; with the final value being consed onto NIL.

* (list 1 2 3)
(1 2 3)

* (cons 1 (cons 2 (cons 3 nil)))
(1 2 3)

* (equal (list 1 2 3) (cons 1 (cons 2 (cons 3 nil))))
T
;; As with cons, it's possible to build up trees, rather than merely lists, using list.

* (list 1 (list 2 3) (list 4 (list (list 5) 6 7 8)))
(1 (2 3) (4 ((5) 6 7 8)))

;; Because car and cdr are purely functional, and return their target value, it's possible 
;; to chain them in order to look into nested structures.

* (cons (cons 1 2) 3)
((1 . 2) . 3)

* (car (car (cons (cons 1 2) 3)))
1
;; This also applies to deeply nested lists.

* (defparameter *tree* (list 1 (list 2 3) (list 4 (list (list 5) 6 7 8))))
*tree*

* *tree*
(1 (2 3) (4 ((5) 6 7 8)))

* (car (cdr (car (cdr *tree*))))
3
;; This is common enough that Lisp supports shorthand for such tree selections.

* (car *tree*)
1

* (cadr *tree*)

(2 3)

* (cadadr *tree*)
3

* (cddr *tree*)
((4 ((5) 6 7 8)))

* (cdddr *tree*)
;; NIL
;; They're actual functions though. Not a reader syntax based on the number of as and d s 
;; between the c and r. So, for instance:

* (caddadddaaar *tree*)
;;    The function COMMON-LISP-USER::CADDADDDAAAR is undefined.
;;       [Condition of type UNDEFINED-FUNCTION]

;; We mentioned before that cons, car, cdr and friends are purely functional. But, sometimes, 
;; you want to destructively modify a list you've defined. For instance, in order to implement
;;  a mutable stack. In order to destructively cons elements onto a list, use push.

* (defvar *stack* nil)
*stack*

* (push 1 *stack*)
(1)

* *stack*
(1)

* (push 2 *stack*)
(2 1)

* (push 3 *stack*)
(3 2 1)

* *stack*
(3 2 1)

;; The other half of the a stack involves destructively removing the first element from it. 
;; This can be done with pop.

* *stack*
(3 2 1)

* (pop *stack*)
3

* *stack*
(2 1)

* (pop *stack*)
2

* (pop *stack*)
1

* *stack*
NIL
;; Calling pop on an empty list has no effect.

* *stack*
NIL

* (pop *stack*)
NIL

* *stack*
NIL

;; Like cons, push isn't limited to the existing type of its target.

* *stack*
NIL

* (push 1 *stack*)
(1)

* (push "b" *stack*)
("b" 1)

* (push 'c *stack*)
(c "b" 1)

* (push (list 4 5) *stack*)
((4 5) C "a" 1)

* *stack*
((4 5) C "a" 1)

;; In addition to car and cdr, it's also possible to manipulate Cons-Cells using the first and rest functions. 
;; They're just different names for the same functions. first is the same as car

* (cons 'a 'b)
(A . B)

* (car (cons 'a 'b))
A

* (first (cons 'a 'b))
A
and rest is the same as cdr

* (cons 1 2)
(1 . 2)

* (cdr (cons 1 2))
2

* (rest (cons 1 2))
2
;; A third function, last, lets you get at the last Cons-Cell in a particular series.

* (last (cons 1 (cons 2 (cons 3 nil))))
(3)

* (last (cons 1 2))
(1 . 2)

* (last (list 3 4 5))
(5)

;; When dealing with linked lists, if you want to get at a particular element somewhere in the middle, 
;; you could either chain some cars and cdrs.

* (car (cdr (cdr (cdr (list 0 1 2 3 4 5)))))
3

* (cadddr (list 0 1 2 3 4 5))
3
;; or you could use the nth function.

* (nth 3 (list 0 1 2 3 4 5))
3

* (nth 4 (list 0 1 2 3 4 5))
4

* (nth 5 (list 5 4 3 2 1 0))
0
;; This isn't any more efficient (in the run-time sense) than cdr traversal, but is shorter to 
;; write if you need to access some deeper list element in a flat list.

;; Putting lists together is the job of the append function.

* (append (list 1 2 3) (list 4 5 6))
(1 2 3 4 5 6)

* (append (list 6 5 4 3) (list 2 1))
(6 5 4 3 2 1)
;; append is an example of a function that takes a &rest argument. Meaning you can pass it any number of lists…

* (append (list 'a 'b 'c 'd) (list 'e 'f) (list 'g))
(A B C D E F G)

* (append (list 1) (list 2) (list 3) (list 4))
(1 2 3 4)
;; …though passing it one list is pointless.

* (append (list 1 2 3))
(1 2 3)

* (list 1 2 3)
(1 2 3)

;; Like car, cdr, first, rest, last and nth, append is functional. It will return a new list 
;; rather than mutating any of its arguments.

* (defvar *lst* (list 1 2 3 4 5))
*lst*

* *lst*
(1 2 3 4 5)

* (append *lst* (list 6 7 8))
(1 2 3 4 5 6 7 8)

* *lst*
(1 2 3 4 5)

* (append (list -3 -2 -1 0) *lst*)
(-3 -2 -1 0 1 2 3 4 5)

* *lst*
(1 2 3 4 5)

* (append (list 0) *lst* (list 6))
(0 1 2 3 4 5 6)

* *lst*
(1 2 3 4 5)
;; This means both that you may safely pass it any data you want appended without worrying 
;; about losing the original lists, and that if you want such behavior, you need to explicitly 
;; assign the result of append yourself.

;; The destructive equivalent of append is nconc. Using such side-effects, it's possible to create circular lists.

* (defparameter *cycle* (list 'a 'b))
*CYCLE*

* (first *cycle*)
A

* (second *cycle*)
B

* (third *cycle*)
NIL

* (fourth *cycle*)
NIL
;; Before we create an actual cycle, we need to tell the interpreter to print them 
;; (otherwise the request to print a circular list would never return; unlike Haskell, 
;; Common Lisp is not a lazy language by default).

* (setf *print-circle* t)
T

* (nconc *cycle* *cycle*)
#1=(A B . #1#)

* (third *cycle*)
A

* (fourth *cycle*)
B

* (loop repeat 15 for elem in *cycle* collect elem)
(A B A B A B A B A B A B A B A)

;; The nconc procedure is fine when all you want is a simple cycle, but it's also possible to use direct 
;; mutation to create more elaborate structures.

* (defparameter *knot* (list 1 2 3 4 (cons nil nil)))
*KNOT*

* (setf (car (nth 4 *knot*)) (cdr *knot*))
#1=(1 2 3 4 (#1#))

* (setf (cdr (nth 4 *knot*)) (cddr *knot*))
#1=(3 4 ((1 2 . #1#) . #1#))
Now we've got a structure that branches back on itself twice.

* (defun cycle-walk (count cycle &key (turn #'car))
    (loop with place = cycle 
          repeat count for elem = (car place) 
          unless (consp elem) do (format t "~a " elem)
          do (setf place (if (consp elem)
                             (funcall turn elem)
                             (cdr place)))))
CYCLE-WALK

* (cycle-walk 25 *knot* :turn #'car)
1 2 3 4 2 3 4 2 3 4 2 3 4 2 3 4 2 3 4 
NIL

* (cycle-walk 25 *knot* :turn #'cdr)
1 2 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 
NIL

* (let ((dir)) 
    (defun switch (pair)
      (setf dir (not dir))
      (if dir 
          (car pair)
          (cdr pair))))
SWITCH

* (cycle-walk 25 *knot* :turn #'switch)
1 2 3 4 3 4 2 3 4 3 4 2 3 4 3 4 2 3 4 
NIL
;; Of course, it's possible to go further. Large, divergent “trees” that eventually cycle 
;; backwards from any number of branches at arbitrary depths. You'd build them the same way though; 
;; using some combination of nconc, setf along with car/cdr and friends.

;; Another way to construct tree structure is using the quote or '.

* (quote (1 2 3))
(1 2 3)

* '(1 2 3)
(1 2 3)

* (list 1 2 3)
(1 2 3)
;; The structures you create this way are equivalent.

* (equal (quote (1 2 3)) '(1 2 3))
T

* (equal '(1 2 3) (list 1 2 3))
T
;; The difference is that, while list essentially means “Return the list of these arguments”, 
;; quote/' means “Return this argument without evaluating it”.

* (defparameter *test* 2)
*test*

* (list 1 *test* 3)
(1 2 3)

* '(1 *test* 3)
(1 *test* 3)

* (list (+ 3 4) (+ 5 6) (+ 7 8))
(7 11 15)

* '((+ 3 4) (+ 5 6) (+ 7 8))
((+ 3 4) (+ 5 6) (+ 7 8))

;; Because quote suppresses evaluation, you can use it to more easily build deeply nested structures.

* (list 1 (list 2 3) (list 4 (list (list 5) 6 7 8)))
(1 (2 3) (4 ((5) 6 7 8)))

* '(1 (2 3) (4 ((5) 6 7 8)))
(1 (2 3) (4 ((5) 6 7 8)))
;; Take care not to use quoted data for mutation though. While the structures produced may be the same, 
;; mutating a quoted structure is undefined by the Common Lisp language spec, 
;; and is thus entirely implementation dependant.

* (defvar *listed* (list 3 2 1))
*listed*

* (defvar *quoted* '(3 2 1))
*quoted*

* (push 4 *listed*)
(4 3 2 1)

* *listed*
(4 3 2 1)

* (push 4 *quoted*)
???

* *quoted*
???
;; The question marks aren't there so you can figure out what the results are 
;; supposed to be. They signify that what you get back in these situations depends 
;; on which implementation of Common Lisp you're using. They may do incompatible things, 
;; but because the spec leaves this situation undefined, none of them are actually wrong. 
;; So, you know … careful.