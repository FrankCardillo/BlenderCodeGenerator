;; Lisp has a way to print Lisp code, formatted in such a way that it's more straightforward to read. 
;; By default, whenever possible the pretty-printer outputs the code all on one line; 
;; if there's no room left in the default margins, then it breaks the code up into multiple lines.

;; To see the pretty-printer in action, I'll be showing you some pretty ugly code. Normally you would 
;; never write Lisp this way—but it shows you how Lisp can take badly formatted code and give you something better.

(pprint '(defun monkey (a b c) "a monkey function" (let ((d 4) (e 5) (f 6)) (values (list a b c) (list d e f)))))

;; What You Should See

* (pprint '(defun monkey (a b c) "a monkey function" (let ((d 4) (e 5) (f 6)) (values (list a b c) (list d e f)))))
(DEFUN MONKEY (A B C)
  "a monkey function"
  (LET ((D 4) (E 5) (F 6))
    (VALUES (LIST A B C) (LIST D E F))))
NIL

;; Remember how I introduced “quoting” in the Common Lisp Bootcamp chapter? It turns Lisp code into data, 
;; so the function definition above is never evaluated by Lisp. It's read, but not eval'd. Otherwise, 
;; don't worry too much about the function definition for now. It's just illustrative, to show how the 
;; pretty printer splits up a long line into multiple lines.

;; Now, if you didn't quote the above function definition, it would be evaluated, and then you'd actually 
;; be pretty-printing the return value of the function instead of the code. Not very useful in this 
;; case—but say you wanted to pretty-print the result of a loop that collects a whole lot of data? It would be useful then.

;; Lisp's Pretty Printing facilities are pretty complex, and that's to say nothing of pretty-printing with format.