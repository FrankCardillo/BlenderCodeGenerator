;; Lisp has two built-in functions for prompting users, y-or-n-p and yes-or-no-p—they are predicate functions, 
;; in the sense that all they do is return T or NIL. Unlike most other predicate functions you'll see, though, 
;; the argument you pass to them in code isn't what they're testing—it's a format string that goes to the user 
;; over *query-io*, and the test is applied to the user's input.

;; Both of these functions are similar in purpose, but y-or-n-p only expects the user to type one character on 
;; the keyboard. yes-or-no-p expects the user to type the whole word “yes” or “no”. In both cases, hit 
;; Return/Enter after to submit your response.

(y-or-n-p "Is your name Colin?")
;; Is your name Colin? (y or n) y
T
(yes-or-no-p "Are you sure?")
;; Are you sure? (yes or no) yes
T
(y-or-n-p "Are you a ~S?" 'monkey)
;; Are you a MONKEY? (y or n) n
NIL