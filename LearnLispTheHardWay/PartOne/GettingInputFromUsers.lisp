;; The read procedure can read from a stream

(read)

;; Nothing actually happens until you type something else.

(read)
test ;; this is the thing I type when read blocks
;; TEST  output

(read)
12345
;; 12345 output

(read)
"A string"
;; "A string" output

;; Since read invokes the Lisp reader, any comments you type in its input are ignored. 
;; It reads an s-expression, not a line. So you can do things like

(read)
"This is a
multiline string"
;; "This is a
;; multiline string" output

(read)
(list 1 2
      3 4
      5 6)
;; (LIST 1 2 3 4 5 6) output

;; As you can see by that call to list, an expression read this way is not evaluated. 
;; Which is why you can still see the symbol list at the front of the List read returns.

(read)
(+ 2 3)
;; (+ 2 3) output

(read)
(defun foo ()
  (+ a b))
;; (DEFUN FOO () (+ A B)) output

(read)
(loop (format t "Fun!~%"))
;; (LOOP (FORMAT T "Fun!~%")) output


;; ================================================

;; In order to evaluate something you read, you need to use eval. Some forms are self-evaluating 
;; (that is, they return themselves when evaluated).

(eval (read)) ;; NEVER DO THIS IN A REAL PROGRAM
12345
;; 12345 output

(eval (read))
"A string"
;; "A string" output

;; Some don't.

(eval (read))
test
;;   The variable TEST is unbound.
;;      [Condition of type UNBOUND-VARIABLE] output

(eval (read))
(+ 2 3)
;; 5 output

(eval (read))
(defun foo ()
  (+ a b))

; in: DEFUN FOO
;     (+ A B)
; 
; caught WARNING:
;   undefined variable: A
; 
; caught WARNING:
;   undefined variable: B
; 
; compilation unit finished
;   Undefined variables:
;     A B
;   caught 2 WARNING conditions
;; FOO output

;; We're not going to try to call that foo we just defined because, as you can see by the 
;; compilation warnings, we don't have values for a and b anywhere

;; Hopefully you noticed the NEVER DO THIS... comment up top. It's because of things like

(eval (read))
(loop (format t "Fun!~%"))
;; Fun!
;; Fun!
;; Fun!
;; Fun!
;; Fun!
;; Fun!
;; Fun!
;; Fun!
;; Fun!
;; Fun!
;; Fun! output
... ;; FOREVER (until you kill the process with an interrupt)
