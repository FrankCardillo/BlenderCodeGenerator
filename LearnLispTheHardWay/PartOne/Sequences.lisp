;; Sequences

;; If you have an existing buffer you want to destructively read into, you can use read-sequence.

(let ((dest (make-list 5)))
    (read-sequence dest *standard-input*)
    dest)
abcde
;; (#\a #\b #\c #\d #\e)
;; This procedure won't read more input than can fit into the buffer you specify.

(let ((dest (make-list 3)))
    (read-sequence dest *standard-input*)
    dest)
abcdefghi
;; (#\a #\b #\c)

(let ((dest (list)))
    (read-sequence dest *standard-input*)
    dest)
NIL
;; Note that it doesn't even wait for input when given a zero-length buffer. 
;; It will also keep reading until the buffer is filled…

(defparameter *buf(make-list 7))
*BUF*

(read-sequence *buf*standard-input*)
one
two
7

*buf*
(#\o #\n #\e #\Newline #\t #\w #\o)
;; or until some sort of break occurs.

(read-sequence *buf*standard-input*)
abc
User Interrupt
   [Condition of type SIMPLE-ERROR]

*buf*
(#\a #\b #\c #\Newline #\t #\w #\o)
;; As you can see from the above examples, the return value of read-sequence is 
;; the number of stream elements it consumed, and it really is a destructive update on the specified buffer.

;; The buffer can be of any sequence type, rather than just a list.

(let ((dest (make-string 5)))
    (read-sequence dest *standard-input*)
    dest)
abcdef
"abcde"

(let ((dest (make-array '(5))))
    (read-sequence dest *standard-input*)
    dest)
abcdef
#(#\a #\b #\c #\d #\e)