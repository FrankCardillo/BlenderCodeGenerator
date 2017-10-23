(format nil "~~")
;; "~"
CL-USER> (format nil "H~CO" #\subscript_two)
;; "H₂O"
CL-USER> (format nil "~R" 10000)
;; "ten thousand"
CL-USER> (format nil "~X" 10000)
;; "2710"
CL-USER> (format nil "~D" 10000)
;; "10000"
CL-USER> (format nil "~O" 10000)
;; "23420"
CL-USER> (format nil "~B" 10000)
;; "10011100010000"

;; Much like with the backslash character in a regular string, you can include a tilde character 
;; in a format string with the tilde-tilde control sequence, ~~. It doesn't consume any arguments.

;; You can consume a character object argument using ~C.

;; You can insert an integer into a format string with ~R, but since it can be a bit overkill 
;; with its flexibility, there are also simplified control sequences for printing the integer to 
;; hexadecimal, decimal, octet, and binary, like we did with write earlier: ~X, ~D, ~O, and ~B, 
;; respectively. They all consume an argument, which must be an integer.

;; You can force the insertion of a #\Newline character with ~%. It doesn't consume any arguments.

;; And you can insert a #\Newline only if the output stream is not already at the beginning of a 
;; line with ~&. It doesn't consume any arguments either.