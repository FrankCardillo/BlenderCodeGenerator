;; Working with Binary Streams in general is very similar to working with file streams. 
;; You open an input, output, or bi-directional file stream, and you simply specify that 
;; you're working with bytes. In the last exercise, we didn't specify a type for input and output, 
;; and thus used the default character stream for files.

(with-open-file (b "~/binary-monkey.txt" :direction :output :element-type 'unsigned-byte :if-exists :supersede)
  (write-byte 109 b)
  (write-byte 111 b)
  (write-byte 110 b)
  (write-byte 107 b)
  (write-byte 101 b)
  (write-byte 121 b))

;; Now go to the file ~/binary-monkey.txt on your computer, and open it in a text editor. 
;; What do you see? Is it what you expected or a complete surprise?

;; If all went well, you should have a file that just contains the word “monkey”. 
;; Remember, everything in a computer is really stored and run in binary—everything else is 
;; just a representation for our benefit, to make working with computers easier and friendlier. 
;; But it helps to remember this point. You won't normally want to read and write to text files in binary, 
;; but for many other file formats and network protocols, this is the best way to work with them.

;; When you set the element-type of the file stream to unsigned-byte, that allowed you to use write-byte 
;; on the stream. Lisp also lets you represent bytes as integers. So when you wrote those numbers to 
;; the text file, you were really writing the character bytes to that text file that spelled out the 
;; word “monkey”. You could also have used the hexadecimal, octal, or binary representation of the 
;; integers, as I showed you in exercise 1.2.8.