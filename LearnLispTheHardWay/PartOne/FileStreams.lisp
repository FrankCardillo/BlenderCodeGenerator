;; While similar in principle to string streams, there's stuff you can do with file streams 
;; that you can't do with string streams.

;; Like with string streams, you can create file streams manually with open. 
;; But for now let's look at the simplest use-case, creating a file using the 
;; with-open-file macro and putting some text in it

(with-open-file (s "~/monkey.txt" :direction :output :if-does-not-exist :create :if-exists :supersede)
  (format s "I had a little monkey,~%Brought him to the country,~%Fed him on ginger-bread...~%"))

(with-open-file (s "~/monkey.txt" :direction :input)
  (format t "~&;;; ~A" (read-line s)))

(with-open-file (s "~/monkey.txt" :direction :input)
  (do ((line (read-line s) (read-line s nil 'eof)))
      ((eq line 'eof) "-- Marilyn Manson")
    (format t "~&;;; ~A~%" line)))

;; As I said above, you can create either an input or output file stream using the function open; 
;; but if you do it manually, you have to remember to close it as soon as you've finished writing 
;; to it or reading from it. The with-open-file macro opens and closes the file for you, and gives 
;; you a stream to work with in its body. It's much more convenient and better reflects the Lisp Way.