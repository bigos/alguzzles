;;; experiment with encoding the rules
(defvar ddd '(kpw-p
              (T
               (next-char
                (space-p
                 (next-char
                  (space-p
                   (next-char ...))
                  (up-case-p
                   (t
                    (end-of-sentence))
                   (nil
                    (skip)))))
                (up-case-p
                 (t
                  (end-of-sentence))
                 (nil
                  (skip)))
                (digit-p
                 (skip))))
              (nil
               (skip))))

(defvar ddd2 '(kpw-p
               (T
                (next-char
                 (space-p
                  (next-char
                   (space-p
                    (next-char
                     (space-p
                      (nextichar ..............))
                     (upcase-p
                      (end-of-sentence))
                     (else
                      (skip))))
                   (upcase-p
                    (end-of-sentence))
                   (else
                    skip)))
                 (upcase-p
                  (t
                   (end-of-sentence))
                  (else
                   (skip)))
                 (else
                  (skip))))
               (else
                (skip))))

(defun first-words ()
  "Beginning of experimenting with natural language parsing."
  (loop for c across (subseq *words* 0 25)
        do (format T "~A~%" c)))

;;; think of rule 7 'quotations within quotations'
;;; also rule 9 can be tricky
(defparameter my-data '(data
                        (sentence-ends ".?!")
                        (quote-ends (list
                                     (#\. #\' #\") ; rule 7
                                     (#\? #\")
                                     (#\! #\")
                                     (#\, #\")
                                     (#\. #\")))
                        (brackets-ends ")]}")))

(defun solve-me (d)
  (format t "~s ~a~%" d (length d))
  ;; find a word starting with a capital letter, could be quoted
  ;; put start mark there
  ;; find an end of the sentence .!? and put end mark there
  ;; problem of quoted sentences
  ;; quoted sentence can have >?" said<
  ;; some space separated words are chunks of words joined by
  ;; punctuation "force?No!" which should be ("force" ? "No" !)
  ;; natural language tokenization is an interesting problem

  ;; https://en.wikipedia.org/wiki/Augmented_transition_network
  ;; file:///home/jacek/Documents/Manuals/Lisp/onlisp.html#SEC141

  ;; punctuation and quotation rules http://www.grammarbook.com/punctuation/quotes.asp
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun read-all-data (stream)
  (loop for line = (read-line stream nil 'eof)
     until (equal line 'eof)
     collect line))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((d (read-all-data stream)))
    (defparameter *words* (car d))
    (solve-me d)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
