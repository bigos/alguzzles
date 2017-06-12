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
  (let ((d (split-by-one-space (car (read-all-data stream)))))
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
