(declaim (optimize (debug 3)))
;;; experiment with encoding the rules

;; (defvar ddd3 '(cond
;;                (kpw-p
;;                 (next-char
;;                  (cond
;;                    (space-p
;;                     (next-char
;;                      (cond
;;                        (space-p
;;                         (next-char
;;                          (cond
;;                            (space-p
;;                             (end-of-sentence))
;;                            (upcase-p
;;                             (end-of-sentence))
;;                            (else
;;                             (skip)))))
;;                        (upcase-p
;;                         (end-of-sentence))
;;                        (else
;;                         (skip)))))
;;                    (upcase-p
;;                     (end-of-sentence))
;;                    (else
;;                     (skip)))))
;;                (else
;;                 (skip))))

(defun else ()
  T)

(defun qudoex-p (char)
  (some (lambda (x) (eq x char))
        '(#\? #\! #\.)))

(defun space-p (char)
  (some (lambda (x) (eq x char))
        '(#\Space #\Newline #\Tab)))

(defun upcase-p (char)
  (search "CAPITAL_LETTER" (char-name char)))

(defun find-sentence-end (string start)
  "Find sentence end in a STRING starting at START."
  (let ((cur (aref string start)))
    (macrolet
        ((skip (char)
           `(find-sentence-end string (1+ ,char)))
         (next-char (n body)
           `(let ((cur (aref string (+ start ,n))))
              ,body))
         (end-of-sentence ()
           `(+ start 1)))
      (cond
        ((>= start (1- (length string)))
         (end-of-sentence))
        ((qudoex-p cur)
         (next-char 1
                    (cond
                      ((space-p cur)
                       (next-char 2
                                  (cond
                                    ((space-p cur)
                                     (next-char 3
                                                (cond
                                                  ((space-p cur)
                                                   (end-of-sentence ))
                                                  ((upcase-p cur)
                                                   (end-of-sentence ))
                                                  ((else)
                                                   (skip start)))))
                                    ((upcase-p cur)
                                     (end-of-sentence))
                                    ((else)
                                     (skip start)))))
                      ((upcase-p cur)
                       (end-of-sentence ))
                      ((else)
                       (skip start)))))
        ((else)
         (skip start))))))

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

  ;; beginning of sentences except the last one
  ;; (loop for x = 0 then (find-sentence-end *words* x) while (< x (length *words*))  collect x)
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
    (defparameter *words* (concatenate 'string (car d) ""))
    (solve-me *words*)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
