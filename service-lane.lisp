;;

(defun main (&optional stream)
  (format t "~a~%" (read-line stream)))

;; (main) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (with-open-file (s "/home/jacek/Programming/HackerRank/service-lane.input1.txt")
    (main s)))

(repl-main)
