;;

(defun main (&optional stream)
  (format t "~a~%" (read-line stream)))

;; (main) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if (search "chess" (machine-instance))
                  "/Users/jacekpodkanski/Documents/hackerrank/"
                  "/home/jacek/Programming/HackerRank/")))
    (with-open-file (s (concatenate 'string path "service-lane.input1.txt"))
      (main s))))

(repl-main)
