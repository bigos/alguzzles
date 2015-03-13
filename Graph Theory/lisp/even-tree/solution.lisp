(require :sb-sprof)

(declaim (optimize speed))

;;; insert your code here

(defun solution (&optional stream)
  (format t "going to solve"))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Graph Theory/lisp/even-tree/"
                                    "input0.txt"))
      (solution s))))

;; using profiler
;; (sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop nil))
;; (sb-sprof:start-profiling)
;; (repl-main)
;; (sb-sprof:report)
