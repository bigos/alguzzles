(defun expotential-divisors (n expot)
  (reverse (loop
              for y from 1 upto n
              for x = 1 then (* x expot )
              collect x)))

(defun expot-len (n)
  (1- (expt 2 n)))

(defun calc-base (n divs a b)
  ;; (format t "~&divs: ~A~%" divs)
  ;; (2048 1024 512 256 128 64 32 16 8 4 2 1)
  (loop for d in divs
     sum (if (zerop (floor n d))
                     a
                     b)
     do (if (>= n d) (setf n (rem n d)))))

(defun puzzle (n a b)
  (let* ((divs (expotential-divisors n 2))
         (res) (found))
    (loop for x from 0 to (expot-len n)
       do (progn
            (setf res (calc-base x divs a b))
            (unless (search (list res) found)
              (push res found)
              (format t "~a " res ))))))

(defun find-values (n a b)
  (puzzle (1- n) a b)
  (terpri))

(defun solution (&optional stream)
  (let* ((tests (parse-integer (read-line stream)))
         (data (loop repeat tests
                  collect (list (parse-integer (read-line stream))
                                (parse-integer (read-line stream))
                                (parse-integer (read-line stream))))))
    (loop for dataset in data
       do (find-values (nth 0 dataset)
                       (nth 1 dataset)
                       (nth 2 dataset)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "manasa-and-stones.input.3.txt"))
      (solution s))))

(repl-main)
(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 10
                                       :report :flat
                                       :loop nil))
(sb-sprof:start-profiling)
(puzzle 22 900 23)
(sb-sprof:report)
