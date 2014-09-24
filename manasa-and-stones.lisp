;;; this is going to be useful
(defun expotential-divisors (n expot)
  (declare (optimize (speed 3)))
  (declare (type fixnum n expot))
  (reverse (loop for x = 1 then (* x expot ) until (> x n) collect x)))

(defun calc-base (n expot)
  (declare (optimize (speed 3)))
  (declare (type fixnum base expot))
  (let ((divs (expotential-divisors n expot)))
    (loop for d in divs
       collect (floor (/ n d))
       do (if (>= n d) (setf n (rem n d))))))

(defun pad-min-len (core len)
  (declare (optimize (speed 3)))
  (declare (type fixnum len)
      )
  (let* ((cl (length core))
         (padl (if (< cl len)
                   (- len cl)
                   0)))
    (concatenate 'list
                 (loop repeat padl collect 0)
                 core)))
;;; fixed but slow
;;; substitute might be a problem
(defun puzzle (a b l)
  (declare (optimize (speed 3)))
  (declare (type fixnum a b l))
  (let* ((term (expt 2 l)))
    (remove-duplicates
     (loop for x from 0 to (- term 1)
        collect (apply '+
                       (substitute a
                                   0
                                   (substitute b
                                               1
                                               (pad-min-len (calc-base x 2)
                                                            l))))))))

(defun find-vals (n a b)
  (let ((res (puzzle a b (1- n))))
    (loop for x in res
       do (format t "~a " x))
    (terpri)))

(defun solution (&optional stream)
  (let* ((tests (parse-integer (read-line stream)))
         (data (loop repeat tests
                  collect (list (parse-integer (read-line stream))
                                (parse-integer (read-line stream))
                                (parse-integer (read-line stream))))))
    (loop for dataset in data
       do (find-vals (nth 0 dataset)
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
                                    "manasa-and-stones.input.1.txt"))
      (solution s))
    ;; (with-open-file (s (concatenate 'string
    ;;                                 (directory-namestring (user-homedir-pathname))
    ;;                                 path
    ;;                                 "manasa-and-stones.input.2.txt"))
    ;;   (solution s))
    ))

(repl-main)

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 10
                                       :report :flat
                                       :loop nil))
(sb-sprof:start-profiling)
(puzzle 10 200 23)
(sb-sprof:report)
