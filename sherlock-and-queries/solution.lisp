(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun puzzle (m n a b c)
  (let ((big-no (+ (expt 10 9) 7))
        (factors (make-array (list m) :initial-value 1)))
    (loop for i from 0 below m do
         (loop for j from 0 below n do
              (progn
                (when (zerop (mod (1+ j) (aref b i)))
                  (setf (aref a j) (* (aref a j)
                                      (aref c i))))
                (when (= i (1- m))
                  (format t "~A " (mod (aref a j) big-no))
                                        ; (format t "~a ~a ~A   " (aref a j) big-no (mod (aref a j) big-no))
                  ))
              ))))

(defun solution (&optional stream)
  (let* ((first-line (split-and-parse (read-line stream)))
         (n (car first-line))
         (m (cadr first-line))
         (a (make-array (list n) :initial-contents (split-and-parse (read-line stream))))
         (b (make-array (list m) :initial-contents (split-and-parse (read-line stream))))
         (c (make-array (list m):initial-contents (split-and-parse (read-line stream)))))
    (puzzle m n a b c)
    ))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "sherlock-and-queries"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input.1.txt"))
      (solution s))
    (format t "~&=========================~%")
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input00.txt"))
      (solution s))
    (format t "~&=========================~%")
    ;; (with-open-file (s (concatenate 'string
    ;;                                 (directory-namestring (user-homedir-pathname))
    ;;                                 path
    ;;                                 puzzle "/" "input13.txt"))
    ;;   (solution s))
    ))

(repl-main)
