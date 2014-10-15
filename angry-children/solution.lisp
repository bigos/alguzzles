;; this generates correct data
;; (loop for a from 0 to 4 do
;;      (loop for b from (1+ a) to 4 do
;;           (loop for c from (1+ b) to 4 do
;;                (progn
;;                  (when (apply #'/= (list a b c))
;;                    (format t "----- "))
;;                  (format t " ~A~%" (sort (list a b c) #'<))))))

;; (puzzle 7 3 #(1 2 3 4 5 6 7))
(defun puzzle (n k data ar &optional (level 0))
  (format t "~A ~A ~A ~A~%" n k  data level)
  (loop for x from (- n k level) below (- n level)
     do (progn
          (if (< level (1- k))
              (puzzle 5 3 data ar (1+ level)))
          (progn  (setf (aref ar level) x)
                  (format t ">>>>  ~A ~A ~%"  ar  level )))))

(defun solution (&optional stream)
  (let* ((n (parse-integer (read-line stream)))
         (k (parse-integer (read-line stream)))
         (data (make-array (list n)
                           :initial-contents
                           (loop repeat n
                              collect  (parse-integer (read-line stream)))))
         (ar (make-array (list k)
                         :initial-element 0)))
    (format nil "~a ~a ~a~%" n k data)
    (puzzle n k data ar)))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "angry-children"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input.1.txt"))
      (solution s))))

(repl-main)
