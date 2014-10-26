;; this generates correct data
;; (loop for a0 from 0 to 4 do
;;      (loop for a1 from (1+ a0) to 4 do
;;           (loop for a2 from (1+ a1) to 4 do
;;                (format t " ~A~%" (list a0 a1 a2)))))

(defun generate-variable (num)
  (intern (format nil "A~A" num)))

(defun previous-variable (num)
  (if (zerop num)
      -1
      (intern (format nil "A~A" (1- num)))))

(defun results (num)
  (loop for x from 0 below num collect (generate-variable x)))

(defmacro qqq (maxv &optional (level 0))
  (if (< level maxv)
      `(loop for ,(generate-variable level)
          from (1+ ,(previous-variable level))
          to ,maxv do ,`(qqq ,maxv ,(1+ level)))
      `(format t "finished ~a ~%" (list ,@(results level)))))

(defun defvars (maxv)
  (concatenate 'list
               '((A0 0))
               (loop for x from 1 to maxv
                  collect (list (intern (format nil "A~A" x))))))

;; -------- above is unfinished recursive macro solution -----------------------

(defun solution (&optional stream)
  (let* ((n (parse-integer (read-line stream)))
         (k (parse-integer (read-line stream)))
         (data (make-array (list n)
                           :initial-contents
                           (loop repeat n
                              collect  (parse-integer (read-line stream))))))
    (setf data (sort data #'<))
    (format T "~a~%"
            (loop for i below (- n k)
               minimize  (- (aref data (+ (1- k) i)) (aref data i))))))

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
