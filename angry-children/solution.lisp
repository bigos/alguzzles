;; this generates correct data
;; (loop for a0 from 0 to 4 do
;;      (loop for a1 from (1+ a0) to 4 do
;;           (loop for a2 from (1+ a1) to 4 do
;;                (format t " ~A~%" (list a0 a1 a2)))))




(defun genvarnum (num)
  (intern (format nil "A~A" num)))

(defun genprevvarnum (num)
(if (zerop num)
    0
    (intern (format nil "A~A" (1- num)))))

(defmacro qqq (maxv &optional (level 0))
  (if (< level maxv)
      `(loop for ,(genvarnum level)
          from (1+ ,(genprevvarnum level))
          to ,maxv do ,`(qqq ,maxv ,(1+ level)))
      `(format t "finished~%")
      ))

(qqq 2)

(defun defvars (maxv)
  (concatenate 'list
               '((A0 0))
               (loop for x from 1 to maxv
                  collect (list (intern (format nil "A~A" x))))))



;; (puzzle 7 3 #(1 2 3 4 5 6 7))
(defun puzzle (n k data ar &optional (level 0))
  (format t "~A ~A ~A ~A~%" n k  data level)
  (loop for x from (- n k level) below (- n level)
     do (progn
          (if (< level (1- k))
              (puzzle 5 3 data ar (1+ level)))
          (progn  (setf (aref ar level) x)
                  (format t ">>>>  ~A ~A ~%"  ar  level )))))

(defun solution (&optional streamba)
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
