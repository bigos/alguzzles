;; this generates correct data
;; (loop for a0 from 0 to 4 do
;;      (loop for a1 from (1+ a0) to 4 do
;;           (loop for a2 from (1+ a1) to 4 do
;;                (format t " ~A~%" (list a0 a1 a2)))))

(defparameter zzz '(0 1 2))

(defun genvarnum (num)
  (intern (format nil "A~A" num)))

(defmacro qqq (maxv var &optional (level 0))
  (if (< level maxv)
      `(loop for ,(genvarnum (1+ level))
          from (1+ ,(genvarnum level))
          to 4 do ,`(qqq ,maxv ,var ,(1+ level)))
      ))

(qqq 3 '(0 1 2))



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
