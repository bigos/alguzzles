;; this generates correct data
;; (loop for a from 0 to 4 do
;;      (loop for b from (1+ a) to 4 do
;;           (loop for c from (1+ b) to 4 do
;;                (format t " ~A~%" (list a b c)))))

(defmacro qqq (n &optional (level 0))
  (if `(< ,level ,n)
      `(loop for a from ,1 to 4 do (qqq ,n ,(+ 1 level)))
      `(format t "~A~%" (list ,@(loop for x from 0 below n collect (intern (format nil "A~A" x)))))))

(qqq 3)

(defun aaa ()
  (let ((a0 -1))
    (loop for a1 from (1+ a0) to 4 do
         (loop for a2 from (1+ a1) to 4 do
              (loop for a3 from (1+ a2) to 4 do
                   (format t "~a~%" (list a1 a2 a3))
                   )))))

(defun ppp (ar &optional (level 0))
  (let ((maxval (1- (length ar))))
    (loop repeat maxval do
         (if (< level maxval)
             (setf ar (ppp ar (+ level 1)))
             (format t "~&the end ~A ~A~%" ar level))
         (incf (elt ar level))
         (when (>= (elt ar level) maxval)
           (setf (elt ar level) 0))))
  ar)

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
