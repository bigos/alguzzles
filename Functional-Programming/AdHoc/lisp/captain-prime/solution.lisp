(defun prime-p (n)
  ;;(format t "~&prime-p ~A~%" n)
  (let ((x (parse-integer n)))
    (not
     (loop for d from 2 to (sqrt x)
        for m = (mod x d)
        when (zerop m)
        collect d
        until (zerop m)))))

;; (successive-left "1367" 4 T )
(defun successive-left (n ln )
  ;;(format t "~&left ~A ~A ~%" n ln )
  (let ((ss (subseq n 1)))
    (let ((p (prime-p ss)))
      (if (not p)
          nil
          (if (= ln 2)
              p
              (successive-left ss (1- ln)))))))

;; (successive-right "2333" 4 T )
(defun successive-right (n ln )
  ;;(format t "~&right ~A ~A~%" n ln )
  (let ((ss (subseq n 0 (1- ln))))
    (let ((p (prime-p ss)))
      (if (not p)
          nil
          (if (= ln 2)
              p
              (successive-right ss (1- ln)))))))



(defun contains-zero-p (n)
  (position #\0 n))

(defun central-p (n ln)
  ;;(format t "~%~%~&trying central ~A ~A~%" n ln)
  (and
   (successive-left n ln)
   (successive-right n ln)))

(defun left-p (n ln)
  ;; (format t "~%~%~&trying left~%")
  (and
   (successive-left n ln)
   (not (successive-right n ln))))

(defun right-p (n ln)
  ;; (format t "~%~%~&trying right~%")
  (and
   (not (successive-left n ln))
   (successive-right n ln)))

(defun solve-me (n)
  ;; (format t "~A~%" n)
  (let ((ln (length n)))
    (princ (if (and (not (contains-zero-p n))
                    (prime-p n))
               (if  (central-p n ln)
                    "CENTRAL"
                    (if (left-p n ln)
                        "LEFT"
                        (if (right-p n ln)
                            "RIGHT"
                            "DEAD")))
               "DEAD"))
    (terpri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let* ((persons (parse-integer (read-line stream))))
    (loop for x from 1 to persons do
         (solve-me (read-line stream)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input1" :type "txt"))
    (solution s)))

(main)
