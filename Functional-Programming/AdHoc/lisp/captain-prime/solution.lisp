(defun prime-p (n)
  ;;(format t "~&prime-p ~A~%" n)
  (let ((x (parse-integer n)))
    (or (eq 1 x)
        (not
         (loop for d from 2 below (sqrt x)
            for m = (mod x d)
            when (zerop m)
            collect d
            until (zerop m))))))

;; (successive-left "1367" 4 T )
(defun successive-left (n ln r)
  (format t "~&left ~A ~A ~A~%" n ln r)
  (let ((ss (subseq n 1)))
    (let ((p (prime-p ss)))
      (format t "~A ~A ~a~%" p ss (eq p r))
      (if (not (eq p r))
          nil
          (if (= ln 2)
              (eq p r)
              (successive-left ss (1- ln) r))))))

;; (successive-right "2333" 4 T )
(defun successive-right (n ln r)
  (format t "~&right ~A ~A ~A~%" n ln r)
  (let ((ss (subseq n 0 (1- ln))))
    (let ((p (prime-p ss)))
      (format t "~A ~A ~a~%" p ss (eq p r))
      (if (not (eq p r))
          nil
          (if (= ln 2)
              (eq p r)
              (successive-right ss (1- ln) r))))))

(defun contains-zero-p (n)
  (position #\0 n))

(defun central-p (n ln)
  (format t "~%~%~&trying central~%")
  (and
   (successive-left n ln T)
   (successive-right n ln T)))

(defun left-p (n ln)
  (format t "~%~%~&trying left~%")
  (and
   (successive-left n ln T)
   (successive-right n ln NIL)))

(defun right-p (n ln)
  (format t "~%~%~&trying right~%")
  (and
   (successive-left n ln NIL)
   (successive-right n ln T)))

(defun solve-me (n)
  ;; (format t "~A~%" n)
  (let ((ln (length n)))
    (princ (if (and (not (contains-zero-p n))
                    (prime-p n))
               (if  (central-p n ln)
                    "CENTRAL"
                    (if (left-p n ln)
                        "LEFT"
                        (if (right-p n ln) "RIGHT")))
               "DEAD"))
    (format t "--------------------")))

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
                      :name "input0" :type "txt"))
    (solution s)))

(main)
