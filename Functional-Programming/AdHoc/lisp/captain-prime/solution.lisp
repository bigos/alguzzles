(defun prime-p (n)
  ;;(format t "~&prime-p ~A~%" n)
  (let ((x (parse-integer n)))
    (not
     (loop for d from 2 to (sqrt x)
        for m = (mod x d)
        when (zerop m)
        collect d
        until (zerop m)))))


(defun dirleft (n l)
         (subseq n 0 (1- l)))

(defun dirright (n l)
          (subseq n 1 l))

(defun choosedir (dir n l)
           (if (equalp dir 'left)
               (dirleft n l)
               (dirright n l)))

(defun successives (dir n l)
  (labels ((succ (n l r)
             ;; (format t "~A~&" n)
             (if (<= l 0)
                 r
                 (succ (choosedir dir n l)
                       (1- l)
                       (push (prime-p n) r)))))
    (succ (choosedir dir n l)
          (1- l)
          nil)))

(defun contains-zero-p (n)
  (position #\0 n))

(defun every-prime (l)
  (not (some #'null l)))

(defun central-p (n ln)
  ;;(format t "~%~%~&trying central ~A ~A~%" n ln)
  (and
   (every-prime (successives 'left n ln))
   (every-prime (successives 'right n ln))))

(defun left-p (n ln)
  ;; (format t "~%~%~&trying left~%")
  (and
   (not (every-prime (successives 'left n ln)))
   (every-prime (successives 'right n ln))))

(defun right-p (n ln)
  ;; (format t "~%~%~&trying right~%")
  (and
   (every-prime (successives 'left n ln))
   (not (every-prime (successives 'right n ln)))))

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
