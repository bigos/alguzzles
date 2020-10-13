;;; solution for: https://www.hackerrank.com/challenges/almost-sorted/problem

;;; possibilities ok swap sort no

(defun sortone (n d &optional prevdir prevzz)
  (declare (optimize (debug 3)))

  (if (null d)
      (progn
        (format t "finished"))
      (progn

        (let ((a (car d))
              (b (cadr d)))
          (let* ((dir (and a b  (if (< a b) 'up 'dn)))
                 (zzz (cond
                        ;; here I can work on the logic to solve the puzzle
                        ((and (eq dir 'up) (null prevdir)) (list n a 'u))
                        ((and (eq dir 'dn) (null prevdir)) (list n a 'd))
                        ((and (eq dir 'up) (eq prevdir 'up)) prevzz)
                        ((and (eq dir 'up) (eq prevdir 'dn)) (cons (list n a dir) prevzz))
                        ((and (eq dir 'dn) (eq prevdir 'dn)) prevzz)
                        ((and (eq dir 'dn) (eq prevdir 'up)) (cons (list n a dir) prevzz))
                        ;; ((and (null dir) (eq prevdir 'up)) prevzz)
                        ;; ((and (null dir) (eq prevdir 'dn)) prevzz)
                        (T (list 'finish prevzz)))))
            (format t "~A~%" (list n d dir prevdir zzz))
            (if (eq 'finish (car zzz))
                (let* ((res (cadr zzz))
                       (r1 (nth 0 res))
                       (r2 (nth 1 res))
                       (r3 (nth 2 res)))
                  (cond
                    ((eq r3 'u)
                     (format t ">>> OK~%"))
                    ((eq r3 'd)
                     (format t ">>> reverse ~A ~A~%" 1 n))
                    (t
                     (format t "==== ~A~%" (cadr zzz))))))

            (sortone (1+ n) (cdr d) dir zzz))))))

(defun solve-me (d)
  (format t " ~A -------------~%" d)
  (sortone 1 d ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (let ((ignored (parse-integer (read-line stream)))
        (d (split-and-parse (read-line stream))))
    (declare (ignore ignored))
    (solve-me d)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
