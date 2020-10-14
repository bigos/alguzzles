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
                        ((and (eq dir 'up) (null prevdir)) (cons (list n a 'u) prevzz))
                        ((and (eq dir 'dn) (null prevdir)) (cons (list n a 'd) prevzz))
                        ((and (eq dir 'up) (eq prevdir 'up)) prevzz)
                        ((and (eq dir 'up) (eq prevdir 'dn)) (cons (list n a dir) prevzz))
                        ((and (eq dir 'dn) (eq prevdir 'dn)) prevzz)
                        ((and (eq dir 'dn) (eq prevdir 'up)) (cons (list n a dir) prevzz))
                        ;; ((and (null dir) (eq prevdir 'up)) prevzz)
                        ;; ((and (null dir) (eq prevdir 'dn)) prevzz)
                        (T (list 'finish prevzz)))))
            (format t "<<<<<<<<<<<< ~A~%" (list n d dir prevdir zzz))
            (if (eq 'finish (car zzz))
                (let* ((res (cadr zzz))
                       (r1 (nth 0 res))
                       (r2 (nth 1 res))
                       (r3 (nth 2 res)))
                  (cond
                    ((equalp res '((1 1 u)))
                     (format t ">>> OK~%"))

                    ((eq (length res) 1)
                     (destructuring-bind (nx ax dx) r1
                       (cond
                         ((and (eq nx 1)
                               (eq dx 'd))
                          (format t ">>> reverse ~A ~A~%" 1 ax))
                         (t
                          (format t "l1==== ~A~%" (cadr zzz))))))

                    ((eq (length res) 2)
                     (break "qqqqqqqqqq222222222222")
                     (cond
                       (t
                        (format t "l2==== ~A~%" (cadr zzz)))))

                    ((eq (length res) 3)
                     (destructuring-bind ((an aa ad) (bn ba bd) (cn ca cd)) res
                       (progn
                         (break "qqqqqqqqqq33333 ~A" (list an aa ad 'I bn ba bd 'I cn ca cd))
                         (cond
                           ((and (equalp r3 '(1 1 u))
                                 (eq bd 'dn)
                                 (eq ad 'up))
                            (format t "l3aaa==== ~A~%" (cadr zzz))
                            (format t ">>> reverse ~A ~A~%" aa ba))
                           (t
                            (format t "l3==== ~A~%" (cadr zzz)))))))
                    (t
                     (format t "==== ~A~%" (cadr zzz))))

                  (break  "finally ~A ~A ~A" r1 r2 r3)

                  ))

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
