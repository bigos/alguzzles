;;; solution for: https://www.hackerrank.com/challenges/almost-sorted/problem



(defun swap-reverse (a b)
  (format t "yes~%")
  (format t "~A ~A ~A~%"
          (if (< (- b a) 3) "swap" "reverse")
          a
          b))

;; after unlocking one failing test I relised I have misunderstood the concept of swapping
;; look like we do not only swap the neighbours but distant elements as well

(defun res1 (r1)
  (declare (optimize (speed 0) (debug 3)))
  (destructuring-bind (nx ax dx pax nax)
      r1

    (declare (ignore ax pax))
                                        ; (break "qqqqqqqqqq11111~A" (list nx ax dx ))
    (cond
      ((and (eq nx 1)
            (eq dx 'u))
       (format t "OK~%"))
      ((and (eq nx 1)
            (eq dx 'd))
       (swap-reverse nx nax))
      (t
       (format t "l1==== ~A~%" r1)))))

(defun res2 (res)
  (declare (optimize (speed 0) (debug 3)))
  (destructuring-bind ((an aa ad paa naa)
                       (bn ba bd pba nba))
      res
    (progn
      (break "qqqqqqqqqq2222 ~A" (list an aa ad paa naa bn ba bd pba nba ))
      (cond
        ((and (eq bn 1)
              (eq bd 'u)
              (eq ad 'dn)
              (< paa an)
              )
         (break "2 before print a ~A~%" (list an aa ad paa naa 'I bn ba bd pba nba))
         ;; (format t "l2aaa==== ~A~%" (cadr zzz))
         (swap-reverse an nba))

        ((and (eq bn 1)
              (eq bd 'd)
              (eq ad 'up)
              (> naa ba)
              )
         (break "2 before print2 b ~A~%" (list an aa ad paa naa 'I bn ba bd pba nba))
         ;; (format t "l2aaa2==== ~A~%" (cadr zzz))
         (swap-reverse bn an))
        (t
         (break "2 ELSE ~A~%" (list an aa ad paa naa'I bn ba bd pba nba))
         (format t "no~%"))))))

(defun res3 (res)
  (declare (optimize (speed 0) (debug 3)))
  (destructuring-bind ((an aa ad paa naa)
                       (bn ba bd pba nba)
                       (cn ca cd pca nca))
      res
    (progn
      (break "qqqqqqqqqq33333 ~A" (list   an aa ad paa naa  bn ba bd pba nba  cn ca cd pca nca ))
      (cond
        ;; --swap ends with middle descending
        ((and
          (eq bd 'dn)
          (> naa ba)
          (< ca aa))
         (break "333333 a ~A" (list   an aa ad paa naa  bn ba bd pba nba  cn ca cd pca nca ))
         (format t "yes~%swap ~A ~A~%" cn (1+ an)))
        ;; --swap or rotate middle
        ((and
          (eq bd 'dn)
          (eq cn 1)
          (eq cd 'u)
          (eq ad 'up)
          (< pba aa))
         (break "333333 b ~A" (list   an aa ad paa naa  bn ba bd pba nba  cn ca cd pca nca ))
         ;; (format t "l3aaa==== ~A~%" (cadr zzz))
         (swap-reverse (car (nth 1 res)) (car (nth 0  res))))
        (t
         (break "333333 else ~A" (list   an aa ad paa naa  bn ba bd pba nba  cn ca cd pca nca ))
         (format t "no~%"))))))

(defun res4 (res)
  (declare (optimize (speed 0) (debug 3)))
  (destructuring-bind ((an aa ad paa naa)
                       (bn ba bd pba nba)
                       (cn ca cd pca nca)
                       (dn da dd pda nda))
      res
    (progn
      (break "qqqqqqqqqq4444 ~A" (list  an aa ad paa naa
                                        bn ba bd pba nba
                                        cn ca cd pca nca
                                        dn da dd pda nda)
             (cond
               (t
                (format t "no~%")))))))
(defun res5 (res)
  (declare (optimize (speed 0) (debug 3)))
  (destructuring-bind ((an aa ad paa naa)
                       (bn ba bd pba nba)
                       (cn ca cd pca nca)
                       (dn da dd pda nda)
                       (en ea ed pea nea))
      res
    (progn
      (break "qqqqqqqqqq555 ~A" (list an aa ad paa naa
                                      bn ba bd pba nba
                                      cn ca cd pca nca
                                      dn da dd pda nda
                                      en ea ed pea nea)
             (cond
               (t
                (format t "no~%")))))))

(defun sortone (n d &optional pd prevdir prevzz)
  (declare (optimize (speed 0) (debug 3)))

  (if (null d)
      (progn
        ;; (format t "finished")
        )
      (progn
        ;; (format t "===== ~A~%" d)
        (let ((a (car d))
              (b (cadr d)))
          (let* ((dir (cond
                        ((and a b) (if (< a b) 'up 'dn))
                        (T 'l)))
                 (zzz (cond
                        ;; THIS NEEDS REVIEW -  we need proper formal solution

                        ;; here I can work on the logic to solve the puzzle
                        ((and (eq dir 'up) (null prevdir)) (cons (list n a 'u pd (cadr d)) prevzz))
                        ((and (eq dir 'dn) (null prevdir)) (cons (list n a 'd pd (cadr d)) prevzz))
                        ((and (eq dir 'up) (eq prevdir 'up)) prevzz)
                        ((and (eq dir 'up) (eq prevdir 'dn)) (cons (list n a dir pd (cadr d)) prevzz))
                        ((and (eq dir 'dn) (eq prevdir 'dn)) prevzz)
                        ((and (eq dir 'dn) (eq prevdir 'up)) (cons (list n a dir pd (cadr d)) prevzz))
                        ;; change handling of the last
                        ;; ((and (eq dir 'l)   (eq prevdir 'up)) (cons (list n a dir pd (cadr d)) prevzz))
                        ;; ((and (eq dir 'l)   (eq prevdir 'dn)) (cons (list n a dir pd (cadr d)) prevzz)
                         )
                        (T
                         (list 'finish prevzz)))))

            (break "aaaaaaaaaaaaaa~%")

            ;; (format t "<<<<<<<<<<<< ~A~%" (list n d dir prevdir zzz))
            (if (eq 'finish (car zzz))
                (let* ((res (cadr zzz))
                       (r1 (nth 0 res)))
                  (break "FFFFFFFFFFFFFFFFFFFFF")
                  (cond
                    ((eq (length res) 1)
                     (res1 r1))

                    ((eq (length res) 2)
                     (res2 res))

                    ((eq (length res) 3)
                     (res3 res))

                    ((eq (length res) 4)
                     (res4 res))

                    ((eq (length res) 5)
                     (res5 res))

                    (t
                     (format t "no~%")))))
            (progn
              (break "eeeeeeeeeeeeee")
              (sortone (1+ n) (cdr d) (car d) dir zzz)))))))


(defun solve-me (d)
  (progn
    (format t "~&~A -------------~%" d)
    (sortone 1 d )))

(defun sortmany ()
  (loop for l in '((11 12) (11 12 13)
                   ;; (14 12) (14 13 12)
                   ;; (11 12 13 15 14) (11 12 13 16 15 14)
                   ;; (11 12 13 15 14 16 17) (11 12 13 16 15 14 17)
                   ;; (11 12 13 25 24)
                   ;; (11 13 18 16 14)
                   ;; (7 11 13 18 16 14 12) ; broken
                   ;; (7 11 13 18 16 14 12 19) ; broken
                   ;; (15 13 11 16 17 18)
                   ;; (15 13 11 14 17 18) ; broken
                   (13 11 12) ; no
                   (11 15 14 13 12 16) ; swap 11 16 or reverse 15 12 - v
                   (11 14 13 12) ; swap 14 12
                   (14 12 13 11) ; reverse 14 11
                   (11 14 13 12 15)
                   (15 12 13 14 11)
                   (18 13 16 15 14 17 12 11)
                   (11 12 17 14 15 16 13 18)
                   )
        do (solve-me l)))

(defun sorthr ()
  (loop for l in '((4 2)
                   (3 1 2)
                   (1 5 4 3 2 6)
                   )
        do (solve-me l)))

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
