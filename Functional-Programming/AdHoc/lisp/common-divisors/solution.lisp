(defun small-divisors (n fls)
  (loop for d from 1 to fls
     when (zerop (mod n d))
     collect d))

(defun divisors (n)
  (let* ((nsqrt (sqrt n))
         (small-divisors (small-divisors n (floor nsqrt)))
         (reversed-divisors))
    ;; do not include sqrt twice
    (setf reversed-divisors             ;we do the assignment only once, so it's not cheating
          (if (eq (floor nsqrt)
                  (ceiling nsqrt))
              (cdr (reverse small-divisors))
              (reverse small-divisors)))
    (if (= n 1)
        '(1)
        (concatenate 'list
                     small-divisors
                     (loop for x in reversed-divisors
                        collect (/ n x))))))

(defun solve-me (m l)
  (let ((dm (divisors m))
        (dl (divisors l)))
    (princ
     (count-if (lambda (x) (when (position x dl) x)) dm))
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
  (let* ((tc (parse-integer (read-line stream)))
         (ml))
    (dotimes (x tc)
      (setf ml (split-and-parse (read-line stream)))
      (solve-me (car ml) (cadr ml)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input10" :type "txt"))
    (solution s)))

(main)
