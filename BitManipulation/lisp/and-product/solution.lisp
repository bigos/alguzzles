;; (proclaim '(optimize (speed 3)))

(defun int2bin (n)
  (format nil "~10,'0b" n))

(defun discrepancies (s e)
  (let ((c (solve-me (list s e)))
        (l (apply 'logand (num-sequence s e))))
    (if (eq c l)
        (format t "~a and ~a are equal   ~A ~A~%" s e c (int2bin c))
        (format t "~a and ~a ARE NOT equal   ~A ~A ~A ~A~%" s e c (int2bin c) l (int2bin l)))))

;; example of missed possibilities
;; (discrepancies-loop 13 31)
(defun discrepancies-loop (s e)
                    (loop for x from s to e do (discrepancies x e)))

(defun rec-num (s e acc)
  (if (> s e)
      acc
      (rec-num (1+ s) e (push (cons (expt 2 s) (1- (expt 2 (1+ s)))) acc))))

(defun num-ranges (e)
  (rec-num 1 e (list (cons 0 1))))

(defun pivot-numbers ()
  (loop for x from 0 to 32 collect  (list x (expt 2 x))))

(defun rangenums (s e)
  (loop for x from s to e collect (list x (logandnums x e))))

(defun logandnums (s e)
  (apply 'logand (num-sequence s e)))

(defun num-sequence (s n)
  (loop for x from s to n collect x))

(defun num-test (s e)
  (loop for c in (num-ranges 31)
     when (and (>= s (car c)) (<= s (cdr c))
               (>= e (car c)) (<= e (cdr c)))
     collect c))

(defun solve-me (n)
  (let ((s (car n))
        (e (cadr n)))
    (if (num-test s e)
        (if (oddp (car n))
            (logand (1- (car n)) (cadr n))
            (logand (car n) (cadr n)))
        0)))

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
  (let ((tc (parse-integer (read-line stream))))
    (dotimes (x tc)
      (solve-me (split-and-parse (read-line stream))))))


;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input10" :type "txt"))
    (solution s)))

(main)
