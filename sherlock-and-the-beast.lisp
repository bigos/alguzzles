(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun largest-ndigits (n)
  (1- (expt 10 n)))

(defun smallest-ndigits (n)
  (+ (largest-ndigits (1- n))
     1))

(defun decentp (n)
  ;; TODO: finish it
  ;; A 'Decent' Number has -
  ;; 1. Only 3 and 5 as its digits.
  ;; 2. Number of times 3 appears is divisible by 5.
  ;; 3. Number of times 5 appears is divisible by 3.
  ;; 555 or 33333 or 55555533333
  (let* ((nstr (format nil "~A" n))
         (count3 (count #\3 nstr))
         (count5 (count #\5 nstr)))
    (if (and (= (+ count3 count5)
                (length nstr))
             (zerop (rem count3 5))
             (zerop (rem count5 3)))
        n
        -1)))

(defun largest-decent (ndigits)
  (let ((big-decent))
    (loop for x from (largest-ndigits ndigits)
       downto (smallest-ndigits ndigits)
       do (when (decentp x)
            (setf big-decent x))
       until (decentp x))
    (if big-decent
        big-decent
        -1)))

(defun solution (&optional stream)
  (let* ((tc (parse-integer (read-line stream)))
         (nums (loop repeat tc collect (parse-integer (read-line stream)))))
    ;; (princ  nums)
    (loop for x in nums
       do (princ (largest-decent x))
         (terpri))
    ))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "sherlock-and-the-beast.input.1.txt"))
      (solution s))))

(repl-main)
