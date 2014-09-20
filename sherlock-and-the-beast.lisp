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

(defun binary-to-decent (bin len)
  (let* ((nstr (substitute #\5
                           #\1
                           (substitute #\3
                                       #\0
                                       (format nil
                                               (format nil "~~~d,'0b" len)
                                               bin) )))
         (count3 (count #\3 nstr))
         (count5 (count #\5 nstr)))
    (format t "~a~%" nstr)
    (if (and (= (+ count3 count5)
                (length nstr))
             (zerop (rem count3 5))
             (zerop (rem count5 3)))
        (parse-integer nstr)
        nil)))

(defun decentp (n)
  ;; (format t "~A~%" n)
  (let* ((nstr (format nil "~A" n))
         (count3 (count #\3 nstr))
         (count5 (count #\5 nstr))
)
    (if (and (= (+ count3 count5)
                (length nstr))
             (zerop (rem count3 5))
             (zerop (rem count5 3)))
        n
        nil)
))

(defun largest-decent (ndigits)
  (let ((big-decent))
    (loop for x from (1+ (largest-ndigits ndigits))
       downto (smallest-ndigits ndigits)
       by 10
       do (progn (when (decentp (+ x 3))
                   (setf big-decent (+ x 3)))
                 (when (decentp (+ x 5))
                   (setf big-decent (+ x 5))))
       until big-decent)
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
