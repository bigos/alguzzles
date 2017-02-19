(defun triangular (n)
  (/ (+ (expt n 2) n) 2))

(defun fifteenator (n)
  (* 15 (+ (* (+ 1 n) 3) (* (/ (+ (* n n) n) 2) 7))))

(defun recursive (n acc l steps)
  (let ((ss (if steps steps '(3 2 1 3 1 2 3))))
    (if (>= n l)
        acc
        (recursive (+ n (car ss))
                   (+ n acc)
                   l
                   (cdr ss)
                   ))))

(defun solve-me (n)
  (format t "~A~%"
          (if (< n 15)
              (recursive 0 0 n nil)
              (let* ((fm (- n (mod n 15)))
                     (acc (fifteenator (- (/ fm 15) 1))))
                (recursive fm acc n nil)))))

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
    (loop for x from 1 to tc do (solve-me (parse-integer (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
