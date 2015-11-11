(defun next-combination (n a)
  (let ((k (length a)) m)
    (loop for i from 1 do
         (when (> i k) (return nil))
         (when (< (aref a (- k i)) (- n i))
           (setf m (aref a (- k i)))
           (loop for j from i downto 1 do
                (incf m)
                (setf (aref a (- k j)) m))
           (return t)))))

;; (defun all-combinations (n k)
;;   (if (or (< k 0) (< n k)) '()
;;       (let ((a (make-array k)))
;;         (loop for i below k do (setf (aref a i) i))
;;         (loop collect (coerce a 'list) while (next-combination n a)))))

(defun map-combinations (n k fun)
  (if (and (>= k 0) (>= n k))
      (let ((a (make-array k)))
        (loop for i below k do (setf (aref a i) i))
        (loop do (funcall fun (coerce a 'list)) while (next-combination n a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solve-me (n a)
  (let ((res 0))
    (loop for x from 1 to n do
         (map-combinations n x 'null
                           ;; (lambda (y)
                           ;;   (let ((tr 0))
                           ;;     (loop for i in y do
                           ;;          (setf tr (logxor tr (elt a i))))
                           ;;     (incf res tr)))
                           )
         )
    (princ (mod res  (+ 7 (expt 10 9))))
    (terpri)))

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
  (let ((tc (parse-integer (read-line stream)))
        (n))
    (dotimes (x tc)
      (setf n (parse-integer (read-line stream)))
      (solve-me
       n (make-array n :initial-contents (split-and-parse (read-line stream)))))))


;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input06" :type "txt"))
    (solution s)))

(main)
