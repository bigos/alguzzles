(declaim (optimize (space 0) (safety 0) (speed 3)))

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

(defun all-combinations (n k)
  (if (or (< k 0) (< n k)) '()
      (let ((a (make-array k)))
        (loop for i below k do (setf (aref a i) i))
        (loop collect (coerce a 'list) while (next-combination n a)))))

(defun map-combinations (n k fun)
  (if (and (>= k 0) (>= n k))
      (let ((a (make-array k)))
        (loop for i below k do (setf (aref a i) i))
        (loop do (funcall fun (coerce a 'list)) while (next-combination n a)))))

(defun comb (m list)
  (let ((result))
    (labels ((comb1 (l c m)
               (when (>= (length l) m)
                 (when (zerop m)
                   (return-from comb1 (push c result)))
                 (comb1 (cdr l) c m)
                 (comb1 (cdr l) (cons (first l) c) (1- m)))))
      (comb1 list nil m))
    result))

(defun solve-me (a s)
  (loop for x from 1 upto (length a)
     for y = (every 'null (map 'list (lambda (x) (>= (apply '+ x) s) ) (comb x a)))
     while y
     finally (return (if (<= x (length a))
                         x
                         -1))))

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
  (let ((size-of-a (parse-integer (read-line stream)))
        (a (split-and-parse (read-line stream)))
        (tc (parse-integer (read-line stream)))
        (s))
    (declare (ignore size-of-a))
    (loop for x from 1 to tc do
         (setf s (parse-integer (read-line stream)))
         (format t "~a~%" (solve-me a s)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
