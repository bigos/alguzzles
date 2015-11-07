(defun solve-me (n a)
  (let ((ar (make-array (list n 3)))
        (lt 1)
        (rt 2))
    (setf (aref ar 0 lt)      0)
    (setf (aref ar (1- n) rt) 0)
    (loop for d in a
       for li from 0 below n do
         (setf (aref ar li 0) d))
    (loop for i from 1 below n do
         (setf (aref ar i lt) (+ (aref ar (1- i) lt)
                                 (aref ar (1- i) 0))))
    (loop for i from (- n 2) downto 0 do
         (setf (aref ar i rt) (+ (aref ar (1+ i) rt)
                                 (aref ar (1+ i) 0))))


    (format t "~A ~A~%" n ar)))

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
  (let* ((tc (parse-integer (read-line stream))))
    (dotimes (x tc)
      (solve-me (parse-integer (read-line stream))
                (split-and-parse (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
