(defun city-map (n mi)
  (let ((cc (make-array n :initial-element nil))
        (pri (make-array n :initial-element n))
        (nxi (make-array n :initial-element n))
        (last-found)
        (nearest (make-array n :initial-element n)))
    (loop for x in mi do
         (setf (aref cc x) T))

    (setq last-found nil)
    (loop for x from 0 below n do
         (if (aref cc x)
             (setf
              (aref pri x) 0
              last-found x)
             (if last-found
                 (setf (aref pri x) (- x last-found)))))

    (setq last-found nil)
    (loop for x from (1- n) downto 0 do
         (if (aref cc x)
             (setf
              (aref nxi x) 0
              last-found x)
             (if last-found
                 (setf (aref nxi x) (- last-found x)))))

    (loop for x from 0 below n do
         (setf (aref nearest x) (min (aref pri x)
                                     (aref nxi x))))
    nearest))

(defun solve-me (n m mi)
  (declare (ignore m))
  (let ((mv (city-map n mi)))
    (format t "~A~%"
            (loop for x across mv
                 maximize x))))

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
  (let ((nm (split-and-parse (read-line stream)))
        (mi (split-and-parse (read-line stream))))
    (solve-me (car nm) (cadr nm) (sort mi #'<))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
