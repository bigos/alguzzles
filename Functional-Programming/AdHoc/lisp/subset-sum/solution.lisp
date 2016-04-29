(declaim (optimize (space 0) (safety 0) (speed 3)))

(defun binary-search-loop (size-of-a added s)
  (loop for low = 0 then (if (< (aref added pointer) s)
                             pointer
                             low)
     for high = size-of-a then (if (< (aref added pointer) s)
                                 high
                                 pointer)
     for pointer = (floor (/ (+ low high) 2))
     until (or (and (zerop pointer)
                    (>= (aref added pointer) s))
               (and (>= (aref added pointer) s)
                    (<  (aref added (1- pointer)) s)))

     finally (return pointer)))

(defun solve-me (size-of-a added s)
  (if (< (aref added (1- size-of-a)) s)
      -1
      (1+ (binary-search-loop  size-of-a added s ))))

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
  (let* ((size-of-a (parse-integer (read-line stream)))
         (a (sort  (split-and-parse (read-line stream)) '>))
         (tc (parse-integer (read-line stream)))
         (s)
         (added (make-array (list size-of-a) :initial-contents (loop for x in a
                                                                  for y = x then (+ x y)
                                                                  collect y))))
    (loop for x from 1 to tc do
         (setf s (parse-integer (read-line stream)))
         ;; (format t "~&~A ~A~%" added s)
         (format t "~A~%" (solve-me size-of-a added s)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input09" :type "txt"))
    (solution s)))

(main)
