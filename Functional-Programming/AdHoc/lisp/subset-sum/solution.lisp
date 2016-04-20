(declaim (optimize (space 0) (safety 0) (speed 3)))

(defun binary-search-val (low high added s)
  (let ((pointer (floor (/ (+ low high) 2))))
    ;; (format t "~A ~A ~A    ~A   ~A ~A~%" low pointer high (aref added pointer) s added)
    (cond ((and (zerop pointer)
                (>= (aref added pointer) s))
           pointer)
          ((and (>= (aref added pointer) s)
                (<  (aref added (1- pointer)) s))
           pointer)
          (T (if (< (aref added pointer) s)
                 (binary-search-val pointer high added s)
                 (binary-search-val low pointer added s)
                 )))))

(defun solve-me (size-of-a added s)
  (if (< (aref added (1- size-of-a)) s)
      -1
      (1+ (binary-search-val 0 (1- size-of-a) added s ))))

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
                      :name "input0" :type "txt"))
    (solution s)))

(main)
