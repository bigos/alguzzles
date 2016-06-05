(setf *print-circle* T)

(declaim (optimize (debug 3)))

(defstruct node
  (id)
  (dist) ; distance from start
  (prev) ; previous node following shortest path
  (neighbours))

(defun try-me (nodes)
  (/ nodes 0))

(defun solve-me (n m edges s)
  (declare (ignore m))                  ; m is number of edges
  (let ((nodes (make-array (list (1+ n))))
        (visited))
    (format t "args are ~A ~A ~A ~%" n edges s)
    ;; (setf (aref nodes 0) nil)
    (loop for i from 1 to n do
         (setf (aref nodes i)
               (make-node :id i :dist most-positive-fixnum)))
    (loop for e in edges do
         (push (car e)  (node-neighbours (aref nodes (cadr e))))
         (push (cadr e) (node-neighbours (aref nodes (car  e)))))
    (setf (node-dist (aref nodes s)) 0)
    (try-me nodes)
    ))

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
  (let ((tc (parse-integer (read-line stream)))
        (nm)
        (n)
        (m)
        (edges)
        (s))
    (dotimes (_a tc)
      (setf edges nil)
      (setf nm (split-and-parse (read-line stream)))
      (setf n (car nm)
            m (cadr nm))
      (dotimes (_b m)
        (push (split-and-parse (read-line stream)) edges))
      (setf s (parse-integer (read-line stream)))
      (solve-me n m edges s)
      (format t "~%"))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
