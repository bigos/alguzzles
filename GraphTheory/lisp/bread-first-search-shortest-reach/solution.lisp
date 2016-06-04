(setf *print-circle* T)
(defparameter *visited* nil)

(defstruct node
  (id)
  (neighbours))

(defun solve-me (n m edges s)
  (let ((nodes (make-array (list (1+ n)))))
    (format t "args are ~A ~A ~A ~A~%" n m edges s)
    ;; finish me
    (loop for i from 1 to n do
         (setf (aref nodes i) (make-node :id i)))
    (loop for vert in edges do
         (progn
           (push (car vert) (node-neighbours (aref nodes (cadr vert))))
           (push (cadr vert) (node-neighbours (aref nodes (car vert))))))


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
      (setf nm (split-and-parse (read-line stream)))
      (setf n (car nm)
            m (cadr nm))
      (dotimes (_b m)
        (push (split-and-parse (read-line stream)) edges))
      (setf s (parse-integer (read-line stream)))
      (solve-me n m edges s)
      )))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
