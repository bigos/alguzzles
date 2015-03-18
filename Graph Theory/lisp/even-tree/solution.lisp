(require :sb-sprof)

;;; (declaim (optimize speed))

(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))


;;; insert your code here
(defun connected-nodes (node nodes)
  (append
   (loop for n in nodes
      when (eq node (car n)) collect (cadr n)
      when (eq node (cadr n)) collect (car n))))

(defun vertices (nodes)
  (remove-duplicates
   (loop for n in nodes
      append (list (car n) (cadr n)))))

(defun solution (&optional stream)
  (let* ((dd (split-and-parse (read-line stream)))
         (n (car dd))
         (m (cadr dd))
         (ar)
         (graph))
    (loop for x in
         (loop for row below m
            collecting (split-and-parse (read-line stream)))
       do (push (cons (car x) (cadr x)) ar))

    (format t "going to solve ~A ~A ~A~%" n m ar)))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Graph Theory/lisp/even-tree/"
                                    "input0.txt"))
      (solution s))))

;; using profiler
;; (sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop nil))
;; (sb-sprof:start-profiling)
;; (repl-main)
;; (sb-sprof:report)
