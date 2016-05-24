(defparameter *operations*
  '((change . change-value)
    (print . print)
    (visit . visit-left)
    (visit . visit-right)
    (visit . visit-parent)
    (visit . visit-child)
    (insert . insert-left)
    (insert . insert-right)
    (insert . insert-child)
    (delete . delete)))

;;; ----------------------------------------------

(defun execute-command (line)
  (let ((command (car line)))))

(defun change-value (new-val)
  (setf (node-value *current-node*) new-val))

;;; ----------------------------------------------
(defstruct node
  (value 0    :type fixnum)
  (parent)
  (left)
  (right)
  (root nil   :type boolean :read-only T))

(defun init-operations ())

(defun init-tree ()
  (defparameter *root* (make-node :root T ))
  (defparameter *current-node* *root*))

(defun process-commands (l)
  (if (null l)
      l
      (progn
        (format t "~A~%" (car l))
        (execute-command (car l))
        (process-commands (cdr l)))))

(defun solve-me (l)
  (init-operations)
  (init-tree)
  (process-commands l))

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
  (let ((n (parse-integer (read-line stream))))
    (format t "~A~%" (solve-me (loop for x from 1 to n
                                  collect
                                    (split-by-one-space (read-line stream)))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
