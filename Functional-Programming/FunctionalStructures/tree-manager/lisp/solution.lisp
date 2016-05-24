(defstruct node
  (value 0    :type fixnum)
  (parent)
  (left)
  (right)
  (root nil   :type boolean :read-only T))

(defun execute-command (command-line)
  (let ((com (car command-line)))
    (format t "~&~S <<<<=======~%" com)
    (cond ((equal com "change")
           (change-value (nth 1 command-line)))
          ((equal com "print")
           (format t "~A~%" (node-value *current-node*)))
          ((equal com "visit")
           (let ((com2 (nth 1 command-line)))
             (cond ((equal com2 "left")
                    (visit-left))
                   ((equal com2 "right")
                    (visit-right))
                   ((equal com2 "parent")
                    (visit-parent))
                   ((equal com2 "child")
                    (visit-child (nth 2 command-line)))
                   (T (cerror "do not select" "not implemented ~A" com2)))))
          ((equal com "insert")
           (let ((com2 (nth 1 command-line)))
             (cond ((equal com2 "left")
                    (insert-left (nth 2 command-line)))
                   ((equal com2 "right")
                    (insert-right (nth 2 command-line)))
                   ((equal com2 "child")
                    (insert-child (nth 2 command-line)))
                   (T (cerror "do not select" "not implemented ~A" com2)))))
          ((equal com "delete")
           (delete-recursively))
          (T (cerror "do not select this option" "not implemented function ~A" com )))))

;;; ----------------------------------------------
(defun msg (l)
  (format t "running ~A~%" l))

(defun change-value (new-val)
  (msg 'change-value)
  (setf (node-value *current-node*) (parse-integer new-val)))

(defun visit-left ()
  )
(defun visit-right ()
  )
(defun visit-parent ()
  )
(defun visit-child (n)
  )

(defun insert-left (x)
  )
(defun insert-right (x)
  )
(defun insert-child (x)
  (make-node :value (parse-integer x)
             :parent *current-node*))

(defun delete-recursively ()
  )
;;; ----------------------------------------------

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
        (format t "~&~A~&" *current-node*)
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
