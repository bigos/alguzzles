;;; this stops from stack stack exhaustion happening when printing parent nodes
(setf *print-circle* T)

(defstruct node
  (value 0    :type fixnum)
  (parent)
  (left)
  (right)
  (root nil   :type boolean :read-only T)
  (children))

(defun execute-command (command-line)
  (let ((com (car command-line)))
    (format t "~&~S <<<<=======~%" com)
    (cond ((equal com "change")
           (change-value (parse-integer (nth 1 command-line))))
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
                    (visit-child (parse-integer (nth 2 command-line))))
                   (T (cerror "do not select" "not implemented ~A" com2)))))
          ((equal com "insert")
           (let ((com2 (nth 1 command-line)))
             (cond ((equal com2 "left")
                    (insert-left (parse-integer (nth 2 command-line))))
                   ((equal com2 "right")
                    (insert-right (parse-integer (nth 2 command-line))))
                   ((equal com2 "child")
                    (insert-child (parse-integer (nth 2 command-line))))
                   (T (cerror "do not select" "not implemented ~A" com2)))))
          ((equal com "delete")
           (delete-recursively))
          (T (cerror "do not select this option" "not implemented function ~A" com )))))

;;; ----------------------------------------------
(defun msg (l)
  (format t "running ~A~%" l))

(defun change-value (new-val)
  (msg 'change-value)
  (setf (node-value *current-node*) new-val))

(defun visit-left ()
  )

(defun visit-right ()
  (setf *current-node* (node-right *current-node*)))

(defun visit-parent ()
  )

(defun visit-child (n)
  (let ((first-child (car (last (node-children *current-node*)))))
    (if (eq 1 n)
        (setf *current-node* first-child)
        (progn
          (setf *current-node*
                (nth (1- n)
                     (reverse (node-children *current-node*))))
          ))))

(defun insert-left (x)
  )

(defun insert-right (x)
  (let ((new-node
         (make-node :value x
                    :parent (node-parent *current-node*)
                    :left *current-node*)))
    (setf (node-right *current-node*) new-node)))

;;; I need a list of previously created nodes
;;; maximum number of operations Q is (expt 10 5)
;;; so I need maximum (expt 10 5) node ids.
;;; but
;;; should i have traversable tree of nodes instead?
;;; esp when we have fixed stack exhaustion

(defun insert-child (x)
  (let ((child-node
         (make-node :value x
                    :parent *current-node*)))
    (push child-node (node-children *current-node*))
    ;; (format t "~& !!!!!!!!!!!!~A ~%!!!~A~%" (node-children *current-node*) *current-node*)
    ))

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
