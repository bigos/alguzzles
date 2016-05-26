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
    ;; (format t "~&~S <<<<=======~%" com)
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

(defun change-value (new-val)
  (setf (node-value *current-node*) new-val))

(defun visit-left ()
  (setf *current-node* (node-left *current-node*)))

(defun visit-right ()
  (setf *current-node* (node-right *current-node*)))

(defun visit-parent ()
  (setf *current-node* (node-parent *current-node*)))

(defun visit-child (n)
  (let ((first-child
         (loop for cc in (node-children *current-node*)
            until (null (node-left cc))
            finally (return cc))))
    (setf *current-node* first-child)
    (when (> n 1)
      (loop for y from n downto 2 do
           (visit-right)))))

(defun insert-left (x)
  (let ((new-node
         (make-node :value x
                    :parent (node-parent *current-node*)
                    :right *current-node*)))
    (setf (node-left *current-node*) new-node)
    (push new-node (node-children (node-parent *current-node*)))
    )  )

(defun insert-right (x)
  (let ((new-node
         (make-node :value x
                    :parent (node-parent *current-node*)
                    :left *current-node*)))
    (setf (node-right *current-node*) new-node)
    (push new-node (node-children (node-parent *current-node*)))))

(defun insert-child (x)
  (let ((current-lefmost (car (node-children *current-node*)))
        (child-node
         (make-node :value x
                    :parent *current-node*)))
    (when current-lefmost
      (setf (node-left current-lefmost) child-node)
      (setf (node-right child-node) current-lefmost))
    (push child-node (node-children *current-node*))))

(defun delete-recursively ()
  (let ((current-value (node-value *current-node*))
        (left-node (node-left *current-node*))
        (right-node (node-right *current-node*)))
    (setf (node-left right-node) left-node)
    (setf (node-right left-node) right-node)
    (setf *current-node* (node-parent *current-node*))
    (delete-if (lambda (x)
                 (eq current-value (node-value x)))
               (node-children *current-node*))))
;;; ----------------------------------------------

(defun init-operations ())

(defun init-tree ()
  (defparameter *root* (make-node :root T ))
  (defparameter *current-node* *root*))

(defun process-commands (l)
  (if (null l)
      l
      (progn
        ;;(format t "~A~%" (car l))
        (execute-command (car l))
        ;;(format t "~&~A~&" *current-node*)
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
    (solve-me (loop for x from 1 to n
                 collect
                   (split-by-one-space (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
