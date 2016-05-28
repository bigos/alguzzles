(declaim (optimize (space 0) (safety 3) (debug 3)))

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

(defun examine-children (&optional (n nil))
  (let ((nodes-left-nil (loop for cc in (node-children (if n n *current-node*))
                           when (null (node-left cc)) collect cc))
        (nodes-right-nil (loop for cc in (node-children (if n n *current-node*))
                            when (null (node-right cc)) collect cc))
        (left-vals (loop for lc in (node-children (if n n *current-node*))
                      collect (if (node-left lc)
                                  (node-value (node-left lc))
                                  nil)))
        (right-vals (loop for rc in (node-children (if n n *current-node*))
                       collect (if (node-right rc)
                                   (node-value (node-right rc))
                                   nil)))
        (left-counts (make-hash-table :test 'equalp))
        (righ-counts (make-hash-table :test 'equalp)))


    (loop for lv in left-vals
       for lhkey = (format nil "~A" lv) then (format nil "~A" lv)
       do
         (incf (gethash lhkey  left-counts 0)))

    (loop for rv in right-vals
       for rhkey = (format nil "~A" rv) then (format nil "~A" rv)
       do
         (incf (gethash rhkey righ-counts 0)))


    (loop for k being the hash-keys of left-counts
       do (when (> (gethash k left-counts) 1)
            (cerror "lll" "zzz")) )

    (loop for k being the hash-keys of righ-counts
       do (when (> (gethash k righ-counts) 1)
            (cerror "rrr" "aaa")))

    ))

(defun examine-siblings ()
  (if (node-parent *current-node*)
      (examine-children (node-parent *current-node*))))
;;; ----------------------------------------------

(defun change-value (new-val)
  (setf (node-value *current-node*) new-val))

(defun visit-left ()
  (setf *current-node* (node-left *current-node*))
  (examine-children)
  (examine-siblings)
  (when (null *current-node*) (cerror "current" "nil")))

(defun visit-right ()
  (setf *current-node* (node-right *current-node*))
  (examine-children)
  (examine-siblings)
  (when (null *current-node*) (cerror "current" "nil")))

(defun visit-parent ()
  (setf *current-node* (node-parent *current-node*))
  (examine-children)
  (examine-siblings)
  (when (null *current-node*) (cerror "current" "nil")))

(defun visit-child (n)
  (examine-children)
  (let ((first-child
         (loop for cc in (node-children *current-node*)
            until (null (node-left cc))
            finally (return cc))))
    (examine-children)
    (setf *current-node* first-child)
    (when (null *current-node*) (cerror "current" "nil"))
    (when (> n 1)
      (loop for y from n downto 2 do
           (visit-right)))
    (examine-children)
    (examine-siblings)))

(defun insert-left (x)
  ;;(format t "~A        <<<  inserting left ~%" *current-node*)
  (let* ((left-node (node-left *current-node*))
         (new-node
          (make-node :value x
                     :parent (node-parent *current-node*)
                     :left left-node
                     :right *current-node*)))
    (format t "aaaaaaaaaaa")
    (when left-node
      (setf  (node-right left-node) new-node))
    (format t "bbbbbbbbbbbbbbbbb ~a" new-node)
    ;;(format t "~&current node after new node creation~A~%" *current-node*)
    (push new-node (node-children (node-parent *current-node*)))
    (format t "ccccccccccccccc")
    (format t "~A~%" "next sibling check")
    (examine-siblings))
  )

(defun insert-right (x)
  (let ((new-node
         (make-node :value x
                    :parent (node-parent *current-node*)
                    :right (node-right *current-node*))))
    (setf (node-right *current-node*) new-node)
    (push new-node (node-children (node-parent *current-node*)))
    (examine-siblings)))

(defun insert-child (x)
  (let ((current-lefmost (car (node-children *current-node*)))
        (child-node
         (make-node :value x
                    :parent *current-node*)))
    (when current-lefmost
      (setf (node-left current-lefmost) child-node)
      (setf (node-right child-node) current-lefmost))
    (push child-node (node-children *current-node*))
    (examine-siblings)))

(defun delete-recursively ()
  (let ((this-node *current-node*)
        (left-node (node-left *current-node*))
        (right-node (node-right *current-node*)))


    (when right-node
      (setf (node-left right-node) left-node))

    (when left-node
      (setf (node-right left-node) right-node))

    (setf (node-value *current-node*) -1)

    (setf *current-node* (node-parent *current-node*))
    (when (null *current-node*) (cerror "current" "nil"))
    (delete-if (lambda (x)
                 (<  (node-value x) 0))
               (node-children *current-node*))
    (setf this-node nil)
    (examine-children)
    (examine-siblings)))
;;; ----------------------------------------------

(defun init-operations ()
  (defparameter *current-node* nil))

(defun init-tree ()
  (defparameter *root* (make-node :root T ))
  (defparameter *current-node* *root*))

(defun process-commands (l)
  (if (null l)
      l
      (progn
        (format t "~A =======~%" (car l))
        ;;(format t "~&before action ~A~&" *current-node*)
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
    (solve-me (loop for x from 1 to n
                 collect
                   (split-by-one-space (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input10" :type "txt"))
    (solution s)))

(main)
