;; Enter your code here. Read input from STDIN. Print output to STDOUT
(defvar n)
(defvar m)
(defvar arr)
(defvar is-visited)
(defvar g)
(defvar *count* 0)


(setq n (read))
(setq m (read))
(setq g (make-array (list n n) :initial-element nil))
(setq is-visited (make-array n :initial-element nil))

(dotimes (i m)
  (let ((a (1- (read)))
        (b (1- (read))))
     (setf (aref g a b) t)
     (setf (aref g b a) t)))

(defvar nc)
(setq nc (make-array n :initial-element 0))

(defun f (node)
  ;;(format t "enter in the function f, node: ~a~%" node)
  ;;(format t "(aref is-visited node): ~a~%" (aref is-visited node))
  (if (null (aref is-visited node)) ;;; if we have not visited this node yet
      (let ((num-children 0))
         ;;(format t "--> if clause, is-visited == nil. ~%")
         (setf (aref is-visited node) t)   ;;; mark this node visited
         (dotimes (i n)
            (if (and (aref g i node) (null (aref is-visited i)))
              (setq num-children (+ num-children (f i) 1))))
         (setf (aref nc node) num-children)
         (if (/= 0 (mod num-children 2))
           (incf *count*))
         num-children)
      (aref nc node)))

(f 0)

(format t "~a~%" (1- *count*))
