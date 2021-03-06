(defun insertion-sort (list &optional (predicate #'>))
  "Return a sorted copy of list."
  (labels ((insert (item list)
             (cond ((null list)
                    (list item))
                   ((funcall predicate item (car list))
                    (cons item list))
                   (t
                    (cons (car list) (insert item (cdr list)))))))
    (when list
      (insert (car list) (insertion-sort (cdr list) predicate)))))

(defun rec-merge-sort (i j &optional (acc nil))
  (cond ((and (null i)
              (null j))
         (reverse acc))
        ((and (null i)
              j)
         (rec-merge-sort i (cdr j) (cons (car j) acc)))
        ((and i
              (null j))
         (rec-merge-sort (cdr i) j (cons (car i) acc)))
        ((>= (car i)
             (car j))
            (rec-merge-sort (cdr i) j (cons (car i) acc)))
        ((< (car i)
            (car j))
           (rec-merge-sort i (cdr j) (cons (car j) acc)))
        (T (cerror "missed" "case missing"))))

(defun command-nums ()
  '(nil find-strongest strongest-died recruit merge-armies))

(defun find-strongest (i)
  (format t "~&~A" (car (aref *armies* i))))

(defun strongest-died (i)
  (setf (aref *armies* i) (cdr (aref *armies* i))))

(defun recruit (i c)
  (setf (aref *armies* i)
        (insertion-sort (cons c (aref *armies* i)))))

;;; merge armies keeping correct sorting
(defun merge-armies (i j)
  ;; recursive merge i and j onto i
  (setf (aref *armies* i)
        (rec-merge-sort (aref *armies* i)
                              (aref *armies* j)))
  ;; make j to be nil
  (setf (aref *armies* j) nil))



(defun process-event (l)
  ;; (format t "processing ~A ~A~%" *n* l)
  (cond ((eq (car l) 1) (find-strongest (cadr l)))
        ((eq (car l) 2) (strongest-died (cadr l)))
        ((eq (car l) 3) (recruit (cadr l)
                                 (caddr l)))
        ((eq (car l) 4) (merge-armies (cadr l)
                                      (caddr l)))
        (T (cerror "no" "function"))))

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
  (let ((nq (split-and-parse (read-line stream))))
    ;; (format t "nq ~A~%" nq)
    (defparameter *n* (car nq))
    (defparameter *armies* (make-array 1100000 :initial-element nil))
    (dotimes (x (cadr nq))
      (process-event (split-and-parse (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
