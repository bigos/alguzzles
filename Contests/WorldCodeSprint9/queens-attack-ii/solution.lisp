(defun n-pos (n rq cq)
  (cdr (loop for rx from rq to n
          until (obstaclep rx cq)
          collect (list rx cq)
            )))
(defun e-pos (n rq cq)
  (cdr (loop for cx from cq to n
          until (obstaclep rq cx)
          collect (list rq cx)
            )))
(defun s-pos (n rq cq)
  (cdr (loop for rx from rq downto 1
          until (obstaclep rx cq)
          collect (list rx cq)
            )))
(defun w-pos (n rq cq)
  (cdr (loop for cx from cq downto 1
          until (obstaclep rq cx)
          collect (list rq cx)
            )))

(defun ne-pos (n rq cq)
  (cdr (loop for rx from rq to n
          for cx from cq to n

          until (obstaclep rx cx)
          collect (list rx cx)
            )))
(defun se-pos (n rq cq)
  (cdr (loop for rx from rq to n
          for cx from cq downto 1

          until (obstaclep rx cx)
          collect (list rx cx)
            )))
(defun sw-pos (n rq cq)
  (cdr (loop for rx from rq downto 1
          for cx from cq downto 1

          until (obstaclep rx cx)
          collect (list rx cx)
            )))
(defun nw-pos (n rq cq)
  (cdr (loop for rx from rq downto 1
          for cx from cq to n

          until (obstaclep rx cx)
          collect (list rx cx)
            )))

(defun obstaclep (x y)
  (get-from-hh x y))

(defun add-to-hh (x y)
  (unless (gethash x *r*)
    (setf (gethash x *r*) (make-hash-table)))
  (setf (gethash y (gethash x *r*)) T ))

(defun get-from-hh (x y)
  (when (gethash x *r*)
    (gethash y (gethash x *r*))))

(defun solve-me (n k rq cq )
  (format t "~A~%"
          (loop for a in '(n-pos e-pos s-pos w-pos ne-pos se-pos sw-pos nw-pos)
             sum (length (funcall a n rq cq )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let* ((nk (split-and-parse (read-line stream)))
         (n (car nk))
         (k (cadr nk))
         (rqcq (split-and-parse (read-line stream)))
         (rq (car rqcq))
         (cq (cadr rqcq))
         (ii))
    (defparameter *r* (make-hash-table))
    (loop for x from 1 to k
       do
         (setf ii (split-and-parse (read-line stream)))
         (add-to-hh (car ii) (cadr ii)))
    (solve-me n k rq cq )))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input1" :type "txt"))
    (solution s)))

(main)
