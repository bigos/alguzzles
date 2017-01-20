;; (declaim (optimize (debug 3)))
;;; (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(defvar *lamps* 0)

;; (append  '((1 2)) (cons '(3 4) '((5 6))) )

(defun hashval (h rl)
  (let ((row (car rl))
        (range (cdr rl)))
    (push range
          (gethash row h))))

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
  (let* ((nmk (split-and-parse (read-line stream)))
         (n (nth 0 nmk))
         (m (nth 1 nmk))
         (k (nth 2 nmk))
         (rh (make-hash-table)))
    (declare (type fixnum n m k ))
    (setf *lamps* (* n m))
    (loop
       for l from 1 to k
       for rl = (split-and-parse (read-line stream))
       do
         (hashval rh rl)
       finally
         (/ rh 0)
         (format t "~s~%" rh))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0a" :type "txt"))
    (solution s)))

(main)
