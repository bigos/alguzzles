(declaim (optimize (debug 3)))

(defun zeroify (l i)
  (format nil (format nil "~~~a,1,0,'0@a" l) i))

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
  (let ((tc (parse-integer (read-line stream)))
        (hash (make-hash-table))
        (unsorted-keys))
    (loop for x from 0 below tc
          for rs = (read-line stream)
          do
             (push rs (gethash (length rs)
                               hash )))
    (maphash (lambda (k v)
               (push k unsorted-keys))
             hash)
    (let ((sorted-keys (sort unsorted-keys '<)))
      (loop for k in sorted-keys
            do
               (loop for z in (sort (gethash k hash)
                                    'string<)
                     do
                        (princ z)
                        (terpri))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input03" :type "txt"))
    (solution s)))

(main)
