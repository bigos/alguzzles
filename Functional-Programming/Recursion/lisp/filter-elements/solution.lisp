(defun solve-me (mc l)
  (let ((count-hash (make-hash-table))
        (pos-hash (make-hash-table))
        (found-hash (make-hash-table))
        (spacer))
    (labels ((solve-rec (mc l pos)
               (unless (null l)
                 (unless (gethash (car l) pos-hash nil)
                   (setf (gethash (car l) pos-hash) pos))
                 (incf (gethash (car l) count-hash 0))
                 (when (eq (gethash (car l) count-hash 0) mc)
                   (setf (gethash (gethash (car l) pos-hash -1)
                                  found-hash)
                         (car l)))
                 (solve-rec mc (cdr l) (1+ pos))))
             (print-results ()
               (if (zerop (hash-table-count found-hash))
                   (format t "-1")
                   (loop for x from 0 to (length l)
                      for y = (gethash x found-hash nil) do
                        (when y
                          (format t "~A~A" (if spacer " " "") y)
                          (setf spacer T))))))
      (solve-rec mc l 0)
      (print-results)
      (terpri))))

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
  (let ((tc (parse-integer (read-line stream))))
    (loop for x from 1 to tc do
         (solve-me
          (cadr (split-and-parse (read-line stream)))
          (split-and-parse (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input01" :type "txt"))
    (solution s)))

(main)
