(defun max-subarray (a m)
  (let ((max-so-far 0)
        (max-ending-here 0))
    (loop for x across a do
         (setf max-ending-here (max 0          (+ max-ending-here x))
               max-so-far      (max max-so-far (mod max-ending-here m))))
    max-so-far))

(defun solve-me (nm ints)
  (let ((a (make-array (car nm) :initial-contents ints)))
    (princ  (max-subarray a (cadr nm))  )
    (terpri)))

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
    (dotimes (x tc)
      (solve-me (split-and-parse (read-line stream))
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
