(defun solve-me (mc l)
  (let ((res (reverse
              (remove-duplicates
               (loop for x in l
                  when (>= (count x l ) mc) collect x)))))
    (if res
        (progn
          (loop for x in res
             for fst = T then nil do
               (format t "~A~A" (if fst "" " ") x))
          (terpri))
        (format t "~A~%" -1))))

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
                      :name "input0" :type "txt"))
    (solution s)))

(main)
