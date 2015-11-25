(defun rots (s l)
  (if (zerop l)
      (terpri)
      (progn
        (princ s)
        (princ #\space)
        (rots (concatenate 'string
                           (subseq s 1)
                           (subseq s 0 1))
              (1- l)))))

(defun solve-me (ss)
  ;; (format t  "args ~a ~%" ss)
  (loop for s in ss do
       (rots (concatenate 'string
                          (subseq s 1)
                          (subseq s 0 1))
             (length s))))

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
  (let* ((tc (parse-integer (read-line stream)))
         (ss (loop for x from 1 to tc collect (read-line stream))))
    (solve-me ss)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
