(defun checkdata (d maxlow maxhi lowcount hicount)
  (if (null d)
      (list hicount lowcount)
      (checkdata (cdr d)
                 (if (< (car d) maxlow)
                     (car d)
                     maxlow)
                 (if (> (car d) maxhi)
                     (car d)
                     maxhi)
                 (if (< (car d) maxlow)
                     (1+ lowcount)
                     lowcount)
                 (if (> (car d) maxhi)
                     (1+ hicount)
                     hicount))))

(defun solve-me (n ii)
  (declare (ignore n))
  (let ((records (checkdata ii (car ii) (car ii) 0 0)))
      (format t "~a ~A" (car records) (cadr records))))

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
  (let ((n (parse-integer (read-line stream)))
        (ii (split-and-parse (read-line stream))))
    (solve-me n ii)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
