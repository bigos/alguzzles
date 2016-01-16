(defun two-same (str)
  (eq (car str)
      (cadr str)))

(defun compress (str count result)
  (format t "~A ~A ~A~%" str count result)
  (if (not str)
      result
      (compress (cdr str)
                (if (two-same str)
                    (1+ count)
                    0)                
                (cons (cons count (car str)) result)
                                        )))

;; (solve-me "aabbbccccdeff")
(defun solve-me (str)
  (princ
   (reverse
    (compress (map 'list (lambda (x) x) str)
              0
              nil))))

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
  (let ((str (read-line stream)))
    (princ (solve-me str))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
