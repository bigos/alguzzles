(defun two-same (str)
  (eq (car str)
      (cadr str)))

(defun compress (rev-str last result)
  (format t "~&------~%~A~% ~A~% ~A~%" rev-str last result)
  (if (not rev-str)
      result
      (if (equalp last (car rev-str))
          (compress (cdr rev-str)
                    last
                    result)
          (compress (cdr rev-str)
                    (car rev-str)
                    (cons (cons (caar rev-str) (cdar rev-str)) result)
                    ))))

(defun count-occurence (str count result)
  ;; (format t "~A ~A ~A~%" str count result)
  (if (not str)
      result
      (count-occurence (cdr str)
                       (if (two-same str)
                           (1+ count)
                           1)
                       (cons (cons (car str) count) result)
                       )))

;; (solve-me "aabbbccccdeff")
(defun solve-me (str)
  (princ
   (compress
    (count-occurence (map 'list (lambda (x) x) str)
                     1
                     nil)
    nil
    nil)))

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
