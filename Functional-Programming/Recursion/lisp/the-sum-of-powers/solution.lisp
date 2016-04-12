(defun powers (l p)
  (loop for x from 1 to l
     for y = (expt x p)
     when (<= y l) collect y
     until (>= y l)))

(defun res-find (wanted powers visited found)
  (dolist (p powers)
    (let (current-total (apply '+ (cons p visited)))
      (cond ((eq current-total wanted)
             ;; add to found
             )
            ((> current-total wanted)
             ;;  give op on the option
             )
            (T
             ;; else try more
             )))))

(defun sums-of-powers (total power)
  "number of ways unique numbers raised to power can be added up"
  (length (res-find total (powers total power) '() nil)))

(defun solve-me (x n)
  (print (sums-of-powers x n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (solve-me (parse-integer (read-line stream))
            (parse-integer (read-line stream))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
