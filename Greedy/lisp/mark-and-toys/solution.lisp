(defun solve-me (n k prices)
  (declare (ignore n))
  ;; (format t "~A ~A ~A" n k prices)
  (princ
   (loop
      for p in prices
      for tc = (car prices) then (+ tc p)
      for c = 0 then (1+ c)
      ;; do (format t "~&~A ~A ~A~%" p tc c)
      until (> tc k)
      finally (return c))))

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

(defun string-to-characters (string)
  (loop for c across string
     collect c))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((nk (split-and-parse (read-line stream)))
        (prices (split-and-parse (read-line stream))))
    (solve-me (car nk) (cadr nk) (sort prices '<))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
