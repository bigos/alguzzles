(defun divides (x y)
  (zerop (mod x y)))

(defun solve-me (n i c incr)
  (if (every (lambda (x) (divides c x)) i)
      (format t "~&~A~%" c)
      (solve-me n i (+ c incr) incr)))

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
  (let* ((n (parse-integer (read-line stream)))
         (i (sort (split-and-parse (read-line stream)) '>))
         (fst (elt i 0))
         (snd (elt i 1)))
    (solve-me n (subseq i 2) (* fst snd) (* fst snd))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input07" :type "txt"))
    (solution s)))

(main)
