(defun odd-rec (a p acc)
  (cond ((null a) acc)
        (T (odd-rec (cdr a)
                    (car a)
                    (cons (if (and (odd (caar a))
                                   (cadr p))
                              (list (caar a) (cadr p))
                              (car a))
                          acc)))))

(defun odd-positions (a)
  (let ((oi nil))
    (loop
       for l in a
       for i = 0 then (1+ i)
       for p = oi then (cond ((and (null p) (odd l)) i)
                             ((and p (odd l)) nil)
                             (T p))
       collect (list l p))))

(defun odd (n) (not (evenp n)))

(defun solvable (a)
  (evenp (loop for l in a when (odd l) count l)))

(defun solve-me (n bn)
  (if (solvable bn)
      (format t "~A~%"
              (loop for cc in  (odd-rec  (odd-positions bn) nil nil)
                 when (cadr cc) collect cc))

      (format T "NO~%")))

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
  (let ((n (parse-integer (read-line stream)))
        (bn (split-and-parse (read-line stream))))
    (solve-me n bn)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
