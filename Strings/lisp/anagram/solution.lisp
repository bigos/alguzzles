(defun levenshtein (a b)
  (let* ((la  (length a))
         (lb  (length b))
         (rec (make-array (list (1+ la) (1+ lb)) :initial-element nil)))
    (labels ((leven (x y)
               (cond
                 ((zerop x) y)
                 ((zerop y) x)
                 ((aref rec x y) (aref rec x y))
                 (t (setf (aref rec x y)
                          (+ (if (char= (char a (- la x)) (char b (- lb y))) 0 1)
                             (min (leven (1- x) y)
                                  (leven x (1- y))
                                  (leven (1- x) (1- y)))))))))
      (leven la lb))))

(defun find-solution (l a)
  (let* ((half-l (/ l 2))
         (s1 (sort (subseq a 0 (- half-l 0)) 'char<))
         (s2 (sort (subseq a half-l) 'char<)))
    (format t "~&~A ~A~%" s1 s2)
    (levenshtein s1 s2)))

(defun solve-me (a)
  (let ((l (length a)))
    (format t "~&~A~%"
            (if (evenp l)
                (find-solution l a)
                -1))))

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
  (let* ((tc (parse-integer (read-line stream))))
    (dotimes (x tc)
      (solve-me (read-line stream)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
