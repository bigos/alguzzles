(defun find-different-count (s1 s2)
  ;; TODO: finish me
  (format nil "~A ~A" (length s1) (intersection s1 s2))
  )

(defun find-solution (l a)
  (let* ((half-l (/ l 2))
         (s1 (subseq a 0 (1- half-l)))
         (s2 (subseq a half-l)))
    (find-different-count s1 s2)))

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
      (solve-me
       (loop for c across (read-line stream) collect c)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
