(defun ud2l (n cc)
  (loop for x from 0 below n
     collect (if (equal "U"
                        (subseq cc x (1+ x)))
                 1
                 -1)))

(defun topo (l)
  (loop for s in l
     when (or (equal s '(-1 1))
              (equal s '(0 -1)))
       collect s))

(defun solve-me (n cc)
  (let ((level 0))
    (format t "~a"
            (/ (length (topo
                        (loop for s in (ud2l n cc)
                           collect (list level s)
                           do
                             (setf level (+ level s)))))
               2))))

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
        (cc (read-line stream)))
    (solve-me n cc)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
