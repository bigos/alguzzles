(defun num2digits (n)
  (format nil "~D"
   (loop for x across n
      sum (- (char-code x)
             (char-code #\0)))))

(defun superdigit (p)
  (if (eq (length p) 1)
      p
      (superdigit (num2digits p))))

(defun solve-me (n k)
  (format t "~A~%"
          (superdigit
           (with-output-to-string (x)
             (dotimes (_ k)
               (format x "~D" n))))))

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
  (let ((nk (split-and-parse (read-line stream))))
    (solve-me (car nk) (cadr nk))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
