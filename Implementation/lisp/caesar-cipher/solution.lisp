(defun decrypt (c k)
  (let ((a2z 26))
    (setf k (mod k a2z))
    (code-char (cond ((<= (char-code #\a) (char-code c) (char-code #\z))
                      (if (> (+ k (char-code c)) (char-code #\z))
                          (+ k (- (char-code c) a2z))
                          (+ k (char-code c)))
                      )
                     ((<= (char-code #\A) (char-code c) (char-code #\Z))
                      (if (> (+ k (char-code c)) (char-code #\Z))
                          (+ k (- (char-code c) a2z))
                          (+ k (char-code c))))

                     (T  (char-code c))
                     ))))

(defun solve-me (n s k)
  (loop for c across s do
       (princ (decrypt c k))))

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
         (s (read-line stream))
         (k (parse-integer (read-line stream))))
    (solve-me n s k)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
