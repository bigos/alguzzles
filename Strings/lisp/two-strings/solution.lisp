(defun freq-hash (s)
  (let ((h (make-hash-table)))
    (loop for c across s do
         (setf (gethash c h) T))
    h))

;; loop though the string s1 until all lower case Latin letters or end of string are found
;; do the same with s2
;; compare hashes of letters of both strings and see if there is at least one key with values above 0
(defun solve-me (s1 s2)
  (let ((s1h (freq-hash s1))
        (s2h (freq-hash s2))
        (res))
    (loop for c from (char-code #\a) to (char-code #\z)
       do
         (if (and (gethash (code-char c) s1h)
                  (eq (gethash (code-char c) s1h)
                      (gethash (code-char c) s2h)))
             (setf res T)))
    (format t "~&~A~%" (if res "YES" "NO"))))

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
      (solve-me  (read-line stream)
                 (read-line stream)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
