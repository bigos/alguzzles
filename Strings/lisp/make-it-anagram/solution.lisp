(defun charpos (char)
  (- (char-code char)
     (char-code #\a)))

(defun solve-me (s1 s2)
  ;; (format t "~&~s ~s~%" s1 s2)
  ;; strings are anagrams if they have the same characters
  ;; find and count characters in each string that does not belong to
  ;; common character list
  (let ((s1c (make-array 26 :initial-element 0))
        (s2c (make-array 26 :initial-element 0)))
    (loop for c in s1 do (incf (aref s1c (charpos c))))
    (loop for c in s2 do (incf (aref s2c (charpos c))))
    ;; (format t "~&~s~%" s1c)
    ;; (format t "~&~s~%" s2c)
    (format t "~&~A~%"
            (loop for y in (loop for x from 0 below 26
                                  collecting (abs (- (aref s1c x)
                                                     (aref s2c x))))
                summing y ))))

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
  (let ((s1 (string-to-characters (read-line stream)))
        (s2 (string-to-characters (read-line stream))))
    (solve-me s1 s2)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
