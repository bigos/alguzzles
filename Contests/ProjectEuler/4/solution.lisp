(defun product3 (n)
  (loop
     for x from 999 downto 100
     for r = (mod n x)
     for d = (/ n x)
     for e = (/ n d)
     for res = (and (zerop r)
                    (< 99 d 1000)
                    (< 99 e 1000))
     until res
     finally (return res)))

(defun palindrome-range ()
  (loop for x from 999 downto 100
     collect (parse-integer
              (concatenate 'string
                           (format nil "~3,'0d" x)
                           (reverse  (format nil "~3,'0d" x))))))

(defparameter *palindrome-range* (palindrome-range))

(defun solve-me% (x)
  (loop for p in *palindrome-range*
     for r = (if (< p x)
                 (product3 p)
                 nil)
     until r
     finally (return p)))

(defun solve-me (x)
  (format t "~&~d" (solve-me% x)))

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
  (let ((tc (parse-integer (read-line stream))))
    (loop for x from 1 to tc do (solve-me (parse-integer (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
      (solution s)))

(main)
