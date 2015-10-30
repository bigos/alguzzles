;; hmm think of Knuth-Morris-Pratt substring search

(defun prefcnt (s1 s2)
  (loop
     for cx = 0 then (1+ cx)
     for c1 in s1
     for c2 in s2
     while (eq c1 c2)
     finally (return cx)))

(defun find-solution (str)
  (let ((c (loop for suffixes = str then (cdr suffixes)
              until (null suffixes)
              sum (prefcnt suffixes str))))
    (format t "~&~A~%" c)))

;; Explanation:
;; For the first case, the suffixes of the string are "ababaa", "babaa", "abaa", "baa", "aa" and "a". The similarities of these strings with the string "ababaa" are 6,0,3,0,1, & 1 respectively. Thus, the answer is 6 + 0 + 3 + 0 + 1 + 1 = 11.

(defun solve-me (tc strings)
  (declare (ignore tc))
  (loop for string in strings do
       (find-solution (loop for c across string collecting c))))

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
  (let* ((tc (parse-integer (read-line stream)))
         (strings ( loop for x from 1 to tc collect (read-line stream))))
    (solve-me tc strings)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input06" :type "txt"))
    (solution s)))

(main)
