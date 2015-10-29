(defun find-solution (str)
  (loop for suffixes = str then (cdr suffixes)
     until (null suffixes)
     sum (loop
            for cx = 0 then (1+ cx)
            for c1 in suffixes
            for c2 in str
            while (eq c1 c2)
            finally (return cx)
              )))

(defun find-ar-solution (ar)
  (let ((l (length ar)))
    (loop for x from 0 below l
       for suffixes = (subseq ar x) ;;;;;;;; optimise me
       sum (loop
              for cx = 0 then (1+ cx)
              for c1 across suffixes
              for c2 across ar
              while (eq c1 c2)
              finally (return cx)
                ))))

;; Explanation:
;; For the first case, the suffixes of the string are "ababaa", "babaa", "abaa", "baa", "aa" and "a". The similarities of these strings with the string "ababaa" are 6,0,3,0,1, & 1 respectively. Thus, the answer is 6 + 0 + 3 + 0 + 1 + 1 = 11.

(defun solve-me (tc strings)
  (declare (ignore tc))
  (loop for string in strings do
       (find-ar-solution (make-array (length string) :initial-contents (loop for c across string collecting c)))))

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
