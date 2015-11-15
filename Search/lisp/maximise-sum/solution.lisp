(defun indexed-subarray (a start end)
  (declare (optimize (speed 3) (safety 0)))
  (if (zerop start)
      (aref a end)
      (- (aref a end) (aref a (1- start)))))

;;; translation of following:
;; https://github.com/arcturus611/Learning-C/blob/master/HRe_CONTEST_max_mod_subarray.c
;;; wrong results
(defun max-mod-subarray (a n m)
  (loop for i from 0 below n do
       (setf (aref a i) (mod (aref a i) m)))
  (let ((max-ending-here (aref a 0))
        (max-so-far (aref a 0))
        (temp-sum)
        (temp-val))
    (loop for i from 1 below n do
         (setf temp-val (aref a i)
               temp-sum (mod (+ temp-val max-ending-here) m)
               max-ending-here (max temp-val temp-sum)
               max-so-far (max max-so-far max-ending-here)))
    max-so-far))

(defun max-subarray (a n m)
  (declare (optimize (speed 3) (safety 0))
           (inline indexed-subarray))
  (let ((indexes (make-array n))
        (my-max 0)
        (found nil))
    (loop
       for x across a
       for i = 0 then (1+ i)
       for y = (aref a 0) then (+ x y) do
         (setf (aref indexes i) (mod y m)))
    (loop for s from (1- n) downto 0 do
         (loop for x from s below n do
              (setq my-max (max my-max
                                (mod (indexed-subarray indexes s x)
                                     m)))))
    my-max))

(defun solve-me (nm ints)
  (let ((a (make-array (car nm) :initial-contents ints)))
    (princ (max-subarray a (car nm) (cadr nm)))
    (terpri)))

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
    (dotimes (x tc)
      (solve-me (split-and-parse (read-line stream))
                (split-and-parse (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input07" :type "txt"))
    (solution s)))

(main)
