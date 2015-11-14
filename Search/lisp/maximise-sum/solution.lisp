(defun sum-vals-mod (a m)
  (mod  (loop for x across a sum x) m))


(defun max-subarray (a m)
  (format t "array: ~A m: ~A" a m)
  (let ((n (length a))
        (ss))
    (loop for w from n downto 1 do
         (loop for s from 0 to(- n w) do
              (setf ss (subseq a s (+ s w)))
              (format t "~&====== ~A ~A ~A ~A" w s ss (sum-vals-mod ss m))
              )
         )
    (terpri)))

(defun solve-me (nm ints)
  (let ((a (make-array (car nm) :initial-contents ints)))
    (max-subarray a (cadr nm))))

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
                      :name "input0" :type "txt"))
    (solution s)))

(main)
