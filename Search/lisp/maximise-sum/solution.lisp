(defun sum-vals-mod (a m)
  (declare (optimize (speed 3) (safety 0)))
  ;; (mod  (loop for x across a sum x) m)
  1
  )

(defun max-subarray (a m)
  (declare (optimize (speed 3) (safety 0)))
  (let ((n (length a))
        (ss)
        (ms 0)
        (max-ms-so-far 0)
        (max-ms (1- m)))
    (loop for w from 1 to n do
         (loop for s from 0 to(- n w) do
              (setq ms (sum-vals-mod
                        (subseq a s (+ s w))
                        m))
              (setq max-ms-so-far (max max-ms-so-far  ms))
            until (= ms max-ms))
       until (= ms max-ms))
    max-ms-so-far))

(defun solve-me (nm ints)
  (let ((a (make-array (car nm) :initial-contents ints)))
    (princ (max-subarray a (cadr nm)))
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
                      :name "input02" :type "txt"))
    (solution s)))

(main)
