(declaim (optimize (debug 3)))

;;; not solved yet


(defun solve-me (d)
  (format t "the data: ~s~%" d))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-array-for-reading ()
  (make-array '(500)
              :element-type 'base-char
              :fill-pointer 0
              :adjustable T))

(defun split-by-char (string char)
  (loop for i = 0 then (1+ j)
        as j = (position char string :start i)
        collect (subseq string i j)
        while j))

(defun read-all-data (stream)
  (let ((contents (make-string (file-length stream))))
    (read-sequence contents stream)
    contents))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((d (read-all-data stream)))
    (solve-me d)))

  ;; (solution) ; uncomment this when running on hacker-rank


(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
