;; 07:05:45PM

;; before
;; 12:40:22AM
;; after
;; 00:40:22

(defun solve-me (s)
  (let ((hr (subseq s 0 2))
        (hri 0)
        (mid (subseq s 2 8))
        (ampm (subseq s 8)))
    ;; (format t "~A ~A ~A~%~%" hr mid ampm)
    (setf hri (parse-integer hr))
    (format t "~2,'0d~A"
            (+ (if (equal ampm "AM")
                   (if (< hri 12)
                       0
                       -12)
                   (if (< hri 12) 12 0))
               hri)
            mid)))

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
  (solve-me (read-line stream)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
