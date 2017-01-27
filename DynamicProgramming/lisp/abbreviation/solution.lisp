(defparameter *char-a-upper-pos-list* nil)
(defparameter *char-a-split* nil)

(defun split-string-at-positions (str positions acc)
  (cond ((or (null positions)
             (equalp str ""))
         (reverse acc))
        (T
         (split-string-at-positions str
                                    (cdr positions)
                                    (push (subseq str
                                                  (car positions)
                                                  (cadr positions))
                                          acc)))))

(defun solve-me (a b)
  (format t "~A ~A~%~%" a b)

  ;; starting at pos 0 find all 'a' or first 'A' without finding other upper case character
  ;; for all finds of 'a'/'A' find all 'b' or first 'B' without finding other upper case character
  ;; for all finds of 'b'/'B' find all 'c' or first 'C' without finding other upper case character

  )

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
  (let ((q (parse-integer (read-line stream))))
    (loop for x from 1 to q
       do
         (solve-me
          (loop for c across
                (car (split-by-one-space (read-line stream)))
                collect c)
          (loop for c across
               (car (split-by-one-space (read-line stream)))
               collect c)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
