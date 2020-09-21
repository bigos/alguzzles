(declaim (optimize (speed 3)))

(defun add-1-str (s)
  (format nil "~a" (1+ (parse-integer s))))

;; (solve-inner "12345" 0 1)
(defun solve-inner (str begi en)
  (let* ((nx (add-1-str (subseq str begi en)))
         (lx (length nx))
         (ne (+ en lx)))

    (if (<= ne (length str))
        (let* ((nb  en)
               (expstr (subseq str nb ne)))
          ;; (format t "~A ~A ~A ~A ~A --- ~S~%" str nx lx nb ne expstr)

          (if (equalp expstr nx)
              (solve-inner str en ne)
              'failure2))
        (progn
          (if (eq en (length str))
              'success
              'failure1)))))

(defun solve-me (s)
  ;; (format t "~A ------~%" s)
  (let ((outcome
          (loop for l from 1 below (length s)
                for res = (solve-inner s 0 l)
                until (eq res 'success)
                finally (return (cons res (subseq s 0 l))))))

    (if (eq (car outcome) 'success)
        (format t "YES ~A~%" (cdr outcome))
        (format t "NO~%"))))

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
  (let ((sl (parse-integer (read-line stream))))
    (loop for x from 1 to sl do
      (solve-me
       (read-line stream)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
