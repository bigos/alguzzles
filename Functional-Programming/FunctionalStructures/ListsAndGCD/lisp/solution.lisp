;;   (if (not l)
;;       (list (reverse r1) (reverse r2))
;;       (split-list (cddr l)
;;                   (cons (car l) r1)
;;                   (cons (cadr l) r2))))

;;; mutually recursive with get-power
(defun get-base (l res)
  (if (not l)
      res
      (get-power (cdr l) (car l) res )))

;;; mutually recursive with get-base
(defun get-power (l base res)
  (get-base (cdr l) (push (expt base (car l)) res)))

(defun get-result (l)
  (let ((number (apply #'* (get-base l nil))))
    (format t "==== ~A ~A~%" l number)))

(defun solve-me (l)
  (format t "~A~%" l)
  (map 'list (lambda (x) (get-result x)) l))

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
  (let ((tc (parse-integer (read-line stream))))
    (solve-me (loop for x from 0 below tc
                 collect (split-and-parse (read-line stream))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)

