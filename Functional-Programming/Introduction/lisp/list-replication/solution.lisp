(defun f (n list)
  ;; (format t "~A ~A~%" n list)
  (if (not list)
      nil
      (progn
        (loop for x from 1 to n do (format t "~d~%" (car list)))
        (f n (cdr list)))))

(defun read-list ()
  (let ((n (read *standard-input* nil)))
    (if (null n)
        nil
        (cons n (read-list)))))

(format t "~{~d~%~}" (f (read) (read-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun solution (&optional stream)
;;   (let ((inp (loop
;;                 for l = (read-line stream nil nil)
;;                 until (null l)
;;                 collect l)))
;;     (f (parse-integer (car inp)) (cdr inp))))

;; ;; (solution) ; uncomment this when running on hacker-rank

;; (defun main ()
;;   (with-open-file (s (make-pathname
;;                       :directory
;;                       (pathname-directory
;;                        (parse-namestring *load-pathname*))
;;                       :name "input0" :type "txt"))
;;     (solution s)))

;; (main)
