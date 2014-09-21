(require :sb-sprof)

(declaim (optimize speed))

(defun decent-binary (bin format-str)
  (let* ((nstr (format nil format-str bin))
         (count3 (count #\0 nstr))
         (count5 (count #\1 nstr)))
    (if (and (= (+ count3 count5)
                (length nstr))
             (zerop (rem count3 5))
             (zerop (rem count5 3)))
        (parse-integer (substitute #\5 #\1 (substitute #\3 #\0 nstr)))
        nil)))

(defun largest-decent (len)
  (let ((format-str (format nil "~~~D,'0b" len)))
    (loop for bin from (expt 2 len) downto 0
       for res = (decent-binary bin format-str)
       until res
       finally (return res))))

(defun solution (&optional stream)
  (let* ((tc (parse-integer (read-line stream)))
         (nums (loop repeat tc collect (parse-integer (read-line stream)))))
    (loop for x in nums
       for r = (largest-decent x)
       do (format t "~A~%" (if r r -1)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "sherlock-and-the-beast.input.11.txt"))
      (solution s))))

(repl-main)
;; using profiler
;; (sb-sprof:with-profiling (:max-samples 1000
;;                                        :report :flat
;;                                        :loop nil))
;; (sb-sprof:start-profiling)
;; (largest-decent 96388)
;; (sb-sprof:report)
;;
;;sequences for large numbers
;; (case (rem 7 3) ((0) 0) ((1) 10) ((2) 5) )
;; (loop for x from 1 to 19 for d = (largest-decent x) do
;;      (format t "x ~A = ~a | ~a ~a ~%" x d (count #\3 (format nil "~a" d)) (count #\5 (format nil "~a" d))))
