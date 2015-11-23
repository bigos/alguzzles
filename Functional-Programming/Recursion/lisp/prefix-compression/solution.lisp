;; (defun rec (x y pref)
;;   (format t "~&pref ~S ~S ~S~%" pref (subseq x 0 pref) (subseq y 0 pref))
;;   (if (or (not (equalp (subseq x 0 pref)
;;                        (subseq y 0 pref)))
;;           )
;;       (list (list pref
;;                   (subseq x 0 (1- pref)))
;;             (list (- (length x) pref)
;;                   (subseq x pref))
;;             (list (- (length y) pref)
;;                   (subseq y pref)))
;;       (rec x y (1+ pref))))

(defun solve-me (x y)
  (let ((prefix-len
         (loop for cx across x
            for cy across y
            while (eql cx cy)
            count cx)))
    (format t "~A ~A~%~A ~A~%~A ~A"
            prefix-len (subseq x 0 prefix-len)
            (- (length x) prefix-len) (subseq x prefix-len)
            (- (length y) prefix-len) (subseq y prefix-len)
            )))

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
  (let ((x (read-line stream))
        (y (read-line stream)))
    (solve-me x y)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
