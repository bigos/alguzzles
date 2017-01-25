(declaim (optimize (speed 3)))


;;; (comb 3 '(1 2 4 7) #'print)
(defun comb (m list fn) ; without repetition
  (labels ((comb1 (l c m)
             (when (>= (length l) m)
               (if (zerop m) (return-from comb1 (funcall fn c)))
               (comb1 (cdr l) c m)
               (comb1 (cdr l) (cons (first l) c) (1- m)))))
    (comb1 list nil m)))

(defun combinator (cc k) ; with repetition
  (cond ((zerop k) '(()))
        ((not cc) nil)
        (T (append
            (map 'list
                 (lambda (x) (cons (car cc) x))
                 (combinator cc (- k 1)))
            (combinator (cdr cc) k)))))

(defun solve-me (n k aaa)
  (let ((aa (loop for ax in aaa collect (mod ax k)))
        (h (make-hash-table))
        (useful-numbers-hash (make-hash-table))
        (result))
    ;; (format t "=========== ~A ~A ~A~%" n k aa)
    (loop for x in aa do (incf (gethash x h 0)))

    (maphash (lambda (hk hv)
               ;; (format T "~A found ~A - ~A~%" hk hv
               ;;         (if (gethash (- k hk) h)
               ;;             (list (- k hk) (gethash (- k hk) h )) nil))
               (if (gethash (- k hk) h)
                   (setf (gethash hk useful-numbers-hash)
                         (list (- k hk)
                               (if (eql hk (- k hk))
                                   (gethash hk h)
                                   (min (gethash hk h)
                                        (gethash (- k hk) h)))))))
             h)
    ;; (maphash (lambda (uk uv) (format T "^^^^ ~A ~A~%" uk uv))
    ;;          useful-numbers-hash)
    (setf result
          (loop for x from (ceiling (/ k 2)) below k
             collect (if (gethash x useful-numbers-hash)
                         (cadr (gethash x useful-numbers-hash))
                         0)))
    (format t "~A~%" (if result
                         (- n (loop for r in result
                                 sum r))
                         1))))

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
  (let ((nk (split-and-parse (read-line stream)))
        (aa (split-and-parse (read-line stream))))
    (solve-me (car nk) (cadr nk) aa)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input03" :type "txt"))
    (solution s)))

(main)
