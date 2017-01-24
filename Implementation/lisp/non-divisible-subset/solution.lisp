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
        (h (make-hash-table)))
    (format t "=========== ~A ~A ~A~%" n k aa)
    (loop for x in aa do (incf (gethash x h 0)))

    (maphash (lambda (hk hv)
               (format T "~A found ~A - ~A~%" hk hv
                       (if (gethash (- k hk) h)
                           (list (- k hk)
                                 (gethash (- k hk) h ))
                           nil)))
             h)

    ))

;; for case 03
;; we can have
;; 2 sets of 4 and 1
;; 2 sets of 3 and 2
;; 10 - 4sets = 6

;; for case 02
;; we can have
;; 1 set of 7 and 2
;; 6 - 1set = 5

;; for case 01
;; we can have
;; 0 sets of 2 and 3
;; 5 - 0 = 5

;; for case 0
;; we caan have
;; 1 set of 2 and 1
;; 4 - 1 = 3
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
                      :name "input0" :type "txt"))
    (solution s)))

(main)
