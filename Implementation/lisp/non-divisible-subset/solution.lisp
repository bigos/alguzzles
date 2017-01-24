(declaim (optimize (speed 3) (space 0)))

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

(defun divides-by (n d)
  (zerop (mod n d)))

(defparameter *cc* nil)
(defparameter *dd* nil)

(defun pushme-cc (x) (push x *cc*))
(defun pushme-dd (x) (push x *dd*))

(defun solve-me (n k aa)
  ;; (format t "=========== ~A ~A ~A~%" n k aa)
  (let ((found))
    (loop for x from n downto 2
       do
         (setf *cc* nil)
         (comb x aa 'pushme-cc)
         ;; (format t "cc: ~a~%" *cc*)
         (loop for y in *cc*
            do
              ;; (format t "### ~a~%" y)
              (setf *dd* nil)
              (comb 2 y 'pushme-dd)

              (when (every 'null
                           (loop for z in *dd*
                              collect (divides-by (apply #'+ z) k)))
                (setf found y))
            until found)
       until found)
    (unless found
      (loop for a in aa
         do
           (when (divides-by a k)
             (setf found (list a)))))
    (format t "~A~%" (length found))
    ))

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
