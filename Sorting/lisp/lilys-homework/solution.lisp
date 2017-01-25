(defun  solve-me (n aa)

  (let* ((bb (sort (copy-list aa) #'<))
         (swaps 0)
         (hi (make-hash-table))
         (arya (make-array (list n) :initial-contents aa))
         (aryb (make-array (list n) :initial-contents bb)))

    (labels ((swap-pos (a b)
               (format T "SWAPPING ~A ~A~%" a b)
               (setf
                (gethash (car a) hi) (cdr b)
                (gethash (car b) hi) (cdr a)
                (aref arya (cdr a)) (car b)
                (aref arya (cdr b)) (car a))
               (incf swaps))

             (dump-hash ()
               (format t "~&=== value => index  ")
               (maphash (lambda (k v) (format t "~a => ~A " k v)) hi)
               (format t "== ~s ~%" arya)
               )
             (swapme (x)
               (format t "~&~A~%" (list (aref arya x)
                                        'is-at-pos
                                        (gethash (aref arya x) hi)
                                        'has-to-be-swapped-with
                                        (aref aryb x)
                                        'which-as-at-pos
                                        (gethash (aref aryb x) hi)))

               (list (cons (aref arya x) (gethash (aref arya x) hi))
                     (cons (aref aryb x) (gethash (aref aryb x) hi))))
             )

      (format t "=== unsorted ~A sorted ~A  ary ~a  ~A~%" aa bb arya aryb)
      (loop
         for a in aa
         for i = 0 then (1+ i)
         do
           (setf (gethash a hi) i))

      (dump-hash)

      (swapme 0)
      (swap-pos (cons 2 0) (cons 1 3))
      (dump-hash)

      )

    (format t "~A~%" swaps)))

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
  (let ((n (parse-integer (read-line stream)))
        (aa (split-and-parse (read-line stream))))
    (solve-me n aa)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
