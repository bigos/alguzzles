(declaim (optimize (speed 3) (space 0)))

(defun combinator (cc k)

  (cond ((zerop k) '(()))
        ((not cc) nil)
        (T (append
            (map 'list
                 (lambda (x) (cons (car cc) x))
                 (combinator cc (- k 1)))
            (combinator (cdr cc) k)))))

;; (remove-duplicates (remove-if (lambda (x) (not  (eql 2 (length x)))) (map 'list 'remove-duplicates (combinator '(1 7 2 4) 4))) :test'equalp)

(defun subset-pairs (aa)
  (remove-if (lambda (x) (eql (car x) (cadr x))) (combinator aa 2)))

(defun divides-by (n d)
  (zerop (mod n d)))

(defun solve-me (n k aa)
  ;(format t "=========== ~A ~A ~A~%" n k aa)
  (let ((found))
    (loop for subset in (sort
                         (map 'list #'remove-duplicates
                              (combinator aa n))
                         (lambda (x y) (> (length x) (length y))))
       for sp = (subset-pairs subset)
       do

         (if sp
             (setf found (every 'null (loop for s in sp
                                         collect
                                           (divides-by (apply '+ s) k))))
             (setf found (not (divides-by (car subset) k)))
             )
       until found
       finally
         (format t "~&~A~%" (length subset)))))

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
    (solve-me (car nk) (cadr nk) (sort aa '>))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input03" :type "txt"))
    (solution s)))

(main)
