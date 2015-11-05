(defmacro mem-defun (name args body)
  (let ((hash-name (gensym)))
    `(let ((,hash-name (make-hash-table :test 'equal)))
       (defun ,name ,args
         (or (gethash (list ,@args) ,hash-name)
             (setf (gethash (list ,@args) ,hash-name)
                   ,body))))))

(mem-defun lcs (xs ys)
           (labels ((longer (a b) (if (> (length a) (length b)) a b)))
             (cond ((or (null xs) (null ys)) nil)
                   ((equal (car xs) (car ys)) (cons (car xs) (lcs (cdr xs) (cdr ys))))
                   (t (longer (lcs (cdr xs) ys)
                              (lcs xs (cdr ys)))))))

(defun find-different-count (s1 s2)
  (let ((ht1 (make-hash-table))
        (ht2 (make-hash-table))
        )
    (loop for c1 in s1 do
         (incf (gethash c1 ht1 0)))
    (loop for c2 in s2 do
         (incf (gethash c2 ht2 0)))
    (format t "~&----------------------~%")
    (maphash (lambda (k v)
               (format t "~@C: ~D~%" k v))
             ht1)
    (format t "~&=====~%")
    (maphash (lambda (k v)
               (format t "~@C: ~D~%" k v))
             ht2)
    (format t "~&++++++++++ ")
    (loop for c in s1 do
         (format t "~A ~A     " c (abs (- (gethash c ht1 0)
                                          (gethash c ht2 0)))))
    (format t "~&~A  ~A~%"  (length s1) (length (lcs s1 s2)))
    (terpri)))

(defun find-solution (l a)
  (let* ((half-l (/ l 2))
         (s1 (subseq a 0 (- half-l 0)))
         (s2 (subseq a half-l)))
    (find-different-count s1 s2)))

(defun solve-me (a)
  (let ((l (length a)))
    (format t "~&~A~%"
            (if (evenp l)
                (find-solution l a)
                -1))))

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
  (let* ((tc (parse-integer (read-line stream))))
    (dotimes (x tc)
      (solve-me
       (loop for c across (read-line stream) collect c)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
