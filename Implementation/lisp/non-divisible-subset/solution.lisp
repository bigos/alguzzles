(defun combinator (cc k)
  (cond ((zerop k) '(()))
        ((not cc) nil)
        (T (append
            (map 'list
                 (lambda (x) (cons (car cc) x))
                 (combinator cc (- k 1)))
            (combinator (cdr cc) k)))))

(defun subsets-of-len (aa l)
  (remove-if
   (lambda (x)
     (< (length (remove-duplicates x)) l))
   (combinator  aa l)))

(defun subset-pairs (aa)
  (remove-if (lambda (x) (eql (car x) (cadr x))) (combinator aa 2)))

(defun anygood (aa i k)
  (notevery 'null
            (map 'list
                 (lambda (x)
                   (every 'null
                          (map 'list (lambda (y)
                                       (zerop (mod (apply '+ y)
                                                   k)))
                               (subset-pairs x))))
                 (subsets-of-len aa i))))

(defun solve-me (n k aa)
  (declare (ignore n))
  (princ
   (loop for i from 1 to k
      for v = (anygood aa i k)
      when v
      maximize i)))

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
