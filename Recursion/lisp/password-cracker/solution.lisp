(defun solve-me (n keys pass)
  (format t "~&==== ~A~A~A~%" n keys pass)
  (format t "---------- ~A~%" (loop for k in keys collect (list k (find-indexes k pass nil))))
  (crack pass keys '()))


(let ((cache (make-hash-table)))
  (defun crack (password keys res)

    (loop for k in keys do
      (or (gethash k cache)
          (setf (gethash k cache) k)))

    (maphash (lambda (k v)
               (format t "~A ~A~%" k v)  )
             cache)))

(defun find-indexes (substring string acc)
  (let ((nv (if acc
                (search substring string :start2 (1+ (car acc)))
                (search substring string))))

    (if (null nv)
        acc
        (find-indexes substring
                      string
                      (cons nv acc)))))

(defun find-keys (string keys i)
  (loop for k in keys
        for nv = (search k string :start2 i)
        when (eq nv i)
          collect
          (list k i)))

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
  (let ((tc (parse-integer (read-line stream))))
    (loop for c from 1 to tc
          do (solve-me
              (parse-integer (read-line stream))
              (split-by-one-space (read-line stream))
              (read-line stream)))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
