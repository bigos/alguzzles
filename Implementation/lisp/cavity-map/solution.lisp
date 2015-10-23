(defun checkaround (m r c)
  ;; (format t "~&called with ~A ~A ~A~%" m r c)
  (let ((res)
        (cell (aref m r c)))
    (loop for coord in (list (cons (1- r) c)
                             (cons r (1- c)) (cons r (1+ c))
                             (cons (1+ r) c))
       for r = (car coord)
       for c = (cdr coord) do
         (setf res (concatenate 'list res (list (aref m r c))) )
         ;; (format t "~&using ~A ~A    ~A | ~A   ~A~%" r c coord res cell)
         )
    (every (lambda (x) (< x cell)) res)))

(defun solve-me (n ints)
  ;; (format t "~A" ints)
  (let ((m (make-array (list n n))))
    (loop for r from 0 below n do
         (loop for c from 0 below n do
              (setf (aref m r c) (elt ints (+ (* n r) c)))))
    ;; (princ m)
    ;; (terpri)
    (loop for r from 0 below n do
         (loop for c from 0 below n do
              (if (and (and (> r 0) (< r (1- n)))
                       (and (> c 0) (< c (1- n))))
                  (format t "~A" (if (checkaround m r c)
                                    "X"
                                    (aref m r c)))
                  (format t "~a" (aref m r c))))
         (format t "~%"))))

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

(defun parsed-digits (string)
  (loop for c across string
     collect (parse-integer (format nil "~c" c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((n (parse-integer (read-line stream)))
        (ints))
    (dotimes (x n)
      (setf ints (concatenate 'list ints (parsed-digits (read-line stream)))))
    (solve-me n ints)))

;; (solution) ; uncomment this when running on hacker-rank


(with-open-file (s (make-pathname
                    :directory
                    (pathname-directory
                     (parse-namestring *load-pathname*))
                    :name "input0" :type "txt"))
  (solution s))
