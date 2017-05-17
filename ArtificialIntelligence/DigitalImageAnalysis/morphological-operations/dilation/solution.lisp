;;;

(declaim (optimize (debug 3) (speed 0)))

(defun los2arr (los &optional (fn 'parse-integer))
  "Load list of strings LOS to array applying FN to every 1 character long substring."
  (let ((w (length (first los)))
        (h (length los)))
    (let ((d (make-array (list h w))))
      (loop for r from 0 below h
         do (loop for c from 0 below w
               do (setf
                   (aref d r c)
                   (funcall fn (subseq (elt los r) c (1+ c))))))
        d)))

(defun coord-op (fn c1 c2)
  (list (funcall fn (first c1) (first c2))
        (funcall fn (second c1) (second c2))))

(defun marked-neighbours (pr pc d m)
  (let ((d-dim (array-dimensions d))
        (m-dim (array-dimensions m))
        (m-orig '(1 1))
        (current)
        (neighbours))
    (loop for r from 0 below (second m-dim)
       do (loop for c from 0 below (first m-dim)
             do (setf current (list (- (+ pr r) (first m-orig))
                                    (- (+ pc c) (second m-orig))))
               (push (if (and (>= (first current) 0)
                              (>= (second current) 0)
                              (< (first current) (first d-dim))
                              (< (second current) (second d-dim)))
                         (aref d (first current) (second current))
                         'outside)
                     ;; current
                     neighbours)))
    neighbours))

(defun solve-me (data mark)
  (let* ((d (los2arr data))
         (d-dim (array-dimensions d))
         (m (los2arr mark))
         (m-dim (array-dimensions m))
         (m-origin (cons 1 1))
         (found-points))

    (loop for r from 0 below (second d-dim)
       do (loop for c from 0 below (first d-dim )
             do (when (some
                       (lambda (x) (eq x 1))
                       (marked-neighbours r c d m))
                  (push (list r c) found-points)))) ; marker coordinates

    (format t "~A~%" (length found-points))))

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

(defun read-all-data (stream)
  (loop for line = (read-line stream nil 'eof)
     until (equal line 'eof)
     collect line))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((data (read-all-data stream))
        (mark '("111" "111" "111")))
    (solve-me data mark)))

;; (solution) ; uncomment this when running on hacker-rank
(solve-me
 (list "0000000000" "0111111100" "0000111100" "0000111100" "0001111100" "0000111100" "0001100000" "0000000000" "0000000000")
 '("111" "111" "111"))

;; (defun main ()
;;   (with-open-file (s (make-pathname
;;                       :directory
;;                       (pathname-directory
;;                        (parse-namestring *load-pathname*))
;;                       :name "input0" :type "txt"))
;;     (solution s)))

;; (main)
