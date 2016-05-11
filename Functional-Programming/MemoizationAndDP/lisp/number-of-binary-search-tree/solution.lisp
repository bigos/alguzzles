;;; http://stackoverflow.com/questions/1701612/permutations-of-bst

(defun catalan1 (n)
  ;; factorial. CLISP actually has "!" defined for this
  (labels ((! (x) (if (zerop x) 1 (* x (! (1- x))))))
    (/ (! (* 2 n)) (! (1+ n)) (! n))))

;; ;; cache
;; (defparameter *catalans* (make-array 5
;;                                      :fill-pointer 0
;;                                      :adjustable t
;;                                      :element-type 'integer))
;; (defun catalan2 (n)
;;   (if (zerop n) 1
;;       ;; check cache
;;       (if (< n (length *catalans*)) (aref *catalans* n)
;;           (loop with c = 0 for i from 0 to (1- n) collect
;;                (incf c (* (catalan2 i) (catalan2 (- n 1 i))))
;;              ;; lower values always get calculated first, so
;;              ;; vector-push-extend is safe
;;              finally (progn (vector-push-extend c *catalans*) (return c))))))

;; (defun catalan3 (n)
;;   (if (zerop n) 1 (/ (* 2 (+ n n -1) (catalan3 (1- n))) (1+ n))))

;; ;;; test all three methods
;; (loop for f in (list #'catalan1 #'catalan2 #'catalan3)
;;    for i from 1 to 3 do
;;      (format t "~%Method ~d:~%" i)
;;      (dotimes (i 16) (format t "C(~2d) = ~d~%" i (funcall f i))))

;; answer modulo (108+7).
(defun solve-me (n)
  (mod (catalan1 n) (+ (expt 10 8) 7)))

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
  (let ((tc (parse-integer (read-line stream))))
    (loop for x from 1 to tc do
         (format t "~a~%" (solve-me
                           (parse-integer (read-line stream)))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
