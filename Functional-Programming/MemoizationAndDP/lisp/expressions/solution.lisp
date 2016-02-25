;;; (variations 2 3) => 000 .. 111
(defun variations (n l)
  (let ((buckets
         (make-array l :initial-element 0))
        (limit n)
        (results))
    (labels ((reset-bucket (level)
               (loop for x from 0 below level do (setf (elt buckets x) 0)))
             (variation (level)
               ;; get value and append it to results
               (when (zerop level )
                 (setf results
                       (append results
                               (list (loop for x from 0 below l
                                        collect (elt buckets x))))))
               ;; increase value
               (incf (elt buckets level))
               ;; go to next level if necessary
               (when (>= (elt buckets level) limit)
                 (when (< level (1- l))
                   (variation (1+ level))))
               ;; zero lower levels
               (reset-bucket level)
               (setf level 0)))
      (loop  while  (every (lambda (x) (< x  n)) buckets) do
           (variation 0)))
    results))

(defun permute (list)
  (if list
      (mapcan #'(lambda (x)
                  (mapcar #'(lambda (y) (cons x y))
                          (permute (remove x list))))
              list)
      '(()))) ; else

;; example imput
;; 22 79 21
;; with infix operators
;; 22 * 79 - 21
;; lisp version
;; (- (* 22 79) 21)


;; (combinator '(iced jam plain) 2)
(defun combinator (cc k)
  (cond ((zerop k) '(()))
        ((not cc) nil)
        (T (append
            (map 'list
                 (lambda (x) (cons (car cc) x))
                 (combinator cc (- k 1)))
            (combinator (cdr cc) k)))))

(defun opernums (nums oper acc)
  (if (not oper)
      acc
      (opernums (cdr nums)
                (cdr oper)
                (funcall (car oper) acc (car nums)))))

(defun calc (nums oper)
  (opernums (cdr nums) oper (car nums)))

(defun print-result (l o)
  (when o
      (format t "~A~a" (car o) (car l))
      (print-result (cdr l)
                    (cdr o))))

(defun solve-me (tc l)
  (let ((ops (loop for o in  (variations 3 (-  tc 1))
                collect (loop for x in o
                           collect (elt '(* + -) x)))))
    ;; (format t "args: ~A ~A ~A~%" tc l ops)
    (loop for o in ops
       for r = (mod (calc l o) 101) then (mod (calc l o) 101)
       ;; do
       ;;   (format t "~&~a ~A ~A~%" o (calc l o) r)
       until (zerop r)
       finally (progn
                 (format t "~A" (car l))
                 (print-result (cdr l) o)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (let ((tc (parse-integer (read-line stream)))
        (l (split-and-parse (read-line stream))))
    (solve-me tc l)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
