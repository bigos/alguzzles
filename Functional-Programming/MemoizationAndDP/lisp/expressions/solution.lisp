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
  (let ((ops (combinator '(* + -) (- tc 1))))
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
