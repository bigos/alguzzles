;;; ----------------------------------------------------------------------------

;;; get factors of all elements in lb
;;; find factors X that are common to all elements of lb
;;; sort them
;;; remove factors X where not all elements in la are factors of X

(defun common-factors (l)
  (reduce 'intersection (map 'list 'factors l)))

(defun a-factors (la lb)
  (length
   (remove-if (lambda (x) (< (length x)
                             (length la)))
              (map 'list (lambda (x)
                           (intersection x la))
                   (map 'list 'factors (common-factors lb))))))

(defun factors (n &aux (lows '()) (highs '()))
  (do ((limit (1+ (isqrt n))) (factor 1 (1+ factor)))
      ((= factor limit)
       (when (= n (* limit limit))
         (push limit highs))
       (remove-duplicates (nreconc lows highs)))
    (multiple-value-bind (quotient remainder) (floor n factor)
      (when (zerop remainder)
        (push factor lows)
        (push quotient highs)))))

(defun solve-me (n m la lb)
  (declare (ignore n m))
  (format t "~A~%" (a-factors la lb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (let ((nm (split-and-parse (read-line stream)))
        (la (split-and-parse (read-line stream)))
        (lb (split-and-parse (read-line stream))))
    (solve-me (car nm) (cadr nm) la lb)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
