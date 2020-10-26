(defun abnormal (ls)
  (let* ((listsorted (sort (copy-list ls) #'<))
	 (med (mapcar (lambda (x y) (if (= x y) 0 1)) ls listsorted))
	 (mm (apply #'+ med)))
    (cond ((= 0 mm) (format t "yes"))
	  ((= 1 mm) (format t "no"))
	  ((= 2 mm) (format t "yes~%swap ~d ~d" (1+ (position 1 med))
			    (1+ (position 1 med :from-end t))))
	  (t (let* ((beg (position 1 med))
		    (end (position 1 med :from-end t))
		    (ll1 (subseq ls beg (1+ end)))
		    (ll2 (subseq listsorted beg (1+ end))))
	       (if (equal ll1 (reverse ll2))
		   (format t "yes~%reverse ~d ~d" (1+ (position 1 med))
			   (1+ (position 1 med :from-end t)))
		   (format t "no")))))))

(let ((med (read-line)))
  (abnormal (read-from-string (format nil "(~a)" (read-line)))))
