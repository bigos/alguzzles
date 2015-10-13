(defun minimal-bribe (bw-nums bwc-costs)
  (let* ((bn (car bw-nums))
         (wn (cadr bw-nums))
         (bc (car bwc-costs))
         (wc (cadr bwc-costs))
         (cc (caddr bwc-costs))
         (bcc (* bn (+ wc cc)))
         (wcc (* wn (+ bc cc))))
    (+  (min (* bn bc) bcc)
        (min (* wn wc) wcc))))

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
  (dotimes (x (parse-integer (read-line stream)))
    (princ (minimal-bribe
            (split-and-parse (read-line stream))
            (split-and-parse (read-line stream))))
    (terpri)))

 ;; (solution) ; uncomment this when running on hacker-rank

;;; still need to add  removing vertices
(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "Implementation/lisp/taum-and-bday/"
                                    "input1.txt"))
      (solution s))))
