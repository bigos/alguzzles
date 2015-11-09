(defun date-part (dmy units)
  (elt units (position dmy (list 'd 'm 'y))))

(defun solve-me (actual expected)
  ;; (format t "~A ~A" actual expected)
  (if (> (date-part 'y actual)
         (date-part 'y expected))
      10000
      (if (and (eq (date-part 'y actual)
                   (date-part 'y expected))
               (> (date-part 'm actual)
                  (date-part 'm expected)))
          (* 500 (- (date-part 'm actual)
                    (date-part 'm expected)))
          (if (and (eq (date-part 'm actual)
                       (date-part 'm expected))
                   (> (date-part 'd actual)
                      (date-part 'd expected)))
              (* 15 (- (date-part 'd actual)
                       (date-part 'd expected)))
              0))))

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
  (let ((d1 (split-and-parse (read-line stream)))
        (d2 (split-and-parse (read-line stream))))
    (princ (solve-me d1 d2))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input00" :type "txt"))
    (solution s)))

(main)
