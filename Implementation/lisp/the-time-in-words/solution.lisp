(defun digit-word (n)
  (case n
    (0 "") (1 "one") (2 "two") (3 "three") (4 "four")
    (5 "five") (6 "six") (7 "seven") (8 "eight") (9 "nine")
    (10 "ten") (11 "eleven") (12 "twelve") (13 "thirteen") (14 "fourteen")
    (15 "fifteen") (16 "sixteen") (17 "seventeen") (18 "eighteen") (19 "nineteen")
    ))

(defun minpart (n)
  (let ((nx (if (<= n 30)
                n
                (- 60 n))))
    (format nil
            "~a ~a"
            (if (>= nx 20)
                (format nil "twenty ~a" (digit-word (- nx 20)))
                (digit-word nx))
            (if (= nx 1)
                "minute"
                "minutes"))))

(defun minute-word (h m)
  (let ((topast)
        (hr-part)
        (min-part))
    (if (= m 0)
        (format t "~A o' clock" (digit-word h))
        (progn
          (cond
            ((= m 30)
             (setf topast "past"
                   hr-part (digit-word h)
                   min-part "half"))
            ((= m 15)
             (setf topast "past"
                   hr-part (digit-word h)
                   min-part "quarter"))
            ((= m 45)
             (setf topast "to"
                   hr-part (digit-word (1+ h))
                   min-part "quarter"))
            ((< m 30)
             (setf topast "past"
                   hr-part (digit-word h)
                   min-part (minpart m))
             )
            ((> m 30)
             (setf topast "to"
                   hr-part (digit-word (1+ h))
                   min-part (minpart m))
             ))
          (format t "~A ~A ~A" min-part topast hr-part)))))

(defun solve-me (h m)
  (minute-word h m))

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
  (let ((h (parse-integer (read-line stream)))
        (m (parse-integer (read-line stream))))
    (solve-me h m)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
