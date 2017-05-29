(defun negative (n)
  (- 0 n))

(defun dpos (pos offset)
  (list (+ (car pos) (car offset))
        (+ (cadr pos) (cadr offset))))

(defun bot-vector (pos goal)
  (list (- (car goal) (car pos))
        (- (cadr goal) (cadr pos))))

(defun cell-at-position (position grid)
  (aref grid (car position) (cadr position)))

(defun on-dirt (bot-position grid)
  (eq #\d (cell-at-position bot-position grid)))

(defun position-coordinates (radius sx xfn sy yfn)
  (loop for r from 1 to radius
     for x = sx then (funcall xfn x)
     for y = sy then (funcall yfn y)
     collect (list x y)))

(defun positions (radius)
  (let ((ne (position-coordinates radius
                                  0 '1+
                                  (negative radius) '1+))
        (es (position-coordinates radius
                                  radius '1-
                                  0 '1+))
        (sw (position-coordinates radius
                                  0 '1-
                                  radius '1-))
        (wn (position-coordinates radius
                                  (negative radius) '1+
                                  0 '1-)))
    `(,@ne ,@es ,@sw ,@wn)))

(defun dirt-positions (bot-position radius grid)
  (loop
     for p in (positions radius)
     for dpos = (dpos bot-position p)
     when (and (and (<= 0 (car dpos) 4)
                    (<= 0 (cadr dpos) 4))
              (on-dirt dpos grid))
     collect dpos))

(defun direction (vector)
           (cond
             ((< (car vector) 0) "LEFT")
             ((> (car vector) 0) "RIGHT")
             ((< (cadr vector) 0) "UP")
             ((> (cadr vector) 0) "DOWN")))

(defun get-direction (bot-position grid)
  (let ((dirt-positions (loop for radius from 1 to 8
                           for positions = (dirt-positions bot-position radius grid)
                           until (some (lambda (p)
                                         (on-dirt p grid))
                                       positions)
                           finally (return  positions))))
    (direction (bot-vector bot-position (car dirt-positions)))))

(defun next-move (bot-position grid)
  (format t "~A~%"
          (if (on-dirt bot-position grid)
              "CLEAN"
              (get-direction bot-position grid))))

(defun solve-me (grid bot-position)
  ;; (format t "~A~%~A~%" grid bot-position)
  ;; (format t "~A~%" (aref grid 0 0))
  (next-move bot-position grid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-string (length string)
  (loop for i from 0 below length collect (char string i )))

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
  (let* ((n 5)
         (bot-position (split-and-parse (read-line stream)))
         (grid (make-array '(5 5)
                           :initial-contents (loop for x from 0 below n
                                                collect (split-string n (read-line stream))))))
    (solve-me grid bot-position)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
