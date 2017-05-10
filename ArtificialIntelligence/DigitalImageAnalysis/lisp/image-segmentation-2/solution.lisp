;;; it should return 2 objects made of 8-connected pixels

(defun solve-me (h w d)
  (loop for r from 0 below h
     do (loop for c from 0 below w
           do (format t "~a" (if (zerop (aref d r c))
                                 " "
                                 "*"))
           finally (terpri))))

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
  (let* ((h 4)
         (w 12)
         (d (make-array (list h w))))
    (loop for r from 0 below h
       for rc = (loop for x across (read-line stream)
                   collect (- (char-code x) (char-code #\0)))
       for rw = (length rc)
       do (loop for cd in rc
             for c = 0 then (1+ c)
             do (setf (aref d r c) cd))
       finally (setf w rw))
    (solve-me h w d)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
