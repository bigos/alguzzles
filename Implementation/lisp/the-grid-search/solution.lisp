;; find matching topleft
;; if rect matches wanted stop
;; otherwise repeat until running out of valid indexes

(defun solve-me (rcg rcp ar ap)
  ; (format t "~&~a~%~a~%~A~%~A~%~%" rcg  rcp ar ap)
  (let ((oho)
        (found)
        (tl (aref ap 0 0)))
    (loop for row from 0 to (- (car rcg) (car rcp))
       do (loop for col from 0 to (- (cadr rcg) (cadr rcp))
             do (when (eq tl (aref ar row col))
                  (setf oho T)
                  (loop for r from 0 below (car rcp)
                     do (loop for c from 0 below (cadr rcp)
                           do (when (not (eq (aref ap r c)
                                             (aref ar (+ row r) (+ col c))))
                                (setf oho nil))
                           while oho)
                     while oho)
                  (when oho (setf found T)))
             until found)
       until found)
    (format t "~&~A~%" (if found "found" "not found"))))

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
  (let ((tc (parse-integer (read-line stream)))
        (rcg) (rcp)
        (r) (p)
        (ar) (ap))
    (dotimes (x tc)
      (setf rcg (split-and-parse (read-line stream)))
      (setf r (loop for r below (car rcg)
                 collecting (loop for c across (read-line stream) collect c)))
      (setf ar (make-array rcg :initial-contents r))
      (setf rcp (split-and-parse (read-line stream)))
      (setf p (loop for r below (car rcp)
                 collecting (loop for c across (read-line stream) collect c)))
      (setf ap (make-array rcp :initial-contents p))
      (solve-me rcg rcp  ar ap))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
