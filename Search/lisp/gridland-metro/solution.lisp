(declaim (optimize (speed 3)))

(defun range-values (s e)
  (loop for x from (min s e) to (max s e) collect x))

(defun range-length (s e)
  (1+ (- (max s e) (min s e))))

(defun find-val (columns ranges)
  (if (null ranges)
      columns
      (- columns (apply 'range-length (car ranges)))))

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
  (let* ((nmk (split-and-parse (read-line stream)))
         (n (nth 0 nmk))
         (m (nth 1 nmk))
         (k (nth 2 nmk))
         (lm (make-array (list n m)
                         :initial-element 1
                         :element-type 'bit))
         (max-lamps (* n m)))
    (loop
       for l from 1 to k
       for rl = (split-and-parse (read-line stream))
       do
         (loop for c from (1- (cadr rl)) to (1- (caddr rl))
            do

              (if (zerop (aref lm (1- (car rl)) c))
                  (progn
                    (setf  (aref lm (1- (car rl)) c) 0))
                  (progn
                    (setf  (aref lm (1- (car rl)) c) 0)
                    (setf max-lamps (1- max-lamps)))
                  )
              )
         )
    (format t "~S~%"  max-lamps)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
