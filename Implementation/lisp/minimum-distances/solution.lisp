(defparameter *result* -1)

(defun set-result (r rv c cv)
  (when (eq cv rv)
    (let ((res (- c r)))
      (when (> res 0)
        (cond ((eq *result* -1)
               (setf *result* res))
              ((< res *result*)
               (setf *result* res)))))))

(defun solve (sa a)
  (loop
     for r from 0 below sa
     for rv = (nth r a) then (nth r a) do
       (loop
          for c from r below sa
          for cv = (nth c a) then (nth c a) do
            (set-result r rv c cv)))
  (format t "~A~%" *result*))

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
  (let ((sa (parse-integer (read-line stream)))
        (a (split-and-parse (read-line stream))))
    (solve sa a)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
