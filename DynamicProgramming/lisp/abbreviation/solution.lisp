(defparameter *found* nil)

(defun split-string-at-positions (str positions acc)
  (cond ((or (null positions)
             (equalp str ""))
         (reverse acc))
        (T
         (split-string-at-positions str
                                    (cdr positions)
                                    (push (subseq str
                                                  (car positions)
                                                  (cadr positions))
                                          acc)))))

(defun find-pos (str start seek-chars)
  ;; (format t "~&seeking ~S~%" seek-chars)
  (let ((acc))
    (loop
       for c across (subseq str start)
       for p = start then (1+ p)
       do
         (when (member c seek-chars)
           (push p acc))
       until (upper-case-p c)
       finally
         (when (and (null seek-chars)
                    (eql (1+ p) (length str))
                    (lower-case-p (elt str p)))
           (push (1+ p) acc)))
    acc))

;;; (solve-me "daBcd" "ABC" 0 0  )
(defun solve-me (a b  ap bp  )
  ;; (format t "~A ~A  ~A ~A  ~%~%" a b ap bp )
  (if (or
          (>= bp (length b)))
      'zzz
      (progn
        (let ((apx-positions (find-pos a ap (list (elt b bp)
                                                  (char-downcase
                                                   (elt b bp))))))
          (loop for apx in apx-positions
             do
               ;(format t "~%------ ~s~%" apx)
               (solve-me a b (1+ apx) (1+ bp)))
           (when (eql (car apx-positions)
                      (1- (length a)))
             (progn
               ;; (format t "~&fouuuunnnnddd~%")
               (setf *found* T)))))))

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
  (let ((q (parse-integer (read-line stream))))
    (loop for x from 1 to q
       do
         (setf *found* nil)
         (solve-me
          (format nil "~a " (car (split-by-one-space (read-line stream))))
          (format nil "~a " (car (split-by-one-space (read-line stream))))
          0
          0)
         (format t "~A~%"     (if *found* "YES" "NO")))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
