(setf *print-circle* T)

(defstruct node
  (id)
  (neighbours))

(defun solve-me (n m edges s)
  (declare (ignore m))
  (let ((nodes (make-array (list (1+ n))))
        (res)
        (visited)
        (found))
    ;; (format t "args are ~A ~A ~A ~A~%" n m edges s)
    (labels ((bdf-shortest (s i)
               (setf visited nil
                     found nil)
               ;; (format t "~&======================= ~A ~A~%" s i)
               (bdf-rec s i 0)
               (unless found
                 ;; (format t ":-( :-( :-( )))not found ~A~%" i)
                 (format t "-1 ")))
             (neighbours (i)
               (node-neighbours (aref nodes i)))
             (not-yet-visited-neighbours (i)
               (remove-if (lambda (x) (some (lambda (y) (eq x y)) visited))
                          (neighbours i)))
             (bdf-rec (s e c)
               (if (eq s e)
                   (progn
                     (setf found T)
                     ;; (format t "!!!!!!! target found ~A~%" c)
                     (format t "~A " (* 6 c)))
                   (progn
                     (push s visited)
                     ;; (format t "visited ~A  c ~A~%" visited c)
                     ;; (format t "not yet visited neighbours ~A~%" (not-yet-visited-neighbours s))
                     (loop for nv in (not-yet-visited-neighbours s) do
                          (unless found
                            (bdf-rec nv e (1+ c)))
                          )))))

      (loop for i from 1 to n do
           (setf (aref nodes i) (make-node :id i)))
      (loop for vert in edges do
           (progn
             (push (car vert)
                   (node-neighbours (aref nodes (cadr vert))))
             (push (cadr vert)
                   (node-neighbours (aref nodes (car vert))))))
      (setf res (loop for i from 1 to n do
                     (unless (eq i s) (bdf-shortest s i)))))))

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
        (nm)
        (n)
        (m)
        (edges)
        (s))
    (dotimes (_a tc)
      (setf edges nil)
      (setf nm (split-and-parse (read-line stream)))
      (setf n (car nm)
            m (cadr nm))
      (dotimes (_b m)
        (push (split-and-parse (read-line stream)) edges))
      (setf s (parse-integer (read-line stream)))
      (solve-me n m edges s)
      (format t "~%"))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
