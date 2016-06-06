(setf *print-circle* T)

(declaim (optimize (debug 3)))

(defstruct node
  (id)
  (dist) ; distance from start
  (prev) ; previous node following shortest path
  (neighbours))

(defun try-me (nodes)                   ; examine errors easily
  (/ nodes 0))

(defun solve-me (n m edges s)
  (declare (ignore m))                  ; m is number of edges
  (let ((nodes (make-array (list (1+ n))))
        (queue)
        (found)
        (separator))
    ;; (format t "args are ~A ~A ~A ~%" n edges s)
    (labels ((node-reset ()
               (loop for i from 1 to n do
                    (setf (node-dist (aref nodes i)) most-positive-fixnum
                          (node-prev (aref nodes i)) nil))
               (setf (node-dist (aref nodes s)) 0
                     queue (list s)
                     found nil))
             (visited? (i)
               (< (node-dist (aref nodes i)) most-positive-fixnum))
             (unvisited-neighbours (i)
               (loop for n in (node-neighbours (aref nodes i))
                  unless (visited? n) collect n))
             (my-search (i)            ;find shortest path from s to i
               (let (new-queue)
                 ;; (format t "going to search ~A  ~A~%" i queue)
                 (loop until (or (null queue) found) do
                      (setf new-queue nil)
                      ;; (format t "iteration~%")
                      (loop for n in queue do
                           ;; (format t "running ~%" n)
                           (loop for unv in (unvisited-neighbours n) do
                                ;; (format t "fooooooooooooooooooo ~A ~%" (aref nodes n))
                                (push unv new-queue)
                                (setf (node-dist (aref nodes unv)) (+ (if (node-prev (aref nodes n))
                                                                          (node-prev (aref nodes n))
                                                                          0)
                                                                      6)
                                      (node-prev (aref nodes unv)) (aref nodes n))
                                (when (eq i unv) (setf found T))
                                ))
                      (setf queue new-queue))
                 (if found
                     (format t "~A~A" separator (node-dist (aref nodes i)))
                     (format t "~A~A" separator -1))
                 (setf separator " "))))

      (loop for i from 1 to n do
           (setf (aref nodes i)
                 (make-node :id i :dist most-positive-fixnum)))
      (loop for e in edges do
           (push (car e ) (node-neighbours (aref nodes (cadr e))))
           (push (cadr e) (node-neighbours (aref nodes (car  e)))))

      (setf separator "")
      (loop for i from 1 to n do
           (unless (eq i s)
             (node-reset)
             (my-search i)))
      ;; (try-me nodes)
      )))

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
