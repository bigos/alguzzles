(setf *print-circle* T)

(declaim (optimize (speed 3) (space 3) (debug 3)))

(defparameter my-big-number 1000000)

(defun try-me (x)
  (/ x 0))

(defstruct node
  (id)
  (dist) ; distance from start
  (prev) ; previous node following shortest path
  (accessible) ; accessible from start
  (visited)
  (neighbours))

(defun solve-me (n m edges s)
  ;; TODO: finish me, old solution needs modification, so I can add up different distances between nodes
  (declare (ignore m))                  ; m is number of edges
  (let ((nodes (make-array (list (1+ n))))
        (queue)
        (found)
        (separator))
    (format t "args are ~A ~A ~A ~%" n edges s)
    (labels ((node-reset ()
               (loop for i from 1 to n do
                    (setf (node-dist (aref nodes i)) my-big-number
                          (node-prev (aref nodes i)) nil
                          (node-visited (aref nodes i)) nil))
               (setf (node-dist (aref nodes s)) 0
                     queue (list s)
                     found nil))
             (unvisited-neighbours (i)
               (loop for nn in (node-neighbours (aref nodes i))
                  unless (node-visited (aref nodes (car nn)))
                  collect nn))
             (my-search (i)            ;find shortest path from s to i
               (let ((new-queue)
                     (alt))
                 (format t "~&=========== ~A~%" i )
                 (loop until  (null queue)  do
                      (setf new-queue nil)
                      (loop for n in queue do
                           (setf (node-visited (aref nodes n)) T)
                           (format t "~&------ ~A ~A uuuuuuuuuu  ~a   qqqq ~A~%" n (node-dist (aref nodes n)) (unvisited-neighbours n) queue)
                           (loop for unv in (unvisited-neighbours n) do
                                (push (car unv) new-queue)
                                (format t "~& .......... ~A~%" new-queue)
                                (format t "~&/////////////////// ~a ~A~%" n unv )

                                (setf alt (+ (node-dist (aref nodes n)) (cdr unv)))
                                (format t "~&alt is ~A    ~%" alt )

                                (when (< alt (node-dist (aref nodes (car unv ))))
                                  (format t "found shorter~%")
                                  (setf (node-dist (aref nodes (car unv))) alt
                                        (node-prev (aref nodes (car unv))) (aref nodes n))
                                        ;(format t "~A~%" nodes)
                                  )


                                ))
                      (setf queue new-queue))
                 (format t "finished~% ~A~%" nodes)
                 )))

      (loop for i from 1 to n do
           (setf (aref nodes i)
                 (make-node :id i :dist my-big-number)))
      (loop for e in edges do
           (push (cons (car e ) (caddr e)) (node-neighbours (aref nodes (cadr e))))
           (push (cons (cadr e) (caddr e)) (node-neighbours (aref nodes (car  e)))))

      (node-reset)
      (setf separator "")
      (loop for i from 1 to n do
           (unless (eq i s)
             (node-reset)

             (my-search i)

             (setf separator " ")))
      ;; (try-me nodes)
      )
    ))

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
