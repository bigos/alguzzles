(defun odd-rec (a p acc)
  (cond ((null a) acc)
        (T (odd-rec (cdr a)
                    (car a)
                    (cons (if (and (odd (caar a))
                                   (cadr p))
                              (list (caar a) (cadr p))
                              (car a))
                          acc)))))

(defun odd-positions (a)
  (loop
     for l in a
     for i = 0 then (1+ i)
     for p = nil then (cond ((and (null p) (odd l)) i)
                            ((and p (odd l)) nil)
                            (T p))
     collect (list l p)))

(defun odd (n) (not (evenp n)))

(defun odd-ranges (a)
  (loop
     for e in a
     with odd-seen = 0
     with b = '_
     with res = nil
     with acc = nil
     do
       (when (odd e) (incf odd-seen))
       (if (odd e)
           (setq b (if (odd odd-seen) 'open-add 'close-add))
           (setq b (if (odd odd-seen) 'add 'skip)))

       (cond ((eq b 'add)
              (push e acc))
             ((eq b 'open-add)
              (push e acc))
             ((eq b 'close-add)
              (push e acc)
              (push acc res)
              (setq acc nil)))
     ;; collect (list  e odd-seen b)
     finally
       (return
         (if (not (or (eq b 'skip)
                      (eq b 'close-add)))
             'no
             res))))

(defun find-sol (r)
  ;; (format t "~&=== ~A~%" r)
  (* 2 (1- (length r))))

(defun solve-me (n bn)
  (declare (ignore n))
  (let ((ranges (odd-ranges bn)))
    (format T "~A~%"
            (cond ((eq ranges 'no)
                   "NO")
                  ((null ranges)
                   0)
                  (T
                   (loop for r in ranges
                      summing (find-sol r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (let ((n (parse-integer (read-line stream)))
        (bn (split-and-parse (read-line stream))))
    (solve-me n bn)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
