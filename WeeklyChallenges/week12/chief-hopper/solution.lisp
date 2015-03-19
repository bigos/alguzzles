(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defparameter *bot-energy* 0)
(defparameter *start-bot* 0)
(defparameter *min-bot* 0)

(defun calc-energy (h)
  (if h
      (progn
        (if (> h *bot-energy*)
            (decf *bot-energy*
                  (- h *bot-energy*))
            (incf *bot-energy*
                  (-  *bot-energy* h)))
        (when (< *bot-energy* *min-bot*)
          (setf *min-bot* *bot-energy*)))
      (format t "~A~%" *start-bot*)))

(defun puzzle (i n buildings)
  ;; (format t "~&||| ~a ~A ~A -- ~A ~A~%" i n buildings *bot-energy* *min-bot*)
  (calc-energy (cadr buildings))
  ;; (format t "~&--- ~a ~A ~A -- ~A ~A~%" i n buildings *bot-energy* *min-bot*)
  (when (and (< i n) (>= *min-bot* 0))
    (puzzle (1+ i) n (cdr buildings))))

(defun solution (&optional stream)
  (let* ((n (parse-integer (read-line stream)))
         (buildings (split-and-parse (read-line stream))))
    (push 0 buildings)
    (setf *min-bot* -1)
    (dotimes (x 100000)
      ;; (format t "~&==== ~A~%" x)
      (if (< *min-bot* 0)
          (progn
            ;; (format t "~&gasgasgasggs")
            (setf *bot-energy* x
                  *start-bot* x
                  *min-bot* x)
            (puzzle 0 n buildings))
          (progn
            ;; (format t "~&..................")
            ;; (format t "~A " x)
            (return x))))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/"))
        (puzzle "weekly-challenges/week12/chief-hopper/"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "input.1.txt"))
      (solution s))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "input.2.txt"))
      (solution s))
    ))

(repl-main)
