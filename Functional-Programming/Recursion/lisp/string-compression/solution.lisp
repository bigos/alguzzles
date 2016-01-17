(defun two-same (str last)
  (eq (car str) last))

;; (compress (str-to-list "aabbbccccdeff-") 1 nil)
(defun compress (str count last)
  (if (not str)
      (progn
        (when last
          (princ last))
        (when (> count 1)
          (princ count))
        nil)
      (progn
        (if (two-same str last)
            (progn
              (compress (cdr str) (1+ count) (car str)))
            (progn
              (when last
                (princ last))
              (when (> count 1)
                (princ count))
              (compress (cdr str) 1 (car str))
              )))))

;; (defun count-occurence (str count result)
;;   ;; (format t "~A ~A ~A~%" str count result)
;;   (if (not str)
;;       result
;;       (count-occurence (cdr str)
;;                        (if (two-same str)
;;                            (1+ count)
;;                            1)
;;                        (cons (cons (car str) count) result)
;;                        )))

(defun str-to-list (str)
  (map 'list (lambda (x) x) str))

;; (solve-me (str-to-list "aabbbccccdeff"))
(defun solve-me (str)
  (compress str
            1
            nil))

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
  (let ((str (read-line stream)))
    (solve-me (str-to-list str))))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
