(proclaim '(optimize (speed 3)))

;; (require :sb-sprof)
;; (sb-sprof:with-profiling (:max-samples 1000
;;                                        :report :flat
;;                                        :mode :alloc
;;                                        :loop T)
;;   (finding 4096 8191))
(defun finding (s e)
  (if (eq s e)
      s
      (if (num-test s e)
          (cond ((eq (1+ s) e) (logand s e))
                (T (logand s (1+ s) e)))
          0)))

(defun value-progression (s e)
  (loop for x from s to e
     for y = x then (logand x y) finally (return y)))

(defun value-progression-64 (s e)
  ;; cheating here with the expotentiation
  (loop for x from s to (min (+ (expt 2 23) s) e)
     for y = x then (logand x y) finally (return y)))

(defun value-progression-cons (s e)
  (loop for x from s to e
     for y = x then (logand x y) collect (cons x y)))

(defun int2bin (n)
  (format nil "~10,'0b" n))

(defun discrepancies (s e)
  (let ((c (value-progression s e))
        (l (apply 'logand (num-sequence s e))))
    (if (eq c l)
        (format t "~a and ~a are equal   ~A ~A    ~%" s e c (int2bin c) )
        (format t "~a and ~a ARE NOT equal   ~A ~A ~A ~A      ~%" s e c (int2bin c) l (int2bin l)))))

;; example of missed possibilities
;; (discrepancies-loop 359 411 )
(defun discrepancies-loop (s e)
                    (loop for x from s to e do (discrepancies x e)))

(defun rec-num (s e acc)
  (if (> s e)
      acc
      (rec-num (1+ s) e (push (cons (expt 2 s) (1- (expt 2 (1+ s)))) acc))))

(defun num-ranges (e)
  (rec-num 1 e (list (cons 0 1))))

(defun rangenums (s e)
  (loop for x from s to e collect (list x (logandnums x e))))

(defun logandnums (s e)
  (apply 'logand (num-sequence s e)))

(defun num-sequence (s n)
  (loop for x from s to n collect x))

(defparameter *numranges* (num-ranges 31))

(defun num-test (s e)
  (loop for c in *numranges*
     when (and (>= s (car c)) (<= s (cdr c))
               (>= e (car c)) (<= e (cdr c)))
     collect c))

;; much garbage collection
;; (time (solve-me (list 32768 65535)))
(defun solve-me (n)
  (let ((s (car n))
        (e (cadr n)))
    (if (num-test s e)
        (value-progression-64 s e)
        0)))

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
  (let ((tc (parse-integer (read-line stream))))
    (dotimes (x tc)
      (princ (solve-me (split-and-parse (read-line stream))))
      (terpri))))


;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input07" :type "txt"))
    (solution s)))

(main)
