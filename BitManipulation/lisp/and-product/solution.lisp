(proclaim '(optimize (speed 3)))

(defun rec-powers-of-two (l n acc)
  (if (>= n l)
      acc
      (rec-powers-of-two l (1+ n) (push (expt 2 n) acc))))

;; (power-of-2-composition 17 (rec-powers-of-two 33 0 nil))
(defun power-of-2-composition (n powers)
  (if (>= n (car powers))
      (list (- n (car powers))
            powers
            (+ (first-gteq (- n (car powers)) (power-2-sums (cdr powers) nil))
               (car powers))
            )
      (power-of-2-composition n (cdr powers))))

;;; eventually i will be able to figure out first number for comparison of only
;;; two numbers and make it super quick
(defun power-2-sums (l acc)
  (if (not l)
      (cons 0 (reverse acc))
      (power-2-sums (cdr l)
                    (push (if (null acc)
                              (car l)
                              (+ (car l) (car acc)))
                          acc))))
;;; almost there
(defun first-gteq (n l)
  (cond ((null l)
         l)
        ((null (cadr l))
         (car l))
        ((<= (car l) n (1- (cadr l)))
         (car l))
        (T
         (first-gteq n (cdr l)))))


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
          (car (last (power-of-2-composition s (rec-powers-of-two e 0 nil))))
          0)))

(defun int2bin (n)
  (format nil "~10,'0b" n))

(defun discrepancies (s e)
  (let ((c (solve-me (list s e)))
        (l (apply 'logand (num-sequence s e)))
        (f (finding s e)))
    (if (eq c l)
        (format t "~a and ~a are equal   ~A ~A    ~A~%" s e c (int2bin c) f)
        (format t "~a and ~a ARE NOT equal   ~A ~A ~A ~A      ~A~%" s e c (int2bin c) l (int2bin l) f))))

;; example of missed possibilities
;; (discrepancies-loop 13 31)
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
        (finding s e)
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
                      :name "input0" :type "txt"))
    (solution s)))

(main)
