(declaim (optimize (debug 3)))
;;; experiment with encoding the rules

;; (defvar ddd3 '(cond
;;                (kpw-p
;;                 (next-char
;;                  (cond
;;                    (space-p
;;                     (next-char
;;                      (cond
;;                        (space-p
;;                         (next-char
;;                          (cond
;;                            (space-p
;;                             (end-of-sentence))
;;                            (upcase-p
;;                             (end-of-sentence))
;;                            (else
;;                             (skip)))))
;;                        (upcase-p
;;                         (end-of-sentence))
;;                        (else
;;                         (skip)))))
;;                    (upcase-p
;;                     (end-of-sentence))
;;                    (else
;;                     (skip)))))
;;                (else
;;                 (skip))))

(defun else ()
  T)

(defun beginning-of-word (string start)
  (loop for i from (1- start) downto 0
        for c = (aref string i)
        while (search "LETTER" (char-name c))
        finally (return (subseq string (1+ i) start))))

(defun qudoex-p (string start)
  (let ((char (aref string start)))
    (some (lambda (x) (eq x char))
          '(#\? #\! ))
    (cond
      ((eq char #\!)
       T)
      ((eq char #\?)
       T)
      ((eq char #\.)
       (let ((word (beginning-of-word string start)))
         (cond ((eq (length word) 1)
                nil)
               ;; bad for the sentence division without sentence parsing
               ;; https://english.stackexchange.com/questions/8382/when-etc-is-at-the-end-of-a-phrase-do-you-place-a-period-after-it
               ((equalp word "Dr") ; add abbreviations ending with dot here
                nil)
               (T
                T))))
      (T
       nil))))

(defun space-p (char)
  (some (lambda (x) (eq x char))
        '(#\Space #\Newline #\Tab)))

(defun upcase-p (char)
  (search "CAPITAL_LETTER" (char-name char)))

(defun find-sentence-end (string start)
  "Find sentence end in a STRING starting at START."
  (let ((cur (aref string start)))
    (macrolet
        ((skip (char)
           `(find-sentence-end string (1+ ,char)))
         (next-char (n body)
           `(let ((cur (aref string (+ start ,n))))
              ,body))
         (end-of-sentence ()
           `(+ start 1)))
      (cond
        ((>= start (1- (length string)))
         (end-of-sentence))
        ((qudoex-p string start)
         (next-char 1
                    (cond
                      ((space-p cur)
                       (next-char 2
                                  (cond
                                    ((space-p cur)
                                     (next-char 3
                                                (cond
                                                  ((space-p cur)
                                                   (end-of-sentence ))
                                                  ((upcase-p cur)
                                                   (end-of-sentence ))
                                                  ((else)
                                                   (skip start)))))
                                    ((upcase-p cur)
                                     (end-of-sentence))
                                    ((else)
                                     (skip start)))))
                      ((upcase-p cur)
                       (end-of-sentence ))
                      ((else)
                       (skip start)))))
        ((else)
         (skip start))))))

(defun solve-me (d)
  (format t "~s ~a~%" d (length d))

  (let ((indexes (loop for y = 0 then (find-sentence-end *words* y)
                       collect y
                       until (>= y (length *words*)))))
    (format t "~a" indexes)))

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

(defun read-all-data (stream)
  (loop for line = (read-line stream nil 'eof)
     until (equal line 'eof)
     collect line))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((d (read-all-data stream)))
    (defparameter *words* (concatenate 'string (car d) ""))
    (solve-me *words*)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
