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

(defun quote-p (char)
  (some (lambda (x) (eq x char))
        '(#\" #\')))

(defun upcase-p (char)
  (search "CAPITAL_LETTER" (char-name char)))

(defun arefy (seq i)
  (if (>= i (length seq))
      (aref seq (1- (length seq)))
      (aref seq i)))

;;; need to add handling of sentences starting with >. "Capital<
(defun find-sentence-end (string start)
  "Find sentence end in a STRING starting at START."
  (macrolet
      ((skip (char)
         `(find-sentence-end string (1+ ,char)))
       (next-char (n body)
         `(let ((cur (arefy string (+ start ,n))))
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
                                  ((quote-p cur)
                                   (next-char 3
                                              (cond
                                                ((upcase-p cur)
                                                 (end-of-sentence))
                                                ((else)
                                                 (skip start)))))
                                  ((else)
                                   (skip start)))))
                    ((upcase-p cur)
                     (end-of-sentence ))
                    ((else)
                     (skip start)))))
      ((else)
       (skip start)))))

(defun list-to-pairs (l &optional acc)
  (if (null l)
      (reverse (cdr acc))
      (list-to-pairs (cdr l)
                     (cons
                      (when (cadr l)
                        (cons (car l)
                              (cadr l)))
                      acc))))

(defun solve-me (d)
  ;; (format t "~s ~a~%" d (length d))
  (let ((indexes (loop for y = 0 then (find-sentence-end d y)
                       collect y
                       until (>= y (length d)))))
    ;; (format t "indexes ~a~%" indexes)
    (loop for p in (list-to-pairs indexes)
          for res = (format nil "~a~%" (string-trim '(#\Space)
                                                    (subseq d (car p) (cdr p))))
          do
             (unless (equalp res "")
               (format t "~a" res)))))

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
  (let ((d (read-line stream nil 'eof)))
    (solve-me d)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestring *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
