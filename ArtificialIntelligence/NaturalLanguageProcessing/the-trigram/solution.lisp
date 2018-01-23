(declaim (optimize (debug 3)))

;;; not solved yet

;;; find-sentence-end helpers
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

(defun indexes-to-sentences (indexes text &optional sentences)
  (if (null indexes)
      (reverse sentences)
      (let ((sent
              (string-trim '(#\Space #\Newline)
                           (subseq text
                                   (car indexes)
                                   (cadr indexes))) ))
        (indexes-to-sentences (cdr indexes)
                              text
                              (if (equalp sent "")
                                  sentences
                                  (push sent sentences))))))

(defun split-to-sentences (text)
  (let ((indexes (loop for y = 0 then (find-sentence-end text y)
                       collect y
                       until (>= y (length text)))))
    (indexes-to-sentences indexes text)))

(defun solve-me (d)
  (format t "the data: ~s~%" d)
  (let ((ds (split-to-sentences d)))
    (format t "sentences ~S~%" ds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-all-data (stream)
  (let ((contents (make-string (file-length stream))))
    (read-sequence contents stream)
    contents))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((d (read-all-data stream)))
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
