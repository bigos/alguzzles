;;; not solved yet

;;; example of putting multiple lines on one sstring
(let
    ((str (make-array '(0) :element-type 'base-char
                           :fill-pointer 0
                           :adjustable T)))
  (with-output-to-string (stream str)
    (loop for s in '("ala ma kota. Ola"
                     "ma psa. Ela "
                     "ma papuge.")
          do (format stream "~a " s )))
  (format t "===== ~S" str))

(with-input-from-string (s (format nil "Lorem ipsum.~%Dolor sic amet."))
  (format t "read ~s" (loop for l = (read-line s nil 'eof nil)
                            until (eql l 'eof)
                            collect l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-by-char (string char)
  (loop for i = 0 then (1+ j)
        as j = (position char string :start i)
        collect (subseq string i j)
        while j))

(defun read-all-data (stream)
  (loop for line = (read-line stream nil 'eof)
        until (equal line 'eof)
        collect (split-by-char (string-downcase line) #\.)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solution (&optional stream)
  (let ((d (read-line stream nil 'eof)))
    (solve-me d)))

;; (solution) ; uncomment this when running on hacker-rank

(defun main ()
  (with-open-file (s (make-pathname
                      :directory
                      (pathname-directory
                       (parse-namestr
                        ing *load-pathname*))
                      :name "input0" :type "txt"))
    (solution s)))

(main)
(progn #|

|#)
