(defvar lprimos nil)
(defvar ta nil)
(defvar modulo (+ (expt 10 9) 7))
(defvar ak nil)
(defvar aa nil)
(defvar ab nil)
(defvar ac nil)
(defvar as nil)
(defvar nn nil)
(defvar nm nil)

(defun primos(n)
  (loop with s = (list 2)
     for i from 3 to (1+ (sqrt n)) do
       (when  (loop for p in s never  (zerop (mod i p)))
         (setq s (nconc s (list i))))
     finally (return s)))

(defun exponente(n p)
  (loop with num = n
     with e = 0
     while (zerop (mod num p)) do
       (incf e)
       (setq num (/ num p))
     finally (return e)))

(defun divisores(num)
  (loop with n = num
     with expo = nil
     for p in lprimos
     when (zerop (mod n p)) collect
       (progn (setq expo (exponente n p)
                    n (/ n (expt p expo)))
              (list p expo))
     into list
     finally (return (if (= n 1) list (cons (list n 1) list)))))

(defun ldivisores(num)
  (loop with s1 = (list 1)
     with s = nil
     for (p e) in (divisores num) do
       (loop  for i from 1 to e do
            (loop for x in s1  do (push (* x (expt p i)) s)))
       (setq s1 (append s1 s))
       (setq s nil)
     finally (return s1)))

(defun procesa(a b c n m)
  (loop for i from 1 to m do
       (loop for j in (multiplo (aref b i) n) do
            (setf (aref a j) (mod (* (aref a j) (aref c i)) modulo))))
  (fresh-line)
  (loop for i from 1 to (1- n) do (princ (aref a i)) (princ #\Space))
  (princ (aref a n)))

(defun out-aa()
  (let ((*standard-output* *standard-output*))
    (fresh-line)
    (loop for i from 1 to (1- nn) do (princ (aref aa i)) (princ #\Space))
    (princ (aref aa nn))))

(defun haz(strm)
  (let ((*standard-input* strm))
    (setq nn (read)
          nm (read))
    (setq aa (make-array (1+ nn) :element-type 'fixnum)
          ab (make-array (1+ nm) :element-type 'fixnum)
          ac (make-array (1+ nm) :element-type 'fixnum))
    (loop for i from 1 to nn do (setf (aref aa i) (read)))
    (loop for i from 1 to nm do (setf (aref ab i) (read)))
    (loop for i from 1 to nm do (setf (aref ac i) (read)))
    (procesa2)))

(setq  lprimos (primos 100000))

(defun multiplo(num n)
  (loop for i from 1
     while (<= (* num i) n) collect (* num i)))

(defun tarta(num)
  (loop with ee = 0  with pp = nil with pe = nil
     for (p e) in (divisores num) do
       (when (> e ee)
         (setq  ee e  pp p))
     finally (return (list (/ num pp) (progn (setq pe (expt pp ee))
                                             (mapcar (lambda(x)(* x pe)) (ldivisores (/ num pe))))))))

(defun crea-tarta()
  (setq ta (make-array (1+ nn)))
  (loop for i from 2 to nn do
       (setf (aref ta i) (tarta i))))

(defun crea-as()
  (setq as (make-array (1+ nn) :initial-element 1))
  (loop for i from 1 to nm
     for j = (aref ab i) do
       (when (<= 1 j nn)
         (setf (aref as j) (mod (* (aref as j) (aref ac i)) modulo)))))

(defun crea-st()
  (setq ak (make-array (1+ nn) :initial-element 1))
  (setf (aref ak 1) (aref as 1))
  (loop for i from 2 to nn
     for (u v) = (tarta i) do
       (setf (aref ak i)  (mod (* (aref ak u)
                                  (loop with prod = 1
                                     for x in v  do (setq prod (mod (* (aref as x) prod) modulo))
                                     finally (return prod)))
                               modulo))))

(defun pon-aa()
  (loop for i from 1 to nn do
       (setf (aref aa i) (mod  (* (aref aa i) (aref ak i)) modulo))))

(defun procesa2()
  (crea-as)
  (crea-tarta)
  (crea-st)
  (pon-aa)
  (out-aa))

(defun repl-main ()
  (let ((path (if (search "chess" (the string (machine-instance)))
                  "Documents/hackerrank/"
                  "Programming/hackerrank/"))
        (puzzle "sherlock-and-queries"))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input.1.txt"))
      (haz s))
    (format t "~&=========================~%")
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input00.txt"))
      (haz s))
    (format t "~&=========================~%")
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    puzzle "/" "input13.txt"))
      (haz s))
    ))

(repl-main)
