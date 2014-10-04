;; Sherlock is stuck. He has an array A1,A2,⋯,AN. He wants to know if there exists a subset, B={Ai1,Ai2,…,Aik}
;; where 1≤i1<i2<…<ik≤N, of this array which follows the property

;; B is non-empty subset.
;; There exists no integer x(x>1) which divides all elements of B. Note that x may or may not be an element of A.

;; Input Format
;; First line contains T, the number of testcases. Each testcase consists of N in one line. The next line contains N integers denoting the array A.

;; Output
;; Print YES or NO, if there exists any such subset or not, respectively.

;; Constraints
;; 1≤T≤10
;; 1≤N≤100
;; 1≤Ai≤10^5 ∀1≤i≤N

;; Sample input

;; 2
;; 3
;; 1 2 3
;; 2
;; 2 4

;; Sample output

;; YES
;; NO

;; Explanation
;; In first testcase, S={1},S={1,2},S={1,3},S={2,3} and S={1,2,3} are all the possible subsets which satisfy the given condition.
;; In second testcase, no non-empty subset exists which satisfies the given condition.

(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
     as j = (position #\Space string :start i)
     collect (subseq string i j)
     while j))

(defun split-and-parse (string)
  (map 'list
       (lambda (x) (parse-integer x))
       (split-by-one-space string)))

(defun puzzle-2 (n nums)
  (if (>= (length nums) 3)
      (princ (subseq nums 0 3))
      nil))

(defun puzzle (data)
  (let ((n (car data))
        (nums (cadr data)))
    ;; (format t "~A ~A~%" n nums)
    (if (puzzle-2 n nums)
        (princ "YES")
        (princ "NO"))
    (terpri)
    ))

(defun solution (&optional stream)
  (let* ((tests (parse-integer (read-line stream)))
         (data (loop repeat tests
                  collect (list (parse-integer (read-line stream))
                                (split-and-parse (read-line stream))))))
    (loop for dataset in data
       do (puzzle dataset))))

;; (solution) ; uncomment this when running on hacker-rank

(defun repl-main ()
  (let ((path (if(search "chess" (machine-instance))
                 "Documents/hackerrank/"
                 "Programming/hackerrank/")))
    (with-open-file (s (concatenate 'string
                                    (directory-namestring (user-homedir-pathname))
                                    path
                                    "sherlock-and-gcd.input.1.txt"))
      (solution s))))

(repl-main)
