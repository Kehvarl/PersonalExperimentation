(defvar *test-name* nil)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Reports the results of a single test case. Calles by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within the test function we can call
   other test functions or use 'chec' to run individual test 
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))


(deftest test-+ ()
  "Test that the addition '+' operator works as expected."
    (check
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6) 
      (= (+ -1 -3) -4)))

(deftest test-* ()
  "Test suite for the '*' multiplication operator."
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))
  

(deftest test-arithmetic ()
  "Test suite for arithmetic operations."
  (combine-results
    (test-+)
    (test-*)))

