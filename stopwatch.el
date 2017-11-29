
(defmacro with-stopwatch (&rest body)
  (declare (indent 0) (debug t))
  `(let ((stamp1 (current-time)))
     (progn ,@body)
     (let* ((stamp2 (current-time))
	    (t1 (format "%d.%06d" (nth 1 stamp1) (nth 2 stamp1)))
	    (t2 (format "%d.%06d" (nth 1 stamp2) (nth 2 stamp2)))
	    (ts (calc-eval (concat t2 "-" t1))))
       (string-to-number ts))))
with-stopwatch

(let (result)
  (dotimes (i 1000)
    (setq result (cons (with-stopwatch (sit-for 0.1)) result)))
  (/ (apply '+ result) 1000))
0.100290187

(with-stopwatch (sit-for 0.1))
0.116881

(with-stopwatch (sit-for 1))
1.01444

(with-stopwatch (sit-for 10))
10.010844
