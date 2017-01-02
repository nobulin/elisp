#!/usr/local/bin/emacs --script

;; Need emacs version 26 or later

(setq __thread-generation 1)

(defun __thread-func ()
  (dotimes (i 10)
    (message "Thread Generation %d - %d" __thread-generation i)
    (sit-for 1))
  (setq __thread-generation (1+ __thread-generation))
  (make-thread '__thread-func "__thread-func"))

(__thread-func)

(while (let (result)
	 (dolist (name (mapcar 'thread-name (all-threads)) result)
	   (and (string= "__thread-func" name) (setq result t))))
  (thread-yield))
