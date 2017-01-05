
(load (expand-file-name "./task-lib") nil t)

(setq global-variable nil
      global-mutex (make-mutex "global-mutex"))

(let* ((task-1 (create-task
		"task-1"
		'(lambda ()
		   (let* ((args (get-task-param 'args)))
		     (task-cond-wait global-mutex '(not global-variable))
		     (set-task-param 'rslt args)))
		1 2))
       (task-2 (create-task
		"task-2"
		'(lambda ()
		   (let* ((args (get-task-param 'args)))
		     (task-cond-notify global-mutex '(setq global-variable t))
		     (set-task-param 'rslt args)))
		3 4)))
  (sit-for 3)
  (start-task "task-1")
  (sit-for 3)
  (start-task "task-2")
  (while (or (thread-alive-p (get-task-thrd (get-task "task-1")))
	     (thread-alive-p (get-task-thrd (get-task "task-2"))))
    (sit-for 0.5)
    (thread-yield))
  (put-debug-log (format "task-1 result - %S" (get-task-rslt (get-task "task-1"))))
  (put-debug-log (format "task-2 result - %S" (get-task-rslt (get-task "task-2"))))
  (exit-task "task-1")
  (exit-task "task-2")
  (pop-to-buffer debug-buffer))
