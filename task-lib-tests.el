
(load (expand-file-name "./task-lib") nil t)

(setq global-variable nil
      global-mutex (make-mutex)
      global-condvar (make-condition-variable global-mutex))

(let* ((task-1 (create-task
		"task-1"
		'(lambda ()
		   (let ((args (get-task-param 'args)))
		     (with-mutex global-mutex
		       (while (not global-variable)
			 (condition-wait global-condvar)))
		     ;; (task-cond-wait global-mutex '(not global-variable))
		     (set-task-param 'rslt args))
		   (put-debug-log "exit task-1"))
		1 2))
       (task-2 (create-task
		"task-2"
		'(lambda ()
		   (let ((args (get-task-param 'args)))
		     (with-mutex global-mutex
		       (setq global-variable t)
		       (condition-notify global-condvar t))
		     ;; (task-cond-notify global-mutex '(setq global-variable t))
		     (set-task-param 'rslt args))
		   (put-debug-log "exit task-2"))
		3 4)))
  (sit-for 5)
  (start-task "task-1")
  (sit-for 5)
  (start-task "task-2")
  (sit-for 5)
  (put-debug-log (format "task-1 result - %S(%S)" (get-task-rslt (get-task "task-1")) (get-task "task-1")))
  (sit-for 5)
  (put-debug-log (format "task-2 result - %S(%S)" (get-task-rslt (get-task "task-2")) (get-task "task-2")))
  (exit-task "task-1")
  (exit-task "task-2"))
