
(load (expand-file-name "./task-lib") nil t)

(setq global-variable nil
      global-mutex (make-mutex "global-mutex"))

(let* ((task-1 (create-task "task-1"
			    '(lambda ()
			       (let* ((task-id (get-task))
				      (args (get-task-args task-id)))
				 (wait-start-task task-id)
				 (task-cond-wait global-mutex
						 '(not global-variable))
				 (set-task-rslt task-id args)))
			    1 2))
       (task-2 (create-task "task-2"
			    '(lambda ()
			       (let* ((task-id (get-task))
				      (args (get-task-args task-id)))
				 (wait-start-task task-id)
				 (task-cond-notify global-mutex
						   '(setq global-variable t))
				 (set-task-rslt task-id args)))
			    3 4)))
  (sit-for 3)
  (start-task task-1)
  (sit-for 3)
  (start-task task-2)
  (while (or (thread-alive-p (get-task-thrd (get-task "task-1")))
	     (thread-alive-p (get-task-thrd (get-task "task-2"))))
    (sit-for 1))
  (exit-task (get-task "task-1"))
  (exit-task (get-task "task-2"))
  (pop-to-buffer debug-buffer))
