#!/usr/local/bin/emacs --script

(load (expand-file-name "./task-lib") nil t)

(defun process-sample ()
  (let* ((task-name (get-task-param 'name))
	 (args (get-task-param 'args))
	 (kind (car args))
	 (stime (cadr args)))
    (if (eq kind 'call)
	(call-process-shell-command
	 (format "echo %s; sleep %d" task-name stime))
      (start-process-shell-command
       "process-sample" nil
       (format "echo %s; sleep %d" task-name stime)))
    (put-debug-log (format "%s is exit" task-name))))

;; (byte-compile 'process-sample)

(create-task "process-1" 'process-sample 'start 10)
(create-task "process-2" 'process-sample 'start 10)
(create-task "process-3" 'process-sample 'start 10)
(create-task "process-4" 'process-sample 'start 10)
(create-task "process-5" 'process-sample 'start 10)

(while (not (and (thread-alive-p (get-task-thrd (get-task "process-1")))
		 (thread-alive-p (get-task-thrd (get-task "process-2")))
		 (thread-alive-p (get-task-thrd (get-task "process-3")))
		 (thread-alive-p (get-task-thrd (get-task "process-4")))
		 (thread-alive-p (get-task-thrd (get-task "process-5")))))
  (sit-for 0.1)
  (thread-yield))

(start-task "process-1")
(start-task "process-2")
(start-task "process-3")
(start-task "process-4")
(start-task "process-5")

(while (or (thread-alive-p (get-task-thrd (get-task "process-1")))
	   (thread-alive-p (get-task-thrd (get-task "process-2")))
	   (thread-alive-p (get-task-thrd (get-task "process-3")))
	   (thread-alive-p (get-task-thrd (get-task "process-4")))
	   (thread-alive-p (get-task-thrd (get-task "process-5"))))
  (sit-for 0.1)
  (thread-yield))

(exit-task "process-1")
(exit-task "process-2")
(exit-task "process-3")
(exit-task "process-4")
(exit-task "process-5")

(put-debug-log "Exit script")
