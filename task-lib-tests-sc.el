#!/usr/local/bin/emacs --script

(load (expand-file-name "./task-lib") nil t)

(defun process-sample ()
  (let* ((task-id (get-task))
	 (task-name (get-task-name task-id))
	 (args (get-task-args task-id))
	 (kind (car args))
	 (stime (cadr args)))
    (wait-start-task task-id)
    (if (eq kind 'call)
	(call-process-shell-command (format "sleep %d" stime))
      (start-process-shell-command (format "sleep %d" stime)))
    (put-debug-log (format "%s is exit" task-name))))

(create-task "process-1" 'process-sample 'start 60)
(create-task "process-2" 'process-sample 'call  60)
(create-task "process-3" 'process-sample 'start 60)
(create-task "process-4" 'process-sample 'call  60)
(create-task "process-5" 'process-sample 'start 60)

(start-task (get-task "process-1"))
(start-task (get-task "process-2"))
(start-task (get-task "process-3"))
(start-task (get-task "process-4"))
(start-task (get-task "process-5"))

(exit-task (get-task "process-1"))
(exit-task (get-task "process-2"))
(exit-task (get-task "process-3"))
(exit-task (get-task "process-4"))
(exit-task (get-task "process-5"))

(put-debug-log "Exit script")
