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

(byte-compile 'process-sample)

(sync-task "process-1" 'process-sample 'start 10)
(sync-task "process-2" 'process-sample 'start 10)
(sync-task "process-3" 'process-sample 'start 10)
(sync-task "process-4" 'process-sample 'start 10)
(sync-task "process-5" 'process-sample 'start 10)

(put-debug-log "Exit script")
