;;
;; Thread management library with little bit of reference to TRON
;;

(require 'cl)

(defvar debug-buffer
  (with-current-buffer (get-buffer-create "*debug-buffer*")
    (erase-buffer)
    (current-buffer)))

(defun put-debug-log (string)
  (let ((mes (format "%s - %s" (format-time-string "%H:%M:%S.%6N") string)))
    (if noninteractive
	(and (string= "DEB" (getenv "DEBUG")) (message "%s" mes))
      (with-current-buffer debug-buffer
	(insert-before-markers mes "\n")))))

(defvar task-list nil)
(defun task-list-init () (setq task-list nil))

;; Management of some threads
(defstruct task
  tname tfunc targs tcvar tgvar tlock tbuff tthrd trslt)

;; Create a thread and instance of task class
(defun create-task (name func &rest args)
  (put-debug-log (format "create-task(name) - %s" name))
  (let ((obj (get-task name)))
    (when (and obj (string= name (task-tname obj)))
      (error "Duplicate-Task-Name")))
  (with-current-buffer (get-buffer-create name)
    (erase-buffer)
    (let* ((mutex (make-mutex name))
	   (condvar (make-condition-variable mutex name))
	   (thread (progn
		     (advice-add func :before 'wait-start-task)
		     (make-thread func name)))
	   (instance (make-task :tname name
				:tfunc func
				:targs args
				:tcvar condvar
				:tgvar nil
				:tlock mutex
				:tbuff (current-buffer)
				:tthrd thread
				:trslt nil)))
      (while (not (thread-alive-p thread)) (thread-yield))
      (push instance task-list)
      instance)))

(defun create-and-start-task (name func &rest args)
  (prog1 (create-task name func &rest args)
    (start-task name)))

;; Obtain an instance that manages a thread
(defun get-task (&optional name)
  (put-debug-log (format "get-task(name) - %S" name))
  (or name (setq name (thread-name (current-thread))))
  (dolist (obj task-list)
    (when (string= name (task-tname obj))
      (put-debug-log (format "get-task - %S" obj))
      (return obj))))

;; Wait until start-task is called
(defun wait-start-task ()
  (let ((obj (get-task)))
    (put-debug-log (format "wait-start-task() - %S" obj))
    (with-mutex (task-tlock obj)
      (setf (task-tgvar obj) nil)
      (while (not (task-tgvar obj))
	(put-debug-log (format "condition-wait(cvar) - %S" obj))
	(condition-wait (task-tcvar obj))))))

;; Start waiting thread by wait-start-task
(defun start-task (name)
  (let ((obj (get-task name)))
    (put-debug-log (format "start-task(name) - %S" obj))
    (with-mutex (task-tlock obj)
      (setf (task-tgvar obj) t)
      (put-debug-log (format "condition-notify(cvar) - %S" obj))
      (condition-notify (task-tcvar obj)))))

;; Terminate task and task instance release
(defun exit-task (name)
  (put-debug-log (format "exit-task(name) - %s" name))
  (let ((obj (get-task name)))
    (and (thread-alive-p (task-tthrd obj))
	 (thread-signal (task-tthrd obj) 'error ""))
    (kill-buffer (task-tbuff obj))
    (setq task-list (delete obj task-list))))

;; Multiplex number for not starting thread too much
(defun wait-exec-task (name)
  (put-debug-log (format "wait-exec-task(name) - %s" name))
  (while (not (zerop (let ((nlist
			    (mapcar
			     #'(lambda (thr)
				 (let ((thn (thread-name thr)))
				   (if (and (stringp thn)
					    (string-match name thn))
				       1
				     0)))
			     (all-threads))))
		       (apply '+ nlist))))
    (sit-for 0.1)
    (thread-yield)))

;; Temporary measure for condition-wait
(defun task-cond-wait (name)
  (put-debug-log (format "task-cond-wait(name) - %s" name))
  (let ((obj (get-task name)))
    (with-mutex (task-tlock obj)
      (setf (task-tgvar obj) nil)
      (while (not (task-tgvar obj))
	(condition-wait (task-tcvar obj))))))

;; Temporary measure for condition-notify
(defun task-cond-notify (name)
  (put-debug-log (format "task-cond-notify(name) - %s" name))
  (let ((obj (get-task name)))
    (with-mutex (task-tlock obj)
      (setf (task-tgvar obj) t)
      (condition-notify (task-tcvar obj)))))

(provide 'task-lib)
