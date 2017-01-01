
(require 'cl)
(require 'eieio)

(setq debug-buffer (with-current-buffer (get-buffer-create "*debug-buffer*") (erase-buffer) (current-buffer)))

(defun put-debug-log (string)
  (with-current-buffer debug-buffer
    (insert-before-markers (format "%s - %s\n" (format-time-string "%H:%M:%S.%3N") string))))

(setq task-list nil)
(defun task-list-init () (setq task-list nil))

(defclass task ()
  ((tname :initarg :tname) (tfunc :initarg :tfunc) (targs :initarg :targs) (tcvar :initarg :tcvar)
   (tlock :initarg :tlock) (tbuff :initarg :tbuff) (tthrd :initarg :tthrd) (trslt :initarg :trslt)))

(dolist (method (loop for tag in '(tname tfunc targs tcvar tlock tbuff tthrd trslt)
		      collect (let* ((slot (substring (symbol-name tag) 1))
				     (func (intern (concat "get-task-" slot))))
				`(defmethod ,func ((obj task)) (oref obj ,tag)))))
  (eval method))

(defun create-task (name func &rest args)
  (let* ((mutex (make-mutex name))
	 (buffer (with-current-buffer (get-buffer-create name) (erase-buffer) (current-buffer)))
	 (thread (make-thread func name))
	 (instance (make-instance 'task :tname name :tfunc func :targs args
				  :tcvar nil :tlock mutex :tbuff buffer :tthrd thread)))
    (put-debug-log (format "create-task(instance) - %S" instance))
    (push instance task-list)
    instance))

(defun get-task (&optional name)
  (put-debug-log (format "get-task(name) - %S" name))
  (let ((task-name (if name name (thread-name (current-thread)))))
    (catch '__get-task (dolist (obj task-list)
			 (when (string= task-name (get-task-name obj))
			   (put-debug-log (format "get-task(obj) - %S" obj))
			   (throw '__get-task obj))))))

(defmethod start-task ((obj task))
  (put-debug-log (format "start-task(obj) - %S" obj))
  (with-mutex (get-task-lock obj) (oset obj tcvar t)))

(defmethod wait-start-task ((obj task))
  (put-debug-log (format "wait-start-task(obj) - %S" obj))
  (while (with-mutex (get-task-lock obj) (not (get-task-cvar obj)))
    ;; (put-debug-log (format "Wait Start Task - %S" obj))
    (sit-for 1)))

(defmethod exit-task ((obj task))
  (put-debug-log (format "exit-task(obj) - %S" obj))
  (while (thread-alive-p (get-task-thrd obj)) (sit-for 1))
  (kill-buffer (get-task-buff obj))
  (delete obj task-list))

(defun wait-count-task (count name)
  (put-debug-log (format "wait-count-task(count,name) - %d,%s" count name))
  (while (< count (apply #'+ (mapcar #'(lambda (obj) (if (string= name (get-task-name)) 1 0)) task-list)))
    (sit-for 1)))

(defun task-cond-wait (lock sexp)
  (put-debug-log (format "task-cond-wait(lock,sexp) - %S,%S" lock sexp))
  (while (with-mutex lock (eval sexp))
    (sit-for 1)))
      
(defun task-cond-notify (lock sexp)
  (put-debug-log (format "task-cond-notify(lock,sexp) - %S,%S" lock sexp))
  (with-mutex lock (eval sexp)))
