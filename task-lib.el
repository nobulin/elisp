;;
;; Thread management library with little bit of reference to TRON
;;

(require 'cl)
(require 'eieio)

(setq debug-buffer
      (with-current-buffer (get-buffer-create "*debug-buffer*")
	(erase-buffer)
	(current-buffer)))

(defun put-debug-log (string)
  (let ((mes (format "%s - %s" (format-time-string "%H:%M:%S.%6N") string)))
    (if noninteractive
	(message "%s" mes)
      (with-current-buffer debug-buffer
	(insert-before-markers mes "\n")))))

(setq task-list nil)
(defun task-list-init () (setq task-list nil))

;; Management of some threads
(defclass task ()
  ((tname :initarg :tname)
   (tftyp :initarg :tftyp)
   (tfunc :initarg :tfunc)
   (targs :initarg :targs)
   (tcvar :initarg :tcvar)
   (tgvar :initarg :tgvar)
   (tlock :initarg :tlock)
   (tbuff :initarg :tbuff)
   (tthrd :initarg :tthrd)
   (trslt :initarg :trslt)))

;; Define accessors
(defmethod get-task-name ((obj task)) (oref obj tname))
(defmethod get-task-ftyp ((obj task)) (oref obj tftyp))
(defmethod get-task-func ((obj task)) (oref obj tfunc))
(defmethod get-task-args ((obj task)) (oref obj targs))
(defmethod get-task-cvar ((obj task)) (oref obj tcvar))
(defmethod get-task-gvar ((obj task)) (oref obj tgvar))
(defmethod get-task-lock ((obj task)) (oref obj tlock))
(defmethod get-task-buff ((obj task)) (oref obj tbuff))
(defmethod get-task-thrd ((obj task)) (oref obj tthrd))
(defmethod get-task-rslt ((obj task)) (oref obj trslt))
(defmethod set-task-name ((obj task) value) (oset obj tname value))
(defmethod set-task-ftyp ((obj task) value) (oset obj tftyp value))
(defmethod set-task-func ((obj task) value) (oset obj tfunc value))
(defmethod set-task-args ((obj task) value) (oset obj targs value))
(defmethod set-task-cvar ((obj task) value) (oset obj tcvar value))
(defmethod set-task-gvar ((obj task) value) (oset obj tgvar value))
(defmethod set-task-lock ((obj task) value) (oset obj tlock value))
(defmethod set-task-buff ((obj task) value) (oset obj tbuff value))
(defmethod set-task-thrd ((obj task) value) (oset obj tthrd value))
(defmethod set-task-rslt ((obj task) value) (oset obj trslt value))

;; Current thread accessors
(defun get-task-param (slot)
  (let* ((tid (get-task))
	 (slot-name (symbol-name slot))
	 (method (intern (concat "get-task-" slot-name))))
    (funcall method tid)))

(defun set-task-param (slot value)
  (let* ((tid (get-task))
	 (slot-name (symbol-name slot))
	 (method (intern (concat "set-task-" slot-name))))
    (funcall method tid value)))

;; Create a thread and instance of task class
(defun create-task (name func &rest args)
  (let* ((mutex (make-mutex name))
	 (condvar (make-condition-variable mutex name))
	 (buffer (with-current-buffer (get-buffer-create name)
		   (erase-buffer)
		   (current-buffer)))
	 (functype '(lambda (func)
		      (if (listp func) t
			(let ((fs (with-output-to-string
				    (prin1 (symbol-function func)))))
			  (and (string-match "^(" fs) t)))))
	 (ftype (funcall functype func))
	 (funcl (let ((f (and ftype (if (listp func)
					func
				      (symbol-function func))))
		      (pos 2)
		      (exp '(wait-start-task)))
		  (if f (if (stringp (nth pos f))
			    (setf (nth pos f) exp)
			  (let ((front (reverse (nthcdr (- (length f) pos)
							(reverse f))))
				(back (nthcdr pos f)))
			    (nconc front (list exp) back)))
		    func)))
	 (thread (make-thread funcl name))
	 (instance (make-instance 'task
				  :tname name
				  :tftyp ftype
				  :tfunc funcl
				  :targs args
				  :tcvar condvar
				  :tgvar nil
				  :tlock mutex
				  :tbuff buffer
				  :tthrd thread
				  :trslt nil)))
    (while (not (thread-alive-p thread)) (thread-yield))
    (put-debug-log (format "create-task(instance) - %S" instance))
    (push instance task-list)
    instance))

;; Obtain an instance that manages a thread
(defun get-task (&optional name)
  ;;(put-debug-log (format "get-task(name) - %S" name))
  (let ((task-name (if name name (thread-name (current-thread)))))
    (dolist (obj task-list)
      (when (string= task-name (get-task-name obj))
	;;(put-debug-log (format "get-task - %S" obj))
	(return obj)))))

;; Start waiting thread by wait-start-task
;; NOP if thread function is byte-code
(defun start-task (name)
  (put-debug-log (format "start-task(name) - %s" name))
  (let ((tid (get-task name)))
    (and (get-task-ftyp tid)
	 (with-mutex (get-task-lock tid)
	   (set-task-gvar tid t)
	   (condition-notify (get-task-cvar tid))))))

;; Wait until start-task is called
(defun wait-start-task ()
  (put-debug-log (format "wait-start-task() - %S" (get-task)))
  (with-mutex (get-task-param 'lock)
    (while (not (get-task-param 'gvar))
      (condition-wait (get-task-param 'cvar)))
    (set-task-param 'gvar nil)))

;; Terminate task and task instance release
(defun exit-task (name)
  (put-debug-log (format "exit-task(name) - %s" name))
  (let ((tid (get-task name)))
    (and (thread-alive-p (get-task-thrd tid))
	 (thread-signal (get-task-thrd tid) 'error ""))
    (kill-buffer (get-task-buff tid))
    (setq task-list (delete tid task-list))))

;; Create, start and exit task
(defun sync-task (name func &rest args)
  (put-debug-log (format "sync-task(name,func,args) - %S,%S,%S"
			 name func args))
  (apply 'create-task name func args)
  (while (not (thread-alive-p (get-task-thrd (get-task name))))
    (sit-for 0.1)
    (thread-yield))
  (start-task name)
  (while (thread-alive-p (get-task-thrd (get-task name)))
    (sit-for 0.1)
    (thread-yield))
  (exit-task name))

;; Multiplex number for not starting thread too much
(defun wait-exec-task (name)
  (put-debug-log (format "wait-exec-task(name) - %S" name))
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
  (put-debug-log (format "task-cond-wait(name) - %S" name))
  (let ((tid (get-task name)))
    (with-mutex (get-task-lock tid)
      (while (not (get-task-gvar tid))
	(condition-wait (get-task-cvar tid))))))

;; Temporary measure for condition-notify
(defun task-cond-notify (name)
  (put-debug-log (format "task-cond-notify(name) - %S" name))
  (let ((tid (get-task name)))
    (with-mutex (get-task-lock tid)
      (set-task-gvar tid t)
      (condition-notify (get-task-cvar tid)))))
