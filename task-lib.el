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
(defmethod get-task-lock ((obj task)) (oref obj tlock))
(defmethod get-task-buff ((obj task)) (oref obj tbuff))
(defmethod get-task-thrd ((obj task)) (oref obj tthrd))
(defmethod get-task-rslt ((obj task)) (oref obj trslt))
(defmethod set-task-name ((obj task) value) (oset obj tname value))
(defmethod set-task-ftyp ((obj task) value) (oset obj tftyp value))
(defmethod set-task-func ((obj task) value) (oset obj tfunc value))
(defmethod set-task-args ((obj task) value) (oset obj targs value))
(defmethod set-task-cvar ((obj task) value) (oset obj tcvar value))
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
  (put-debug-log (format "create-task(func) = %S" func))
  (let* ((mutex (make-mutex name))
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
		  (put-debug-log (format "f = %S" f))
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
				  :tcvar nil
				  :tlock mutex
				  :tbuff buffer
				  :tthrd thread
				  :trslt nil)))
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
    (and (get-task-func tid)
	 (with-mutex (get-task-lock tid) (set-task-cvar tid t)))))

;; Wait until start-task is called
(defun wait-start-task ()
  (put-debug-log (format "wait-start-task() - %S" (get-task)))
  (while (with-mutex (get-task-param 'lock) (not (get-task-param 'cvar)))
    (sit-for 0.1)
    (thread-yield)))

(defun exit-task (name)
  (put-debug-log (format "exit-task(name) - %s" name))
  (let ((tid (get-task name)))
    (while (thread-alive-p (get-task-thrd tid))
      (sit-for 0.1)
      (thread-yield))
    (kill-buffer (get-task-buff tid))
    (setq task-list (delete tid task-list))))

;; Multiplex number for not starting thread too much
(defun wait-count-task (count)
  (put-debug-log (format "wait-count-task(count) - %d" count))
  (while (< count (1- (length (all-threads))))
    (sit-for 0.1)
    (thread-yield)))

;; Temporary measure for condition-wait
;; sexp is reference to global variable
(defun task-cond-wait (lock sexp)
  (put-debug-log (format "task-cond-wait(lock,sexp) - %S,%S" lock sexp))
  (while (with-mutex lock (eval sexp))
    (thread-yield)))

;; Temporary measure for condition-notify
;; sexp is setting to global variable
(defun task-cond-notify (lock sexp)
  (put-debug-log (format "task-cond-notify(lock,sexp) - %S,%S" lock sexp))
  (with-mutex lock (eval sexp)))
