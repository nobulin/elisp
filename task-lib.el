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
	(message mes)
      (with-current-buffer debug-buffer
	(insert-before-markers mes "\n")))))

(setq task-list nil)
(defun task-list-init () (setq task-list nil))

;; Management of some threads
(defclass task ()
  ((tname :initarg :tname)
   (tfunc :initarg :tfunc)
   (targs :initarg :targs)
   (tcvar :initarg :tcvar)
   (tlock :initarg :tlock)
   (tbuff :initarg :tbuff)
   (tthrd :initarg :tthrd)
   (trslt :initarg :trslt)))

;; Creating accessors
(defmethod get-task-name ((obj task)) (oref obj tname))
(defmethod get-task-func ((obj task)) (oref obj tfunc))
(defmethod get-task-args ((obj task)) (oref obj targs))
(defmethod get-task-cvar ((obj task)) (oref obj tcvar))
(defmethod get-task-lock ((obj task)) (oref obj tlock))
(defmethod get-task-buff ((obj task)) (oref obj tbuff))
(defmethod get-task-thrd ((obj task)) (oref obj tthrd))
(defmethod get-task-rslt ((obj task)) (oref obj trslt))
(defmethod set-task-name ((obj task) value) (oset obj tname value))
(defmethod set-task-func ((obj task) value) (oset obj tfunc value))
(defmethod set-task-args ((obj task) value) (oset obj targs value))
(defmethod set-task-cvar ((obj task) value) (oset obj tcvar value))
(defmethod set-task-lock ((obj task) value) (oset obj tlock value))
(defmethod set-task-buff ((obj task) value) (oset obj tbuff value))
(defmethod set-task-thrd ((obj task) value) (oset obj tthrd value))
(defmethod set-task-rslt ((obj task) value) (oset obj trslt value))

;; Create a thread and instance of task object
(defun create-task (name func &rest args)
  (let* ((mutex (make-mutex name))
	 (buffer (with-current-buffer (get-buffer-create name)
		   (erase-buffer)
		   (current-buffer)))
	 (thread (make-thread func name))
	 (instance (make-instance 'task
				  :tname name
				  :tfunc func
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
  (put-debug-log (format "get-task(name) - %S" name))
  (let ((task-name (if name name (thread-name (current-thread)))))
    (dolist (obj task-list)
      (when (string= task-name (get-task-name obj))
	(put-debug-log (format "get-task - %S" obj))
	(return obj)))))

;; Start waiting thread by wait-start-task
(defmethod start-task ((obj task))
  (put-debug-log (format "start-task(obj) - %S" obj))
  (with-mutex (oref obj tlock) (oset obj tcvar t)))

;; Wait until start-task is called
(defmethod wait-start-task ((obj task))
  (put-debug-log (format "wait-start-task(obj) - %S" obj))
  (while (with-mutex (oref obj tlock) (not (oref obj tcvar)))
    (thread-yield)))

(defmethod exit-task ((obj task))
  (put-debug-log (format "exit-task(obj) - %S" obj))
  (while (thread-alive-p (oref obj tthrd)) (thread-yield))
  (kill-buffer (oref obj tbuff))
  (setq task-list (delete obj task-list)))

;; Multiplex number for not starting thread too much
(defun wait-count-task (count name)
  (put-debug-log (format "wait-count-task(count,name) - %d,%s" count name))
  (while (< count
	    (apply #'+
		   (mapcar #'(lambda (obj)
			       (if (string= name (get-task-name obj)) 1 0))
			   task-list)))
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
