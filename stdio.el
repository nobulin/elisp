
(defconst stdio-fifo-name
  (expand-file-name "~/elisp/stdio/.stdio-fifo"))

(call-process-shell-command
 (format "test -p %s || /usr/bin/mkfifo %s"
	 stdio-fifo-name stdio-fifo-name))

(setq stdio-input-process
      (start-process-shell-command
       "*STDIO*"
       nil
       (format "/bin/cat < %s" stdio-fifo-name)))

(setq stdio-input-buffer
      (with-current-buffer (get-buffer-create "*STDIN*")
	(erase-buffer)
	(set-process-buffer stdio-input-process (current-buffer))
	(set-marker (process-mark stdio-input-process) (point))
	(current-buffer)))

(set-process-sentinel stdio-input-process '(lambda (proc event)))

(set-process-filter
 stdio-input-process
 '(lambda (proc string)
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  (goto-char (process-mark proc))
	  (insert string)
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc)))))))

(setq stdio-stdin-point 1)
(setq stdio-stdout-point 1)

(setq stdio-stdout-buffer
      (with-current-buffer (get-buffer-create "*STDOUT*")
	(setq truncate-lines t) (erase-buffer) (current-buffer)))

(defun stdio-exit ()
  (save-excursion
    (let ((proc (get-process "*STDIO*")))
      (and proc (kill-process proc)))
    (call-process-shell-command
     (format "test -p %s && /bin/rm -f %s"
	     stdio-fifo-name stdio-fifo-name))
    (and (not noninteractive) (pop-to-buffer stdio-stdout-buffer))))

(when (not noninteractive)
  (let* ((input (read-string "INPUT COMMAND: "))
	 (command (if (string= "" input) "echo -n" input)))
    (start-process-shell-command
     "*COMMAND-FOR-INPUT*" nil
     (format "%s > %s" command stdio-fifo-name))))

(defun getchar ()
  (with-current-buffer stdio-input-buffer
    (goto-char stdio-stdin-point)
    (while (and (eobp) (get-process "*STDIO*"))
      (sit-for 0.1) (goto-char stdio-stdin-point))
    (prog1 (following-char)
      (or (eobp) (forward-char))
      (setq stdio-stdin-point (point)))))

(defun putchar (c)
  (if noninteractive
      (princ (char-to-string c))
    (with-current-buffer stdio-stdout-buffer
      (goto-char stdio-stdout-point)
      (insert (char-to-string c))
      (setq stdio-stdout-point (point-max)))))

(defun printf (fmt &rest args)
  (let ((l (string-to-list (apply 'format fmt args))))
    (dolist (c l) (putchar c))))
