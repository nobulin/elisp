
(load (expand-file-name "./task-lib") nil t)

(defun find-type-f-lisp (directory fexp)
  (let (files dirs (current directory) (match "[^.]$") (case-fold-search nil))
    (while (progn (dolist (file (directory-files current t match))
		    (if (file-directory-p file)
			(push file dirs)
		      (and (string-match fexp file) (push file files))))
		  (setq current (pop dirs))))
    (sort files 'string<)))

(defun find-grep-buffer ()
  (let* ((name (get-task-param 'name))
	 (gexp (car (get-task-param 'args)))
	 (buffer (get-task-param 'buff)))
    (wait-start-task)
    (put-debug-log (format "find-grep-buffer(gexp) - %s" gexp))
    (with-temp-buffer
      (insert-file-contents name)
      (goto-char (point-min))
      (while (re-search-forward gexp nil t)
	(sit-for 0.1)
	(let ((line (count-lines (point-min) (point)))
	      (found (buffer-substring-no-properties
		      (line-beginning-position)
		      (progn
			(forward-line)
			(point)))))
	  (with-current-buffer buffer
	    (insert (format "%s:%d: %s" name line found))))))))

(defun find-grep-thread (dir fexp gexp)
  (interactive "Ddir: \nsfilename regexp: \nssearch keyword: ")
  (let* ((files (find-type-f-lisp dir fexp))
	 (buffer (generate-new-buffer "*Find-Grep-Thread*")))
    (dolist (file files)
      (let* ((task-id (create-task file 'find-grep-buffer gexp)))
	(start-task file)
	(wait-count-task 3)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (dolist (file files)
	(let ((tbuff (get-task-buff (get-task file))))
	  (insert-before-markers (with-current-buffer tbuff (buffer-string)))
	  (exit-task file)))
      (compilation-mode)
      (pop-to-buffer (current-buffer)))))
