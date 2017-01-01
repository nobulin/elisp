
(defun find-type-f-lisp (directory fexp)
  (let (files dirs (current directory) (match "[^.]$"))
    (while (progn (dolist (file (directory-files current t match))
		    (if (file-directory-p file)
			(push file dirs)
		      (and (string-match fexp file) (push file files))))
		  (setq current (pop dirs))))
    (sort files 'string<)))

(defun find-grep-lisp (dir fexp gexp)
  (interactive "Ddir: \nsfilename regexp: \nssearch keyword: ")
  (let* ((files (find-type-f-lisp dir fexp))
	 (buffer (generate-new-buffer "*Find-Grep-Lisp*")))
    (dolist (file files)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(while (re-search-forward gexp nil t)
	  (let ((line (count-lines (point-min) (point)))
		(found (buffer-substring-no-properties
			(line-beginning-position)
			(progn (forward-line) (point)))))
	    (with-current-buffer buffer
	      (insert (format "%s:%d: %s" file line found)))))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (compilation-mode)
      (pop-to-buffer (current-buffer)))))
