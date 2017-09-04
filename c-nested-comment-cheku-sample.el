
(load "stdio" nil t)

(with-current-buffer (completing-read "BUFFER: " (mapcar 'buffer-name (buffer-list)))
  (goto-char (point-min))
  (let (string line)
    (while (setq string (let* ((search1 (search-forward "/*" nil t))
			       (beg (and search1
					 (progn
					   (setq line
						 (cons (count-lines (point-min) (point))
						       (buffer-substring-no-properties
							(line-beginning-position)
							(line-end-position))))
					   (point))))
			       (search2 (search-forward "*/" nil t))
			       (end (and search2 (- search2 2))))
			  (and beg end (buffer-substring-no-properties beg end))))
      (with-temp-buffer
	(insert string)
	(goto-char (point-min))
	(when (search-forward "/*" nil t)
	  (printf "%d: %s\n" (car line) (cdr line))))))
  (with-current-buffer "*STDOUT*"
    (and (= (point-min) (point-max)) (message "NO ERROR."))))

(stdio-exit)
