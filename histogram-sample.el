
(load "stdio" nil t)

(with-temp-buffer
  (let (c words (fmt "%%%ds-%%s%%s\n") maxlen maxnum)
    (while (not (= 0 (setq c (getchar)))) (insert (char-to-string c)))
    (goto-char (point-min))
    (while (re-search-forward "[A-Za-z][0-9A-Za-z]+" nil t)
      (let* ((key (match-string 0))
	     (found (assoc key words)))
	(if found (setcdr found (1+ (cdr found)))
	  (add-to-list 'words (cons key 1)))))
    (setq maxlen (apply 'max (mapcar 'length (mapcar 'car words))))
    (setq maxnum (apply 'max (mapcar 'cdr words)))
    (setq fmt (format fmt maxlen))
    (dolist (e (sort words #'(lambda (a b) (string< (car a) (car b)))))
      (printf fmt (car e)
	      (make-string (cdr e) ?+) (make-string (- maxnum (cdr e)) ? )))))

(with-current-buffer "*STDOUT*"
  (let* ((string (buffer-substring-no-properties (point-min)
						 (1- (point-max))))
	 (lines (split-string string "\n"))
	 (hlines (mapcar #'(lambda (l)
			     (substring l 0 (1- (length l))))
			 lines))
	 (len (length (car hlines)))
	 (buffer (with-current-buffer (get-buffer-create "*HISTOGRAM*")
		   (erase-buffer) (current-buffer))))
    (with-current-buffer buffer
      (dotimes (i len)
	(dolist (e hlines) (insert (substring e i (1+ i))))
	(insert "\n"))
      (goto-char (point-min))
      (while (search-forward "-" nil t) (replace-match "|")))))

(stdio-exit)
