
(defvar dedie2excel-top-dir
  "define path string, top directory of dedie8 data"
  ""
  )

(defvar dedie2excel-save-dir
  "define path string, top directory of An attachment"
  nil
  )

(defun dedie2excel-csv-picker2 (filename)
  (with-temp-buffer
    (let ((coding-system-for-read 'japanese-shift-jis-dos)
	  (record-list nil))
      (insert-file-contents filename)
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((string (buffer-substring-no-properties
			(1+ (point))
			(progn
			  (end-of-line)
			  (if (re-search-forward "^\"[0-9]+\",\"" nil t)
			      (progn
				(forward-line -1)
				(end-of-line))
			    (progn
			      (goto-char (point-max))
			      (forward-line -1)
			      (end-of-line)))
			  (1- (point)))))
	       (recordlist (split-string string "\",\"")))
	  (setq record-list (cons recordlist record-list)))
	(forward-line))
      (reverse record-list))))

(defconst dedie2excel-search-1 "<value type=\"File\" id=\"\\([0-9]+\\)\">")

(defun dedie2excel-get-files (file)
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (let* (result
	   (did (and (string-match "db\\([0-9]+\\)\\.xml"
				   (file-name-nondirectory file))
		     (string-to-number (match-string-no-properties
					1
					(file-name-nondirectory file)))))
	   (otop (format "%s/file/DB/%d" dedie2excel-top-dir did))
	   (dtop (format "%s/%d" dedie2excel-save-dir did)))
      (while (re-search-forward "<record id=\"\\([0-9]+\\)\">" nil t)
	(let ((rid (string-to-number (match-string-no-properties 1))))
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (line-beginning-position)
				(progn
				  (search-forward "</record>")
				  (forward-line)
				  (line-beginning-position)))
	      (goto-char (point-min))
	      (while (re-search-forward dedie2excel-search-1 nil t)
		(let* ((fid (string-to-number
			     (match-string-no-properties 1)))
		       (name (buffer-substring-no-properties
			      (progn (search-forward ">") (point))
			      (progn (search-forward "<") (1- (point)))))
		       (orig (format "%s/%d/%d" otop fid rid))
		       (dest (format "%s/%d/%d/%s" dtop rid fid name)))
		  (add-to-list 'result (list did rid fid orig dest) t)))))))
      result)))

(defun dedie2excel-make-files-dir (lst)
  (let* (result
	 (did (caar lst))
	 (top (format "%s/%d" dedie2excel-save-dir did)))
    (dolist (e lst)
      (let* ((dir (file-name-directory (nth 4 e)))
	     (subdir (split-string (cadr (split-string dir top)) "/"))
	     (wd1 (concat top "/" (nth 1 subdir)))
	     (wd2 (concat top "/" (nth 1 subdir) "/" (nth 2 subdir))))
	(message "Make directory: %s" wd2)
	(or (file-exists-p wd1) (make-directory wd1))
	(or (file-exists-p wd2) (make-directory wd2))))))

(defun dedie2excel-record-file-copy (lst)
  (dolist (e lst)
    (let* ((ofile (nth 3 e))
	   (dfile (nth 4 e)))
      (message "Copying: %s -> %s" ofile dfile)
      (copy-file ofile dfile t))))

(defconst dedie2excel-search-2
  "<field id=\"\\([0-9]+\\)\" type=\"File\">\\(.+\\)</field>")

(defun dedie2excel-get-file-field (xmlfile)
  (with-current-buffer (find-file-noselect xmlfile)
    (goto-char (point-min))
    (save-excursion
      (save-restriction
	(narrow-to-region (progn
			    (search-forward "<field-list>")
			    (line-beginning-position))
			  (progn
			    (search-forward "</field-list>")
			    (forward-line)
			    (line-beginning-position)))
	(goto-char (point-min))
	(let (result)
	  (while (re-search-forward dedie2excel-search-2 nil t)
	    (add-to-list 'result
			 (list (string-to-number
				(match-string-no-properties 1))
			       (match-string-no-properties 2))
			 t))
	  result)))))


;;; output dedie8 library including attachment to CSV
;;; pick up db???.csv and db???.xml from dedie8
;;; invoke dedie2excel-make-excel
;;; after complite, db???-excel.csv is created

(defun dedie2excel-make-excel (csv xml)
  "Usage: (dedie2excel-make-excel \"db???.csv\" \"db???.xml\")"
  (let* ((db-csv (dedie2excel-csv-picker2 csv))
	 (db-header (car db-csv))
	 (db-records (cdr db-csv))
	 (db-files (dedie2excel-get-files xml))
	 (db-file-fields (dedie2excel-get-file-field xml))
	 (excel (let* ((file (file-name-nondirectory csv))
		       (num (progn
			      (string-match "db\\([0-9][0-9][0-9]\\)\\.csv"
					    file)
			      (match-string-no-properties 1 file))))
		  (format "db%s-excel.csv" num))))
    (dedie2excel-make-files-dir db-files)
    (dedie2excel-record-file-copy db-files)
    (dolist (e db-files)
      (let* ((rid (nth 1 e))
	     (fid (nth 2 e))
	     (file (nth 4 e))
	     (fn (cadr (assq fid db-file-fields)))
	     (pos (position-if '(lambda (key) (string= key fn)) db-header))
	     (record (assoc (number-to-string rid) db-records)))
	(setf (nth pos record) file)))
    (with-temp-buffer
      (dolist (e db-csv)
	(insert "\"" (mapconcat 'concat e "\",\"") "\"\n"))
      (write-region (point-min) (point-max) excel))))
