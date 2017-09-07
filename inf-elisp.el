;; inf-elisp-eval.el is other file, please put the file in site-lisp
(setq run-elisp-program "emacs -batch -l inf-elisp-eval")

(run-lisp run-elisp-program)

(defun run-elisp-eval-last-sexp ()
  (interactive)
  (let* ((proc (get-process "inferior-lisp"))
	 (input (buffer-substring-no-properties
		 (save-excursion (backward-sexp) (point)) (point)))
	 (sexp (with-temp-buffer
		 (insert input)
		 (goto-char (point-min))
		 (while (search-forward "\n" nil t) (replace-match " "))
		 (buffer-string))))
    (process-send-string proc sexp)
    (process-send-string proc "\n")))

(define-key emacs-lisp-mode-map (kbd "C-j") 'run-elisp-eval-last-sexp)
