#!/usr/local/bin/emacs --script

(let* ((dir (nth 0 command-line-args-left))
       (fex (nth 1 command-line-args-left))
       (gex (nth 2 command-line-args-left)))
  (if (= 26 emacs-major-version)
      (progn
	(load (expand-file-name "./find-grep-thread") nil t)
	(find-grep-thread dir fex gex)
	(with-current-buffer "*Find-Grep-Thread*"
	  (message "%s" (buffer-string))))
    (load (expand-file-name "./find-grep-lisp") nil t)
    (find-grep-lisp dir fex gex)
    (with-current-buffer "*Find-Grep-Lisp*"
      (message "%s" (buffer-string)))))
