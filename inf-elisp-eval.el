
(while (setq run-emacs-sexp (read--expression ""))
  (princ (format "%S\n\n" (eval run-emacs-sexp))))
