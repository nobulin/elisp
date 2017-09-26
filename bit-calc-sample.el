
(defun my-calc-number-format (number base &optional width)
  (let* ((str (format "10#%d" number))
	 (numsharp (progn
		     (cond ((=  2 base) (calc-binary-radix))
			   ((=  8 base) (calc-octal-radix))
			   ((= 16 base) (calc-hex-radix)))
		     (calc-eval str)))
	 (numstr (substring numsharp (1+ (string-match "#" numsharp))))
	 (numwidth (concat (if width
			       (make-string (- width (length numstr)) ?0)
			     "")
			   numstr)))
    (format "%d#%s" base numwidth)))

(defun my-bit-shift-number (number width shift)
  (let* ((numsharp (my-calc-number-format number 2 width))
	 (numstring (substring numsharp 2))
	 (result (if (< 0 shift)
		     (format "2#%s%s"
			     (substring numstring (abs shift))
			     (make-string (abs shift) ?0))
		   (format "2#%s%s"
			   (make-string (abs shift) ?0)
			   (substring numstring
				      0
				      (- (length numstring) (abs shift)))))))
    (calc-decimal-radix)
    (list (string-to-number (calc-eval result)) result)))

(defun my-bit-rotate-number (number width shift)
  (calc-binary-radix)
  (let* ((numsharp (my-calc-number-format number 2 width))
	 (numstring (substring numsharp 2))
	 (rotate (if (< 0 shift)
		     (cons (substring numstring (abs shift))
			   (substring numstring 0 (abs shift)))
		   (cons (substring numstring (- (length numstring)
						 (abs shift)))
			 (substring numstring 0 (- (length numstring)
						   (abs shift)))))))
    (calc-decimal-radix)
    (list (string-to-number
	   (calc-eval (format "2#%s%s" (car rotate) (cdr rotate))))
	  numsharp
	  rotate)))
