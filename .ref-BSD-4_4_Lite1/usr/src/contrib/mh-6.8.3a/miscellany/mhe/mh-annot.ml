;  This autoloaded file implementes the (annotate) function of mhe
; 
;  Insert into the current buffer, at the current cursor position,
;  a message header annotation like "Replied: <<26-Apr-82 09:22>>"
;  Argument 1 is the annotation field string, e.g. "Replied".
;  Argument 2 is an annotation comment field, to be put before the date
(defun 
    (annotate
	(save-excursion 
	    (temp-use-buffer "message")
	    (beginning-of-file)
	    (insert-string 
		(concat (arg 1) ": "
			(if (> (nargs) 1)
			    (concat (arg 2) " ")
			    "")
			(arpa-fmt-date)
			"\n")
	    )
	    (write-current-file)
	    (temp-use-buffer (concat "+" mh-folder))
	    (beginning-of-line)
	    (goto-character (+ (dot) 4))
	    (delete-next-character)
	    (insert-character (string-to-char (arg 1)))
	    (setq buffer-is-modified 0)
	)
    )
    (arpa-fmt-date cd
	(setq cd (current-time)); "Mon Apr 26 09:13:37 1982"
	(concat 
		(substr cd 9 2) " "
		(substr cd 5 3) " "
		(substr cd -2 2) " "
		(substr cd 12 5)
	)
    )
)
