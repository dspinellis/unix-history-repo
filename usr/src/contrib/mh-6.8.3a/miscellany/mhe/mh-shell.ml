; These functions are used to run shell commands and check for errors.
; this file is loaded from mh-e with an explicit load command.
(defun
    (send-to-shell pname progress
	(save-excursion
	    (temp-use-buffer "mh-temp") (erase-buffer)
	    (insert-string (arg 1))
	    (beginning-of-file) (setq progress -1)
	    (while (& (! (eobp)) (< progress 50))
		   (beginning-of-line)
		   (setq progress (+ progress 1))
		   (if (! (eolp))
		       (progn 
			      (set-mark) (end-of-line)
			      (setq pname (region-to-string))
			      (delete-to-killbuffer)
			      (fast-filter-region pname)
		       )
		   )
		   (next-line)
	    )
	)
    )
    
    (show-shell-errors
	(save-excursion 
	    (temp-use-buffer "mh-temp")
	    (if (!= (buffer-size) 0)
		(progn
		      (pop-to-buffer "mh-temp")
		      (sit-for 0)
		      (beginning-of-file) (set-mark)
		      (error-occured (re-replace-string "\n\n* *" "; "))
		      (end-of-line)
		      (backward-character) (backward-character)
		      (kill-to-end-of-line)
		      (send-string-to-terminal "\")
		      (message (region-to-string))
		      (send-string-to-terminal "\")
		      (sit-for 15)
		)
	    )
	)
    )
)
