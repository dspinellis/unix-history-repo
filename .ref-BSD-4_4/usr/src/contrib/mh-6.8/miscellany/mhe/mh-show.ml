;  This autoloaded file implements the "t" command of mhe
(defun 
    (&mh-show msgn sm fn fl
	(setq msgn (&mh-get-msgnum))
	(message  "Typing message " msgn) (sit-for 0)
	(if 
	    (error-occured
		(pop-to-buffer (concat "+" mh-folder))
		(setq fn (&mh-get-fname))
		(setq fl mh-folder)
		(pop-to-buffer "show")
		(read-file fn)
		(setq mode-line-format
		      (concat "{%b}	%[%p of +" fl "/" msgn
			      "%]	^X^C exits to top level"))
		(use-local-map "&mh-keymap")
		(setq mode-string "mhe")
		
		(setq mode-line-format
		      (concat "{%b}	%[%p of +" fl "/" msgn "%]"))
		(&mh-set-cur)
	    )
	    (progn (delete-window)
		   (error-message "message " msgn " does not exist!")
	    )
	)
    )
)
