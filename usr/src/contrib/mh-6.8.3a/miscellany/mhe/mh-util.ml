; These utility functions return message number, file name, or path
; information. They are explicitly loaded from the root.
(defun     
    (&mh-get-msgnum
	(save-excursion
	    (temp-use-buffer (concat "+" mh-folder))
	    (beginning-of-line)
	    (while (= (following-char) ' ') (forward-character))
	    (set-mark)
	    (beginning-of-line)
	    (goto-character (+ (dot) 3))
	    (region-to-string)
	)
    )
    
    (&mh-get-fname
	(save-excursion 
	    (temp-use-buffer (concat "+" mh-folder))
	    (concat mh-buffer-filename "/" (&mh-get-msgnum))
	)
    )

    (find-path			; Look in ~/.mh_profile to find inbox path
	(save-window-excursion 
	    (temp-use-buffer "mhprofile")
	    (setq backup-before-writing 0)
	    (erase-buffer)
	    (if (= 0 (file-exists (concat (getenv "HOME") "/.mh_profile")))
		(progn
		      (pop-to-buffer "sorry") (delete-other-windows)
		      (insert-string "\n\nI can't find your .mh_profile file.\n"
				 "That means I can't continue. Sorry.\n"
				 "If you don't know what this means, then"
				 " you should run the program\n"
				 "'install-mh' now, to build that file.\n")
		      (sit-for 0)
		      (setq stack-trace-on-error 0)
		      (exit-emacs)
		))
	    (read-file (concat (getenv "HOME") "/.mh_profile"))
	    (setq mh-path "Mail")
	    (error-occured 
		(search-forward "Path:")
		(while (looking-at "[\t ]") (forward-character))
		(set-mark) (end-of-line)
		(setq mh-path (region-to-string))
	    )
	    (if (!= (string-to-char (substr mh-path 1 1)) '/')
		(setq mh-path (concat (getenv "HOME") "/" mh-path)))
	    
	    (beginning-of-file)
	    (error-occured 
		(search-forward "current-folder:")
		(while (looking-at "[\t ]") (forward-character))
		(set-mark) (end-of-line)
		(setq mh-folder (region-to-string))
	    )
	    (if (error-occured (search-forward "\nmhe:"))
		(progn
		      (end-of-file)
		      (insert-string "mhe: audit\n"); UCI
;UCI		      (insert-string "mhe: audit")
		      (write-current-file)
		)
	    )
	    (delete-buffer "mhprofile")
	)
    )
)
