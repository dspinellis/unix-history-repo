;  This autoloaded file implements the "r" command of mhe
(defun     
    (&mh-repl actn exfl sm fn annotate ft
	(save-window-excursion 
	    (error-occured
		(&mh-save-killbuffer)
		(temp-use-buffer (concat "+" mh-folder))
		(setq fn (&mh-get-fname))
		(setq annotate mh-annotate)
		(setq ft mh-folder-title)
		(temp-use-buffer "message")
		(setq mode-string "mhe")
		(setq sm mode-line-format)
		(message  "Replying to message " (&mh-get-msgnum) "...")
		(sit-for 0)
		(pop-to-buffer "message")
		(if (error-occured (read-file fn))
		    (error-message "Message " fn " does not exist!"))
		(pop-to-buffer "reply") (erase-buffer)
		(insert-string "One moment, please....")
		(unlink-file (concat mh-path "/reply"))
		(error-occured 
		    (send-to-shell 
			(concat mh-progs "/repl -build +" ft " "
				(&mh-get-msgnum))
		    ))
		(show-shell-errors)
	    )
	    (if (file-exists (concat mh-path "/reply"))
		(read-file (concat mh-path "/reply"))
		(error-message "Reply failed: cannot construct header"
		    " (file " mh-path "/reply)")
	    )
	    (pop-to-buffer "reply")
	    (local-bind-to-key "exit-emacs" "\\")
	    (mail-mode) (end-of-file)
	    (setq exfl 0)
	    (while (= exfl 0)
		   (error-occured
			   (pop-to-buffer "message")
			   (setq mode-line-format
				 "{%b}		%[^X^C exits to top level%]          %M")
			   (pop-to-buffer "reply")
			   (setq mode-line-format
				 (concat "{%b}"
					 "%[%p of " mh-path "/reply"
					 "%]	(^X^C exits to top level)     %M"))
			   (&mh-restore-killbuffer)
			   (recursive-edit)
			   (setq mode-line-format
				 (concat "%[%p of " mh-path "/reply%]     	%M"))
			   (pop-to-buffer "message")
			   (setq mode-line-format sm)
			   (sit-for 0)
		   )
		   (setq actn (get-response "Ready to send. Action? (m, q, e, or ?) " "mMqQeE\" 
				  "m: mail it, q: quit, e: resume editing, ?: this msg."))
		   (if (= actn 'm')
		       (progn (message "Sending...") (sit-for 0)
			      (pop-to-buffer "reply")
			      (write-current-file)
			      (send-to-shell 
				  (concat mh-progs "/send -noverbose "
					  mh-path "/reply"
				  ))
			      (pop-to-buffer "message")
			      (if annotate
				  (annotate "Replied")
			      )
			      (setq exfl 1))
		       (= actn 'q')
		       (progn
			     (&mh-restore-killbuffer)
			     (pop-to-buffer (concat "+" mh-folder))
			     (delete-other-windows)
			     (setq mode-line-format sm)
			     (error-message "Message not sent; its text remains in buffer 'reply'")
		       )
		   )
	    )
	)
	(pop-to-buffer (concat "+" mh-folder)) (delete-other-windows)
	(pop-to-buffer "message")
	(setq mode-line-format sm) (previous-window)
    )
)
