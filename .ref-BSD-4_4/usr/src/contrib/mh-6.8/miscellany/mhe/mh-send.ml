;  This autoloaded file implements the "m" command of mhe. We call "comp" to
;  compose the message into buffer "draft", and then when we are ready to
;  send it we call "send" to do the evil deed.
(defun 
    (&mh-send actn exfl sm
	(save-window-excursion 
	    (&mh-save-killbuffer)
	    (message "Composing a message...") (sit-for 0)
	    (error-occured 
		(unlink-file (concat mh-path "/draft")))
	    (pop-to-buffer (concat "+" mh-folder))
	    (setq sm mode-line-format)
	    (delete-other-windows)
	    (pop-to-buffer "draft") (erase-buffer)
	    (if (file-exists
		    (concat mh-path "/components"))
		(insert-file (concat mh-path "/components"))
		(insert-file (concat mh-progs "/components"))
	    )
	    (write-named-file (concat mh-path "/draft"))
	    (local-bind-to-key "exit-emacs" "\\")
	    (mail-mode) (header-line-position)
	    (sit-for 0) (setq exfl 0)
	    (while (= exfl 0)
		   (error-occured
		       (save-window-excursion
			   (pop-to-buffer (concat "+" mh-folder))
			   (setq mode-line-format
				 "{%b}	^X^C exits to top level  %M")
			   (pop-to-buffer "draft")
			   (setq mode-line-format
				 (concat "{%b}	%[%p of "
					 mh-path "/draft%]	"
					 "(^X^C to exit)  %M"))
			   (&mh-restore-killbuffer)
			   (recursive-edit)
			   (&mh-save-killbuffer)
			   (setq mode-line-format
				 (concat "{%b}	%[%p of "
					 mh-path "/draft%] %M"))
			   (pop-to-buffer (concat "+" mh-folder))
			   (setq mode-line-format " ")
		       )
		   )
		   (setq actn (get-response "Ready to send. Action? (m, q, e, or ?) " "mMqQeE\" 
				  "m: mail it, q: quit, e: resume editing, ?: this msg."))
		   (if (= actn 'm')
		       (progn (message "Sending...") (sit-for 0)
			      (write-current-file)
			      (setq buffer-is-modified 0)
			      (send-to-shell 
				  (concat mh-progs "/send -noverbose "
					  mh-path "/draft"
				  ))
			      (setq exfl 1)
		       )
		       (= actn 'q')
		       (progn
			     (&mh-restore-killbuffer)
			     (pop-to-buffer (concat "+" mh-folder))
			     (delete-other-windows)
			     (setq mode-line-format sm)
			     (error-message "Message not sent; its text remains in buffer 'draft'")
		       )
		   )
	    )
	)
	(&mh-restore-killbuffer)
	(pop-to-buffer (concat "+" mh-folder)) (delete-other-windows)
	(setq mode-line-format sm)
	(pop-to-buffer "draft") (previous-window)
    )
)
