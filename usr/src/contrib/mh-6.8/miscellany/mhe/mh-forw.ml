;  This autoloaded file implements the "f" command of mhe
(defun 
    (&mh-forw actn exfl sm annotate fn
	(save-window-excursion 
	    (progn
		  (message  "Forwarding message " (&mh-get-msgnum) "...")
		  (sit-for 0)
		  (&mh-save-killbuffer)
		  (pop-to-buffer (concat "+" mh-folder))
		  (setq sm mode-line-format)
		  (delete-other-windows)
		  (setq annotate mh-annotate)
		  (setq fn (&mh-get-fname))
		  (pop-to-buffer "draft")
		  (erase-buffer)
		  (insert-string "One moment, please...")
		  (unlink-file (concat mh-path "/draft"))
		  (send-to-shell 
		      (concat mh-progs "/forw -build +" mh-folder " "
			      (&mh-get-msgnum))
		  )
		  (show-shell-errors)
		  (read-file (concat mh-path "/draft"))
		  (local-bind-to-key "exit-emacs" "\\")
		  (mail-mode) (header-line-position)
		  (sit-for 0) (setq exfl 0)
		  (while (= exfl 0)
			 (error-occured
			     (save-window-excursion
				 (pop-to-buffer (concat "+" mh-folder))
				 (setq mode-line-format
				       "{%b}	^X^C exits to top level")
				 (pop-to-buffer "draft")
				 (setq mode-line-format
				       (concat "{%b}	%[%p of "
					       mh-path "/draft%]	"
					       "^X^C exits to top level"))
				 (&mh-restore-killbuffer)
				 (recursive-edit)
				 (setq mode-line-format
				       (concat "{%b}	%[%p of "
					       mh-path "/draft%]"))
				 (pop-to-buffer (concat "+" mh-folder))
				 (setq mode-line-format " ")
			     )
			 )
			 (setq actn (get-response "Ready to send. Action? (m, q, e, or ?) " "mMqQeE\" 
					"m: mail it, q: quit, e: resume editing, ?: this msg."))
			 (if (= actn 'm')
			     (progn (message "Sending...") (sit-for 0)
				    (write-current-file)
				    (send-to-shell 
					(concat mh-progs "/send -noverbose "
						mh-path "/draft"
					))
				    (if annotate
					(progn
					      (temp-use-buffer "message")
					      (read-file fn)
					      (annotate "Replied" "Forwarded")
					)
				    )
				    (setq exfl 1))
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
	)
	(pop-to-buffer (concat "+" mh-folder)) (delete-other-windows)
	(setq mode-line-format sm)
	(pop-to-buffer "draft") (previous-window)
    )
)
