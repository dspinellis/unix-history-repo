; This function implements the "u" command of mhe.
; It removes a delete or move mark that has been placed on a
; message. To do this, we must remove the "D" or "^" flag in the header
; line, and also remove the message number from the requisite "rmm"
; or "file" command in the command buffer.
(defun 
    (&mh-unmark
	(pop-to-buffer (concat "+" mh-folder))
	(if (! mh-writeable)
	    (error-message "Sorry;  this folder is read-only."))
	(beginning-of-line)
	(goto-character (+ (dot) 3))
	(if (= (following-char) 'D')
	    (progn
		  (save-excursion 
		      (temp-use-buffer "cmd-buffer")
		      (beginning-of-file)
		      (if (error-occured 
			      (re-search-forward
				  (concat "^rmm +" mh-folder
					  " .*\\b" (&mh-get-msgnum) "\\b"))
			  )
			  (message "Can't find msg num!!!")
			  (progn (delete-previous-word)
				 (delete-previous-character)
				 (end-of-line)
				 (backward-word) (backward-word)
				 (if (looking-at
					 (concat "^rmm +" mh-folder))
				     (progn 
					    (kill-to-end-of-line)
					    (kill-to-end-of-line))
				 )
			  )
		      )
		      (setq buffer-is-modified 0)
		  )
	    )
	    (= (following-char) '^')
	    (progn
		  (save-excursion 
		      (temp-use-buffer "cmd-buffer")
		      (beginning-of-file)
		      (if (error-occured 
			      (re-search-forward
				  (concat 
					  "^filem -src +" mh-folder
					  " +.*\\b" (&mh-get-msgnum) "\\b"
				  )
			      )
			  )
			  (message "Can't find msg num!!!")
			  (progn (delete-previous-word)
				 (delete-previous-character)
				 (end-of-line)
				 (backward-word)
				 (backward-character)
				 (if (looking-at "+")
				     (progn 
					    (beginning-of-line)
					    (kill-to-end-of-line)
					    (kill-to-end-of-line)
				     )
				 )
			  )
		      )
		      (setq buffer-is-modified 0)
		  )
	    )
	)
	(if (! (eobp))
	    (progn 
		   (delete-next-character)
		   (insert-string " ")
	    )
	)
	(setq buffer-is-modified 0)
	(another-line)
    )
)

