; This file implements the autoloaded "exit" function (not a command) of mhe.
(defun 
    (&mh-exit ans retval
	(temp-use-buffer "cmd-buffer")
	(setq retval 0)
	(setq ans (get-response "Preparing to exit. Action? [q, e, u, ?] "
		      "qQeEuU\" 
		      "q: quit (don't process) e: exit (after processing) u: undo (don't exit)"))
	(if (| (= ans 'q') (= ans '\'))
	    (progn
		  (temp-use-buffer "cmd-buffer") (setq ans 'y')
		  (if (> (buffer-size) 0)
		      (setq ans
			    (get-response "Really exit without processing? "
				"yYnN\" "y for Yes or n for No")))
		  (if (| (= ans 'y') (= ans '\'))
		      (progn
			    (pop-to-buffer (concat "+" mh-folder))
			    (erase-buffer)
			    (setq retval 1)
		      )
		  )
	    )
	    (= ans 'e')
	    (progn
		  (temp-use-buffer "cmd-buffer")
		  (if (!= 0 (buffer-size))
		      (progn
			    (temp-use-buffer "cmd-buffer")
			    (message "Processing deletes and moves...")
			    (beginning-of-file)
			    (split-long-lines)
			    (beginning-of-file) (set-mark) (end-of-file)
			    (progn s
				  (setq s (region-to-string))
				  (send-to-shell s)
				  (setq buffer-is-modified 0)
				  (temp-use-buffer (concat "+" mh-folder))
				  (&mh-make-headers-current)
				  (setq buffer-is-modified 0)
				  (setq retval 1)
			    )
		      )
		      (setq retval 1)
		  )
	    )
	)
	(sit-for 10)
	retval
    )
    
    (split-long-lines t s	; make sure no overlong lines in cmd-buffer
	(beginning-of-file)
	(while (! (eobp))
	       (next-line)
	       (while
		     (progn (beginning-of-line)
			    (setq t (dot)) (end-of-line) (> (dot) (+ t 200)))
		     (beginning-of-line) (set-mark)
		     (if (looking-at "rmm")
			 (progn (forward-word) (forward-word) (forward-word)
				(backward-word))
			 (looking-at "filem")
			 (progn (forward-word) (forward-word)
				(forward-word) (forward-word)
				(forward-word) (backward-word))
		     )
		     (setq s (region-to-string)) (beginning-of-line)
		     (goto-character (+ (dot) 200)) (backward-word)
		     (delete-previous-character) (newline)
		     (insert-string s)
	       )
	)
	(setq buffer-is-modified 0)
    )
)
