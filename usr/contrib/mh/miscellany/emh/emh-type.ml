; emh-type.ml :: implements the emh type commands
; Wed Oct  5 00:55:45 1983	/mtr  <mrose@uci-750a>


(declare-buffer-specific &mhargs)

(defun 
    
    (&mh-show &args &buffer &dir &folder &msg
	(setq &args 
	      (if (| prefix-argument-provided (> (nargs) 0))
		  (arg 1 ": mh-show (args) ") ""))
	(save-excursion
	    (pop-to-buffer (&mh-cur-folder))
	    (setq &dir &mhdir)
	    (setq &folder &mhfolder))
	(setq &msg (&mh-cur-message))
	(if (>= (process-status
		    (setq &buffer (concat "message " &folder "/" &msg))) 0)
	    (error-message "already doing show " &msg))
	(if (| (error-occured (next-buffer-name &buffer))
	       (save-excursion (temp-use-buffer &buffer) (!= &args &mhargs)))
	    (save-excursion
		(error-occured (delete-buffer &buffer))
		(temp-use-buffer &buffer)
		(use-local-map "&mh-keymap")
		(setq needs-checkpointing 0)
		(erase-buffer)
		(setq &mhargs &args)
		(setq &mhdir &dir)
		(setq &mhmsg &msg)
		(&mh-start-process
		    (concat "show " (setq &mhfolder &folder) " " &msg 
			    (if (!= &args "") (concat " " &args) "") " | cat")
		    &buffer)
		(insert-sentinel &buffer "&mh-show-sentinel")
		(setq mode-string "Starting")
		(setq mode-line-format "  %b: show (status: %m)  %M"))
	    (save-excursion (pop-to-buffer &buffer) (beginning-of-file)))
	(&mh-set-cur &msg)
	
	(novalue)
    )
    
    (&mh-show-sentinel &flag &text
	(setq &flag (>> prefix-argument 16))
	(setq &text (process-output))
	(save-excursion 
	    (temp-use-buffer MPX-process)
	    (setq mode-string (substr &text 1 (- (length &text) 1)))
	    (if (= mode-string "Exited")
		(progn 
		       (setq mode-string "emh")
		       (setq mode-line-format
			     "  %b: display (%m) %M %[%p%]"))))
	(dot-is-visible)	; hack...
	(if (bitwise-and &flag 12)
	    (save-excursion (pop-to-buffer MPX-process) (beginning-of-file)))
    )
    
    (&mh-prev &args
	(setq &args 
	      (if (| prefix-argument-provided (> (nargs) 0))
		  (arg 1 ": mh-prev (args) ") ""))
	(pop-to-buffer (&mh-cur-folder))
	(if (bobp) (error-message "no prev message"))
	(previous-line) (beginning-of-line)
	(if (error-occured (&mh-cur-message))
	    (progn (next-line) (error-message "no prev message")))
	(&mh-show &args)
    )
    
    (&mh-next &args
	(setq &args 
	      (if (| prefix-argument-provided (> (nargs) 0))
		  (arg 1 ": mh-next (args) ") ""))
	(pop-to-buffer (&mh-cur-folder))
	(if (eobp) (error-message "no next message"))
	(next-line) (beginning-of-line)
	(if (error-occured (&mh-cur-message))
	    (progn (previous-line) (error-message "no next message")))
	(&mh-show &args)
    )
)

(novalue)
