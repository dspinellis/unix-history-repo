; emh-move.ml :: implements the emh move commands
; Wed Oct  5 13:27:42 1983	/mtr  <mrose@uci-750a>


(defun 
    
    (&mh-file &args &buffer &dest &dir &folder &msg &readonly
	(setq &dest (arg 1 ": mh-file (destination folder) "))
	(if (= (substr &dest 1 1) "+")
	    (setq &dest (substr &dest 2 (- (length &dest) 1))))
	(if (= (setq &args (file-exists (setq &dir (&mh-path &dest)))) 0)
	    (error-message "no such folder as +" &dest)
	    (< &args 0) (error-message "folder +" &dest " is not writable"))
	(setq &args 
	      (if (| prefix-argument-provided (> (nargs) 1))
		  (arg 2  (concat ": mh-file +" &dest " (args) ")) ""))
	(save-excursion
	    (pop-to-buffer (&mh-cur-folder))
	    (setq &folder (&mh-cur-folder))
	    (setq &msg (&mh-cur-message))
	    (setq &readonly &mhreadonly)
	    (beginning-of-line)
	    (provide-prefix-argument 2 (kill-to-end-of-line))
	    (error-occured
		(delete-buffer (concat "message " &folder "/" &msg)))
	    (setq &buffer (&mh-unique &mhexec))
	    (error-occured (delete-buffer &buffer))
	    (error-occured (delete-buffer (setq &dest (concat "+" &dest))))
	    (save-excursion 
		(temp-use-buffer &buffer)
		(setq needs-checkpointing 0)
		(erase-buffer)
		(&mh-start-process
		    (concat "refile -src " &folder " " &msg " " &dest
			    (if (!= &args "") (concat " " &args)
				(if &readonly " -link" " -nolink")))
		    &buffer)
		(insert-sentinel &buffer "&mh-move-sentinel")
		(setq mode-string "Starting")
		(setq mode-line-format
		      (concat "  " &mhexec ": file " &msg " " &dest
			      " (status: %m)  %M"))))	
	
	(novalue)
    )
    
    (&mh-rmm &args &buffer &folder &msg
	(setq &args 
	      (if (| prefix-argument-provided (> (nargs) 0))
		  (arg 1 ": mh-rmm (args) ") ""))
	(save-excursion
	    (pop-to-buffer (&mh-cur-folder))
	    (setq &folder (&mh-cur-folder))
	    (setq &msg (&mh-cur-message))
	    (beginning-of-line)
	    (provide-prefix-argument 2 (kill-to-end-of-line))
	    (error-occured
		(delete-buffer (concat "message " &folder "/" &msg)))
	    (if (! &mhreadonly)
		(progn 
		       (setq &buffer (&mh-unique &mhexec))
		       (error-occured (delete-buffer &buffer))
		       (save-excursion 
			   (temp-use-buffer &buffer)
			   (setq needs-checkpointing 0)
			   (erase-buffer)
			   (&mh-start-process
			       (concat "rmm " &folder " " &msg
				       (if (!= &args "") (concat " " &args) ""))
			       &buffer)
			   (insert-sentinel &buffer "&mh-move-sentinel")
			   (setq mode-string "Starting")
			   (setq mode-line-format
				 (concat "  " &mhexec ": rmm " &folder " "
					 &msg " (status: %m)  %M"))))))
	
	(novalue)
    )	
    
    (&mh-move-sentinel &abnormal &flag &text
	(setq &abnormal 1)
	(setq &flag (>> prefix-argument 16))
	(setq &text (process-output))
	(save-excursion 
	    (temp-use-buffer MPX-process)
	    (setq mode-string (substr &text 1 (- (length &text) 1)))
	    (setq &abnormal (!= mode-string "Exited")))
	(dot-is-visible)	; hack...
	(if (bitwise-and &flag 12)
	    (if &abnormal
		(save-excursion 
		    (pop-to-buffer MPX-process)
		    (beginning-of-file))
		(&mh-daemon)))
    )
)

(novalue)
