; emh-inc.ml :: implements emh inc command
; Wed Oct  5 11:15:51 1983	/mtr  <mrose@uci-750a>


(defun 
    
    (&mh-inc &buffer &drop &folder
	(setq &folder
	      (if (| prefix-argument-provided (> (nargs) 0))
		  (arg 1 ": mh-inc (to folder) ") "+inbox"))
	(setq &drop
	      (if (| prefix-argument-provided (> (nargs) 1))
		  (arg 2
		       (concat ": mh-inc (to folder) " &folder " (from drop) "))
		  ""))
	(if (!= (substr &folder 1 1) "+")
	    (setq &folder (concat "+" &folder)))
	(setq &buffer (&mh-unique &mhexec))
	(error-occured (delete-buffer &buffer))
	(save-excursion 
	    (temp-use-buffer &buffer)
	    (setq needs-checkpointing 0)
	    (erase-buffer)
	    (&mh-start-process
		(concat "inc " (setq &mhfolder &folder) 
			(if (!= &drop "")
			    (concat " -ms " (&mh-path &drop)) ""))
		&buffer)
	    (insert-sentinel &buffer "&mh-inc-sentinel")
	    (setq mode-string "Starting")
	    (setq mode-line-format
		  (concat "  " &mhexec ": inc " &folder " from "
			  (if (!= &drop "") &drop "maildrop")
			  " (status: %m)  %M")))
	
	(novalue)
    )
    
    (&mh-inc-sentinel &abnormal &flag &text
	(setq &abnormal 1)
	(setq &flag (>> prefix-argument 16))
	(setq &text (process-output))
	(save-excursion 
	    (temp-use-buffer MPX-process)
	    (setq mode-string (substr &text 1 (- (length &text) 1)))
	    (if (= mode-string "Exited") 
		(progn 
		       (beginning-of-file)
		       (if (looking-at "^Incorporating") 
			   (progn (&mh-scan &mhfolder) (setq &abnormal 0))))))
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
