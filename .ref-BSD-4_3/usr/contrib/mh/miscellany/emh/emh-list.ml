; emh-list.ml :: implements the emh list command
; Wed Oct  5 23:57:39 1983	/mtr  <mrose@uci-750a>


(if (! (is-bound &mhlist))
    (setq-default &mhlist "MH folders"))

(defun 
    
    (&mh-folders &args
	(setq &args 
	      (if (| prefix-argument-provided (> (nargs) 0))
		  (arg 1 ": mh-folders (args) ") ""))
	(if (>= (process-status &mhlist) 0)
	    (error-message "already listing folders"))
	(error-occured (delete-buffer &mhlist))
	(save-excursion 
	    (temp-use-buffer &mhlist)
	    (setq needs-checkpointing 0)
	    (erase-buffer)
	    (&mh-start-process
		(concat "folders" (if (!= &args "") (concat " " &args) ""))
		&mhlist)
	    (insert-sentinel &mhlist "&mh-list-sentinel")
	    (setq mode-string "Starting")
	    (setq mode-line-format "  %b: in progress  %M"))
	
	(novalue)
    )
    
    (&mh-list-sentinel &flag &text
	(setq &flag (>> prefix-argument 16))
	(setq &text (process-output))
	(save-excursion 
	    (temp-use-buffer MPX-process)
	    (setq mode-string (substr &text 1 (- (length &text) 1)))
	    (if (= mode-string "Exited")
		(progn 
		       (error-occured (normal-mode))
		       (setq mode-line-format
			     "  %b: ESC-^V to scroll (%m) %M %[%p%]"))))
	(dot-is-visible)	; hack...
	(if (bitwise-and &flag 12)
	    (save-excursion (pop-to-buffer MPX-process) (beginning-of-file)))
    )
)

(novalue)

