; emh-scan.ml :: implements the emh scan facility
; Wed Oct  5 23:40:17 1983	/mtr  <mrose@uci-750a>


(defun 

    (&mh-scan &buffer &dir &folder
	(setq &folder (arg 1 ": mh-scan (on folder) "))
	(if (= (substr &folder 1 1) "+")
	    (setq &folder (substr &folder 2 (- (length &folder) 1))))
	(if (! (file-exists (setq &dir (&mh-path &folder))))
	    (error-message "no such folder as +" &folder))
	(if (>= (process-status (setq &buffer (concat "+" &folder))) 0)
	    (error-message "already doing mh " &buffer))
	(error-occured (delete-buffer &buffer))
	(save-excursion 
	    (temp-use-buffer &buffer)
	    (use-local-map "&mh-keymap")
	    (setq needs-checkpointing 0)
	    (erase-buffer)
	    (setq &mhfolder (setq &folder &buffer))
	    (setq &mhreadonly (= (setq &mhdir (concat &dir "/")) -1))
	    (&mh-purge)
	    (&mh-start-process (concat "scan " &folder) &buffer)
	    (insert-sentinel &buffer "&mh-scan-sentinel")
	    (setq mode-string "Starting")
	    (setq mode-line-format "  %b: scan (status: %m)  %M"))
	
	(novalue)
    )
    
    (&mh-scan-sentinel &flag &text
	(setq &flag (>> prefix-argument 16))
	(setq &text (process-output))
	(save-excursion 
	    (temp-use-buffer MPX-process)
	    (setq mode-string (substr &text 1 (- (length &text) 1)))
	    (if (= mode-string "Exited")
		(progn 
		       (setq mode-string "emh")
		       (setq mode-line-format
			     "  %b: scan listing (%m) %M %[%p%]")
		       (setq &mhbuffer MPX-process)
		       (&mh-trim-long-lines))))
	(dot-is-visible)	; hack...
	(if (bitwise-and &flag 12)
	    (save-excursion 
		(&mh-daemon)
		(pop-to-buffer MPX-process)
		(&mh-find-cur)))
    )

    (&mh-find-cur &current
	(beginning-of-file)
	(if (!= (setq &current (&mh-get-cur)) 0)
	    (progn
		  (while (< (length &current) &mhdmax)
			 (setq &current (concat " " &current)))
		  (end-of-file)
		  (error-occured (re-search-reverse (concat "^" &current)))
		  (if (! (eobp))
		      (progn
			    (beginning-of-line)
			    (provide-prefix-argument &mhdmax (forward-character))
			    (delete-next-character) (insert-character '+')
			    (beginning-of-line) (line-to-top-of-window)
			    (set-mark)
			    (provide-prefix-argument (/ (window-height) 2)
				(scroll-one-line-down))
			    (exchange-dot-and-mark))
		      (beginning-of-file))))
	(beginning-of-line)
    )
    
    (&mh-get-cur &cur &file
	(if &mhreadonly
	    (setq &cur (&mh-find-entry (concat "cur-" &mhdir)))
	    (if (file-exists (setq &file (concat &mhdir "cur")))
		(save-excursion
		    (temp-use-buffer &mhtemp)
		    (erase-buffer)
		    (error-occured (insert-file &file))
		    (beginning-of-file) (set-mark) (end-of-line)
		    (setq &cur (region-to-string))
		    (delete-buffer &mhtemp))))
	(if (error-occured (+ &cur 0))
	    (setq &cur 0))
	
	&cur
    )

    (&mh-purge &current &template
	(setq &template (concat "message " &mhfolder "/"))
	(setq &current "")
	(while (!= (setq &current (next-buffer-name &current)) "")
	       (if (&mh-prefix &template &current)
		   (progn &deleted
			  (setq &current
				(next-buffer-name (setq &deleted &current)))
			  (delete-buffer &deleted))))
    )
    
    (&mh-trim-long-lines
	(beginning-of-file)
	(while (! (eobp))
	       (end-of-line)
	       (while (> (current-column) (screen-width))
		      (delete-previous-character))
	       (next-line))
    )
)

(novalue)
