; emh-send.ml :: implements the emh send commands
; Wed Oct  5 03:43:43 1983	/mtr  <mrose@uci-750a>


(declare-buffer-specific &mhdraft &mhprocess)

(defun 
    
    (&mh-comp &args &buffer &components
	(setq &args 
	      (if (| prefix-argument-provided (> (nargs) 0))
		  (arg 1 ": mh-comps (args) ") "components"))
	(if (! (file-exists (setq &components (&mh-path &args))))
	    (if (!= &args "components")
		(error-message "no such file as " &components)
		(if (! (file-exists
			   (setq &components "/usr/local/lib/mh/components")))
		    (error-message "no default components file"))))
	(setq &buffer (&mh-unique "compose"))
	(error-occured (delete-buffer &buffer))
	(pop-to-buffer &buffer)
	(provide-prefix-argument 0 (&mh-finish-build &components))
	
	(novalue)
    )
    
    (&mh-forw &args &buffer &folder &msg
	(setq &args 
	      (if (| prefix-argument-provided (> (nargs) 0))
		  (arg 1 ": mh-forw (args) ") ""))
	(save-excursion 
	    (pop-to-buffer (&mh-cur-folder))
	    (setq &folder &mhfolder))
	(setq &msg (&mh-cur-message))
	(if (>= (process-status 
		    (setq &buffer (concat "forward " &folder "/" &msg))) 0)
	    (error-message "already doing forw " &msg))
	(save-excursion 
	    (temp-use-buffer &buffer)
	    (if (>= (process-status &mhprocess) 0)
		(error-message "already posting a draft for " &msg)))
	(error-occured (delete-buffer &buffer))
	(save-excursion 
	    (temp-use-buffer &buffer)
	    (setq needs-checkpointing 0)
	    (erase-buffer)
	    (setq &mhfolder &folder)
	    (setq &mhmsg &msg)
	    (&mh-start-process
		(concat "forw " &folder " " &msg
			(if (!= &args "") (concat " " &args) "") " -build")
		&buffer)
	    (insert-sentinel &buffer "&mh-forw-sentinel")
	    (setq mode-string "Starting")
	    (setq mode-line-format "  %b: forw (status: %m)  %M"))
	(&mh-set-cur &msg)
	
	(novalue)
    )
    
    (&mh-forw-sentinel &flag &text
	(setq &flag (>> prefix-argument 16))
	(setq &text (process-output))
	(save-excursion 
	    (temp-use-buffer MPX-process)
	    (setq mode-string (substr &text 1 (- (length &text) 1)))
	    (if (= mode-string "Exited")
		(provide-prefix-argument 1 (&mh-finish-build "draft"))))
	(dot-is-visible)	; hack...
	(if (bitwise-and &flag 12)
	    (save-excursion (pop-to-buffer MPX-process) (beginning-of-file)))
    )
    
    (&mh-repl &args &buffer &folder &msg
	(setq &args 
	      (if (| prefix-argument-provided (> (nargs) 0))
		  (arg 1 ": mh-repl (args) ") ""))
	(save-excursion 
	    (pop-to-buffer (&mh-cur-folder))
	    (setq &folder &mhfolder))
	(setq &msg (&mh-cur-message))
	(if (>= (process-status 
		    (setq &buffer (concat "reply " &folder "/" &msg))) 0)
	    (error-message "already doing repl " &msg))
	(save-excursion 
	    (temp-use-buffer &buffer)
	    (if (>= (process-status &mhprocess) 0)
		(error-message "already posting a reply to " &msg)))
	(error-occured (delete-buffer &buffer))
	(save-excursion 
	    (temp-use-buffer &buffer)
	    (setq needs-checkpointing 0)
	    (erase-buffer)
	    (setq &mhfolder &folder)
	    (setq &mhmsg &msg)
	    (&mh-start-process
		(concat "repl " &folder " " &msg 
			(if (!= &args "") (concat " " &args) "") " -build")
		&buffer)
	    (insert-sentinel &buffer "&mh-repl-sentinel")
	    (setq mode-string "Starting")
	    (setq mode-line-format "  %b: repl (status: %m)  %M"))
	(&mh-set-cur &msg)
	
	(novalue)
    )
    
    (&mh-repl-sentinel &flag &text
	(setq &flag (>> prefix-argument 16))
	(setq &text (process-output))
	(save-excursion 
	    (temp-use-buffer MPX-process)
	    (setq mode-string (substr &text 1 (- (length &text) 1)))
	    (if (= mode-string "Exited")
		(provide-prefix-argument 1 (&mh-finish-build "reply"))))
	(dot-is-visible)	; hack...
	(if (bitwise-and &flag 12)
	    (save-excursion (pop-to-buffer MPX-process) (beginning-of-file)))
    )
    
    (&mh-finish-build &file &remove
	(setq &file (&mh-path (arg 1 ": mh-finish-build (from file) ")))
	(setq &remove prefix-argument)
	(remove-all-local-bindings)
	(setq mode-string "Normal")
	(if (file-exists &file)
	    (progn
		  (erase-buffer)
		  (insert-file &file)
		  (error-occured 
		      (if (is-bound &mh-draft-automode)
			  (execute-mlisp-line &mh-draft-automode)
			  (text-mode)))
		  (setq mode-line-format
			"  %b*: ^X-^S to post (%m) %M %[%p%]")
		  (local-bind-to-key "&mh-send" "\^X\^S")
		  (local-bind-to-key "&mh-@-show" "\^X@")
		  (if &remove
		      (unlink-file &file)))
	    (setq mode-line-format "  %b: build of draft failed  %M"))
    )
    
    (&mh-send &args &buffer &draft &file
	(setq &args 
	      (if (| prefix-argument-provided (> (nargs) 0))
		  (arg 1 ": mh-send (args) ") ""))
	(if (>= (process-status &mhprocess) 0)
	    (error-message "already sending draft"))
	(write-named-file
	    (setq &file (concat &mhpath (setq &buffer (&mh-unique &mhexec)))))
	(error-occured (delete-buffer (setq &mhprocess &buffer)))
	(setq &draft (current-buffer-name))
	(setq mode-line-format
	      "  %b*: posting in progress (%m) %M %[%p%]")
	(delete-window)
	(save-excursion 
	    (temp-use-buffer &buffer)
	    (setq needs-checkpointing 0)
	    (erase-buffer)
	    (setq &mhdraft &draft) 
	    (local-bind-to-key "&mh-@-show" "\^X@")
	    (&mh-start-process
		(concat "send " &file (if (!= &args "") (concat " " &args) ""))
		&buffer)
	    (insert-sentinel &buffer "&mh-send-sentinel")
	    (setq mode-string "Starting")
	    (setq mode-line-format 
		  (concat "  %b: send of " &draft " (status: %m)  %M")))
	
	(novalue)
    )
    
    (&mh-send-sentinel &flag &text
	(setq &flag (>> prefix-argument 16))
	(setq &text (process-output))
	(save-excursion 
	    (temp-use-buffer MPX-process)
	    (setq mode-string (substr &text 1 (- (length &text) 1))))
	(dot-is-visible)	; hack...
	(if (bitwise-and &flag 12)
	    (save-excursion
		(pop-to-buffer MPX-process)
		(beginning-of-file)
		(save-excursion 
		    (temp-use-buffer &mhdraft)
		    (if (file-exists (current-file-name))
			(setq mode-line-format
			      (concat "  %b: posting of draft in " 
				      MPX-process " failed (%m) %M %[%p%]"))
			(delete-buffer (current-buffer-name))))))
    )
    
    (&mh-@-show
	 (if
	    (& (!= &mhfolder "") (!= &mhmsg ""))
	    (progn (&mh-set-cur &mhmsg) (&mh-show))
	    (!= &mhdraft "")
	    (if (error-occured (next-buffer-name &mhdraft))
		(save-excursion (pop-to-buffer &mhdraft))
		(error-message "no draft message"))
	    (error-message "no target message"))
	 
	 (novalue)
    )
)

(novalue)
