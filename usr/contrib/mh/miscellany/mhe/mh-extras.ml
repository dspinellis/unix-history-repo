;  This autoloaded file implements the "x" key of mhe: extended commands.
(defun
    (&mh-extras
	(save-excursion 
	    (pop-to-buffer "mh-xcommands")
	    (use-local-map "&mh-x-keymap")
	    (if (= 0 (buffer-size))
		(insert-string
		    "Key	Meaning		(Type extended command character:  )\n"
		    " q	Quit: get out of this extended command mode\n"
		    " p	Pack the current folder (renumber messages to be 1-N)\n"
		    " c	Close the current folder (process deletes and moves).\n"
		    " s	Scavenge the current folder (regenerate header buffer)\n"
		    " f	Show a list of the existing folders\n"
		    " l	Print the current message on the line printer.\n"
		    " m	Make a new folder.\n"
		    " k	Kill a folder (erase it and all of its contents)\n"
		)
	    )
	    (setq mode-line-format
		  "mhe extended command mode. Type 'q' to quit this mode   %M")
	    (setq buffer-is-modified 0)
	    (beginning-of-file) (end-of-line) (backward-character)
	    (backward-character)
	    (local-bind-to-key "&mh-xpack" "p")
	    (local-bind-to-key "&mh-xclose" "c")
	    (local-bind-to-key "&mh-xscavenge" "s")
	    (local-bind-to-key "&mh-xfolders" "f")
	    (local-bind-to-key "&mh-xlprint" "l")
	    (local-bind-to-key "&mh-xmake" "m")
	    (local-bind-to-key "&mh-xkill" "k")
	    (recursive-edit)
	    (pop-to-buffer "mh-xcommands")
	    (delete-window)
	)
    )
    (&mh-beep (error-message "Use 'q' to quit this extended command mode."))
    
    (&mh-xpack
	(pop-to-buffer (concat "+" mh-folder))
	(&mh-pack-folder)
	(&mh-adjust-window)
	(exit-emacs)
    )
    
    (&mh-xclose
	(message "C: close folder...") (sit-for 0)
	(pop-to-buffer (concat "+" mh-folder))
	(message "C: close folder...") (sit-for 1)
	(&mh-close-folder)
	(exit-emacs)
    )
    
    (&mh-xscavenge sm
	(pop-to-buffer (concat "+" mh-folder))
	(setq sm mode-line-format)
	(&mh-regenerate-headers)
	(setq mode-line-format sm)
	(exit-emacs)
    )
    
    (&mh-xfolders
	(message "F: list folders...")
	(pop-to-buffer "mh-temp")
	(use-local-map "&mh-keymap")
	(erase-buffer) (sit-for 0)
	(send-to-shell (concat mh-progs "/folders"))
	(exit-emacs)
    )
    
    (&mh-xlprint
	(error-message "L: command not implemented.")
    )
    
    (&mh-xmake exists msgg name
	(message "M: make a new folder...")
	(setq exists 1)
	(setq msgg "M: make a new folder...name for it? ")
	(while exists
	       (setq name (get-tty-string msgg))
	       (if (= 0 (length name))
		   (progn 
			  (message "Aborted.") (sit-for 5)
			  (exit-emacs)))
	       (if (!= (string-to-char (substr name 1 1)) '/')
		   (setq t-buffer-filename (concat mh-path "/" name))
		   (setq t-buffer-filename name)
	       )
	       (setq exists (file-exists t-buffer-filename))
	       (if (= exists 1)
			  (setq msgg (concat "Folder +" name " already exists. Try another name? "))
	       )
	)
	(send-to-shell 
	    (concat "mkdir " t-buffer-filename))
	(exit-emacs)
    )
    
    (&mh-xkill exists action name msgg
	(message "K: kill a folder, erasing all of its contents...")
	(setq exists 0)
	(setq msgg "K: kill a folder, erasing all of its contents...which folder? ")
	(while (! exists)
	       (setq name (get-tty-string msgg))
	       (if (= 0 (length name))
		   (progn 
			  (message "Aborted.") (sit-for 5)
			  (exit-emacs)))
	       (if (!= (string-to-char (substr name 1 1)) '/')
		   (setq t-buffer-filename (concat mh-path "/" name))
		   (setq t-buffer-filename name)
	       )
	       (setq exists (file-exists t-buffer-filename))
	       (if (= exists 0)
			  (setq msgg (concat "Folder +" name " does not exist. Try another name? "))
	       )
	)
	(setq action
	      (get-response (concat "Do you really want to destroy folder +"
				    name " and all its contents? ")
		  "yYnN\3" "Please answer y or n"))
	(if (= name "inbox")
	    (setq action
		  (get-response "That's your one and only inbox you are asking me to destroy. Still sure? "
		      "yYnN\3" "Please answer y or n: destroy inbox??? ")))
	(if (= action 'y')
	    (progn 
		   (send-to-shell (concat "rmf +" name))
		   (message "OK, the deed is done... +" name " destroyed.")
	    )
	    (message "Nothing has been destroyed.")
	)
	(sit-for 10)
	(exit-emacs)
    )
)
