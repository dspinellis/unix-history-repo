; This file implements "mhe", the display-oriented front end to the MH mail
; system. Documentation is in file mh-doc.ml.
; To install this at your site you must edit the variables flagged with
; an asterisk below.
; 
;  Brian K. Reid, Stanford, April 1982
;
; This is version 4 (September 1982); it uses fast-filter-region.
; 
; UCI modification: we don't need fast-filter-region since we have
;		    use-users-shell
    (setq stack-trace-on-error 1)
    (declare-global		;*marks installation constants
	 mh-keymap-defined	; T iff keymap exists.
	 mh-folder		; string name, e.g. "inbox"
	 mh-path		; "/mnt/reid/Mail", or whatever
	 mh-progs		;*"/usr/local/lib/mh", or whatever
	 bboard-path		;*"/usr/spool/netnews", or whatever
	 mh-buffer-filename	; "/mnt/reid/Mail/inbox", or whatever
	 t-buffer-filename	; scratch for side effect from mh-folder
	 mh-flist		; "inbox,carbons,news", or whatever
	 mh-direction		; 1 is up, -1 is down.
	 mh-annotate		; are we annotating processed msgs?
	 mh-writeable		; is this folder write-enabled?
	 mh-last-destination	; destination of last "move" command
	 mhe-debug		; are we debugging macro package?
    )

    (argc)			; is this early enough, James?
    (setq mh-keymap-defined 0)
    (setq mhe-debug 0)
    (setq-default mh-annotate 1)
    (setq-default mh-writeable 1)
    (setq bboard-path "/dev/null"); UCI
    (setq mh-path "")
    (setq mh-progs "/usr/uci")	; UCI
    (setq mh-flist "")
    (setq-default right-margin 77)
    (setq-default mh-direction 1)
    (setq pop-up-windows 1)	; mhe requires popup windows!

    (declare-buffer-specific
	mh-direction
	mh-buffer-filename
	mh-folder-title
	mh-annotate
	mh-writeable
	backup-before-writing
	wrap-long-lines
    )

(defun				; (mh "folder" "range")
    (mh folder range
	(temp-use-buffer "cmd-buffer") (erase-buffer)
	(setq backup-before-writing 0)
	(find-path)
	(setq folder (arg 1 (concat ": mh on folder? [" mh-folder "] ")))
	(if (= folder "")
	    (setq folder mh-folder))
	(if (= '+' (string-to-char (substr folder 1 1)))
	    (setq folder (substr folder 2 -1)))
	(setq range (arg 2))
	(setq mh-folder (get-folder-name "??" folder 1))
	(&mh-read-folder mh-folder range t-buffer-filename mh-folder)
	(progn stop-loop
	       (setq stop-loop 0)
	       (while (! stop-loop)
		      (pop-to-buffer (concat "+" mh-folder))
		      (use-local-map "&mh-keymap")
		      (error-occured (recursive-edit))
		      (setq stop-loop (&mh-exit))
	       )
	)
    )
)
; This function marks a message as being deleted. This mark has two parts.
; The letter "D" is placed in column 4 of the header line, and the message
; number is added to the text of an "rmm" command that is being assembled
; in the command buffer.
(defun 
    (&mh-Mark-file-deleted
	(pop-to-buffer (concat "+" mh-folder))
	(if (! mh-writeable)
	    (error-message "Sorry; this folder is read-only."))
	(beginning-of-line)
	(goto-character (+ (dot) 3))
	(if (| (= (following-char) ' ') (= (following-char) '+'))
	    (progn 
		   (delete-next-character)
		   (insert-string "D")
		   (setq buffer-is-modified 0)
		   (temp-use-buffer "cmd-buffer")
		   (beginning-of-file)
		   (if (error-occured
			   (re-search-forward
			       (concat "^rmm +" mh-folder)))
		       (progn 
			      (end-of-file)
			      (insert-string (concat "rmm +" mh-folder "\n"))
			      (backward-character)
		       )
		   )
		   (end-of-line)
		   (insert-string (concat " " (&mh-get-msgnum)))
		   (setq buffer-is-modified 0)
		   (pop-to-buffer (concat "+" mh-folder))
	    )
	)
	(another-line)
    )
)
; These functions create (and make current) a header buffer on a new message
; or bboard directory.
(defun 
    (&mh-new-folder which
	(setq which (get-folder-name "New" "" 1))
	(&mh-read-folder which "" t-buffer-filename which)
    )
    
    (&mh-bboard which
	(error-message "B: command not implemented at UCI."); UCI
;UCI	(setq which (get-bboard-name))
;UCI	(&mh-read-folder which "" t-buffer-filename t-buffer-filename)
;UCI	(setq mh-annotate 0)
;UCI	(setq mh-writeable 0)
    )
)

(defun    
    (&mh-remove
	(if (= "+" (substr (current-buffer-name) 1 1))
	    (progn 
		   (beginning-of-line)
		   (&mh-unmark)
		   (kill-to-end-of-line) (kill-to-end-of-line)
		   (setq buffer-is-modified 0)
	    )
	    (error-message "The " (char-to-string (last-key-struck)) " command works only in header windows.")
	)
    )

; This function gets redefined when &mh-move is autoloaded. Shame on me for
; giving it a name so similar to the function above.
    (&mh-re-move
	(error-message "I can't repeat the last ^ command because you haven't typed one yet")
    )

    (&mh-summary
	(message
		"nxt prev del ^put !rpt unmrk typ edit mail forw inc repl get bboard ^X^C ?")
    )

;  This function is redefined when file mh-extras.ml is autoloaded
    (&mh-beep (send-string-to-terminal ""))
)
; These functions are used to preserve the contents of the kill buffer
; across things that we want to be invisible, so that the keyboard-level
; user does not have to worry about system functions clobbering the kill
; buffer.
(defun     
    (&mh-save-killbuffer
	(save-excursion 
	    (temp-use-buffer "Kill buffer")
	    (temp-use-buffer "Kill save")
	    (setq backup-before-writing 0)
	    (erase-buffer)
	    (yank-buffer "Kill buffer")
	    (setq buffer-is-modified 0)
	)
    )
    
    (&mh-restore-killbuffer
	(save-excursion 
	    (temp-use-buffer "Kill buffer")
	    (erase-buffer)
	    (yank-buffer "Kill save")
	)
    )
)
; These functions move the cursor around in a header buffer, and possibly
; also display the message that the cursor now points to.
(defun     
    (&mh-next-line
	(pop-to-buffer (concat "+" mh-folder))
	(setq mh-direction 1)
	(next-line) (beginning-of-line)
	(if (eobp)
	    (progn (previous-line)
		   (setq mh-direction -1)))
    )
    (&mh-previous-line
	(pop-to-buffer (concat "+" mh-folder))
	(setq mh-direction -1)
	(previous-line) (beginning-of-line)
	(if (bobp)
	    (setq mh-direction 1))
    )
    
    (another-line old-direction
	(setq old-direction mh-direction)
	(if (> mh-direction 0)
	    (&mh-next-line)
	    (&mh-previous-line)
	)
	(if (!= old-direction mh-direction)
	    (if (> mh-direction 0)
		(beginning-of-line)
		(&mh-previous-line)
	    )
	)
    )
    
)
; These functions query the user for various things, and error-check the
; responses. "get-response" reads a 1-letter response code in the minibuffer.
; "get-folder-name" extracts the string name of an MH folder or file.
; "get-bboard-name" gets the string name of a bboard file.
(defun     
    (get-response chr ok s c pr
	(setq ok 0) (setq pr (arg 1))
	(while (! ok)
	       (setq chr
		     (string-to-char 
			 (setq c
			       (get-tty-string pr)
			 )
		     )
	       )
	       
	       (setq s (arg 2))
	       (while (> (length s) 0)
		      (if (= chr (string-to-char (substr s 1 1)))
			  (progn (setq ok 1) (setq s ""))
			  (setq s (substr s 2 -1))
		      )
	       )
	       (if (= ok 0)
		   (progn (if (!= chr '?')
			      (setq pr (concat "Illegal response '"
					       (char-to-string chr)
					       "'. " (arg 1)))
			      (setq pr (arg 3))
			  )
		   )
	       )
	)
	(if (& (>= chr 'A') (<= chr 'Z'))
	    (+ chr (- 'a' 'A'))
	    chr
	)
    )
    
    (get-folder-name		; (g-f-n "prompt" "default" can-create)
	exists msgg name defarg
	(setq exists 0)
	(if (> (nargs) 1) (setq defarg (arg 2)) (setq defarg ""))
	(setq msgg (concat (arg 1) " folder name? "))
	(while (! exists)
	       (if (= 0 (length defarg))
		   (setq name (get-tty-string msgg))
		   (setq name defarg)
	       )
	       (setq defarg "")
	       (if (= 0 (length name))
		   (error-message "Aborted."))
	       (if (!= (string-to-char (substr name 1 1)) '/')
		   (setq t-buffer-filename (concat mh-path "/" name))
		   (setq t-buffer-filename name)
	       )
	       (setq exists (file-exists t-buffer-filename))
	       (if (& (!= exists 1) (!= (arg 3) 0))
		   (progn ans
			  (setq ans (get-response
					(concat "Folder +" name " does not exist. May I create it for you? ")
					"yYnN\"
					"Please answer y or n"))
			  (if (= ans 'y')
			      (progn 
				     (message "OK, I will create one for you.")
				     (send-to-shell 
					 (concat "mkdir " t-buffer-filename))
				     (setq exists 1)
			      )
			  )
		   )
	       )
	       (if (!= exists 1)
		   (setq msgg  (concat "Sorry, no such folder as `" name
				       "'.  Folder name? "))
	       )
	)
	name
    )
    
    (get-bboard-name  exists msgg name
	(setq exists 0)
	(setq msgg "BBoard name? ")
	(while (! exists)
	       (setq name (get-tty-string msgg))
	       (if (= 0 (length name))
		   (error-message "Aborted."))
	       (if (!= (string-to-char (substr name 1 1)) '/')
		   (setq t-buffer-filename (concat bboard-path "/" name))
		   (setq t-buffer-filename name)
	       )
	       (setq exists (file-exists t-buffer-filename))
	       (if (!= exists 1)
		   (setq msgg  (concat "Sorry, no such BBoard as `" name
				       "'.  BBoard name? "))
	       )
	)
	name
    )
)
; UCI hack for fast-filter-region
(defun (fast-filter-region UseUsersShell
		(setq UseUsersShell use-users-shell)
		(setq use-users-shell 0)
		(filter-region
		    (arg 1 ": fast-filter-region (through command) "))
		(setq use-users-shell UseUsersShell)
       )
)
; These functions are the initial entry points to mhe. "startup" is 
; expecting an argv like "emacs -lmh-e.ml -estartup +inbox 100-last
(defun
    (startup
	    (setq stack-trace-on-error 0)
	    (mh (if (> (argc) 3)
		    (argv 3)
		    "")
		(if (> (argc) 4)
		    (argv 4)
		    "")
	    )
	    (error-occured (kill-process "newtime"))
	    (exit-emacs)
    )
    
    (debug-startup
	(setq mh-progs "/usr/local/src/cmd/mh/progs")
	(setq stack-trace-on-error 0)
	(startup)
    )
)
    (load "mh-util.ml")
    (load "mh-shell.ml")
    (load "mh-cache.ml")
    (autoload "&mh-send" "mh-send.ml")
    (autoload "&mh-show" "mh-show.ml")
    (autoload "&mh-edit" "mh-edit.ml")
    (autoload "&mh-repl" "mh-repl.ml")
    (autoload "&mh-inc" "mh-inc.ml")
    (autoload "&mh-help" "mh-help.ml")
    (autoload "&mh-move" "mh-move.ml")
    (autoload "&mh-unmark" "mh-unmark.ml")
    (autoload "&mh-forw" "mh-forw.ml")
    (autoload "&mh-exit" "mh-exit.ml")
    (autoload "annotate" "mh-annot.ml")
    (autoload "mail-mode" "mh-mode.ml")
    (autoload "&mh-extras" "mh-extras.ml")
    (autoload "&mh-xpack" "mh-extras.ml")
    (if (! (is-bound time))
	(load "time.ml")
	(time)
    )
    (load "mh-keymap.ml")
