; emh.ml :: another emacs-based interface to the Rand MH system
; Tue Oct  4 22:57:25 1983	/mtr  <mrose@uci-750a>
; This is meant to be a "fast" interface for emacs.  We use the process
; sentinel stuff to help us do things asynchronously.


(declare-global &mhpath &mhbuffer &mhdmax)
(declare-buffer-specific &mhdir &mhfolder &mhmsg &mhreadonly)
(setq &mhdmax 4)
(if (! (is-bound &mhexec))
    (setq-default &mhexec "emh"))
(if (! (is-bound &mhtemp))
    (setq-default &mhtemp "MH scratch"))
(if (! (is-bound &mhunique))
    (setq-default &mhunique 0))

(defun 
    
    (&emh &args
	  (setq &args 
		(if (| prefix-argument-provided (> (nargs) 0))
		    (arg 1 ": emh (args) ") "+inbox"))
	  (&mh-daemon)
	  (if (!= (substr &args 1 1) "+")
	      (setq &args (concat "+" &args)))
	  (if (error-occured (next-buffer-name &args)) 
	      (&mh-scan &args)
	      (pop-to-buffer &args))
	  
	  (novalue)
    )
    
    (&mh-cur-folder &fdr
	(if (!= &mhfolder "") &mhfolder 
	    (!= &mhbuffer "") &mhbuffer
	    (error-message "no cur folder"))
    )
    
    (&mh-cur-message &msg
	(save-excursion 
	    (temp-use-buffer (&mh-cur-folder))
	    (beginning-of-line) (set-mark)
	    (if (error-occured
		    (provide-prefix-argument &mhdmax (forward-character)))
		(error-message "no cur message"))
	    (setq &msg (region-to-string))
	    (beginning-of-line)
	)
	(if (error-occured (setq &msg (+ &msg 0)))
	    (error-message "no cur message"))
	
	&msg
    )
    
    (&mh-daemon &current
	(setq &current "")
	(while (!= (setq &current (next-buffer-name &current)) "")
	       (save-excursion 
		   (temp-use-buffer &current)
		   (if (& (!= &current MPX-process)
			  (&mh-prefix &mhexec &current)
			  (< (process-status &current) 0)
			  (= mode-string "Exited"))
		       (progn &deleted
			      (setq &current
				    (next-buffer-name
					(setq &deleted &current)))
			      (delete-buffer &deleted)))))
    )
    
    (&mh-find-entry &field &value
	(setq &field (arg 1 ": mh-find-entry (name) "))
	(save-excursion 
	    (temp-use-buffer &mhtemp)
	    (erase-buffer)
	    (error-occured 
		(insert-file (expand-file-name "~/.mh_profile"))
		(beginning-of-file)
		(re-search-forward (concat "^" (quote &field) ": "))
		(delete-white-space) (set-mark)
		(end-of-line) (delete-white-space)
		(setq &value (region-to-string)))
	    (delete-buffer &mhtemp))
	
	&value
    )
    
    (&mh-path &name
	(setq &name (arg 1 ": mh-path (name) "))
	(if (= (substr &name 1 1) "/") &name
	    (= (substr &name 1 1) ".") (expand-file-name &name)
	    (concat &mhpath &name))
    )
    
    (&mh-prefix &pattern &target
	(setq &pattern (arg 1 ": mh-prefix (pattern) "))
	(setq &target
	      (arg 2 (concat ": mh-prefix (pattern) " &pattern " (target) ")))
	(& (> (length &target) (length &pattern))
	   (= &pattern (substr &target 1 (length &pattern))))
    )
    
    (&mh-set-cur &current
	(setq &current (arg 1 ": mh-setcur (msg) "))
	(while (< (length &current) &mhdmax)
	       (setq &current (concat " " &current)))
	(save-excursion 
	    (temp-use-buffer (&mh-cur-folder))
	    (beginning-of-file)
	    (error-occured (re-replace-string "^\\([ ]*[0-9]*\\)+" "\\1 "))
	    (end-of-file)
	    (error-occured (re-search-reverse (concat "^" &current)))
	    (if (! (eobp))
		(progn 
		       (beginning-of-line)
		       (provide-prefix-argument &mhdmax (forward-character))
		       (delete-next-character) (insert-character '+'))
		(beginning-of-file)))
	
	(novalue)
    )
    
    (&mh-start-process &command &connect &ushell
	(setq &command (arg 1 ": mh-start-process (command) "))
	(setq &connect
	      (arg 2
		   (concat ": mh-start-process (command) " &command 
			   " (buffer) ")))
	(error-occured 
	    (setq &ushell use-users-shell)
	    (setq use-users-shell 0))
	(start-process &command &connect)
	(error-occured (setq use-users-shell &ushell))
	(novalue)
    )
    
    (&mh-unique
	(concat
	       (arg 1 ": mh-unique (prefix) ")
	       (setq &mhunique (+ &mhunique 1)))
    )
)

(progn 
    (if (= (file-exists (expand-file-name "~/.mh_profile")) 0)
	(error-message "no MH profile"))
    (if (!= (substr (setq &mhpath (&mh-find-entry "Path")) 1 1) "/")
	(setq &mhpath (expand-file-name (concat "~/" &mhpath))))
    (if (!= (substr &mhpath -1 1) "/")
	(setq &mhpath (concat &mhpath "/")))

    (autoload "&mh-folders" "emh-list.ml")
    (autoload "&mh-file"    "emh-move.ml")
    (autoload "&mh-rmm"     "emh-move.ml")
    (autoload "&mh-help"    "emh-help.ml")
    (autoload "&mh-inc"     "emh-inc.ml")
    (autoload "&mh-scan"    "emh-scan.ml")
    (autoload "&mh-comp"    "emh-send.ml")
    (autoload "&mh-forw"    "emh-send.ml")
    (autoload "&mh-repl"    "emh-send.ml")
    (autoload "&mh-next"    "emh-type.ml")
    (autoload "&mh-prev"    "emh-type.ml")
    (autoload "&mh-show"    "emh-type.ml")

    (bind-to-key "&mh-folders" "\^Xf")
    (bind-to-key "&mh-inc"     "\^Xi")
    (bind-to-key "&mh-comp"    "\^Xm")
    (bind-to-key "&emh"        "\^Xr")

    (save-excursion &i
	(temp-use-buffer &mhtemp)
	(define-keymap "&mh-keymap")
	(use-local-map "&mh-keymap")

	(setq &i ' ')
	(while (< &i 127)
	    (local-bind-to-key "illegal-operation" &i)
	    (setq &i (+ &i 1)))
	(setq &i '0')
	(while (< &i '9')
	    (local-bind-to-key "digit" &i)
	    (setq &i (+ &i 1)))
	(local-bind-to-key "minus" "-")

	(local-bind-to-key "&mh-prev" "\^B")
	(local-bind-to-key "&mh-next" "\^F")
	(local-bind-to-key "&mh-prev" "\^H")
	(local-bind-to-key "&mh-help" "?")
	(local-bind-to-key "&mh-comp" "C")
	(local-bind-to-key "&mh-rmm"  "D")
	(local-bind-to-key "&mh-forw" "F")
        (local-bind-to-key "&mh-help" "H")
	(local-bind-to-key "&mh-inc"  "I")
	(local-bind-to-key "&mh-file" "M")
	(local-bind-to-key "&mh-next" "N")
	(local-bind-to-key "&mh-prev" "P")
	(local-bind-to-key "&mh-repl" "R")
	(local-bind-to-key "&mh-show" "S")
	(local-bind-to-key "&mh-comp" "c")
	(local-bind-to-key "&mh-rmm"  "d")
	(local-bind-to-key "&mh-forw" "f")
        (local-bind-to-key "&mh-help" "h")
	(local-bind-to-key "&mh-inc"  "i")
	(local-bind-to-key "&mh-file" "m")
	(local-bind-to-key "&mh-next" "n")
	(local-bind-to-key "&mh-prev" "p")
	(local-bind-to-key "&mh-repl" "r")
	(local-bind-to-key "&mh-show" "s")

	(delete-buffer &mhtemp))

    (error-occured (load "emh-custom.ml"))
)

(novalue)
