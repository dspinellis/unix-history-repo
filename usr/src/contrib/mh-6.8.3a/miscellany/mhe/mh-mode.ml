; This file implements a "mail draft mode" for composition of messages in
; the MH mail handler (q.v.). When MH calls Emacs, its customary call
;  is
; 	emacs ./reply ./message -lmh-mode -email-draft-mode
;  for the case of a reply, and
; 	emacs ./draft -lmh-mode -email-draft-mode
;  for a newly originated message. 
; 
; For use from mhe, in which Emacs calls MH instead of vice versa, it will
; work fine as long as the function mail-draft-mode is not called.
; 
; 	Brian Reid, December 1981

(defun
    (dot-in-header wasdot	; return True iff cursor in message hdr
	(save-excursion
	    (setq wasdot (dot))
	    (beginning-of-file)
	    (re-search-forward "^-*$")
	    (beginning-of-line) (backward-character)
	    (>= (dot) wasdot)
	)
    )
    (header-line-position	; position cursor w.r.t. header line
	(if (dot-in-header)
	    (progn 
		   (if (save-excursion 
			   (beginning-of-line)
			   (& (!= (following-char) ' ')
			      (!= (following-char) '\t'))
		       )
		       (progn (beginning-of-line)
			      (error-occured (search-forward ":"))
			      (if (eolp) 
				  (insert-character ' ')
				  (progn
					(forward-character)
					(if (! (eolp))
					    (progn
						  (forward-word)
						  (backward-word))
					))))
		   )))
    )
    
    (header-next		; modified ^N command.
	(next-line)
	(header-line-position)
    )
    
    (header-previous		; modified ^P command
	(previous-line)
	(header-line-position)
    )
    
    (find-starting-line		; back cursor up to first line of this para.
	(beginning-of-line)
	(while (& (! (bobp))
		  (! (eolp))
		  (!= (following-char) '	')
		  (! (looking-at "^-*$"))
	       )
	       (previous-line)
	)
	(next-line)
    )
    (justify-mail-paragraph	;  like ordinary justify-para, but
	(error-occured		; avoids trashing mail header.
	    (if (! (dot-in-header))
		(progn 
		       (save-excursion
			   (find-starting-line)
			   (if (& (! (eolp)) (! (eobp)))
			       (progn 
				      (set-mark)
				      (forward-paragraph)
				      (backward-word) (forward-word)
				      (forward-character)
				      (narrow-region)
				      (error-occured (justify-mail-region))
				      (widen-region))
			   )
		       )
		       (message "Done!")
		       (novalue)
		)))
    )
    
    (justify-mail-region	; justify the entire buffer
	(beginning-of-file)
	(delete-white-space)
	(to-col left-margin)
	(while (progn	; Turn it all into 1 long line....
		   (end-of-line)
		   (if (! (eobp))
		       (forward-character))
		   (! (eobp))
	       )
	       (delete-previous-character)
	       (delete-white-space)
	       (insert-string " ")
	)
	(beginning-of-line)
	(while (save-excursion 
		   (end-of-line)
		   (> (current-column) right-margin)
	       )
	       (goto-character (+ (dot) right-margin))
	       (forward-character) (backward-word)
	       (while (progn 
			     (backward-character)
			     (& (!= (following-char) ' ')
				(!= (following-char) '\t')
				(!= (following-char) '\n')
				(! (bobp)))
		      )
		      (novalue)
	       )
	       (delete-next-character) (newline)
	)
    )
)

(defun 
    (mail-mode
	(set "right-margin" 72)
	(local-bind-to-key "header-next" '')
	(local-bind-to-key "header-previous" '')
	(local-bind-to-key "justify-mail-paragraph" "\ej")
	(use-syntax-table "text-mode")
	(setq mode-string "mh-mail")
	(novalue)
    )
    
    (mail-draft-mode
	(if (> (argc) 4)
	    (progn 
		   (visit-file (argv 1))
		   (mail-mode)
		   (visit-file (argv 2))
		   (mail-mode)
		   (visit-file (argv 1))
		   (end-of-file)
	    )
	    (progn 
		   (visit-file (argv 1))
		   (mail-mode)
		   (beginning-of-file)
		   (header-line-position)
	    )
	)
    )
)
