;;
;; P O S T . E L
;;
;; Yet another mail interface.  this for the rmail system to provide
;;  the missing sendmail interface on systems without /usr/lib/sendmail,
;;   but with /usr/uci/post.
;;
;; created by: Gary Delp <delp at huey.Udel.Edu>
;;             Mon Jan 13 14:45:12 1986
;;
;;

;; (setq send-mail-function 'post-mail-send-it)

(defun post-mail-send-it ()
  "\
the MH -post interface for rmail-mail to call.
to use it, include (setq send-mail-function 'post-mail-send-it) in site-init."
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " post-mail errors")
		  0))
	(temfile "/tmp/,rpost")
	(tembuf (generate-new-buffer " post-mail temp"))
	(case-fold-search nil)
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what post-mail expects.
	  (goto-char (point-min))
	  (search-forward (concat "\n" mail-header-separator "\n"))
	  (replace-match "\n\n")
	  (backward-char 1)
	  (setq delimline (point-marker))
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  ;; Find and handle any FCC fields.
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(mail-do-fcc delimline))
	    ;; If there is a From and no Sender, put it a Sender.
	    (goto-char (point-min))
	    (and (re-search-forward "^From:"  delimline t)
		 (not (save-excursion
			(goto-char (point-min))
			(re-search-forward "^Sender:" delimline t)))
		 (progn
		   (forward-line 1)
		   (insert "Sender: " (user-login-name) "\n")))
	    ;; don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:[ \t]*\n" delimline t)
		(replace-match ""))
	    (if mail-interactive
		(save-excursion
		  (set-buffer errbuf)
		  (erase-buffer))))
	  (write-file (setq temfile (make-temp-name temfile)))
	  (set-file-modes temfile 384)
	  (apply 'call-process
		 (append (list (if (boundp 'post-mail-program)
				   post-mail-program
				 "/usr/uci/lib/mh/post")
			       nil errbuf nil
			       "-nofilter" "-msgid")
			 (if mail-interactive '("-watch") '("-nowatch"))
			 (list temfile)))
	  (if mail-interactive
	      (save-excursion
		(set-buffer errbuf)
		(goto-char (point-min))
		(while (re-search-forward "\n\n* *" nil t)
		  (replace-match "; "))
		(if (not (zerop (buffer-size)))
		    (error "Sending...failed to %s"
			   (buffer-substring (point-min) (point-max)))))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (switch-to-buffer errbuf)))))
