;; Mail sending commands for Emacs.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


(provide 'sendmail)

;(defconst mail-self-blind nil
;  "Non-nil means insert BCC to self in messages to be sent.
;This is done when the message is initialized,
;so you can remove or alter the BCC field to override the default.")

;(defconst mail-interactive nil
;  "Non-nil means when sending a message wait for and display errors.
;nil means let mailer mail back a message to report errors.")

;(defconst mail-yank-ignored-headers
;   "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:"
;   "Delete these headers from old message when it's inserted in a reply.")
;(defvar send-mail-function 'sendmail-send-it
;  "Function to call to send the current buffer as mail.
;The headers are be delimited by a line which is mail-header-separator"")

; really defined in loaddefs for emacs 17.17+
;(defvar mail-header-separator "--text follows this line--"
;  "*Line used to separate headers from text in messages being composed.")
; really defined in loaddefs for emacs 17.17+
;(defvar mail-archive-file-name nil
;  "*Name of file to write all outgoing messages in, or nil for none.")
; really defined in loaddefs for emacs 17.17+
(defvar mail-aliases t
  "Alias of mail address aliases,
or t meaning should be initialized from .mailrc.")

(defvar mail-default-reply-to nil
  "*Address to insert as default Reply-to field of outgoing messages.")

(defvar mail-abbrevs-loaded nil)
(defvar mail-mode-map nil)

(autoload 'build-mail-aliases "mailalias"
  "Read mail aliases from ~/.mailrc and set mail-aliases."
  nil)

(autoload 'expand-mail-aliases "mailalias"
  "Expand all mail aliases in suitable header fields found between BEG and END.
Suitable header fields are To, CC and BCC."
  nil)

(defun mail-setup (to subject in-reply-to cc replybuffer)
  (if (eq mail-aliases t)
      (progn
	(setq mail-aliases nil)
	(if (file-exists-p "~/.mailrc")
	    (build-mail-aliases))))
  (setq mail-reply-buffer replybuffer)
  (goto-char (point-min))
  (insert "To: ")
  (save-excursion
    (if to
	(progn
	  (insert to "\n")
	  ;;; Here removed code to extract names from within <...>
	  ;;; on the assumption that mail-strip-quoted-names
	  ;;; has been called and has done so.
	  (let ((fill-prefix "\t"))
	    (fill-region (point-min) (point-max))))
      (newline))
    (if cc
	(let ((opos (point))
	      (fill-prefix "\t"))
	  (insert "CC: " cc "\n")
	  (fill-region-as-paragraph opos (point-max))))
    (if in-reply-to
	(insert "In-reply-to: " in-reply-to "\n"))
    (insert "Subject: " (or subject "") "\n")
    (if mail-default-reply-to
	(insert "Reply-to: " mail-default-reply-to "\n"))
    (if mail-self-blind
	(insert "BCC: " (user-login-name) "\n"))
    (if mail-archive-file-name
	(insert "FCC: " mail-archive-file-name "\n"))
    (insert mail-header-separator "\n"))
  (if to (goto-char (point-max)))
  (or to subject in-reply-to
      (set-buffer-modified-p nil))
  (run-hooks 'mail-setup-hook))

(defun mail-mode ()
  "Major mode for editing mail to be sent.
Separate names of recipients (in To: and Cc: fields) with commas.
Like Text Mode but with these additional commands:
C-c C-s  mail-send (send the message)    C-c C-c  mail-send-and-exit
C-c C-f  move to a header field (and create it if there isn't):
	 C-c C-f C-t  move to To:	C-c C-f C-s  move to Subj:
	 C-c C-f C-b  move to BCC:	C-c C-f C-c  move to CC:
C-c C-w  mail-signature (insert ~/.signature at end).
C-c C-y  mail-yank-original (insert current message, in Rmail).
C-c C-q  mail-fill-yanked-message (fill what was yanked)."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'mail-reply-buffer)
  (setq mail-reply-buffer nil)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map mail-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'mail-mode)
  (setq mode-name "Mail")
  (setq buffer-offer-save t)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^" mail-header-separator
				"$\\|^[ \t]*[-_][-_][-_]+$\\|"
				paragraph-start))
  (setq paragraph-separate (concat "^" mail-header-separator
				   "$\\|^[ \t]*[-_][-_][-_]+$\\|"
				   paragraph-separate))
  (run-hooks 'text-mode-hook 'mail-mode-hook))

(if mail-mode-map
    nil
  (setq mail-mode-map (make-sparse-keymap))
  (define-key mail-mode-map "\C-c?" 'describe-mode)
  (define-key mail-mode-map "\C-c\C-f\C-t" 'mail-to)
  (define-key mail-mode-map "\C-c\C-f\C-b" 'mail-bcc)
  (define-key mail-mode-map "\C-c\C-f\C-c" 'mail-cc)
  (define-key mail-mode-map "\C-c\C-f\C-s" 'mail-subject)
  (define-key mail-mode-map "\C-c\C-w" 'mail-signature)		; who
  (define-key mail-mode-map "\C-c\C-y" 'mail-yank-original)
  (define-key mail-mode-map "\C-c\C-q" 'mail-fill-yanked-message)
  (define-key mail-mode-map "\C-c\C-c" 'mail-send-and-exit)
  (define-key mail-mode-map "\C-c\C-s" 'mail-send))

(defun mail-send-and-exit (arg)
  "Send message like mail-send, then, if no errors, exit from mail buffer.
Prefix arg means don't delete this window."
  (interactive "P")
  (mail-send)
  (bury-buffer (current-buffer))
  (if (and (not arg)
	   (not (one-window-p))
	   (save-excursion
	     (set-buffer (window-buffer (next-window (selected-window) 'not)))
	     (eq major-mode 'rmail-mode)))
      (delete-window)
    (switch-to-buffer (other-buffer (current-buffer)))))

(defun mail-send ()
  "Send the message in the current buffer.
If  mail-interactive  is non-nil, wait for success indication
or error messages, and inform user.
Otherwise any failure is reported in a message back to
the user from the mailer."
  (interactive)
  (message "Sending...")
  (funcall send-mail-function)
  (set-buffer-modified-p nil)
  (delete-auto-save-file-if-necessary)
  (message "Sending...done"))

(defun sendmail-send-it ()
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " sendmail errors")
		  0))
	(tembuf (generate-new-buffer " sendmail temp"))
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
	  ;; Change header-delimiter to be what sendmail expects.
	  (goto-char (point-min))
	  (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "\n"))
	  (replace-match "\n")
	  (backward-char 1)
	  (setq delimline (point-marker))
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    ;; Find and handle any FCC fields.
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
	  (apply 'call-process-region
		 (append (list (point-min) (point-max)
			       (if (boundp 'sendmail-program)
				   sendmail-program
				 "/usr/lib/sendmail")
			       nil errbuf nil
			       "-oi" "-t")
			 ;; Always specify who from,
			 ;; since some systems have broken sendmails.
			 (list "-f" (user-login-name))
;;;			 ;; Don't say "from root" if running under su.
;;;			 (and (equal (user-real-login-name) "root")
;;;			      (list "-f" (user-login-name)))
			 ;; These mean "report errors by mail"
			 ;; and "deliver in background".
			 (if (null mail-interactive) '("-oem" "-odb"))))
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
	  (kill-buffer errbuf)))))

(defun mail-do-fcc (header-end)
  (let (fcc-list
	(rmailbuf (current-buffer))
	(tembuf (generate-new-buffer " rmail output"))
	(case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^FCC:[ \t]*" header-end t)
	(setq fcc-list (cons (buffer-substring (point)
					       (progn
						 (end-of-line)
						 (skip-chars-backward " \t")
						 (point)))
			     fcc-list))
	(delete-region (match-beginning 0)
		       (progn (forward-line 1) (point))))
      (set-buffer tembuf)
      (erase-buffer)
      (insert "\nFrom " (user-login-name) " "
	      (current-time-string) "\n")
      (insert-buffer-substring rmailbuf)
      ;; Make sure messages are separated.
      (goto-char (point-max))
      (insert ?\n)
      (goto-char 2)
      ;; ``Quote'' "^From " as ">From "
      ;;  (note that this isn't really quoting, as there is no requirement
      ;;   that "^[>]+From " be quoted in the same transparent way.)
      (let ((case-fold-search nil))
	(while (search-forward "\nFrom " nil t)
	  (forward-char -5)
	  (insert ?>)))
      (while fcc-list
	(let ((buffer (get-file-buffer (car fcc-list))))
	  (if buffer
	      ;; File is present in a buffer => append to that buffer.
	      (let ((curbuf (current-buffer))
		    (beg (point-min)) (end (point-max)))
		(save-excursion
		  (set-buffer buffer)
		  (goto-char (point-max))
		  (insert-buffer-substring curbuf beg end)))
	    ;; Else append to the file directly.
	    (write-region (point-min) (point-max) (car fcc-list) t)))
	(setq fcc-list (cdr fcc-list))))
    (kill-buffer tembuf)))

(defun mail-to ()
  "Move point to end of To-field."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "To"))

(defun mail-subject ()
  "Move point to end of Subject-field."
  (interactive)
  (expand-abbrev)
  (mail-position-on-field "Subject"))

(defun mail-cc ()
  "Move point to end of CC-field.  Create a CC field if none."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "cc" t)
      (progn (mail-position-on-field "to")
	     (insert "\nCC: "))))

(defun mail-bcc ()
  "Move point to end of BCC-field.  Create a BCC field if none."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "bcc" t)
      (progn (mail-position-on-field "to")
	     (insert "\nBCC: "))))

(defun mail-position-on-field (field &optional soft)
  (let (end
	(case-fold-search t))
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n"))
    (setq end (match-beginning 0))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" (regexp-quote field) ":") end t)
	(progn
	  (re-search-forward "^[^ \t]" nil 'move)
	  (beginning-of-line)
	  (skip-chars-backward "\n")
	  t)
      (or soft
	  (progn (goto-char end)
		 (skip-chars-backward "\n")
		 (insert "\n" field ": ")))
      nil)))

(defun mail-signature ()
  "Sign letter with contents of ~/.signature file."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert-file-contents (expand-file-name "~/.signature"))))

(defun mail-fill-yanked-message (&optional justifyp)
  "Fill the paragraphs of a message yanked into this one.
Numeric argument means justify as well."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n") nil t)
    (fill-individual-paragraphs (point)
				(point-max)
				justifyp
				t)))
(defun mail-yank-original (arg)
  "Insert the message being replied to, if any (in rmail).
Puts point before the text and mark after.
Indents each nonblank line ARG spaces (default 3).
Just \\[universal-argument] as argument means don't indent
and don't delete any header fields."
  (interactive "P")
  (if mail-reply-buffer
      (let ((start (point)))
	(delete-windows-on mail-reply-buffer)
	(insert-buffer mail-reply-buffer)
	(if (consp arg)
	    nil
	  (mail-yank-clear-headers start (mark))
	  (indent-rigidly start (mark)
			  (if arg (prefix-numeric-value arg) 3)))
	(exchange-point-and-mark)
	(if (not (eolp)) (insert ?\n)))))

(defun mail-yank-clear-headers (start end)
  (save-excursion
    (goto-char start)
    (if (search-forward "\n\n" end t)
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char start)
	  (while (let ((case-fold-search t))
		   (re-search-forward mail-yank-ignored-headers nil t))
	    (beginning-of-line)
	    (delete-region (point)
			   (progn (re-search-forward "\n[^ \t]")
				  (forward-char -1)
				  (point))))))))

;; Put these last, to reduce chance of lossage from quitting in middle of loading the file.

(defun mail (&optional noerase to subject in-reply-to cc replybuffer)
  "Edit a message to be sent.  Argument means resume editing (don't erase).
Returns with message buffer selected; value t if message freshly initialized.
While editing message, type C-c C-c to send the message and exit.

Separate names of recipients with commas.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields:
\\{mail-mode-map}

If mail-self-blind is non-nil, a BCC to yourself is inserted
when the message is initialized.

If mail-default-reply-to is non-nil, it should be an address (a string);
a Reply-to: field with that address is inserted.

If mail-archive-file-name is non-nil, an FCC field with that file name
is inserted.

If mail-setup-hook is bound, its value is called with no arguments
after the message is initialized.  It can add more default fields.

When calling from a program, the second through fifth arguments
 TO, SUBJECT, IN-REPLY-TO and CC specify if non-nil
 the initial contents of those header fields.
 These arguments should not have final newlines.
The sixth argument REPLYBUFFER is a buffer whose contents
 should be yanked if the user types C-c C-y."
  (interactive "P")
  (switch-to-buffer "*mail*")
  (setq default-directory (expand-file-name "~/"))
  (auto-save-mode auto-save-default)
  (mail-mode)
  (and (not noerase)
       (or (not (buffer-modified-p))
	   (y-or-n-p "Unsent message being composed; erase it? "))
       (progn (erase-buffer)
	      (mail-setup to subject in-reply-to cc replybuffer)
	      t)))

(defun mail-other-window (&optional noerase to subject in-reply-to cc replybuffer)
  "Like `mail' command, but display mail buffer in another window."
  (interactive "P")
  (let ((pop-up-windows t))
    (pop-to-buffer "*mail*"))
  (mail noerase to subject in-reply-to cc replybuffer))

;;; Do not add anything but external entries on this page.
