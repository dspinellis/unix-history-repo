;; "RMAIL" mail reader for Emacs.
;; Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;; Souped up by shane@mit-ajax based on ideas of rlk@athena.mit.edu
;;   New features include attribute and keyword support, message
;;   selection by dispatch table, summary by attributes and keywords,
;;   expunging by dispatch table, sticky options for file commands.

(require 'mail-utils)
(provide 'rmail)

; these variables now declared in loaddefs or paths.el
;(defvar rmail-spool-directory "/usr/spool/mail/"
;  "This is the name of the directory used by the system mailer for\n\
;delivering new mail.  It's name should end with a slash.")
;(defvar rmail-dont-reply-to-names
;  nil
;  "*A regexp specifying names to prune of reply to messages.
;nil means dont reply to yourself.")
;(defvar rmail-ignored-headers
;   "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^received:\\|^message-id:\\|^summary-line:"
;   "*Gubbish headers one would rather not see.")
;(defvar rmail-file-name
;  (expand-file-name "~/RMAIL")
;  "")
;
;(defvar rmail-delete-after-output nil
;  "*Non-nil means automatically delete a message that is copied to a file.")
;
;(defvar rmail-primary-inbox-list 
;  '("/usr/spool/mail/$USER" "~/mbox")
; "")

;; these may be altered by site-init.el to match the format of mmdf files
;;  delimitation used on a given host (delim1 and delim2 from the config
;;  files)

(defvar mmdf-delim1 "^\001\001\001\001\n"
  "Regexp marking the start of an mmdf message")
(defvar mmdf-delim2 "^\001\001\001\001\n"
  "Regexp marking the end of an mmdf message")

(defvar rmail-message-filter nil
  "If non nil, is a filter function for new headers in RMAIL.
Called with region narrowed to unformatted header.")

(defvar rmail-mode-map nil)

;; Message counters and markers.  Deleted flags.

(defvar rmail-current-message nil)
(defvar rmail-total-messages nil)
(defvar rmail-message-vector nil)
(defvar rmail-deleted-vector nil)

;; These are used by autoloaded rmail-summary.

(defvar rmail-summary-buffer nil)
(defvar rmail-summary-vector nil)

;; `Sticky' default variables.

;; Last individual label specified to a or k.
(defvar rmail-last-label nil)
;; Last set of labels specified to C-M-n or C-M-p or C-M-l.
(defvar rmail-last-multi-labels nil)
(defvar rmail-last-file nil)
(defvar rmail-last-rmail-file nil)

;;;; *** Rmail Mode ***

(defun rmail (&optional file-name-arg)
  "Read and edit incoming mail.
Moves messages into file named by  rmail-file-name  (a babyl format file)
 and edits that file in RMAIL Mode.
Type \\[describe-mode] once editing that file, for a list of RMAIL commands.

May be called with filename as argument;
then performs rmail editing on that file,
but does not copy any new mail into the file."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Run rmail on RMAIL file: "
					 nil nil t))))
  (or rmail-last-file
      (setq rmail-last-file (expand-file-name "~/xmail")))
  (or rmail-last-rmail-file
      (setq rmail-last-rmail-file (expand-file-name "~/XMAIL")))
  (let* ((file-name (expand-file-name (or file-name-arg rmail-file-name)))
	 (existed (get-file-buffer file-name)))
    ;; Like find-file, but in the case where a buffer existed
    ;; and the file was reverted, recompute the message-data.
    (if (and existed (not (verify-visited-file-modtime existed)))
	(progn
	  (find-file file-name)
	  (if (verify-visited-file-modtime existed)
	      (progn (rmail-forget-messages)
		     (rmail-set-message-counters))))
      (find-file file-name))
    (if existed
	nil
      (rmail-mode)
      ;; Provide default set of inboxes for primary mail file ~/RMAIL.
      (and (null rmail-inbox-list)
	   (null file-name-arg)
	   (setq rmail-inbox-list
		 (or rmail-primary-inbox-list
		     (list "~/mbox"
			   (concat rmail-spool-directory
				   (or (getenv "LOGNAME")
				       (getenv "USER")
				       (user-login-name)))))))
      ;; Convert all or part to Babyl file if possible.
      (rmail-convert-file)
      (goto-char (point-max))
      (if (null rmail-inbox-list)
	  (progn
	    (rmail-set-message-counters)
	    (rmail-show-message))))
    (rmail-get-new-mail)))

(defun rmail-convert-file ()
  (let (convert)
    (widen)
    (goto-char (point-min))
    ;; If file doesn't start like a Babyl file,
    ;; convert it to one, by adding a header and converting each message.
    (cond ((looking-at "BABYL OPTIONS:"))
	  ((looking-at "Version: 5\n")
	   ;; Losing babyl file made by old version of Rmail.
	   ;; Just fix the babyl file header; don't make a new one,
	   ;; so we don't lose the Labels: file attribute, etc.
	   (let ((buffer-read-only nil))
	     (insert "BABYL OPTIONS:\n")))
	  (t
	   (setq convert t)
	   (rmail-insert-rmail-file-header)))
    ;; If file was not a Babyl file or if there are
    ;; Unix format messages added at the end,
    ;; convert file as necessary.
    (if (or convert
	    (progn (goto-char (point-max))
		   (search-backward "\^_")
		   (forward-char 1)
		   (looking-at "\n*From ")))
	(let ((buffer-read-only nil))
	  (message "Converting to Babyl format...")
	  (narrow-to-region (point) (point-max))
	  (rmail-convert-to-babyl-format)
	  (message "Converting to Babyl format...done")))))

(defun rmail-insert-rmail-file-header ()
  (let ((buffer-read-only nil))
    (insert "BABYL OPTIONS:
Version: 5
Labels:
Note:   This is the header of an rmail file.
Note:   If you are seeing it in rmail,
Note:    it means the file has no messages in it.\n\^_")))

(if rmail-mode-map
    nil
  (setq rmail-mode-map (make-keymap))
  (suppress-keymap rmail-mode-map)
  (define-key rmail-mode-map "." 'rmail-beginning-of-message)
  (define-key rmail-mode-map " " 'scroll-up)
  (define-key rmail-mode-map "\177" 'scroll-down)
  (define-key rmail-mode-map "n" 'rmail-next-undeleted-message)
  (define-key rmail-mode-map "p" 'rmail-previous-undeleted-message)
  (define-key rmail-mode-map "\en" 'rmail-next-message)
  (define-key rmail-mode-map "\ep" 'rmail-previous-message)
  (define-key rmail-mode-map "\e\C-n" 'rmail-next-labeled-message)
  (define-key rmail-mode-map "\e\C-p" 'rmail-previous-labeled-message)
  (define-key rmail-mode-map "a" 'rmail-add-label)
  (define-key rmail-mode-map "k" 'rmail-kill-label)
  (define-key rmail-mode-map "d" 'rmail-delete-forward)
  (define-key rmail-mode-map "u" 'rmail-undelete-previous-message)
  (define-key rmail-mode-map "e" 'rmail-expunge)
  (define-key rmail-mode-map "x" 'rmail-expunge)
  (define-key rmail-mode-map "s" 'rmail-expunge-and-save)
  (define-key rmail-mode-map "g" 'rmail-get-new-mail)
  (define-key rmail-mode-map "h" 'rmail-summary)
  (define-key rmail-mode-map "\e\C-h" 'rmail-summary)
  (define-key rmail-mode-map "l" 'rmail-summary-by-labels)
  (define-key rmail-mode-map "\e\C-l" 'rmail-summary-by-labels)
  (define-key rmail-mode-map "\e\C-r" 'rmail-summary-by-recipients)
  (define-key rmail-mode-map "t" 'rmail-toggle-header)
  (define-key rmail-mode-map "m" 'rmail-mail)
  (define-key rmail-mode-map "r" 'rmail-reply)
  (define-key rmail-mode-map "c" 'rmail-continue)
  (define-key rmail-mode-map "f" 'rmail-forward)
  (define-key rmail-mode-map "\es" 'rmail-search)
  (define-key rmail-mode-map "j" 'rmail-show-message)
  (define-key rmail-mode-map "o" 'rmail-output-to-rmail-file)
  (define-key rmail-mode-map "\C-o" 'rmail-output)
  (define-key rmail-mode-map "i" 'rmail-input)
  (define-key rmail-mode-map "q" 'rmail-quit)
  (define-key rmail-mode-map ">" 'rmail-last-message)
  (define-key rmail-mode-map "?" 'describe-mode)
  (define-key rmail-mode-map "w" 'rmail-edit-current-message)
  (define-key rmail-mode-map "\C-d" 'rmail-delete-backward))

;; Rmail mode is suitable only for specially formatted data.
(put 'rmail-mode 'mode-class 'special)

(defun rmail-mode ()
  "Rmail Mode is used by \\[rmail] for editing Rmail files.
All normal editing commands are turned off.
Instead, these commands are available:

.	Move point to front of this message (same as \\[beginning-of-buffer]).
SPC	Scroll to next screen of this message.
DEL	Scroll to previous screen of this message.
n	Move to Next non-deleted message.
p	Move to Previous non-deleted message.
M-n	Move to Next message whether deleted or not.
M-p	Move to Previous message whether deleted or not.
>	Move to the last message in Rmail file.
j	Jump to message specified by numeric position in file.
M-s	Search for string and show message it is found in.
d	Delete this message, move to next nondeleted.
C-d	Delete this message, move to previous nondeleted.
u	Undelete message.  Tries current message, then earlier messages
	till a deleted message is found.
e	Expunge deleted messages.
s	Expunge and save the file.
q       Quit Rmail: expunge, save, then switch to another buffer.
C-x C-s Save without expunging.
g	Move new mail from system spool directory or mbox into this file.
m	Mail a message (same as \\[mail-other-window]).
c	Continue composing outgoing message started before.
r	Reply to this message.  Like m but initializes some fields.
f	Forward this message to another user.
o       Output this message to an Rmail file (append it).
C-o	Output this message to a Unix-format mail file (append it).
i	Input Rmail file.  Run Rmail on that file.
a	Add label to message.  It will be displayed in the mode line.
k	Kill label.  Remove a label from current message.
C-M-n   Move to Next message with specified label
          (label defaults to last one specified).
          Standard labels: filed, unseen, answered, forwarded, deleted.
          Any other label is present only if you add it with `a'.
C-M-p   Move to Previous message with specified label
C-M-h	Show headers buffer, with a one line summary of each message.
C-M-l	Like h only just messages with particular label(s) are summarized.
C-M-r   Like h only just messages with particular recipient(s) are summarized.
t	Toggle header, show Rmail header if unformatted or vice versa.
w	Edit the current message.  C-c C-c to return to Rmail."
  (interactive)
  (kill-all-local-variables)
  (rmail-mode-1)
  (rmail-variables)
  (run-hooks 'rmail-mode-hook))

(defun rmail-mode-1 ()
  (setq major-mode 'rmail-mode)
  (setq mode-name "RMAIL")
  (setq buffer-read-only t)
  ;; No need to auto save RMAIL files.
  (setq buffer-auto-save-file-name nil)
  (if (boundp 'mode-line-modified)
      (setq mode-line-modified "--- ")
    (setq mode-line-format
	  (cons "--- " (cdr (default-value 'mode-line-format)))))
  (use-local-map rmail-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table))

(defun rmail-variables ()
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'rmail-revert)
  (make-local-variable 'rmail-last-label)
  (make-local-variable 'rmail-deleted-vector)
  (make-local-variable 'rmail-keywords)
  (make-local-variable 'rmail-summary-buffer)
  (make-local-variable 'rmail-summary-vector)
  (make-local-variable 'rmail-current-message)
  (make-local-variable 'rmail-total-messages)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil)
  (make-local-variable 'version-control)
  (setq version-control 'never)
  (make-local-variable 'file-precious-flag)
  (setq file-precious-flag t)
  (make-local-variable 'rmail-message-vector)
  (make-local-variable 'rmail-last-file)
  (make-local-variable 'rmail-inbox-list)
  (setq rmail-inbox-list (rmail-parse-file-inboxes))
  (make-local-variable 'rmail-keywords)
  ;; this gets generated as needed
  (setq rmail-keywords nil))

;; Handle M-x revert-buffer done in an rmail-mode buffer.
(defun rmail-revert (arg noconfirm)
  (let (revert-buffer-function)
    ;; Call our caller again, but this time it does the default thing.
    (if (revert-buffer arg noconfirm)
	;; If the user said "yes", and we changed something,
	;; reparse the messages.
	(progn
	  (rmail-convert-file)
	  (goto-char (point-max))
	  (rmail-set-message-counters)
	  (rmail-show-message)))))

;; Return a list of files from this buffer's Mail: option.
;; Does not assume that messages have been parsed.
;; Just returns nil if buffer does not look like Babyl format.
(defun rmail-parse-file-inboxes ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char 1)
      (cond ((looking-at "Babyl options:")
	     (search-forward "\^_" nil 'move)
	     (narrow-to-region 1 (point))
	     (goto-char 1)
	     (if (search-forward "\nMail:" nil t)
		 (progn
		   (narrow-to-region (point) (progn (end-of-line) (point)))
		   (goto-char (point-min))
		   (mail-parse-comma-list))))))))

(defun rmail-expunge-and-save ()
  "Expunge and save RMAIL file."
  (interactive)
  (rmail-expunge)
  (save-buffer))

(defun rmail-quit ()
  "Quit out of RMAIL."
  (interactive)
  (rmail-expunge-and-save)
  ;; Don't switch to the summary buffer even if it was recently visible.
  (if rmail-summary-buffer
      (bury-buffer rmail-summary-buffer))
  (let ((obuf (current-buffer)))
    (switch-to-buffer (other-buffer))
    (bury-buffer obuf)))

(defun rmail-input (filename)
  "Run RMAIL on file FILENAME."
  (interactive "FRun rmail on RMAIL file: ")
  (rmail filename))


;;;; *** Rmail input ***

;; RLK feature not added in this version:
;; argument specifies inbox file or files in various ways.

(defun rmail-get-new-mail (&optional file-name)
  "Move any new mail from this RMAIL file's inbox files.
The inbox files can be specified with the file's Mail: option.
The variable rmail-primary-inbox-list specifies the inboxes for
your primary RMAIL file if it has no Mail: option.
These are normally your ~/mbox and your /usr/spool/mail/$USER.

You can also specify the file to get new mail from.  In this
case, the file of new mail is not changed or deleted.
Noninteractively, you can pass the inbox file name as an argument.
Interactively, a prefix argument causes us to read a file name
and use that file as the inbox."
  (interactive
   (list (if current-prefix-arg
	     (read-file-name "Get new mail from file: "))))
  (or (verify-visited-file-modtime (current-buffer))
      (progn
	(find-file (buffer-file-name))
	(if (verify-visited-file-modtime (current-buffer))
	    (rmail-forget-messages))))
  (rmail-maybe-set-message-counters)
  (widen)
  (unwind-protect
      (let ((opoint (point))
	    (new-messages 0)
	    (delete-files ())
	    ;; If buffer has not changed yet, and has not been saved yet,
	    ;; don't replace the old backup file now.
	    (make-backup-files (and make-backup-files (buffer-modified-p)))
	    (buffer-read-only nil))
	(goto-char (point-max))
	(skip-chars-backward " \t\n")	; just in case of brain damage
	(delete-region (point) (point-max))	; caused by require-final-newline
	(save-excursion
	  (save-restriction
	    (narrow-to-region (point) (point))
	    ;; Read in the contents of the inbox files,
	    ;; renaming them as necessary,
	    ;; and adding to the list of files to delete eventually.
	    (if file-name
		(rmail-insert-inbox-text (list file-name) nil)
	      (setq delete-files (rmail-insert-inbox-text rmail-inbox-list t)))
	    ;; Scan the new text and convert each message to babyl format.
	    (goto-char (point-min))
	    (save-excursion
	      (setq new-messages (rmail-convert-to-babyl-format)))
	    (or (zerop new-messages)
		(let (success)
		  (widen)
		  (search-backward "\^_")
		  (narrow-to-region (point) (point-max))
		  (goto-char (1+ (point-min)))
		  (rmail-count-new-messages)
		  (save-buffer)))
	    ;; Delete the old files, now that babyl file is saved.
	    (while delete-files
	      (condition-case ()
		  (delete-file (car delete-files))
		(file-error nil))
	      (setq delete-files (cdr delete-files)))))
	(if (= new-messages 0)
	    (progn (goto-char opoint)
		   (if (or file-name rmail-inbox-list)
		       (message "(No new mail has arrived)")))
	    (message "%d new message%s read"
		     new-messages (if (= 1 new-messages) "" "s"))))
    ;; Don't leave the buffer screwed up if we get a disk-full error.
    (rmail-show-message)))

(defun rmail-insert-inbox-text (files renamep)
  (let (file tofile delete-files movemail)
    (while files
      (setq file (expand-file-name (substitute-in-file-name (car files)))
	    ;;>> un*x specific <<
	    tofile (concat file "~"))
      ;; If getting from mail spool directory,
      ;; use movemail to move rather than renaming.
      (setq movemail (equal (file-name-directory file) rmail-spool-directory))
      (if movemail
	  (progn
	    (setq tofile (expand-file-name
			   ".newmail"
			   (file-name-directory
			     (expand-file-name rmail-file-name))))
	    ;; On some systems, /usr/spool/mail/foo is a directory
	    ;; and the actual inbox is /usr/spool/mail/foo/foo.
	    (if (file-directory-p file)
		(setq file (expand-file-name (or (getenv "LOGNAME")
						 (getenv "USER")
						 (user-login-name))
					     file)))))
      (if (or (file-exists-p tofile) (file-exists-p file))
	  (message "Getting mail from %s..." file))
      ;; Set TOFILE if have not already done so, and
      ;; rename or copy the file FILE to TOFILE if and as appropriate.
      (cond ((not renamep)
	     (setq tofile file))
	    ((or (file-exists-p tofile) (not (file-exists-p file)))
	     nil)
	    ((not movemail)
	     (rename-file file tofile nil))
	    (t
	     (let ((errors nil))
	       (unwind-protect
		   (save-excursion
		     (setq errors (generate-new-buffer " *rmail loss*"))
		     (buffer-flush-undo errors)
		     (call-process
		       (expand-file-name "movemail" exec-directory)
		       nil errors nil file tofile)
		     (if (not (buffer-modified-p errors))
			 ;; No output => movemail won
			 nil
		       (set-buffer errors)
		       (subst-char-in-region (point-min) (point-max)
					     ?\n ?\  )
		       (goto-char (point-max))
		       (skip-chars-backward " \t")
		       (delete-region (point) (point-max))
		       (goto-char (point-min))
		       (if (looking-at "movemail: ")
			   (delete-region (point-min) (match-end 0)))
		       (signal 'file-error
			       (list "movemail"
				     (buffer-substring (point-min)
						       (point-max))
				     ;file tofile
				     ))))
		 (if errors (kill-buffer errors))))))
      ;; At this point, TOFILE contains the name to read:
      ;; Either the alternate name (if we renamed)
      ;; or the actual inbox (if not renaming).
      (if (file-exists-p tofile)
	  (progn (goto-char (point-max))
		 (insert-file-contents tofile)
		 (goto-char (point-max))
		 (or (= (preceding-char) ?\n)
		     (insert ?\n))
		 (setq delete-files (cons tofile delete-files))))
      (message "")
      (setq files (cdr files)))
    delete-files))

;; the  rmail-break-forwarded-messages  feature is not implemented
(defun rmail-convert-to-babyl-format ()
  (let ((count 0) start
	(case-fold-search nil))
    (goto-char (point-min))
    (save-restriction
      (while (not (eobp))
	(cond ((looking-at "Babyl Options:");Babyl header
	       (search-forward "\n\^_")
	       (delete-region (point-min) (point)))
	      ;; Babyl format message
	      ((looking-at "\^L")
	       (or (search-forward "\n\^_" nil t)
		   (progn
		     (message "Invalid Babyl format in inbox!")
		     (sit-for 1)
		     (goto-char (point-max))))
	       (setq count (1+ count))
	       (skip-chars-forward " \t\n")
	       (narrow-to-region (point) (point-max)))
	      ;;*** MMDF format
	      ((let ((case-fold-search t))
		 (looking-at mmdf-delim1))
	       (let ((case-fold-search t))
		 (replace-match "\^L\n0, unseen,,\n*** EOOH ***\n")
		 (setq start (point))
		 (re-search-forward mmdf-delim2 nil t)
		 (replace-match "\^_"))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (1- (point)))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t) ; single char "\^_"
		     (replace-match "\n^_")))) ; 2 chars: "^" and "_"
	       (narrow-to-region (point) (point-max))
	       (setq count (1+ count)))
	      ;;*** Mail format
	      ((looking-at "^From ")
	       (setq start (point))
	       (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
	       (rmail-nuke-pinhead-header)
	       (if (re-search-forward
		    (concat "^[\^_]?\\("
			    "From [^ \n]*\\(\\|\".*\"[^ \n]*\\)  ?[^ \n]* [^ \n]* *"
			    "[0-9]* [0-9:]*\\( ?[A-Z]?[A-Z][A-Z]T\\| ?[-+]?[0-9][0-9][0-9][0-9]\\|\\) " ; EDT, -0500
			    "19[0-9]* *$\\|"
			    mmdf-delim1 "\\|"
			    "^Babyl Options:\\|"
			    "\^L\n[01],\\)") nil t)
		   (goto-char (match-beginning 1))
		 (goto-char (point-max)))
	       (setq count (1+ count))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (point))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (insert ?\^_)
	       (narrow-to-region (point) (point-max)))
	      ;;
	      ;;This is a kludge, in case we're wrong about mmdf not
	      ;;allowing anything in between.  If it loses, we'll have
	      ;;to look for something else
	      (t (delete-char 1)))))
    count))

(defun rmail-nuke-pinhead-header ()
  (save-excursion
    (save-restriction
      (let ((start (point))
  	    (end (progn
		   (condition-case ()
		       (search-forward "\n\n")
		     (error
		      (goto-char (point-max))
		      (insert "\n\n")))
		   (point)))
	    has-from has-date)
	(narrow-to-region start end)
	(let ((case-fold-search t))
	  (goto-char start)
	  (setq has-from (search-forward "\nFrom:" nil t))
	  (goto-char start)
	  (setq has-date (and (search-forward "\nDate:" nil t) (point)))
	  (goto-char start))
	(let ((case-fold-search nil))
	  (if (re-search-forward
	       "^From \\([^ ]*\\(\\|\".*\"[^ ]*\\)\\)  ?\\([^ ]*\\) \\([^ ]*\\) *\\([0-9]*\\) \\([0-9:]*\\)\\( ?[A-Z]?[A-Z][A-Z]T\\| ?[-+]?[0-9][0-9][0-9][0-9]\\|\\) 19\\([0-9]*\\) *\n" nil t)
	      (replace-match
		(concat
		  ;; Keep and reformat the date if we don't
		  ;;  have a Date: field.
		  (if has-date
		      ""
		    ;; If no time zone specified, assume est.
		    (if (= (match-beginning 7) (match-end 7))
			"Date: \\3, \\5 \\4 \\8 \\6 EST\n"
			"Date: \\3, \\5 \\4 \\8 \\6\\7\n"))
		  ;; Keep and reformat the sender if we don't
		  ;; have a From: field.
		  (if has-from
		      ""
		    "From: \\1\n")))))))))

;;;; *** Rmail Message Formatting and Header Manipulation ***

(defun rmail-reformat-message (beg end)
  (goto-char beg)
  (forward-line 1)
  (if (/= (following-char) ?0)
      (error "Bad format in RMAIL file."))
  (let ((buffer-read-only nil)
	(delta (- (buffer-size) end)))
    (delete-char 1)
    (insert ?1)
    (forward-line 1)
    (if (looking-at "Summary-line: ")
	(forward-line 1))
    (if (looking-at "\\*\\*\\* EOOH \\*\\*\\*\n")
	(delete-region (point)
		       (progn (forward-line 1) (point))))
    (let ((str (buffer-substring (point)
				 (save-excursion (search-forward "\n\n" end 'move)
						 (point)))))
      (insert str "*** EOOH ***\n")
      (narrow-to-region (point) (- (buffer-size) delta)))
    (goto-char (point-min))
    (if rmail-ignored-headers (rmail-clear-headers))
    (if rmail-message-filter (funcall rmail-message-filter))))

(defun rmail-clear-headers ()
  (if (search-forward "\n\n" nil t)
      (save-restriction
        (narrow-to-region (point-min) (point))
	(let ((buffer-read-only nil))
	  (while (let ((case-fold-search t))
		   (goto-char (point-min))
		   (re-search-forward rmail-ignored-headers nil t))
	    (beginning-of-line)
	    (delete-region (point)
			   (progn (re-search-forward "\n[^ \t]")
				  (forward-char -1)
				  (point))))))))

(defun rmail-toggle-header ()
  "Show original message header if pruned header currently shown, or vice versa."
  (interactive)
  (rmail-maybe-set-message-counters)
  (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
  (let ((buffer-read-only nil))
    (goto-char (point-min))
    (forward-line 1)
    (if (= (following-char) ?1)
	(progn (delete-char 1)
	       (insert ?0)
	       (forward-line 1)
	       (if (looking-at "Summary-Line:")
		   (forward-line 1))
	       (insert "*** EOOH ***\n")
	       (forward-char -1)
	       (search-forward "\n*** EOOH ***\n")
	       (forward-line -1)
	       (let ((temp (point)))
		 (and (search-forward "\n\n" nil t)
		      (delete-region temp (point))))
	       (goto-char (point-min))
	       (search-forward "\n*** EOOH ***\n")
	       (narrow-to-region (point) (point-max)))
      (rmail-reformat-message (point-min) (point-max)))))

;;;; *** Rmail Attributes and Keywords ***

;; Make a string describing current message's attributes and keywords
;; and set it up as the name of a minor mode
;; so it will appear in the mode line.
(defun rmail-display-labels ()
  (let ((blurb "") (beg (point-min-marker)) (end (point-max-marker)))
    (save-excursion
      (unwind-protect
	  (progn
	    (widen)
	    (goto-char (rmail-msgbeg rmail-current-message))
	    (forward-line 1)
	    (if (looking-at "[01],")
		(progn
		  (narrow-to-region (point) (progn (end-of-line) (point)))
		  ;; Truly valid BABYL format requires a space before each
		  ;; attribute or keyword name.  Put them in if missing.
		  (let (buffer-read-only)
		    (goto-char (point-min))
		    (while (search-forward "," nil t)
		      (or (looking-at "[ ,]") (eobp)
			  (insert " "))))
		  (goto-char (point-max))
		  (if (search-backward ",," nil 'move)
		      (progn
			(if (> (point) (1+ (point-min)))
			    (setq blurb (buffer-substring (+ 1 (point-min)) (point))))
			(if (> (- (point-max) (point)) 2)
			    (setq blurb
				  (concat blurb
					  ";"
					  (buffer-substring (+ (point) 3)
							    (1- (point-max)))))))))))
	(narrow-to-region beg end)
	(set-marker beg nil)
	(set-marker end nil)))
    (while (string-match " +," blurb)
      (setq blurb (concat (substring blurb 0 (match-beginning 0)) ","
			  (substring blurb (match-end 0)))))
    (while (string-match ", +" blurb)
      (setq blurb (concat (substring blurb 0 (match-beginning 0)) ","
			  (substring blurb (match-end 0)))))
    (setq mode-line-process
	  (concat " " rmail-current-message "/" rmail-total-messages
		  blurb))))

;; Turn an attribute of the current message on or off according to STATE.
;; ATTR is the name of the attribute, as a string.
(defun rmail-set-attribute (attr state)
  (let ((omax (- (buffer-size) (point-max)))
	(omin (- (buffer-size) (point-min)))
	(buffer-read-only nil))
    (unwind-protect
	(save-excursion
	  (widen)
	  (goto-char (+ 3 (rmail-msgbeg rmail-current-message)))
	  (let ((curstate (search-backward (concat ", " attr ",")
					   (prog1 (point) (end-of-line)) t)))
	    (or (eq curstate (not (not state)))
		(if curstate
		    (delete-region (point) (1- (match-end 0)))
		  (beginning-of-line)
		  (forward-char 2)
		  (insert " " attr ","))))
	  (if (string= attr "deleted")
	      (rmail-set-message-deleted-p rmail-current-message state)))
      (narrow-to-region (max 1 (- (buffer-size) omin))
			(- (buffer-size) omax))
      (rmail-display-labels))))

;; Return t if the attributes/keywords line of msg number MSG
;; contains a match for the regexp LABELS.
(defun rmail-message-labels-p (msg labels)
  (goto-char (rmail-msgbeg msg))
  (forward-char 3)
  (re-search-backward labels (prog1 (point) (end-of-line)) t))

;;;; *** Rmail Message Selection And Support ***

(defun rmail-msgend (n)
  (marker-position (aref rmail-message-vector (1+ n))))

(defun rmail-msgbeg (n)
  (marker-position (aref rmail-message-vector n)))

(defun rmail-widen-to-current-msgbeg (function)
  "Call FUNCTION with point at start of internal data of current message.
Assumes that bounds were previously narrowed to display the message in Rmail.
The bounds are widened enough to move point where desired,
then narrowed again afterward.
Assumes that the visible text of the message is not changed by FUNCTION."
  (save-excursion
    (let ((obeg (- (point-max) (point-min)))
      (unwind-protect
	  (progn
	    (narrow-to-region (rmail-msgbeg rmail-current-message)
			      (point-max))
	    (goto-char (point-min))
	    (funcall function))
	(narrow-to-region (- (point-max) obeg) (point-max)))))))

(defun rmail-forget-messages ()
  (unwind-protect
      (if (vectorp rmail-message-vector)
	  (let* ((i 0)
		 (v rmail-message-vector)
		 (n (length v)))
	    (while (< i n)
	      (move-marker (aref v i)  nil)
	      (setq i (1+ i)))))
    (setq rmail-message-vector nil)
    (setq rmail-deleted-vector nil)))

(defun rmail-maybe-set-message-counters ()
  (if (not (and rmail-deleted-vector
		rmail-message-vector
		rmail-current-message
		rmail-total-messages))
      (rmail-set-message-counters)))

(defun rmail-count-new-messages (&optional nomsg)
  (let* ((case-fold-search nil)
	 (total-messages 0)
	 (messages-head nil)
	 (deleted-head nil))
    (or nomsg (message "Counting new messages..."))
    (goto-char (point-max))
    ;; Put at the end of messages-head
    ;; the entry for message N+1, which marks
    ;; the end of message N.  (N = number of messages).
    (search-backward "\^_")
    (setq messages-head (list (point-marker)))
    (rmail-set-message-counters-counter (point-min))
    (setq rmail-current-message (1+ rmail-total-messages))
    (setq rmail-total-messages
	  (+ rmail-total-messages total-messages))
    (setq rmail-message-vector
	  (vconcat rmail-message-vector (cdr messages-head)))
    (aset rmail-message-vector
	  rmail-current-message (car messages-head))
    (setq rmail-deleted-vector
	  (concat rmail-deleted-vector deleted-head))
    (setq rmail-summary-vector
	  (vconcat rmail-summary-vector (make-vector total-messages nil)))
    (goto-char (point-min))
    (or nomsg (message "Counting new messages...done (%d)" total-messages))))

(defun rmail-set-message-counters ()
  (rmail-forget-messages)
  (save-excursion
    (save-restriction
      (widen)
      (let* ((point-save (point))
	     (total-messages 0)
	     (messages-after-point)
	     (case-fold-search nil)
	     (messages-head nil)
	     (deleted-head nil))
	(message "Counting messages...")
	(goto-char (point-max))
	;; Put at the end of messages-head
	;; the entry for message N+1, which marks
	;; the end of message N.  (N = number of messages).
	(search-backward "\^_")
	(setq messages-head (list (point-marker)))
	(rmail-set-message-counters-counter (min (point) point-save))
	(setq messages-after-point total-messages)
	(rmail-set-message-counters-counter)
	(setq rmail-total-messages total-messages)
	(setq rmail-current-message
	      (min total-messages
		   (max 1 (- total-messages messages-after-point))))
	(setq rmail-message-vector
	      (apply 'vector (cons (point-min-marker) messages-head))
	      rmail-deleted-vector (concat "D" deleted-head)
	      rmail-summary-vector (make-vector rmail-total-messages nil))
	(message "Counting messages...done")))))
	
(defun rmail-set-message-counters-counter (&optional stop)
  (while (search-backward "\^_\^L\n" stop t)
    (setq messages-head (cons (point-marker) messages-head))
    (save-excursion
      (setq deleted-head
	    (cons (if (search-backward ", deleted,"
				       (prog1 (point)
					 (forward-line 2))
				       t)
		      ?D ?\ )
		  deleted-head)))
    (if (zerop (% (setq total-messages (1+ total-messages)) 20))
	(message "Counting messages...%d" total-messages))))

(defun rmail-beginning-of-message ()
  "Show current message starting from the beginning."
  (interactive)
  (rmail-show-message rmail-current-message))

(defun rmail-show-message (&optional n)
  "Show message number N (prefix argument), counting from start of file."
  (interactive "p")
  (rmail-maybe-set-message-counters)
  (widen)
  (if (zerop rmail-total-messages)
      (progn (narrow-to-region (point-min) (1- (point-max)))
	     (goto-char (point-min))
	     (setq mode-line-process nil))
    (let (blurb)
      (if (not n)
	  (setq n rmail-current-message)
	(cond ((<= n 0)
	       (setq n 1
		     rmail-current-message 1
		     blurb "No previous message"))
	      ((> n rmail-total-messages)
	       (setq n rmail-total-messages
		     rmail-current-message rmail-total-messages
		     blurb "No following message"))
	      (t
	       (setq rmail-current-message n))))
      (let ((beg (rmail-msgbeg n))
	    (end (rmail-msgend n)))
	(goto-char beg)
	(forward-line 1)
	(if (= (following-char) ?0)
	    (progn
	      (rmail-reformat-message beg end)
	      (rmail-set-attribute "unseen" nil))
	  (search-forward "\n*** EOOH ***\n" end t)
	  (narrow-to-region (point) end))
	(goto-char (point-min))
	(rmail-display-labels)
	(run-hooks 'rmail-show-message-hook)
	(if blurb
	    (message blurb))))))

(defun rmail-next-message (n)
  "Show following message whether deleted or not.
With prefix argument N, moves forward N messages,
or backward if N is negative."
  (interactive "p")
  (rmail-maybe-set-message-counters)
  (rmail-show-message (+ rmail-current-message n)))

(defun rmail-previous-message (n)
  "Show previous message whether deleted or not.
With prefix argument N, moves backward N messages,
or forward if N is negative."
  (interactive "p")
  (rmail-next-message (- n)))  

(defun rmail-next-undeleted-message (n)
  "Show following non-deleted message.
With prefix argument N, moves forward N non-deleted messages,
or backward if N is negative."
  (interactive "p")
  (rmail-maybe-set-message-counters)
  (let ((lastwin rmail-current-message)
	(current rmail-current-message))
    (while (and (> n 0) (< current rmail-total-messages))
      (setq current (1+ current))
      (if (not (rmail-message-deleted-p current))
	  (setq lastwin current n (1- n))))
    (while (and (< n 0) (> current 1))
      (setq current (1- current))
      (if (not (rmail-message-deleted-p current))
	  (setq lastwin current n (1+ n))))
    (if (/= lastwin rmail-current-message)
	(rmail-show-message lastwin))
    (if (< n 0)
	(message "No previous nondeleted message"))
    (if (> n 0)
	(message "No following nondeleted message"))))

(defun rmail-previous-undeleted-message (n)
  "Show previous non-deleted message.
With prefix argument N, moves backward N non-deleted messages,
or forward if N is negative."
  (interactive "p")
  (rmail-next-undeleted-message (- n)))

(defun rmail-last-message ()
  "Show last message in file."
  (interactive)
  (rmail-maybe-set-message-counters)
  (rmail-show-message rmail-total-messages))

(defun rmail-what-message ()
  (let ((where (point))
	(low 1)
	(high rmail-total-messages)
	(mid (/ rmail-total-messages 2)))
    (while (> (- high low) 1)
      (if (>= where (rmail-msgbeg mid))
	  (setq low mid)
	(setq high mid))
      (setq mid (+ low (/ (- high low) 2))))
    (if (>= where (rmail-msgbeg high)) high low)))

(defvar rmail-search-last-regexp nil)
(defun rmail-search (regexp &optional reversep)
  "Show message containing next match for REGEXP.
Search in reverse (earlier messages) with non-nil 2nd arg REVERSEP.
Interactively, empty argument means use same regexp used last time,
and reverse search is specified by a negative numeric arg."
  (interactive
    (let* ((reversep (< (prefix-numeric-value current-prefix-arg) 0))
	   (prompt (concat (if reversep "Reverse " "") "Rmail search (regexp): "))
	   regexp)
      (if rmail-search-last-regexp
	  (setq prompt (concat prompt
			       "(default "
			       rmail-search-last-regexp
			       ") ")))
      (setq regexp (read-string prompt))
      (cond ((not (equal regexp ""))
	     (setq rmail-search-last-regexp regexp))
	    ((not rmail-search-last-regexp)
	     (error "No previous Rmail search string")))
      (list rmail-search-last-regexp reversep)))
  (message "%sRmail search for %s..."
	   (if reversep "Reverse " "")
	   regexp)
  (let ((omin (point-min))
	(omax (point-max))
	(opoint (point))
	win
	(msg rmail-current-message))
    (unwind-protect
	(progn
	  (widen)
	  ;; Check messages one by one, advancing message number up or down
	  ;; but searching forward through each message.
	  (if reversep
	      (while (and (null win) (> msg 1))
		(goto-char (rmail-msgbeg (setq msg (1- msg))))
		(setq win (re-search-forward
			   regexp (rmail-msgend msg) t)))
	    (while (and (null win) (< msg rmail-total-messages))
	      (goto-char (rmail-msgbeg (setq msg (1+ msg))))
	      (setq win (re-search-forward regexp (rmail-msgend msg) t)))))
      (if win
	  (progn
	    ;; If this is a reverse search and we found a message,
	    ;; search backward thru this message to position point.
	    (if reversep
		(progn
		  (goto-char (rmail-msgend msg))
		  (re-search-backward
		   regexp (rmail-msgbeg msg) t)))
	    (setq win (point))
	    (rmail-show-message msg)
	    (message "%sRmail search for %s...done"
		     (if reversep "Reverse " "")
		     regexp)
	    (goto-char win))
	(goto-char opoint)
	(narrow-to-region omin omax)
	(ding)
	(message "Searched failed: %s" regexp)))))

;;;; *** Rmail Message Deletion Commands ***

(defun rmail-message-deleted-p (n)
  (= (aref rmail-deleted-vector n) ?D))

(defun rmail-set-message-deleted-p (n state)
  (aset rmail-deleted-vector n (if state ?D ?\ )))

(defun rmail-delete-message ()
  "Delete this message and stay on it."
  (interactive)
  (rmail-set-attribute "deleted" t))

(defun rmail-undelete-previous-message ()
  "Back up to deleted message, select it, and undelete it."
  (interactive)
  (let ((msg rmail-current-message))
    (while (and (> msg 0)
		(not (rmail-message-deleted-p msg)))
      (setq msg (1- msg)))
    (if (= msg 0)
	(error "No previous deleted message")
      (if (/= msg rmail-current-message)
	  (rmail-show-message msg))
      (rmail-set-attribute "deleted" nil))))

(defun rmail-delete-forward (&optional backward)
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given.
With prefix argument, delete and move backward."
  (interactive "P")
  (rmail-set-attribute "deleted" t)
  (rmail-next-undeleted-message (if backward -1 1)))

(defun rmail-delete-backward ()
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given."
  (interactive)
  (rmail-delete-forward t))

(defun rmail-expunge ()
  "Actually erase all deleted messages in the file."
  (interactive)
  (message "Expunging deleted messages...")
  (rmail-maybe-set-message-counters)
  (let* ((omax (- (buffer-size) (point-max)))
	 (omin (- (buffer-size) (point-min)))
	 (opoint (if (and (> rmail-current-message 0)
			  (= ?D (aref rmail-deleted-vector rmail-current-message)))
		     0 (- (point) (point-min))))
	 (messages-head (cons (aref rmail-message-vector 0) nil))
	 (messages-tail messages-head)
	 (win))
    (unwind-protect
	(save-excursion
	  (widen)
	  (goto-char (point-min))
	  (let ((counter 0)
		(number 1)
		(total rmail-total-messages)
		(new-message-number rmail-current-message)
		(new-summary nil)
		(buffer-read-only nil)
		(messages rmail-message-vector)
		(deleted rmail-deleted-vector)
		(summary rmail-summary-vector))
	    (setq rmail-total-messages nil
		  rmail-current-message nil
		  rmail-message-vector nil
		  rmail-deleted-vector nil
		  rmail-summary-vector nil)
	    (while (<= number total)
	      (if (= (aref deleted number) ?D)
		  (progn
		    (delete-region
		      (marker-position (aref messages number))
		      (marker-position (aref messages (1+ number))))
		    (move-marker (aref messages number) nil)
		    (if (> new-message-number counter)
			(setq new-message-number (1- new-message-number))))
		(setq counter (1+ counter))
		(setq messages-tail
		      (setcdr messages-tail
			      (cons (aref messages number) nil)))
		(setq new-summary
		      (cons (if (= counter number) (aref summary (1- number)))
			    new-summary)))
	      (if (zerop (% (setq number (1+ number)) 20))
		  (message "Expunging deleted messages...%d" number)))
	    (setq messages-tail
		  (setcdr messages-tail
			  (cons (aref messages number) nil)))
	    (setq rmail-current-message new-message-number
		  rmail-total-messages counter
		  rmail-message-vector (apply 'vector messages-head)
		  rmail-deleted-vector (make-string (1+ counter) ?\ )
		  rmail-summary-vector (vconcat (nreverse new-summary))
		  win t)))
      (message "Expunging deleted messages...done")
      (if (not win)
	  (narrow-to-region (- (buffer-size) omin) (- (buffer-size) omax)))
      (rmail-show-message
       (if (zerop rmail-current-message) 1 nil))
      (forward-char opoint))))

;;;; *** Rmail Mailing Commands ***

(defun rmail-mail ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (interactive)
  (mail-other-window nil nil nil nil nil (current-buffer)))

(defun rmail-continue ()
  "Continue composing outgoing message previously being composed."
  (interactive)
  (mail-other-window t))

(defun rmail-reply (just-sender)
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.
While composing the reply, use \\[mail-yank-original] to yank the
original message into it."
  (interactive "P")
  ;;>> this gets set even if we abort. Can't do anything about it, though.
  (rmail-set-attribute "answered" t)
  (rmail-display-labels)
  (let (from reply-to cc subject date to message-id resent-reply-to)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (rmail-msgbeg rmail-current-message))
	(forward-line 1)
	(if (= (following-char) ?0)
	    (narrow-to-region
	     (progn (forward-line 2)
		    (point))
	     (progn (search-forward "\n\n" (rmail-msgend rmail-current-message)
				    'move)
		    (point)))
	  (narrow-to-region (point)
			    (progn (search-forward "\n*** EOOH ***\n")
				   (beginning-of-line) (point))))
	(setq resent-reply-to (mail-fetch-field "resent-reply-to" t)
	      from (mail-fetch-field "from")
	      reply-to (or resent-reply-to
			   (mail-fetch-field "reply-to" nil t)
			   from)
	      cc (cond (just-sender nil)
		       (resent-reply-to (mail-fetch-field "resent-cc" t))
		       (t (mail-fetch-field "cc" nil t)))
	      subject (or (and resent-reply-to
			       (mail-fetch-field "resent-subject" t))
			  (mail-fetch-field "subject"))
	      date (cond (resent-reply-to
			  (mail-fetch-field "resent-date" t))
			 ((mail-fetch-field "date")))
	      to (cond (resent-reply-to
			(mail-fetch-field "resent-to" t))
		       ((mail-fetch-field "to" nil t))
		       ;((mail-fetch-field "apparently-to")) ack gag barf
		       (t ""))
	      message-id (cond (resent-reply-to
				(mail-fetch-field "resent-message-id" t))
			       ((mail-fetch-field "message-id"))))))
    (and subject
	 (string-match "\\`Re: " subject)
	 (setq subject (substring subject 4)))
    (mail-other-window nil
      (mail-strip-quoted-names reply-to)
      subject
      (rmail-make-in-reply-to-field from date message-id)
      (if just-sender
	  nil
	(let* ((cc-list (rmail-dont-reply-to
			  (mail-strip-quoted-names
			    (if (null cc) to (concat to ", " cc))))))
	  (if (string= cc-list "") nil cc-list)))
      (current-buffer))))

(defun rmail-make-in-reply-to-field (from date message-id)
  (if mail-use-rfc822 (require 'rfc822))
  (let (field)
    (if (and mail-use-rfc822 from)
	(let ((tem (car (rfc822-addresses from))))
	  (and message-id
	       (setq field (if (string-match
				 (regexp-quote
				   (if (string-match "@[^@]*\\'" tem)
				       (substring tem
						  0 (match-beginning 0))
				       tem))
				 message-id)
			       message-id
			       (concat message-id " \"" tem "\""))
		     message-id nil date nil))
	  (or field
	      (setq field (prin1-to-string tem))))
;	(if message-id
;	    (setq field message-id message-id nil date nil)
;	    (setq field (car (rfc882-addresses from))))
	)
    (or field
	(not from)
	;; Compute the sender for the in-reply-to; prefer full name.
	(let* ((stop-pos (string-match "  *at \\|  *@ \\|  *<" from))
	       (start-pos (if stop-pos 0
			    ;;>> this loses on nested ()'s
			    (let ((pos (string-match " *(" from)))
			      (if (not pos) nil
				(setq stop-pos (string-match ")" from pos))
				(if (zerop pos) 0 (+ 2 pos)))))))
	  (setq field (if stop-pos
			  (substring from start-pos stop-pos)
			  from))))
    (if date (setq field (concat field "'s message of " date)))
    (if message-id (setq field (concat field " " message-id)))
    field))

(defun rmail-forward ()
  "Forward the current message to another user."
  (interactive)
  ;;>> this gets set even if we abort. Can't do anything about it, though.
  (rmail-set-attribute "forwarded" t)
  (let ((forward-buffer (current-buffer))
	(subject (concat "["
			 (mail-strip-quoted-names (mail-fetch-field "From"))
			 ": " (or (mail-fetch-field "Subject") "") "]")))
    ;; If only one window, use it for the mail buffer.
    ;; Otherwise, use another window for the mail buffer
    ;; so that the Rmail buffer remains visible
    ;; and sending the mail will get back to it.
    (if (if (one-window-p t)
	    (mail nil nil subject)
	  (mail-other-window nil nil subject))
	(save-excursion
	  (goto-char (point-max))
	  (forward-line 1)
	  (insert-buffer forward-buffer)))))

;;;; *** Rmail Specify Inbox Files ***

(autoload 'set-rmail-inbox-list "rmailmsc"
  "Set the inbox list of the current RMAIL file to FILE-NAME.
This may be a list of file names separated by commas.
If FILE-NAME is empty, remove any inbox list."
  t)

;;;; *** Rmail Commands for Labels ***

(autoload 'rmail-add-label "rmailkwd"
  "Add LABEL to labels associated with current RMAIL message.
Completion is performed over known labels when reading."
  t)

(autoload 'rmail-kill-label "rmailkwd"
  "Remove LABEL from labels associated with current RMAIL message.
Completion is performed over known labels when reading."
  t)

(autoload 'rmail-next-labeled-message "rmailkwd"
  "Show next message with LABEL.  Defaults to last label used.
With prefix argument N moves forward N messages with this label."
  t)

(autoload 'rmail-previous-labeled-message "rmailkwd"
  "Show previous message with LABEL.  Defaults to last label used.
With prefix argument N moves backward N messages with this label."
  t)

;;;; *** Rmail Edit Mode ***

(autoload 'rmail-edit-current-message "rmailedit"
  "Edit the contents of the current message"
  t)

;;;; *** Rmail Summary Mode ***

(autoload 'rmail-summary "rmailsum"
  "Display a summary of all messages, one line per message."
  t)

(autoload 'rmail-summary-by-labels "rmailsum"
  "Display a summary of all messages with one or more LABELS.
LABELS should be a string containing the desired labels, separated by commas."
  t)

(autoload 'rmail-summary-by-recipients "rmailsum"
  "Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers;
but if PRIMARY-ONLY is non-nil (prefix arg given),
 only look in the To and From fields.
RECIPIENTS is a string of names separated by commas."
  t)

;;;; *** Rmail output messages to files ***

(autoload 'rmail-output-to-rmail-file "rmailout"
  "Append the current message to an Rmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the Emacs
buffer visiting that file."
  t)

(autoload 'rmail-output "rmailout"
  "Append this message to Unix mail file named FILE-NAME."
  t)

;;;; *** Rmail undigestification ***

(autoload 'undigestify-rmail-message "undigest"
  "Break up a digest message into its constituent messages.
Leaves original message, deleted, before the undigestified messages."
  t)
