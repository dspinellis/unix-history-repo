;;;  mh-e.el	(Version: 3.6 for GNU Emacs Version 18 and MH.5 and MH.6)

(defvar mh-e-RCS-id)
(setq mh-e-RCS-id "$Header: mh-e.el,v 2.24 88/08/29 12:07:53 larus Exp $")
(provide 'mh-e)

;;;  Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation, Inc.
;;;     Author:  James Larus (larus@ginger.Berkeley.EDU or ucbvax!larus)
;;;	Please send suggestions and corrections to the above address.
;;;
;;;  This file contains mh-e, a GNU Emacs front end to the MH mail system.


;; GNU Emacs is distributed in the hope that it will be useful,
;; but without any warranty.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; document "GNU Emacs copying permission notice".   An exact copy
;; of the document is supposed to have been given to you along with
;; GNU Emacs so that you can know how you may redistribute it all.
;; It should be in a file named COPYING.  Among other things, the
;; copyright notice and this notice must be preserved on all copies.


;;;  Original version for Gosling emacs by Brian Reid, Stanford, 1982.
;;;  Modified by James Larus, BBN, July 1984 and UCB, 1984 & 1985.
;;;  Rewritten for GNU Emacs, James Larus 1985.  larus@ginger.berkeley.edu
;;;  Modified by Stephen Gildea 1988.  gildea@bbn.com


;;;  NB.  MH must have been compiled with the MHE compiler flag or several
;;;  features necessary mh-e will be missing from MH commands, specifically
;;;  the -build switch to repl and forw.



;;; Constants:

;;; Set for local environment:
;;;* These are now in paths.el.
;;;(defvar mh-progs "/usr/new/mh/" "Directory containing MH commands")
;;;(defvar mh-lib "/usr/new/lib/mh/" "Directory of MH library")

(defvar mh-redist-full-contents t
  "Non-nil if the `dist' command needs whole letter for redistribution (i.e.,
when `send' is compiled with the BERK option).  Nil otherwise.")


;;; Mode hooks:

(defvar mh-folder-mode-hook nil
  "*Invoked in mh-folder-mode on a new folder.")

(defvar mh-letter-mode-hook nil
  "*Invoked in mh-letter-mode on a new letter.")

(defvar mh-compose-letter-hook nil
  "*Invoked in mh-compose-and-send-mail on an outgoing letter.  It is passed
three arguments: TO recipients, SUBJECT, and CC recipients.")

(defvar mh-inc-folder-hook nil
  "*Invoked after incorporating new mail into a folder.")



;;; Personal preferences:

(defvar mh-clean-message-header nil
  "*Non-nil means remove invisible header lines or only show visible header
lines in messages.")

(defvar mh-visible-headers nil
  "*If non-nil, it contains a regexp specifying the headers that are shown in
a message if mh-clean-message-header is non-nil.  Setting this variable
overrides mh-invisible-headers.")

(defvar mhl-formfile nil
  "*Name of format file to be used by mhl to show messages.
A value of T means use the default format file.
Nil means don't use mhl to format messages.")

(defvar mh-lpr-command-format "lpr -p -J '%s'"
  "*Format for Unix command line to print a message. The format should be
a unix command line, with the string \"%s\" where the folder and message
number should appear.")

(defvar mh-print-background nil
  "*Print messages in the background if non-nil.  WARNING: do not delete
the messages until printing is finished; otherwise, your output may be
truncated.")

(defvar mh-summary-height 4
  "*Number of lines in summary window.")

(defvar mh-recenter-summary-p nil
  "*Recenter summary window when the show window is toggled off if
this is non-nil.")

(defvar mh-ins-buf-prefix ">> "
  "*String to put before each non-blank line of the the current message
as it is inserted in an outgoing letter.")

(defvar mh-do-not-confirm nil
  "*Non-nil means do not prompt for confirmation before executing some
innocuous commands.")

(defvar mh-bury-show-buffer t
  "*Non-nil means that the displayed show buffer for a folder is buried.")

(defvar mh-delete-yanked-msg-window nil
  "*If non-nil, yanking the current message into a letter being composed,
with \\[mh-yank-cur-msg], deletes any windows displaying the message.")

(defvar mh-yank-from-start-of-msg t
  "*If non-nil, \\[mh-yank-cur-msg] will include the entire message.  If
`body' then the message minus the header will be yanked.  If nil, only the
portion of the message following the point will be yanked.  If there is a
region in the show buffer, this variable is ignored.")

(defvar mh-reply-default-reply-to nil
  "*If non-nil, then \\[mh-reply] will use this as the person or persons to
which the reply will be sent.  The value should be one of \"from\", \"to\", or
\"cc\".")

(defvar mh-recursive-folders nil
  "*If non-nil, then commands which operate on folders do so recursively.")


;;; Parameterize mh-e to work with different scan formats.  The defaults work
;;; the standard MH scan listings.

(defvar mh-cmd-note 4
  "Offset to insert notation")

(defvar mh-good-msg-regexp  "^....[^D^]"
  "Regexp specifiying the scan lines that are 'good' messages.")

(defvar mh-deleted-msg-regexp "^....D"
  "Regexp matching scan lines of deleted messages.")

(defvar mh-refiled-msg-regexp  "^....\\^"
  "Regexp matching scan lines of refiled messages.")

(defvar mh-valid-scan-line "^[ ]*[0-9]"
  "Regexp matching scan lines for messages (not error messages).")

(defvar mh-msg-number-regexp "^[ ]*\\([0-9]+\\)"
  "Regexp matching the number of a message in a scan line.  It must surround
the number with \\( \\)")

(defvar mh-msg-search-regexp "^[^0-9]*%d[^0-9]"
  "String for format that will return a regexp matching the scan listing for
a given message number.")

(defvar mh-flagged-scan-msg-regexp "^....\\D\\|^....\\^\\|^....\\+\\|^.....%"
  "Regexp matching scan lines marked as deleted, refiled, in a sequence, or
the cur message.")

(defvar mh-cur-scan-msg-regexp "^....\\+"
  "regexp matching scan line for the cur message.")


;;; Real constants:

(defvar mh-invisible-headers
  "^Received: \\|^Message-Id: \\|^Remailed-\\|^Via: \\|^Mail-from: \\|^Return-Path: \\|^In-Reply-To: \\|^Resent-"
  "Regexp specifying headers that are not to be shown.")

(defvar mh-rejected-letter-start "^   ----- Unsent message follows -----$"
  "Regexp specifying the beginning of the wrapper around a letter returned
by the mail system.")

(defvar mh-to-field-choices '((?t . "To:") (?s . "Subject:") (?c . "Cc:")
			      (?b . "Bcc:") (?f . "Fcc:"))
  "A-list of (character . field name) strings for mh-to-field.")


;;; Global variables:

(defvar mh-user-path  ""
  "User's mail folder.")

(defvar mh-last-destination nil
  "Destination of last `refile' command.")

(defvar mh-folder-mode-map (make-keymap)
  "Keymap for MH folders.")

(defvar mh-letter-mode-map (make-sparse-keymap)
  "Keymap for composing mail.")

(defvar mh-pick-mode-map (make-sparse-keymap)
  "Keymap for searching folder.")

(defvar mh-letter-mode-syntax-table nil
  "Syntax table used while in mh-e letter mode.")

(if mh-letter-mode-syntax-table
    ()
    (setq mh-letter-mode-syntax-table
	  (make-syntax-table text-mode-syntax-table))
    (set-syntax-table mh-letter-mode-syntax-table)
    (modify-syntax-entry ?% "." mh-letter-mode-syntax-table))

(defvar mh-folder-list nil
  "List of folder names for completion.")

(defvar mh-draft-folder nil
  "Name of folder containing draft messages.
NIL means do not use draft folder.")

(defvar mh-unseen-seq nil
  "Name of the unseen sequence.")


;;; Macros and generic functions:

(defmacro mh-push (v l)
  (list 'setq l (list 'cons v l)))

(defmacro when (pred &rest body)
  (list 'cond (cons pred body)))

(defun mapc (func list)
  (while list
    (funcall func (car list))
    (setq list (cdr list))))


(defun mh-list* (&rest args) (mh-make-list* args))

(defun mh-make-list* (arglist)
  (cond ((null arglist) ())
	((null (cdr arglist)) (car arglist))
	(t (cons (car arglist) (mh-make-list* (cdr arglist))))))



;;; Entry points:

(defun mh-rmail (&optional arg)
  "Inc(orporate) new mail (no arg) or scan a MH mail box (arg given).
This front end uses the MH mail system, which uses different conventions
from the usual mail system."
  (interactive "P")
  (mh-find-path)
  (if arg
      (call-interactively 'mh-visit-folder)
      (mh-inc-folder)))


(defun mh-smail ()
  "Send mail using the MH mail system."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send))


(defun mh-smail-other-window ()
  "Send mail in other window using the MH mail system."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send-other-window))



;;; User executable mh-e commands:

(defun mh-burst-digest ()
  "Burst apart the current message, which should be a digest.  Message is
replaced by its table of contents and the letters from the digest are inserted
into the folder after that message."
  (interactive)
  (let ((digest (mh-get-msg-num t)))
    (mh-process-or-undo-commands mh-current-folder)
    (message "Bursting digest...")
    (mh-exec-cmd "burst" mh-current-folder digest "-inplace")
    (mh-scan-folder mh-current-folder (format "%d-last" mh-first-msg-num))
    (message "Bursting digest...done")))


(defun mh-copy-msg (prefix-provided msg-or-seq dest)
  "Copy specified MESSAGE(s) (default: displayed message) to another
FOLDER without deleting them.
If (optional) prefix argument provided, then prompt for the message sequence."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq "Copy" t mh-narrowed-to-seq)
			 (mh-get-msg-num t))
		     (mh-prompt-for-folder "Copy to" "" t)))
  (mh-exec-cmd "refile" msg-or-seq "-link" "-src" mh-current-folder dest)
  (if prefix-provided
      (mh-notate-seq msg-or-seq ?C mh-cmd-note)
      (mh-notate msg-or-seq ?C mh-cmd-note)))


(defun mh-delete-msg (prefix-provided msg-or-seq)
  "Mark the specified MESSAGE(s) (default: displayed message) for later
deletion.
If (optional) prefix argument provided, then prompt for the message sequence."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq "Delete" t mh-narrowed-to-seq)
			 (mh-get-msg-num t))))
  (if prefix-provided
      (mh-map-to-seq-msgs 'mh-delete-a-msg msg-or-seq)
      (mh-delete-a-msg msg-or-seq))
  (mh-next-msg))


(defun mh-delete-msg-from-seq (prefix-provided msg-or-seq &optional from-seq)
  "Delete MESSAGE (default: displayed message) from SEQUENCE.
If (optional) prefix argument provided, then delete all messages from a
sequence."
  (interactive (let ((argp current-prefix-arg))
		 (list argp
		       (if argp
			   (mh-read-seq "Delete" t mh-narrowed-to-seq)
			   (mh-get-msg-num t))
		       (if (not argp)
			   (mh-read-seq "Delete from" t mh-narrowed-to-seq)))))
  (if prefix-provided
      (mh-remove-seq msg-or-seq)
      (mh-remove-msg-from-seq msg-or-seq from-seq)))


(defun mh-edit-again (msg)
  "Clean-up a draft or a message previously sent and make it resendable."
  (interactive (list (mh-get-msg-num t)))
  (let* ((from-folder mh-current-folder)
	 (config (current-window-configuration))
	 (draft
	  (cond ((and mh-draft-folder (equal from-folder mh-draft-folder))
		 (find-file (mh-msg-filename msg))
		 (rename-buffer (format "draft-%d" msg))
		 (buffer-name))
		(t
		 (mh-read-draft "clean-up" (mh-msg-filename msg) nil)))))
    (mh-clean-msg-header (point-min)
			 "^Date:\\|^Received:\\|^Message-Id:\\|^From:"
			 nil)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (mh-compose-and-send-mail draft "" from-folder nil nil nil nil nil nil
			      config)))


(defun mh-execute-commands ()
  "Process outstanding delete and refile requests."
  (interactive)
  (if mh-narrowed-to-seq (mh-widen))
  (save-excursion
    (mh-process-commands mh-current-folder))
  (mh-goto-cur-msg)
  (mh-set-scan-mode)
  (mh-make-folder-mode-line))


(defun mh-extract-rejected-mail (msg)
  "Extract a letter returned by the mail system (default: displayed message)
and make it resendable."
  (interactive (list (mh-get-msg-num t)))
  (let ((from-folder mh-current-folder)
	(config (current-window-configuration))
	(draft (mh-read-draft "extraction" (mh-msg-filename msg) nil)))
    (goto-char (point-min))
    (cond ((re-search-forward mh-rejected-letter-start nil t)
	   (forward-char 1)
	   (delete-region (point-min) (point))
	   (mh-clean-msg-header (point-min)
				"^Date:\\|^Received:\\|^Message-Id:\\|^From:\\|^Sender:"
				nil))
	  (t
	   (message "Does not appear to be a rejected letter.")))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (mh-compose-and-send-mail draft "" from-folder msg (mh-get-field "To")
			      (mh-get-field "From") (mh-get-field "cc")
			      nil nil config)))


(defun mh-forward (prefix-provided msg-or-seq to cc)
  "Forward MESSAGE(s) (default: displayed message).
If (optional) prefix argument provided, then prompt for the message sequence."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq "Forward" t mh-narrowed-to-seq)
			 (mh-get-msg-num t))
		     (read-string "To: ")
		     (read-string "Cc: ")))
  (let* ((folder mh-current-folder)
	 (config (current-window-configuration))
	 ;; forw always leaves file in "draft" since it doesn't have -draft
	 (draft-name (mh-expand-file-name "draft" mh-user-path))
	 (draft (cond ((or (not (file-exists-p draft-name))
			   (y-or-n-p "The file 'draft' exists.  Discard it? "))
		       (mh-exec-cmd "forw" "-build"
				    mh-current-folder msg-or-seq)
		       (prog1
			   (mh-read-draft "" draft-name t)
			 (mh-insert-fields "To:" to "Cc:" cc)
			 (set-buffer-modified-p nil)))
		      (t
		       (mh-read-draft "" draft-name nil)))))
    (goto-char (point-min))
    (re-search-forward "^------- Forwarded Message")
    (previous-line 1)
    (narrow-to-region (point) (point-max))
    (let* ((subject (save-excursion (mh-get-field "From:")))
	   (trim (string-match "<" subject))
	   (forw-subject (save-excursion (mh-get-field "Subject:"))))
      (if trim
	  (setq subject (substring subject 0 (- trim 1))))
      (widen)
      (save-excursion
	(mh-insert-fields "Subject:" (format "[%s: %s]" subject forw-subject)))
      (delete-other-windows)
      (if prefix-provided
	  (mh-add-msgs-to-seq (mh-seq-to-msgs msg-or-seq) 'forwarded t)
	  (mh-add-msgs-to-seq msg-or-seq 'forwarded t))
      (mh-compose-and-send-mail draft "" folder msg-or-seq
				to subject cc
				"F" "Forwarded:"
				config))))


(defun mh-goto-msg (number &optional no-error-if-no-message dont-show)
  "Position the cursor at message NUMBER.
Non-nil second argument means do not signal an error if message does not exist.
Non-nil third argument means not to show the message.
Return non-nil if cursor is at message."
  (interactive "NMessage number? ")
  (let ((cur-msg (mh-get-msg-num nil))
	(starting-place (point))
	(msg-pattern (mh-msg-search-pat number)))
    (cond ((cond ((and cur-msg (= cur-msg number)) t)
		 ((and cur-msg
		       (< cur-msg number)
		       (re-search-forward msg-pattern nil t)) t)
		 ((and cur-msg
		       (> cur-msg number)
		       (re-search-backward msg-pattern nil t)) t)
		 (t			; Do thorough search of buffer
		  (goto-char (point-min))
		  (re-search-forward msg-pattern nil t)))
	    (beginning-of-line)
	    (if (not dont-show) (mh-maybe-show number))
	    t)
	  (t
	   (goto-char starting-place)
	   (if (not no-error-if-no-message)
	       (error "No message %d " number))
	   nil))))


(defun mh-inc-folder (&optional maildrop-name)
  "Inc(orporate) new mail into +inbox.
Optional prefix argument specifies an alternate maildrop from the default.
If this is given, mail is incorporated into the current folder, rather
than +inbox."
  (interactive (list (if current-prefix-arg
			 (expand-file-name
			  (read-file-name "inc mail from file: "
					  mh-user-path)))))
  (let ((config (current-window-configuration)))
    (if (not maildrop-name)
	(cond ((not (get-buffer "+inbox"))
	       (mh-make-folder "+inbox")
	       (setq mh-previous-window-config config))
	      ((not (eq (current-buffer) (get-buffer "+inbox")))
	       (switch-to-buffer "+inbox")
	       (setq mh-previous-window-config config)))))
  (mh-get-new-mail maildrop-name)
  (run-hooks 'mh-inc-folder-hook))


(defun mh-kill-folder ()
  "Remove the current folder."
  (interactive)
  (if (or mh-do-not-confirm
	  (yes-or-no-p (format "Remove folder %s? " mh-current-folder)))
      (let ((folder mh-current-folder))
	(mh-exec-cmd-demon "rmf" folder)
	(mh-remove-folder-from-folder-list folder)
	(message "Folder removed")
	(kill-buffer folder))
      (message "Folder not removed")))


(defun mh-list-folders ()
  "List mail folders."
  (interactive)
  (with-output-to-temp-buffer " *mh-temp*"
    (save-excursion
      (switch-to-buffer " *mh-temp*")
      (erase-buffer)
      (message "listing folders...")
      (mh-exec-cmd-output "folders" t)
      (goto-char (point-min))
      (message "listing folders...done"))))


(defun mh-msg-is-in-seq (msg)
  "Display the sequences that contain MESSAGE (default: displayed message)."
  (interactive (list (mh-get-msg-num t)))
  (message "Message %d is in sequences: %s"
	   msg
	   (mapconcat 'concat
		      (mh-list-to-string (mh-seq-containing-msg msg))
		      " ")))


(defun mh-narrow-to-seq (seq)
  "Restrict display of this folder to just messages in a sequence.
Reads which sequence.  Use \\[mh-widen] to undo this command."
  (interactive (list (mh-read-seq "Narrow to" t)))
  (let ((eob (point-max))
	(buffer-read-only nil))
    (cond ((mh-seq-to-msgs seq)
	   (mh-copy-seq-to-point seq eob)
	   (narrow-to-region eob (point-max))
	   (mh-make-folder-mode-line (symbol-name seq))
	   (recenter)
	   (setq mh-narrowed-to-seq seq))
	  (t
	   (error "No messages in sequence `%s'" (symbol-name seq))))))


(defun mh-next-undeleted-msg (&optional arg)
  "Move to next undeleted message in window."
  (interactive "p")
  (forward-line (if arg arg 1))
  (setq mh-next-direction 'forward)
  (cond ((re-search-forward mh-good-msg-regexp nil 0 arg)
	 (beginning-of-line)
	 (mh-maybe-show (mh-get-msg-num t)))
	(t
	 (forward-line -1)
	 (if (get-buffer mh-show-buffer)
	     (delete-windows-on mh-show-buffer)))))


(defun mh-pack-folder ()
  "Execute any outstanding commands for the current folder, then renumber the
remaining messages to be 1..N."
  (interactive)
  (message "packing buffer...")
  (mh-pack-folder-1)
  (mh-goto-cur-msg)
  (message "packing buffer...done"))


(defun mh-refile-msg (prefix-provided msg-or-seq dest)
  "Refile MESSAGE(s) (default: displayed message) in FOLDER.
If (optional) prefix argument provided, then prompt for message sequence."
  (interactive
   (list current-prefix-arg
	 (if current-prefix-arg
	     (mh-read-seq "Refile" t mh-narrowed-to-seq)
	     (mh-get-msg-num t))
	 (intern
	  (mh-prompt-for-folder "Destination"
				(if (eq 'refile (car mh-last-destination))
				    (symbol-name (cdr mh-last-destination))
				    "")
				t))))
  (setq mh-last-destination (cons 'refile dest))
  (if prefix-provided
      (mh-map-to-seq-msgs 'mh-refile-a-msg msg-or-seq dest)
      (mh-refile-a-msg msg-or-seq dest))
  (mh-next-msg))


(defun mh-refile-or-write-again (msg)
  "Re-execution the last refile or write command on the given MESSAGE (default:
displayed message).
Use the same folder or file as the previous refile or write command."
  (interactive (list (mh-get-msg-num t)))
  (if (null mh-last-destination)
      (error "No previous refile"))
  (cond ((eq (car mh-last-destination) 'refile)
	 (mh-refile-a-msg msg (cdr mh-last-destination))
	 (message "Destination folder: %s" (cdr mh-last-destination)))
	(t
	 (mh-write-msg-to-file msg (cdr mh-last-destination))
	 (message "Destination: %s" (cdr mh-last-destination))))
  (mh-next-msg))


(defun mh-reply (prefix-provided msg)
  "Reply to a MESSAGE (default: displayed message).
If (optional) prefix argument provided, then include the message in the reply."
  (interactive (list current-prefix-arg (mh-get-msg-num t)))
  (let ((minibuffer-help-form
	 "from => Sender only\nto => Sender and primary recipients\ncc or all => Sender and all recipients"))
    (let ((reply-to (or mh-reply-default-reply-to
			(completing-read "Reply to whom: "
					 '(("from") ("to") ("cc") ("all"))
					 nil
					 t)))
	  (msg-filename (mh-msg-filename msg))
	  (folder mh-current-folder)
	  (show-buffer mh-show-buffer)
	  (config (current-window-configuration)))
      (message "Composing a reply...")
      (cond ((or (equal reply-to "from") (equal reply-to ""))
	     (apply 'mh-exec-cmd
		    (mh-list* "repl" "-build"
			      "-nodraftfolder" mh-current-folder
			      msg
			      "-nocc" "all"
			      (if prefix-provided
				  (list "-filter" "mhl.reply")))))
	    ((equal reply-to "to")
	     (apply 'mh-exec-cmd
		    (mh-list* "repl" "-build"
			      "-nodraftfolder" mh-current-folder
			      msg
			      "-cc" "to"
			      (if prefix-provided
				  (list "-filter" "mhl.reply")))))
	    ((or (equal reply-to "cc") (equal reply-to "all"))
	     (apply 'mh-exec-cmd
		    (mh-list* "repl" "-build"
			      "-nodraftfolder" mh-current-folder
			      msg
			      "-cc" "all" "-nocc" "me"
			      (if prefix-provided
				  (list "-filter" "mhl.reply"))))))

      (let ((draft (mh-read-draft "reply"
				  (mh-expand-file-name "reply" mh-user-path)
				  t)))
	(delete-other-windows)
	(set-buffer-modified-p nil)

	(let ((to (mh-get-field "To:"))
	      (subject (mh-get-field "Subject:"))
	      (cc (mh-get-field "Cc:")))
	  (goto-char (point-min))
	  (mh-goto-header-end 1)
	  (if (not prefix-provided)
	      (mh-display-msg msg msg-filename show-buffer))
	  (mh-add-msgs-to-seq msg 'answered t)
	  (message "Composing a reply...done")
	  (mh-compose-and-send-mail draft "" folder msg to subject cc
				    "-" "Replied:" config))))))


(defun mh-restore-window-config ()
  "Restore the previous window configuration, if one exists."
  (interactive)
  (if mh-previous-window-config
      (set-window-configuration mh-previous-window-config)))


(defun mh-page-digest ()
  "Advance displayed message to next digested message."
  (interactive)
  (save-excursion
    (mh-show-message-in-other-window)
    ;; Go to top of screen (in case user moved point).
    (move-to-window-line 0)
    (let ((case-fold-search nil))
      ;; Search for blank line and then for From:
      (when (not (and (search-forward "\n\n" nil t)
		      (search-forward "From:" nil t)))
	(other-window -1)
	(error "No more messages.")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (search-backward "\n\n" nil t)
    (forward-line 2)
    (recenter 0)
    (other-window -1)))


(defun mh-page-digest-backwards ()
  "Back up displayed message to previous digested message."
  (interactive)
  (save-excursion
    (mh-show-message-in-other-window)
    ;; Go to top of screen (in case user moved point).
    (move-to-window-line 0)
    (let ((case-fold-search nil))
      (beginning-of-line)
      (when (not (and (search-backward "\n\n" nil t)
		      (search-backward "From:" nil t)))
	(other-window -1)
	(error "No more messages.")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (search-backward "\n\n" nil t)
    (forward-line 2)
    (recenter 0)
    (other-window -1)))


(defun mh-page-msg (&optional arg)
  "Page the displayed message forwards ARG lines or a full screen if no
argument is supplied."
  (interactive "P")
  (scroll-other-window arg))


(defun mh-previous-page (&optional arg)
  "Page the displayed message backwards ARG lines or a full screen if no
argument is supplied."
  (interactive "P")
  (save-excursion
    (mh-show-message-in-other-window)
    (unwind-protect
	(scroll-down arg)
      (other-window -1))))


(defun mh-previous-undeleted-msg (&optional arg)
  "Move to previous undeleted message in window."
  (interactive "p")
  (setq mh-next-direction 'backward)
  (beginning-of-line 1)
  (cond ((re-search-backward mh-good-msg-regexp nil 0 arg)
	 (mh-maybe-show (mh-get-msg-num t)))
	(t
	 (if (get-buffer mh-show-buffer)
	     (delete-windows-on mh-show-buffer)))))


(defun mh-print-msg (prefix-provided msg-or-seq)
  "Print MESSAGE(s) (default: displayed message) on a line printer.
If (optional) prefix argument provided, then prompt for the message sequence."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (reverse (mh-seq-to-msgs
				   (mh-read-seq "Print" t mh-narrowed-to-seq)))
			 (list (mh-get-msg-num t)))))
  (if prefix-provided
      (message "printing sequence...")
      (message "printing message..."))
  (let ((command
	 (if prefix-provided
	     (format "(scan -clear %s ; %s -nobell -clear %s %s) | %s"
		     (mapconcat (function (lambda (msg) msg)) msg-or-seq " ")
		     (mh-expand-file-name "mhl" mh-lib)
		     (if (stringp mhl-formfile)
			 (format "-form %s" mhl-formfile)
		         "")
		     (mh-msg-filenames msg-or-seq mh-folder-filename)
		     (format mh-lpr-command-format
			     (if prefix-provided
				 (format "Sequence from %s" mh-current-folder)
				 (format "%s/%d" mh-current-folder
					 (car msg-or-seq)))))
	     (format "%s -nobell -clear %s %s | %s"
		     (mh-expand-file-name "mhl" mh-lib)
		     (mh-msg-filenames msg-or-seq mh-folder-filename)
		     (if (stringp mhl-formfile)
			 (format "-form %s" mhl-formfile)
		         "")
		     (format mh-lpr-command-format
			     (if prefix-provided
				 (format "Sequence from %s" mh-current-folder)
				 (format "%s/%d" mh-current-folder
					 (car msg-or-seq))))))))
    (if mh-print-background
	(mh-exec-cmd-demon shell-file-name "-c" command)
	(call-process shell-file-name nil nil nil "-c" command))
    (if prefix-provided
	(mh-notate-seq msg-or-seq ?P mh-cmd-note)
	(mh-notate (car msg-or-seq) ?P mh-cmd-note))
    (mh-add-msgs-to-seq msg-or-seq 'printed t)
    (if prefix-provided
	(message "printing sequence...done")
        (message "printing message...done"))))


(defun mh-put-msg-in-seq (prefix-provided from to)
  "Add MESSAGE(s) (default: displayed message) to SEQUENCE.
If (optional) prefix argument provided, then prompt for the message sequence."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-seq-to-msgs
			   (mh-read-seq "Add messages from" t
					mh-narrowed-to-seq))
			 (mh-get-msg-num t))
		     (mh-read-seq "Add to" nil mh-narrowed-to-seq)))
  (mh-add-msgs-to-seq from to))


(defun mh-rescan-folder (range)
  "Rescan a folder after optionally processing the outstanding commands.
If (optional) prefix argument provided, prompt for the range of messages to
display.  Otherwise show the entire folder."
  (interactive (list (if current-prefix-arg
			  (read-string "Range [all]? ")
			  "all")))
  (setq mh-next-direction 'forward)
  (mh-scan-folder mh-current-folder range))


(defun mh-redistribute (to cc msg)
  "Redistribute a letter."
  (interactive (list (read-string "Redist-To: ")
		     (read-string "Redist-Cc: ")
		     (mh-get-msg-num t)))
  (save-window-excursion
    (let ((msg-filename (mh-msg-filename msg))
	  (folder mh-current-folder)
	  (draft (mh-read-draft "redistribution"
				(if mh-redist-full-contents
				    (mh-msg-filename msg)
				    nil)
				nil)))
      (mh-goto-header-end 0)
      (insert "Resent-To: " to "\n")
      (if (not (equal cc "")) (insert "Resent-cc: " cc "\n"))
      (mh-clean-msg-header (point-min)
			   "^Message-Id:\\|^Received:\\|^Return-Path:\\|^Sender:\\|^Date:\\|^From:"
			   nil)
      (save-buffer)
      (message "Redistributing...")
      (if mh-redist-full-contents
	  (call-process "/bin/sh" nil 0 nil "-c"
			(format "mhdist=1 mhaltmsg=%s %s -push %s"
				(buffer-file-name)
				(mh-expand-file-name "send" mh-progs)
				(buffer-file-name)))
	  (call-process "/bin/sh" nil 0 nil "-c"
			(format "mhdist=1 mhaltmsg=%s mhannotate=1 %s -push %s"
				msg-filename
				(mh-expand-file-name "send" mh-progs)
				(buffer-file-name))))
      (mh-annotate-msg msg folder "R"
		       "-component" "Resent:"
		       "-text" (format "\"%s %s\"" to cc))
      (kill-buffer draft)
      (message "Redistributing...done"))))


(defun mh-write-msg-to-file (msg file)
  "Append MESSAGE to the end of a FILE."
  (interactive (list (mh-get-msg-num t)
		     (expand-file-name
		      (read-file-name "Save message in file: "
				      (if (eq 'write (car mh-last-destination))
					  (cdr mh-last-destination)
					  "")))))
  (setq mh-last-destination (cons 'write file))
  (let ((file-name (mh-msg-filename msg)))
    (save-excursion
      (set-buffer (get-buffer-create " *mh-temp*"))
      (erase-buffer)
      (insert-file-contents file-name)
      (append-to-file (point-min) (point-max) file))))


(defun mh-search-folder (folder)
  "Search FOLDER for messages matching a pattern."
  (interactive (list (mh-prompt-for-folder "Search"
					   mh-current-folder
					   t)))
  (switch-to-buffer-other-window "pick-pattern")
  (if (or (zerop (buffer-size))
	  (not (y-or-n-p "Reuse pattern? ")))
      (mh-make-pick-template)
      (message ""))
  (setq mh-searching-folder folder))


(defun mh-send (to cc subject)
  "Compose and send a letter."
  (interactive "sTo: \nsCc: \nsSubject: ")
  (let ((config (current-window-configuration)))
    (delete-other-windows)
    (mh-send-sub to cc subject config)))


(defun mh-send-other-window (to cc subject)
  "Compose and send a letter in another window.."
  (interactive "sTo: \nsCc: \nsSubject: ")
  (let ((pop-up-windows t))
    (mh-send-sub to cc subject (current-window-configuration))))


(defun mh-send-sub (to cc subject config)
  "Do the real work of composing and sending a letter.
Expects the TO, CC, and SUBJECT fields as arguments.
CONFIG is the window configuration before sending mail."
  (let ((folder (if (boundp 'mh-current-folder) mh-current-folder))
	(msg-num (mh-get-msg-num nil)))
    (message "Composing a message...")
    (let ((draft (mh-read-draft
		  "message"
		  (if (file-exists-p (mh-expand-file-name "components"
							  mh-user-path))
		      (mh-expand-file-name "components" mh-user-path)
		      (if (file-exists-p (mh-expand-file-name "components"
							      mh-lib))
			  (mh-expand-file-name "components" mh-lib)
			  (error "Can't find components file")))
		  nil)))
      (mh-insert-fields "To:" to "Subject:" subject "Cc:" cc)
      (set-buffer-modified-p nil)
      (goto-char (point-max))
      (message "Composing a message...done")
      (mh-compose-and-send-mail draft "" folder msg-num
				to subject cc
				nil nil config))))


(defun mh-show (msg)
  "Show MESSAGE (default: displayed message)."
  (interactive (list (mh-get-msg-num t)))
  (setq mh-summarize nil)
  (mh-set-mode-name "mh-e show")
  (let ((folder mh-current-folder))
    (mh-display-msg msg (mh-msg-filename msg) mh-show-buffer)

    ;; These contortions are to force the summary line to be the top window.
    (switch-to-buffer-other-window folder)
    (delete-other-windows)
    (mh-show-message-in-other-window)
    (switch-to-buffer-other-window folder)
    (shrink-window (- (window-height) mh-summary-height))
    (recenter '(4))			;center this line
    (if mh-bury-show-buffer (bury-buffer mh-show-buffer))
    (if (not (memq msg mh-seen-list)) (mh-push msg mh-seen-list))))


(defun mh-sort-folder ()
  "Sort the messages in the current folder by date."
  (interactive "")
  (mh-process-or-undo-commands mh-current-folder)
  (setq mh-next-direction 'forward)
  (message "sorting folder...")
  (mh-exec-cmd "sortm" mh-current-folder)
  (message "sorting folder...done")
  (mh-scan-folder mh-current-folder "all"))


(defun mh-toggle-summarize ()
  "Turn the summary mode of displaying messages on or off."
  (interactive)
  (if mh-summarize
      (mh-show (mh-get-msg-num t))
      (mh-set-scan-mode)))


(defun mh-undo (prefix-provided msg-or-seq)
  "Undo the deletion or refile of the specified MESSAGE(s)
\(default: displayed message).
If (optional) prefix argument provided, then prompt for the message sequence."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq "Undo" t mh-narrowed-to-seq)
			 (mh-get-msg-num t))))
  (beginning-of-line)
  (cond ((looking-at mh-deleted-msg-regexp)
	 (cond (prefix-provided
		(mapc (function (lambda (msg)
			(setq mh-delete-list
			      (delq msg mh-delete-list))
			(mh-remove-msg-from-seq msg 'deleted t)))
		      (mh-seq-to-msgs msg-or-seq))
		(mh-notate-seq msg-or-seq ?  mh-cmd-note))
	       (t
		(setq mh-delete-list (delq msg-or-seq mh-delete-list))
		(mh-remove-msg-from-seq msg-or-seq 'deleted t)
		(mh-notate msg-or-seq ?  mh-cmd-note))))

	((looking-at mh-refiled-msg-regexp)
	 (cond (prefix-provided
		(mapc (function (lambda (msg)
			(mapc (function
			       (lambda (dest)
				(mh-remove-msg-from-seq msg dest t)))
			      mh-refile-list)))
		      (mh-seq-to-msgs msg-or-seq))
		(mh-notate-seq msg-or-seq ?  mh-cmd-note))
	       (t
		(mapc (function (lambda (dest)
			(mh-remove-msg-from-seq msg-or-seq dest t)))
		      mh-refile-list)
		(mh-notate msg-or-seq ?  mh-cmd-note))))

	(t nil))
  (if (mh-outstanding-commands-p)
      (mh-set-folder-modified-p nil)))


(defun mh-undo-folder ()
  "Undo all commands in current folder."
  (interactive "")
  (cond ((or mh-do-not-confirm
	     (yes-or-no-p "Undo all commands in folder? "))
	 (setq mh-delete-list nil
	       mh-refile-list nil
	       mh-seq-list nil
	       mh-next-direction 'forward)
	 (mh-unmark-all-headers t)
	 (mh-set-folder-modified-p nil))
	(t
	 (message "Commands not undone.")
	 (sit-for 2))))


(defun mh-visit-folder (folder range config)
  "Visit FOLDER and display RANGE of messages."
  (interactive (list (mh-prompt-for-folder "Visit" "+inbox" t)
		     (read-string "Range [all]? ")
		     (current-window-configuration)))
    (mh-scan-folder folder (if (equal range "") "all" range))
    (setq mh-previous-window-config config))


(defun mh-widen ()
  "Remove restrictions from the current folder, thereby showing all messages."
  (interactive "")
  (let ((buffer-read-only nil))
    (delete-region (point-min) (point-max))
    (widen)
    (mh-make-folder-mode-line))
  (setq mh-narrowed-to-seq nil))



;;; Support routines.

(defun mh-delete-a-msg (msg)
  ;; Delete the MESSAGE.
  (save-excursion
    (mh-goto-msg msg nil nil)
    (if (looking-at mh-refiled-msg-regexp)
	(error "Message %d is refiled.  Undo refile before deleting." msg))
    (mh-push msg mh-delete-list)
    (mh-add-msgs-to-seq msg 'deleted t)
    (mh-notate msg ?D mh-cmd-note)
    (mh-set-folder-modified-p t)))


(defun mh-refile-a-msg (msg destination)
  ;; Refile the MESSAGE in the FOLDER.
  (save-excursion
    (mh-goto-msg msg nil nil)
    (cond ((looking-at mh-deleted-msg-regexp)
	   (error "Message %d is deleted.  Undo delete before moving." msg))
	  (t
	   (if (not (memq destination mh-refile-list))
	       (mh-push destination mh-refile-list))
	   (mh-add-msgs-to-seq msg destination t)
	   (mh-notate msg ?^ mh-cmd-note)
	   (mh-set-folder-modified-p t)))))


(defun mh-display-msg (msg-num msg-filename show-buffer)
  ;; Display the message NUMBER and PATHNAME in BUFFER.
  (if (not (file-exists-p msg-filename))
      (error "Message %d does not exist." msg-num))
  ;; Bind these variables in case they are local to folder buffer.
  (let ((formfile mhl-formfile)
	(clean-message-header mh-clean-message-header)
	(invisible-headers mh-invisible-headers)
	(visible-headers mh-visible-headers))
    (switch-to-buffer show-buffer)
    (if mh-bury-show-buffer (bury-buffer (current-buffer)))
    (when (not (equal msg-filename buffer-file-name))
      ;; Buffer does not yet contain message.
      (clear-visited-file-modtime)
      (unlock-buffer)
      (erase-buffer)
      (if formfile
	  (if (stringp formfile)
	      (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
				      "-form" formfile msg-filename)
	      (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
				      msg-filename))
	  (insert-file-contents msg-filename t))
      (goto-char (point-min))
      (cond (clean-message-header
	     (mh-clean-msg-header (point-min)
				  invisible-headers
				  visible-headers)
	     (goto-char (point-min)))
	    (t
	     (let ((case-fold-search t))
	       (re-search-forward
		"^To:\\|^From:\\|^Subject:\\|^Date:" nil t)
	       (beginning-of-line)
	       (recenter 0))))
      (set-buffer-modified-p nil)
      (setq buffer-file-name msg-filename)
      (set-mark nil)
      (setq mode-line-buffer-identification
	    (list "{%b}  " (format "%s" folder) "/" (format "%d" msg-num))))))


(defun mh-show-message-in-other-window ()
  (let ((buffer mh-show-buffer))
    (switch-to-buffer-other-window buffer)
    (if mh-bury-show-buffer (bury-buffer (current-buffer)))))


(defun mh-clean-msg-header (start invisible-headers visible-headers)
  ;; Flush extraneous lines in a message header, from the given POINT to the
  ;; end of the message header.  If VISIBLE-HEADERS is non-nil, it contains a
  ;; regular expression specifying the lines to display, otherwise
  ;; INVISIBLE-HEADERS contains a regular expression specifying lines to
  ;; delete from the header.
  (let ((case-fold-search t))
    (save-restriction
      (goto-char start)
      (if (search-forward "\n\n" nil t)
	  (backward-char 2))
      (narrow-to-region start (point))
      (goto-char (point-min))
      (if visible-headers
	  (while (< (point) (point-max))
	    (beginning-of-line)
	    (cond ((looking-at visible-headers)
		   (forward-line 1)
		   (while (looking-at "^[ \t]+") (forward-line 1)))
		  (t
		    (mh-delete-line 1)
		    (while (looking-at "^[ \t]+")
		      (beginning-of-line)
		      (mh-delete-line 1)))))
	  (while (re-search-forward invisible-headers nil t)
	    (beginning-of-line)
	    (mh-delete-line 1)
	    (while (looking-at "^[ \t]+")
	      (beginning-of-line)
	      (mh-delete-line 1))))
      (unlock-buffer))))


(defun mh-delete-line (lines)
  ;; Delete version of kill-line.
  (delete-region (point) (save-excursion (forward-line lines) (point))))


(defun mh-read-draft (use initial-contents delete-contents-file)
  ;; Read draft file into a draft buffer and make that buffer the current one.
  ;; USE is a message used for prompting about the intended use of the message.
  ;; INITIAL-CONTENTS is filename that is read into an empty buffer, or NIL
  ;; if buffer should not be modified.  Delete the initial-contents file if
  ;; DELETE-CONTENTS-FILE flag is set.
  ;; Returns the draft folder's name.
  ;; If the draft folder facility is enabled in ~/.mh_profile, a new buffer is
  ;; used each time and saved in the draft folder.  The draft file can then be
  ;; reused.
  (cond (mh-draft-folder
	 (pop-to-buffer (find-file-noselect (mh-new-draft-name) t))
	 (rename-buffer (format "draft-%s" (buffer-name))))
	(t
	 (let ((draft-name (mh-expand-file-name "draft" mh-user-path)))
	   (pop-to-buffer "draft")	; Create if necessary
	   (if (buffer-modified-p)
	       (if (y-or-n-p "Draft has been modified; kill anyway? ")
		   (set-buffer-modified-p nil)
		   (error "Draft preserved.")))
	   (setq buffer-file-name draft-name)
	   (clear-visited-file-modtime)
	   (unlock-buffer)
	   (when (and (file-exists-p draft-name)
		      (not (equal draft-name initial-contents)))
	     (insert-file-contents draft-name)
	     (delete-file draft-name)))))
  (when (and initial-contents
	     (or (zerop (buffer-size))
		 (not (y-or-n-p
		       (format "A draft exists.  Use for %s? " use)))))
	(erase-buffer)
	(insert-file-contents initial-contents)
	(if delete-contents-file (delete-file initial-contents)))
  (auto-save-mode 1)
  (if mh-draft-folder
      (save-buffer))			; Do not reuse draft name
  (buffer-name))


(defun mh-new-draft-name ()
  ;; Returns the pathname of folder for draft messages.
  (save-excursion
    (set-buffer (get-buffer-create " *mh-temp*"))
    (erase-buffer)
    (mh-exec-cmd-output "mhpath" nil mh-draft-folder "new")
    (buffer-substring (point) (- (mark) 1))))


(defun mh-next-msg ()
  ;; Move backward or forward to the next undeleted message in the buffer.
  (if (eq mh-next-direction 'forward)
      (mh-next-undeleted-msg 1)
      (mh-previous-undeleted-msg 1)))


(defun mh-set-scan-mode ()
  ;; Display the scan listing buffer, but do not show a message.
  (if (get-buffer mh-show-buffer)
      (delete-windows-on mh-show-buffer))
  (mh-set-mode-name "mh-e scan")
  (setq mh-summarize t)
  (if mh-recenter-summary-p
      (recenter (/ (window-height) 2))))


(defun mh-maybe-show (msg)
  ;; If the scan listing is not summarized, then display the message pointed
  ;; to by the cursor is the scan listing.
  (if (not mh-summarize) (mh-show msg)))


(defun mh-set-mode-name (mode-name-string)
  ;; Set the mode-name and ensure that the mode line is updated.
  (setq mode-name mode-name-string)
  ;; Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p)))



;;; The folder data abstraction.

(defvar mh-current-folder nil "Name of current folder")
(defvar mh-show-buffer nil "Buffer that displays mesage for this folder")
(defvar mh-folder-filename nil "Full path of directory for this folder")
(defvar mh-summarize nil "If non-nil, show scan list only")
(defvar mh-next-seq-num nil "Index of free sequence id")
(defvar mh-delete-list nil "list of msg numbers to delete")
(defvar mh-refile-list nil "list of folder names in mh-seq-list")
(defvar mh-seq-list nil "alist of (seq .msgs ) numbers")
(defvar mh-seen-list nil "list of displayed messages")
(defvar mh-next-direction 'forward "direction to move to next message")
(defvar mh-narrowed-to-seq nil "sequence display is narrowed to")
(defvar mh-first-msg-num nil "number of first msg in buffer")
(defvar mh-last-msg-num nil "number of last msg in buffer")

(defun mh-make-folder (name)
  ;; Create and initialize a new mail folder called NAME and make it the
  ;; current folder.
  (switch-to-buffer name)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (make-local-vars
   'mh-current-folder name		; Name of folder
   'mh-show-buffer (format "show-%s" name) ; Buffer that displays messages
   'mh-folder-filename			; e.g. /usr/foobar/Mail/inbox/
   (file-name-as-directory (mh-expand-file-name name))
   'mh-summarize t			; Show scan list only?
   'mh-next-seq-num 0			; Index of free sequence id
   'mh-delete-list nil			; List of msgs nums to delete
   'mh-refile-list nil			; List of folder names in mh-seq-list
   'mh-seq-list nil			; Alist of (seq . msgs) nums
   'mh-seen-list nil			; List of displayed messages
   'mh-next-direction 'forward		; Direction to move to next message
   'mh-narrowed-to-seq nil		; Sequence display is narrowed to
   'mh-first-msg-num nil		; Number of first msg in buffer
   'mh-last-msg-num nil			; Number of last msg in buffer
   'mh-previous-window-config nil)	; Previous window configuration
  (mh-folder-mode)
  (setq buffer-read-only t)
  (mh-set-folder-modified-p nil)
  (auto-save-mode -1)
  (setq buffer-offer-save t)
  (mh-set-mode-name "mh-e scan"))



(defun make-local-vars (&rest pairs)
  ;; Take VARIABLE-VALUE pairs and makes local variables initialized to the
  ;; value.
  (while pairs
    (make-variable-buffer-local (car pairs))
    (set (car pairs) (car (cdr pairs)))
    (setq pairs (cdr (cdr pairs)))))


(defun mh-folder-mode ()
  "Major mode for \"editing\" an MH folder scan listing.
Messages can be marked for refiling and deletion.  However, both actions
are deferred until you request execution with \\[mh-execute-commands].
\\{mh-folder-mode-map}
  A prefix argument (\\[universal-argument]) to delete, refile, list, or undo applies the action to a message sequence.

Variables controlling mh-e operation are (defaults in parentheses):

 mh-bury-show-buffer (t)
    Non-nil means that the buffer used to display message is buried.
    It will never be offered as the default other buffer.

 mh-clean-message-header (nil)
    Non-nil means remove header lines matching the regular expression
    specified in mh-invisible-headers from messages.

 mh-visible-headers (nil)
    If non-nil, it contains a regexp specifying the headers that are shown in
    a message if mh-clean-message-header is non-nil.  Setting this variable
    overrides mh-invisible-headers.

 mh-do-not-confirm (nil)
    Non-nil means do not prompt for confirmation before executing some
    non-recoverable commands such as mh-kill-folder and mh-undo-folder.

 mhl-formfile (nil)
    Name of format file to be used by mhl to show messages.
    A value of T means use the default format file.
    Nil means don't use mhl to format messages.

 mh-lpr-command-format (\"lpr -p -J '%s'\")
    Format for command used to print a message on a system printer.

 mh-recenter-summary-p (nil)
    If non-nil, then the scan listing is recentered when the window displaying
    a messages is toggled off.

 mh-summary-height (4)
    Number of lines in the summary window.

 mh-ins-buf-prefix (\">> \")
    String to insert before each non-blank line of a message as it is
    inserted in a letter being composed."

  (use-local-map mh-folder-mode-map)
  (setq major-mode 'mh-folder-mode)
  (mh-set-mode-name "mh-e folder")
  (run-hooks 'mh-folder-mode-hook))


(defun mh-scan-folder (folder range)
  ;; Scan the FOLDER over the RANGE.  Return in the folder's buffer.
  (cond ((null (get-buffer folder))
	 (mh-make-folder folder))
	(t
	 (mh-process-or-undo-commands folder)
	 (switch-to-buffer folder)))
  (mh-regenerate-headers range)
  (when (= (count-lines (point-min) (point-max)) 0)
    (if (equal range "all")
	(message  "Folder %s is empty" folder)
	(message  "No messages in %s, range %s" folder range))
    (sit-for 5))
  (mh-goto-cur-msg))


(defun mh-regenerate-headers (range)
  ;; Replace buffer with scan of its contents over range RANGE.
  (let ((buffer-read-only nil)
	(folder (buffer-name)))
    (message (format "scanning %s..." folder))
    (erase-buffer)
    (mh-exec-cmd-output "scan" nil
			"-noclear" "-noheader"
			"-width" (window-width)
			folder range)
    (goto-char (point-min))
    (cond ((looking-at "scan: no messages in")
	   (keep-lines mh-valid-scan-line)) ; Flush random scan lines
	  ((looking-at "scan: "))	; Keep error messages
	  (t
	   (keep-lines mh-valid-scan-line))) ; Flush random scan lines
    (mh-delete-seq-locally 'cur)	; To pick up new one
    (setq mh-seq-list (mh-read-folder-sequences folder t))
    (mh-notate-user-sequences)
    (mh-make-folder-mode-line)
    (mh-set-folder-modified-p nil)
    (message (format "scanning %s...done" folder))))


(defun mh-get-new-mail (maildrop-name)
  ;; Read new mail from a maildrop into the current buffer.
  ;; Return T if there was new mail, NIL otherwise.  Return in the current
  ;; buffer.
  (let ((buffer-read-only nil)
	(point-before-inc (point))
	(folder (buffer-name))
	(folder-modified-flag (buffer-modified-p)))
    (message (if maildrop-name
		 (format "inc %s -file %s..." folder maildrop-name)
		 (format "inc %s..." folder)))
    (mh-unmark-all-headers nil)
    (setq mh-next-direction 'forward)
    (keep-lines mh-valid-scan-line)	; Kill old error messages
    (goto-char (point-max))
    (let ((start-of-inc (point)))
      (if maildrop-name
	  (mh-exec-cmd-output "inc" nil folder
			      "-file" (expand-file-name maildrop-name)
			      "-width" (window-width)
			      "-truncate")
	  (mh-exec-cmd-output "inc" nil
			      "-width" (window-width)))
      (message
       (if maildrop-name
	   (format "inc %s -file %s...done" folder maildrop-name)
	   (format "inc %s...done" folder)))
      (mh-delete-seq-locally 'cur)	; To pick up new one
      (setq mh-seq-list (mh-read-folder-sequences folder t))
      (mh-notate-user-sequences)
      (goto-char start-of-inc)
      (cond ((looking-at "inc: no mail")
	     (keep-lines  mh-valid-scan-line) ; Flush random scan lines
	     (mh-make-folder-mode-line)
	     (goto-char point-before-inc)
	     (message "No new mail%s%s." (if maildrop-name " in " "")
		      (if maildrop-name maildrop-name ""))
	     nil)
	    ((looking-at "inc:")	; Error messages
	     (mh-make-folder-mode-line)
	     (goto-char point-before-inc)
	     (message "inc error")
	     nil)
	    (t
	     (keep-lines mh-valid-scan-line)
	     (mh-make-folder-mode-line)
	     (mh-goto-cur-msg)
	     t)))
    (mh-set-folder-modified-p folder-modified-flag)))


(defun mh-make-folder-mode-line (&optional annotation)
  ;; Set the fields of the mode line for a folder buffer.
  ;; The optional ANNOTATION string is displayed after the folder's name.
  (save-excursion
    (goto-char (point-min))
    (setq mh-first-msg-num (mh-get-msg-num nil))
    (let* ((lines (count-lines (point-min) (point-max)))
	   (case-fold-search nil))
      (goto-char (point-max))
      (previous-line 1)
      (setq mh-last-msg-num (mh-get-msg-num nil))
      (setq mode-line-buffer-identification
	    (list (format "{%%b%s}  %d msg%s"
			  (if annotation (format "/%s" annotation) "")
			  lines
			  (if (= lines 0)
			      "s"
			      (if (> lines 1)
				  (format "s (%d-%d)" mh-first-msg-num
					  mh-last-msg-num)
				  (format " (%d)" mh-first-msg-num)))))))))


(defun mh-unmark-all-headers (remove-all-flags)
  ;; Remove all '+' flags from the headers, and if called with a non-nil
  ;; argument, remove all 'D', '^' and '%' flags too.
  (save-excursion
    (let ((buffer-read-only nil)
	  (case-fold-search nil))
      (goto-char (point-min))
      (while (if remove-all-flags
		 (re-search-forward mh-flagged-scan-msg-regexp nil t)
		 (re-search-forward mh-cur-scan-msg-regexp nil t))
	(delete-backward-char 1)
	(insert " ")
	(beginning-of-line)))))		; Check line again


(defun mh-goto-cur-msg ()
  ;; Position the cursor at the current message.
  (let ((cur-msg (car (mh-seq-to-msgs 'cur))))
    (cond ((or (null cur-msg) (not (mh-goto-msg cur-msg t nil)))
	   (goto-char (point-max))
	   (forward-line -1)
	   (message "No current message"))
	  (t
	   (mh-notate cur-msg ?+ mh-cmd-note)
	   (recenter 0)
	   (mh-maybe-show cur-msg)))))


(defun mh-pack-folder-1 ()
  ;; Close and pack the current folder.
  (let ((buffer-read-only nil))
    (message "closing folder...")
    (mh-process-or-undo-commands mh-current-folder)
    (message "packing folder...")
    (save-excursion
      (mh-exec-cmd-quiet " *mh-temp*" "folder" mh-current-folder "-pack"))
    (mh-regenerate-headers "all")
    (message "packing done")))


(defun mh-process-or-undo-commands (folder)
  ;; If FOLDER has outstanding commands, then either process or discard them.
  (set-buffer folder)
  (if (mh-outstanding-commands-p)
      (if (or mh-do-not-confirm
	      (y-or-n-p
		"Process outstanding deletes and refiles (or lose them)? "))
	  (mh-process-commands folder)
	  (mh-undo-folder))
      (mh-invalidate-show-cache)))


(defun mh-process-commands (folder)
  ;; Process outstanding commands for the folder FOLDER.
  (message "Processing deletes and refiles...")
  (set-buffer folder)
  (let ((buffer-read-only nil))
    ;; Update the unseen sequence if it exists
    (if (and mh-seen-list (mh-seq-to-msgs mh-unseen-seq))
	(mh-undefine-sequence mh-unseen-seq mh-seen-list))

    ;; Then refile messages
    (mapc (function
	   (lambda (dest)
	     (let ((msgs (mh-seq-to-msgs dest)))
	       (when msgs
		 (mh-delete-scan-msgs msgs)
		 (apply 'mh-exec-cmd
			(nconc (cons "refile" msgs)
			       (list "-src" folder (symbol-name dest))))))))
	  mh-refile-list)

    ;; Now delete messages
    (when mh-delete-list
      (apply 'mh-exec-cmd (mh-list* "rmm" (format "%s" folder) mh-delete-list))
      (mh-delete-scan-msgs mh-delete-list))

    ;; Don't need to remove sequences since delete and refile do so.

    ;; Mark cur message
    (mh-define-sequence 'cur (list (or (mh-get-msg-num nil) "last")))

    (mh-invalidate-show-cache)

    (setq mh-delete-list nil
	  mh-refile-list nil
	  mh-seq-list (mh-read-folder-sequences mh-current-folder nil)
	  mh-seen-list nil)
    (mh-unmark-all-headers t)
    (mh-notate-user-sequences)
    (mh-set-folder-modified-p nil)
    (message "Processing deletes and refiles...done")))


(defun mh-invalidate-show-cache ()
  ;; Invalidate show buffer file cache.
  (if (get-buffer mh-show-buffer)
      (save-excursion
	(set-buffer mh-show-buffer)
	(setq buffer-file-name nil))))


(defun mh-delete-scan-msgs (msgs)
  ;; Delete the scan listing lines for each of the msgs in the LIST.
  (save-excursion
    (goto-char (point-min))
    (flush-lines (mapconcat 'mh-msg-search-pat msgs "\\|"))))


(defun mh-set-folder-modified-p (flag)
  "Mark current folder as modified or unmodified according to FLAG."
  (set-buffer-modified-p flag))


(defun mh-outstanding-commands-p ()
  ;; Returns non-nil if there are outstanding deletes or refiles.
  (or mh-delete-list mh-refile-list))



;;; Mode for composing and sending a message.

(defun mh-letter-mode ()
  "Mode for composing letters in mh-e.
When you have finished composing, type \\[mh-send-letter] to send the letter.

Variables controlling this mode (defaults in parentheses):

 mh-delete-yanked-msg-window (nil)
    If non-nil, \\[mh-yank-cur-msg] will delete any windows displaying
    the yanked message.

 mh-yank-from-start-of-msg (t)
    If non-nil, \\[mh-yank-cur-msg] will include the entire message.
    If `body', just yank the body (no header).
    If nil, only the portion of the message following the point will be yanked.
    If there is a region, this variable is ignored.

Upon invoking mh-letter-mode, text-mode-hook and mh-letter-mode-hook are
invoked with no args, if those values are non-nil.

\\{mh-letter-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-start))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	(concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-separate))
  (make-local-variable 'mh-send-args)
  (make-local-variable 'mh-annotate-char)
  (make-local-variable 'mh-sent-from-folder)
  (make-local-variable 'mh-sent-from-msg)
  (use-local-map mh-letter-mode-map)
  (setq major-mode 'mh-letter-mode)
  (mh-set-mode-name "mh-e letter")
  (set-syntax-table mh-letter-mode-syntax-table)
  (run-hooks 'text-mode-hook 'mh-letter-mode-hook))


(defun mh-to-field ()
  "Move point to the end of the header field indicated by the previous
keystroke.  Create the field if it does not exist.  Set the mark to the
point before moving."
  (interactive "")
  (expand-abbrev)
  (let ((target (cdr (assoc (logior last-input-char ?`) mh-to-field-choices)))
	(case-fold-search t))
    (cond ((mh-position-on-field target t)
	   (if (not (looking-at "[ \t]")) (insert " ")))
	  (t
	   (goto-char (dot-min))
	   (re-search-forward "^To:")
	   (forward-line 1)
	   (while (looking-at "^[ \t]") (forward-line 1))
	   (insert (format "%s \n" target))
	   (backward-char 1)))))


(defun mh-to-fcc ()
  "Insert a Fcc: field in the current message, prompting for the field
name with a completion list of the current folders."
  (interactive)
  (expand-abbrev)
  (save-excursion
    (mh-insert-fields "Fcc:"
		      (substring (mh-prompt-for-folder "Fcc" "" t) 1 nil))))


(defun mh-insert-signature ()
  "Insert the file ~/.signature at the current point."
  (interactive "")
  (insert-file-contents "~/.signature"))


(defun mh-check-whom ()
  "Verify recipients of the current letter."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (set-buffer-modified-p t)		; Force writing of contents
    (save-buffer)
    (message "Checking recipients...")
    (switch-to-buffer-other-window "*Mail Recipients*")
    (bury-buffer (current-buffer))
    (erase-buffer)
    (mh-exec-cmd-output "whom" t file-name)
    (other-window -1)
    (message "Checking recipients...done")))



;;; Routines to make a search pattern and search for a message.

(defun mh-make-pick-template ()
  ;; Initialize the current buffer with a template for a pick pattern.
  (erase-buffer)
  (kill-all-local-variables)
  (make-local-variable 'mh-searching-folder)
  (insert "From: \n"
	  "To: \n"
	  "Cc: \n"
	  "Date: \n"
	  "Subject: \n"
	  "---------\n")
  (mh-letter-mode)
  (use-local-map mh-pick-mode-map)
  (goto-char (point-min))
  (end-of-line))


(defun mh-do-pick-search ()
  "Find messages in the folder named in mh-searching-folder that match the
qualifications in current buffer.  Put messages found in a sequence
named `search'."
  (interactive)
  (let ((pattern-buffer (buffer-name))
	(searching-buffer mh-searching-folder)
	(range)
	(pattern nil)
	(new-buffer nil))
    (save-excursion
      (cond ((get-buffer searching-buffer)
	     (set-buffer searching-buffer)
	     (setq range (format "%d-%d" mh-first-msg-num mh-last-msg-num)))
	    (t
	     (mh-make-folder searching-buffer)
	     (setq range "all")
	     (setq new-buffer t))))
    (message "Searching...")
    (goto-char (point-min))
    (while (setq pattern (mh-next-pick-field pattern-buffer))
      (setq msgs (mh-seq-from-command searching-buffer
				      'search
				      (nconc (cons "pick" pattern)
					     (list searching-buffer
						   range
						   "-sequence" "search"
						   "-list"))))
      (setq range "search"))
    (message "Searching...done")
    (if new-buffer
	(mh-scan-folder searching-buffer msgs)
	(switch-to-buffer searching-buffer))
    (delete-other-windows)
    (mh-notate-seq 'search ?% (+ mh-cmd-note 1))))


(defun mh-next-pick-field (buffer)
  ;; Return the next piece of a pick argument that can be extracted from the
  ;; BUFFER.  Returns nil if no pieces remain.
  (set-buffer buffer)
  (let ((case-fold-search t))
    (cond ((eobp)
	   nil)
	  ((re-search-forward "^\\([a-z].*\\):[ \t]*\\([a-z0-9].*\\)$" nil t)
	   (let* ((component
		   (format "-%s"
			   (downcase (buffer-substring (match-beginning 1)
						       (match-end 1)))))
		  (pat (buffer-substring (match-beginning 2) (match-end 2))))
	       (forward-line 1)
	       (list component pat)))
	  ((re-search-forward "^-*$" nil t)
	   (forward-char 1)
	   (let ((body (buffer-substring (point) (point-max))))
	     (if (and (> (length body) 0) (not (equal body "\n")))
		 (list "-search" body)
		 nil)))
	  (t
	   nil))))



;;; Routines to compose and send a letter.

(defun mh-compose-and-send-mail (draft send-args
				       sent-from-folder sent-from-msg
				       to subject cc
				       annotate-char annotate-field
				       config)
  ;; Edit and compose a draft message in buffer DRAFT and send or save it.
  ;; SENT-FROM-FOLDER is buffer containing scan listing of current folder, or
  ;; nil if none exists.
  ;; SENT-FROM-MSG is the message number or sequence name or nil.
  ;; SEND-ARGS is an optional argument passed to the send command.
  ;; nThe TO, SUBJECT, and CC fields are passed to the mh-compose-letter-hook.
  ;; If ANNOTATE-CHAR is non-null, it is used to notate the scan listing of the
  ;; message.  In that case, the ANNOTATE-FIELD is used to build a string
  ;; for mh-annotate-msg.
  ;; CONFIG is the window configuration to restore after sending the letter.
  (pop-to-buffer draft)
  (mh-letter-mode)
  (make-local-vars
   'mh-annotate-field annotate-field
   'mh-previous-window-config config)
  (setq mh-sent-from-folder sent-from-folder)
  (setq mh-sent-from-msg sent-from-msg)
  (setq mh-send-args send-args)
  (setq mh-annotate-char annotate-char)
  (setq mode-line-buffer-identification (list "{%b}"))
  (if (and (boundp 'mh-compose-letter-hook)
	   (symbol-value 'mh-compose-letter-hook))
      ;; run-hooks will not pass arguments.
      (let ((value (symbol-value 'mh-compose-letter-hook)))
	(if (and (listp value) (not (eq (car value) 'lambda)))
	    (while value
	      (funcall (car value) to subject cc)
	      (setq value (cdr value)))
	    (funcall mh-compose-letter-hook to subject cc)))))


(defun mh-send-letter (&optional arg)
  "Send the draft letter in the current buffer.
If (optional) prefix argument provided, monitor delivery."
  (interactive "P")
  (set-buffer-modified-p t)		; Make sure buffer is written
  (save-buffer)
  (message "Sending...")
  (let ((buffer-name (buffer-name))
	(file-name (buffer-file-name))
	(config mh-previous-window-config))
    (cond (arg
	   (pop-to-buffer "MH mail delivery")
	   (erase-buffer)
	   (if mh-send-args
	       (mh-exec-cmd-output "send" t "-watch" "-nopush"
				   "-nodraftfolder" mh-send-args file-name)
	       (mh-exec-cmd-output "send" t "-watch" "-nopush"
				   "-nodraftfolder" file-name)))

	  (mh-send-args
	   (mh-exec-cmd-demon "send" "-nodraftfolder" "-noverbose"
			      mh-send-args file-name))
	  (t
	   (mh-exec-cmd-demon "send" "-nodraftfolder" "-noverbose"
			      file-name)))

    (if mh-annotate-char
	(mh-annotate-msg mh-sent-from-msg
			 mh-sent-from-folder
			 mh-annotate-char
			 "-component" mh-annotate-field
			 "-text" (format "\"%s %s\""
					 (mh-get-field "To:")
					 (mh-get-field "Cc:"))))

    (when (or (not arg)
	      (y-or-n-p "Kill draft buffer? "))
      (kill-buffer buffer-name)
      (if config
	  (set-window-configuration config)))
    (message "Sending...done")))



(defun mh-insert-letter (prefix-provided folder msg)
  "Insert a message from any folder into the current letter.
Removes the message's headers using mh-invisible-headers.
Prefixes each non-blank line with mh-ins-buf-prefix (default \">> \").
If (optional) prefix argument provided, do not indent and do not delete
headers.
Leaves the mark before the letter and point after it."
  (interactive
   (list current-prefix-arg
	 (mh-prompt-for-folder "Message from" mh-sent-from-folder nil)
	 (read-input (format "Message number%s: "
			     (if mh-sent-from-msg
				 (format " [%d]" mh-sent-from-msg)
				 "")))))
  (save-restriction
    (narrow-to-region (point) (point))
    (let ((start (point-min)))
      (if (equal msg "") (setq msg (format "%d" mh-sent-from-msg)))
      (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
			      (mh-expand-file-name msg
						   (mh-expand-file-name
						    folder)))
      (when (not prefix-provided)
	    (mh-clean-msg-header start mh-invisible-headers mh-visible-headers)
	    (set-mark start)		; since mh-clean-msg-header moves it
	    (mh-insert-prefix-string mh-ins-buf-prefix)))))


(defun mh-yank-cur-msg ()
  "Insert the currently displayed message into the draft buffer.  Prefix each
non-blank line in the message with the string in mh-ins-buf-prefix.  If a
region is set in the message's buffer, then only the region will be inserted.
Otherwise, the entire message will be inserted if mh-yank-from-start-of-msg is
non-nil.   If this variable is nil, the portion of the message following the
point will be yanked.  If mh-delete-yanked-msg-window is non-nil, any window
displaying the yanked message will be deleted."
  (interactive)
  (if (and (boundp 'mh-sent-from-folder) mh-sent-from-folder mh-sent-from-msg)
      (let ((to-point (point))
	    (to-buffer (current-buffer)))
	(set-buffer mh-sent-from-folder)
	(if mh-delete-yanked-msg-window
	    (delete-windows-on mh-show-buffer))
	(set-buffer mh-show-buffer)	; Find displayed message
	(let ((mh-ins-str (cond ((mark)
				 (buffer-substring (point) (mark)))
				((eq 'body mh-yank-from-start-of-msg)
				 (buffer-substring
				  (save-excursion
				    (mh-goto-header-end 1)
				    (point))
				  (point-max)))
				(mh-yank-from-start-of-msg
				 (buffer-substring (point-min) (point-max)))
				(t
				 (buffer-substring (point) (point-max))))))
	  (set-buffer to-buffer)
	  (narrow-to-region to-point to-point)
	  (insert mh-ins-str)
	  (mh-insert-prefix-string mh-ins-buf-prefix)
	  (insert "\n")
	  (widen)))
      (error "There is no current message.")))

(defun mh-insert-prefix-string (ins-string)
  ;; Preface each line in the current buffer with STRING.
  (goto-char (point-min))
  (while (not (eobp))
    (insert ins-string)
    (forward-line 1)))


(defun mh-fully-kill-draft ()
  "Kill the draft message file and the draft message buffer.
Use \\[kill-buffer] if you don't want to delete the draft message file."
  (interactive "")
  (if (y-or-n-p "Kill draft message? ")
      (let ((config mh-previous-window-config))
	(if (file-exists-p (buffer-file-name))
	    (delete-file (buffer-file-name)))
	(set-buffer-modified-p nil)
	(kill-buffer (buffer-name))
	(if config
	    (set-window-configuration config)))
    (error "Message not killed")))



;;; Commands to manipulate sequences.  Sequences are stored in an alist
;;; of the form:
;;;	((seq-name msgs ...) (seq-name msgs ...) ...)

(defun mh-make-seq (name msgs) (cons name msgs))

(defmacro mh-seq-name (pair) (list 'car pair))

(defmacro mh-seq-msgs (pair) (list 'cdr pair))

(defun mh-find-seq (name) (assoc name mh-seq-list))


(defun mh-seq-to-msgs (seq)
  "Return a list of the messages in SEQUENCE."
  (mh-seq-msgs (mh-find-seq seq)))


(defun mh-seq-containing-msg (msg)
  ;; Return a list of the sequences containing MESSAGE.
  (let ((l mh-seq-list)
	(seqs ()))
    (while l
      (if (memq msg (mh-seq-msgs (car l)))
	  (mh-push (mh-seq-name (car l)) seqs))
      (setq l (cdr l)))
    seqs))


(defun mh-msg-to-seq (msg)
  ;; Given a MESSAGE number, return the first sequence in which it occurs.
  (car (mh-seq-containing-msg msg)))


(defun mh-read-seq (prompt not-empty &optional default)
  ;; Read and return a sequence name.  Prompt with PROMPT, raise an error
  ;; if the sequence is empty and the NOT-EMPTY flag is non-nil, and supply
  ;; an optional DEFAULT sequence.
  ;; A reply of '%' defaults to the first sequence containing the current
  ;; message.
  (let* ((input (completing-read (format "%s %s %s" prompt "sequence:"
					 (if default
					     (format "[%s] " default)
					     ""))
				 (mh-seq-names mh-seq-list)))
	 (seq (cond ((equal input "%") (mh-msg-to-seq (mh-get-msg-num t)))
		    ((equal input "") default)
		    (t (intern input))))
	 (msgs (mh-seq-to-msgs seq)))
    (if (and (null msgs) not-empty)
	(error (format "No messages in sequence `%s'" seq)))
    seq))


(defun mh-read-folder-sequences (folder define-sequences)
  ;; Read and return the predefined sequences for a FOLDER.  If
  ;; DEFINE-SEQUENCES is non-nil, then define mh-e's sequences before
  ;; reading MH's sequences.
  (let ((seqs ()))
    (when define-sequences
      (mh-define-sequences mh-seq-list)
      (mapc (function (lambda (seq)	; Save the internal sequences
	      (if (mh-folder-name (mh-seq-name seq))
		  (mh-push seq seqs))))
	    mh-seq-list))
    (save-excursion
      (mh-exec-cmd-quiet " *mh-temp*" "mark" folder "-list")
      (goto-char (point-min))
      (while (re-search-forward "\\(^[a-zA-Z][a-zA-Z]*\\)" nil t)
	(mh-push (mh-make-seq (intern (buffer-substring (match-beginning 1)
							(match-end 1)))
			      (mh-read-msg-list))
		 seqs)))
    seqs))


(defun mh-seq-names (seq-list)
  ;; Return an alist containing the names of the SEQUENCES.
  (mapcar (function (lambda (entry) (list (symbol-name (mh-seq-name entry)))))
	  seq-list))


(defun mh-seq-from-command (folder seq command)
  ;; In FOLDER, make a sequence named SEQ by executing COMMAND.
  (let ((msg)
	(msgs ())
	(case-fold-search t))
    (save-excursion
      (save-window-excursion
	(apply 'mh-exec-cmd-quiet (cons " *mh-temp*" command))
	(goto-char (point-min))
	(while (setq msg (car (mh-read-msg-list)))
	  (mh-push msg msgs)
	  (forward-line 1)))
      (set-buffer folder)
      (setq msgs (nreverse msgs))	; Put in ascending order
      (mh-push (mh-make-seq seq msgs) mh-seq-list)
      msgs)))


(defun mh-read-msg-list ()
  ;; Return a list of message numbers from the current point to the end of
  ;; the line.
  (let ((msgs ())
	(end-of-line (save-excursion (end-of-line) (point))))
    (while (re-search-forward "\\([0-9]+\\)" end-of-line t)
      (let ((num (string-to-int (buffer-substring (match-beginning 1)
						  (match-end 1)))))
	(cond ((looking-at "-")		; Message range
	       (forward-char 1)
	       (re-search-forward "\\([0-9]+\\)" end-of-line t)
	       (let ((num2 (string-to-int (buffer-substring (match-beginning 1)
							    (match-end 1)))))
		 (if (< num2 num)
		     (error "Bad message range: %d-%d" num num2))
		 (while (<= num num2)
		   (mh-push num msgs)
		   (setq num (+ num 1)))))
	      ((not (zerop num)) (mh-push num msgs)))))
    msgs))


(defun mh-remove-seq (seq)
  ;; Delete the SEQUENCE.
  (mh-map-to-seq-msgs 'mh-notate-if-in-one-seq seq ?  (+ mh-cmd-note 1) seq)
  (mh-undefine-sequence seq (list "all"))
  (mh-delete-seq-locally seq))


(defun mh-delete-seq-locally (seq)
  ;; Remove mh-e's record of SEQUENCE.
  (let ((entry (mh-find-seq seq)))
    (setq mh-seq-list (delq entry mh-seq-list))))


(defun mh-remove-msg-from-seq (msg seq &optional internal-flag)
  ;; Remove MESSAGE from the SEQUENCE.  If optional FLAG is non-nil, do not
  ;; inform MH of the change.
  (let ((entry (mh-find-seq seq)))
    (when entry
      (mh-notate-if-in-one-seq msg ?  (+ mh-cmd-note 1) (mh-seq-name entry))
      (if (not internal-flag)
	  (mh-undefine-sequence seq (list msg)))
      (setcdr entry (delq msg (mh-seq-msgs entry))))))


(defun mh-add-msgs-to-seq (msgs seq &optional internal-flag)
  ;; Add MESSAGE(s) to the SEQUENCE.  If optional FLAG is non-nil, do not mark
  ;; the message in the scan listing or inform MH of the addition.
  (let ((entry (mh-find-seq seq)))
    (if (and msgs (atom msgs)) (setq msgs (list msgs)))
    (if (null entry)
	(mh-push (mh-make-seq seq msgs) mh-seq-list)
	(if msgs (setcdr entry (append msgs (cdr entry)))))
    (when (not internal-flag)
      (mh-add-to-sequence seq msgs)
      (mh-notate-seq seq ?% (+ mh-cmd-note 1)))))


(defun mh-rename-seq (seq new-name)
  "Rename a SEQUENCE to have a new NAME."
  (interactive "SOld sequence name: \nSNew name: ")
  (let ((old-seq (mh-find-seq seq)))
    (if old-seq
	(rplaca old-seq new-name)
	(error "Sequence %s does not exists" seq))
    (mh-undefine-sequence seq (mh-seq-msgs old-seq))
    (mh-define-sequence new-name (mh-seq-msgs old-seq))))


(defun mh-notate-user-sequences ()
  ;; Mark the scan listing of all messages in user-defined sequences.
  (let ((seqs mh-seq-list))
    (while seqs
      (let ((name (mh-seq-name (car seqs))))
	(if (not (mh-internal-seq name))
	    (mh-notate-seq name ?% (+ mh-cmd-note 1)))
	(setq seqs (cdr seqs))))))


(defun mh-internal-seq (name)
  ;; Return non-NIL if NAME is the name of an internal mh-e sequence.
  (or (memq name '(answered cur deleted forwarded printed))
      (eq name mh-unseen-seq)
      (mh-folder-name name)))


(defun mh-folder-name (name)
  ;; Return non-NIL if NAME is the possible name of a folder (i.e., begins
  ;; with "+").
  (if (symbolp name)
      (mh-folder-name (symbol-name name))
      (equal (substring name 0 1) "+")))


(defun mh-notate-seq (seq notation offset)
  ;; Mark the scan listing of all messages in the SEQUENCE with the CHARACTER
  ;; at the given OFFSET from the beginning of the listing line.
  (mh-map-to-seq-msgs 'mh-notate seq notation offset))


(defun mh-notate-if-in-one-seq (msg notation offset seq)
  ;; If the MESSAGE is in only the SEQUENCE, then mark the scan listing of the
  ;; message with the CHARACTER at the given OFFSET from the beginning of the
  ;; listing line.
  (let ((in-seqs (mh-seq-containing-msg msg)))
    (if (and (eq seq (car in-seqs)) (null (cdr in-seqs)))
	(mh-notate msg notation offset))))


(defun mh-map-to-seq-msgs (func seq &rest args)
  ;; Invoke the FUNCTION at each message in the SEQUENCE, passing the
  ;; remaining ARGS as arguments.
  (save-excursion
    (let ((msgs (mh-seq-to-msgs seq)))
      (while msgs
	(if (mh-goto-msg (car msgs) t t)
	    (apply func (cons (car msgs) args)))
	(setq msgs (cdr msgs))))))


(defun mh-map-over-seqs (func seq-list)
  ;; Apply the FUNCTION to each element in the list of SEQUENCES,
  ;; passing the sequence name and the list of messages as arguments.
  (while seq-list
    (funcall func (mh-seq-name (car seq-list)) (mh-seq-msgs (car seq-list)))
    (setq seq-list (cdr seq-list))))


(defun mh-define-sequences (seq-list)
  ;; Define the sequences in SEQ-LIST.
  (mh-map-over-seqs 'mh-define-sequence seq-list))


(defun mh-add-to-sequence (seq msgs)
  ;; Add to a SEQUENCE each message the list of MSGS.
  (if (not (equal (substring (symbol-name seq) 0 1) "+"))
      (if msgs
	  (apply 'mh-exec-cmd (mh-list* "mark" mh-current-folder
					"-sequence" (format "%s" seq)
					"-add" msgs)))))

(defun mh-define-sequence (seq msgs)
  ;; Define the SEQUENCE to contain the list of MSGS.  Do not mark
  ;; pseudo-sequences or empty sequences.
  (if (and msgs
	   (not (equal (substring (symbol-name seq) 0 1) "+")))
      (save-excursion
	(apply 'mh-exec-cmd-quiet (mh-list* " *mh-temp*"
					    "mark" mh-current-folder
					    "-sequence" (format "%s" seq)
					    "-add" "-zero" msgs)))))


(defun mh-undefine-sequence (seq msgs)
  ;; Remove from the SEQUENCE the list of MSGS.
  (apply 'mh-exec-cmd (mh-list* "mark" mh-current-folder
				"-sequence" (format "%s" seq)
				"-delete" msgs)))


(defun mh-copy-seq-to-point (seq location)
  ;; Copy the scan listing of the messages in SEQUENCE to after the point
  ;; LOCATION in the current buffer.
  (mh-map-to-seq-msgs 'mh-copy-line-to-point seq location))


(defun mh-copy-line-to-point (msg location)
  ;; Copy the current line to the LOCATION in the current buffer.
  (beginning-of-line)
  (let ((beginning-of-line (point)))
    (forward-line 1)
    (copy-region-as-kill beginning-of-line (point))
    (goto-char location)
    (yank)
    (goto-char beginning-of-line)))



;;; Issue commands to MH.

(defun mh-exec-cmd (command &rest args)
  ;; Execute MH command COMMAND with ARGS.  Any output is shown to the user.
  (save-window-excursion
    (switch-to-buffer-other-window " *mh-temp*")
    (erase-buffer)
    (apply 'call-process
	   (mh-list* (mh-expand-file-name command mh-progs) nil t nil
		     (mh-list-to-string args)))
    (if (> (buffer-size) 0)
	(sit-for 5))))


(defun mh-exec-cmd-quiet (buffer command &rest args)
  ;; In BUFFER, execute MH command COMMAND with ARGS.  Return in buffer, if
  ;; one exists.
  (when (stringp buffer)
    (switch-to-buffer buffer)
    (erase-buffer))
  (apply 'call-process
	 (mh-list* (mh-expand-file-name command mh-progs) nil buffer nil
		   (mh-list-to-string args))))


(defun mh-exec-cmd-output (command display &rest args)
  ;; Execute MH command COMMAND with DISPLAY flag and ARGS putting the output
  ;; into buffer after point.  Set mark after inserted text.
  (push-mark (point) t)
  (apply 'call-process
	 (mh-list* (mh-expand-file-name command mh-progs) nil t display
		   (mh-list-to-string args)))
  (exchange-point-and-mark))


(defun mh-exec-cmd-demon (command &rest args)
  ;; Execute MH command COMMAND with ARGS.  Any output from command is
  ;; displayed in an asynchronous pop-up window.
  (save-excursion
    (switch-to-buffer " *mh-temp*")
    (erase-buffer))
  (let ((process (apply 'start-process
			(mh-list* "mh-output" nil
				  (expand-file-name command mh-progs)
				  (mh-list-to-string args)))))
    (set-process-filter process 'mh-process-demon)))


(defun mh-process-demon (process output)
  ;; Process demon that puts output into a temporary buffer.
  (pop-to-buffer " *mh-temp*")
  (insert output)
  (other-window 1))


(defun mh-exec-lib-cmd-output (command &rest args)
  ;; Execute MH library command COMMAND with ARGS.  Put the output into
  ;; buffer after point.  Set mark after inserted text.
  (push-mark (point) t)
  (apply 'call-process
	 (mh-list* (mh-expand-file-name command mh-lib) nil t nil
		   (mh-list-to-string args)))
  (exchange-point-and-mark))


(defun mh-list-to-string (l)
  ;; Flattens the list L and makes every element of the new list into a string.
  (let ((new-list nil))
    (while l
      (cond ((null (car l)))
	    ((symbolp (car l)) (mh-push (symbol-name (car l)) new-list))
	    ((numberp (car l)) (mh-push (int-to-string (car l)) new-list))
	    ((equal (car l) ""))
	    ((stringp (car l)) (mh-push (car l) new-list))
	    ((listp (car l))
	     (setq new-list (nconc (nreverse (mh-list-to-string (car l)))
				   new-list)))
	    (t (error "Bad argument %s" (car l))))
      (setq l (cdr l)))
    (nreverse new-list)))



;;; Commands to annotate a message.

(defun mh-annotate-msg (msg buffer note &rest args)
  ;; Mark the MESSAGE in BUFFER listing with the character NOTE and annotate
  ;; the saved message with ARGS.
  (apply 'mh-exec-cmd (mh-list* "anno" buffer msg args))
  (save-excursion
    (set-buffer buffer)
    (if (symbolp msg)
	(mh-notate-seq msg note (+ mh-cmd-note 1))
	(mh-notate msg note (+ mh-cmd-note 1)))))


(defun mh-notate (msg notation offset)
  ;; Marks MESSAGE with the character NOTATION at position OFFSET.
  (save-excursion
    (if (mh-goto-msg msg t t)
	(let ((buffer-read-only nil)
	      (folder-modified-flag (buffer-modified-p)))
	  (beginning-of-line)
	  (goto-char (+ (point) offset))
	  (delete-char 1)
	  (insert notation)
	  (mh-set-folder-modified-p folder-modified-flag)))))



;;; User prompting commands.

(defun mh-prompt-for-folder (prompt default can-create)
  ;; Prompt for a folder name with PROMPT.  Returns the folder's name.
  ;; DEFAULT is used if the folder exists and the user types return.
  ;; If the CAN-CREATE flag is t, then a non-existant folder is made.
  (let* ((prompt (format "%s folder%s" prompt
			 (if (equal "" default)
			     "? "
			     (format " [%s]? " default))))
	 name)
    (if (null mh-folder-list)
	(setq mh-folder-list (mh-make-folder-list)))
    (while (and (setq name (completing-read prompt mh-folder-list
					    nil nil "+"))
		(equal name "")
		(equal default "")))
    (cond ((or (equal name "") (equal name "+"))
	   (setq name default))
	  ((not (equal (substring name 0 1) "+"))
	   (setq name (format "+%s" name))))
    (let ((new-file-p (not (file-exists-p (mh-expand-file-name name)))))
      (cond ((and new-file-p
		  (y-or-n-p
		   (format "Folder %s does not exist. Create it? " name)))
	     (message "Creating %s" name)
	     (call-process "mkdir" nil nil nil (mh-expand-file-name name))
	     (message "Creating %s...done" name)
	     (mh-push (list name) mh-folder-list)
	     (mh-push (list (substring name 1 nil)) mh-folder-list))
	    (new-file-p
	     (error ""))
	    (t
	     (when (null (assoc name mh-folder-list))
	       (mh-push (list name) mh-folder-list)
	       (mh-push (list (substring name 1 nil)) mh-folder-list)))))
    name))


(defun mh-make-folder-list ()
  "Return a list of the user's folders.
Result is in a form suitable for completing read."
  (interactive)
  (message "Collecting folder names...")
  (save-window-excursion
    (mh-exec-cmd-quiet " *mh-temp*" "folders" "-fast"
		       (if mh-recursive-folders
			   "-recurse"
			   "-norecurse"))
    (goto-char (point-min))
    (let ((list nil))
      (while (not (eobp))
	(let ((start (point)))
	  (search-forward "\n" nil t)
	  (let ((folder (buffer-substring start (- (point) 1))))
	    (mh-push (list (format "+%s" folder)) list))))
      (message "Collecting folder names...done")
      list)))


(defun mh-remove-folder-from-folder-list (folder)
  ;; Remove FOLDER from the list of folders.
  (setq mh-folder-list
	(delq (assoc (substring folder 1 nil) mh-folder-list)
	      mh-folder-list)))



;;; Misc. functions.

(defun mh-get-msg-num (error-if-no-message)
  ;; Return the message number of the displayed message.  If the argument
  ;; ERROR-IF-NO-MESSAGE is non-nil, then complain if the cursor is not
  ;; pointing to a message.
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at mh-msg-number-regexp)
	   (string-to-int (buffer-substring (match-beginning 1)
					    (match-end 1))))
	  (error-if-no-message
	   (error "Cursor not pointing to message"))
	  (t nil))))


(defun mh-msg-search-pat (n)
  ;; Return a search pattern for message N in the scan listing.
  (format mh-msg-search-regexp n))


(defun mh-msg-filename (msg)
  ;; Returns a string containing the file name of the MESSAGE.
  (mh-expand-file-name (int-to-string msg) mh-folder-filename))


(defun mh-msg-filenames (msgs folder)
  ;; Return a string of filenames for MSGS in FOLDER.
  (let ((mh-folder-filename folder))
    (mapconcat (function (lambda (msg) (mh-msg-filename msg))) msgs " ")))


(defun mh-find-path ()
  ;; Set mh-user-path, mh-draft-folder, and mh-unseen-seq from  ~/.mh_profile.
  (save-window-excursion
    (let ((profile (or (getenv "MH") "~/.mh_profile")))
      (if (not (file-exists-p profile))
	  (error "Cannot find ~/.mh_profile"))
      (switch-to-buffer " *mh-temp*")
      (erase-buffer)
      (insert-file-contents profile)
      (setq mh-draft-folder (mh-get-field "Draft-Folder:" ))
      (cond ((equal mh-draft-folder "")
	     (setq mh-draft-folder nil))
	    ((not (equal (substring mh-draft-folder 0 1) "+"))
	     (setq mh-draft-folder (format "+%s" mh-draft-folder))))
      (setq mh-user-path (mh-get-field "Path:"))
      (if (equal mh-user-path "")
	  (setq mh-user-path "Mail"))
      (setq mh-user-path
	    (file-name-as-directory
	     (expand-file-name mh-user-path (expand-file-name "~"))))
      (if (and mh-draft-folder
	       (not (file-exists-p (mh-expand-file-name mh-draft-folder))))
	  (error "Draft folder does not exist.  Create it and try again."))
      (setq mh-unseen-seq (mh-get-field "Unseen-Sequence:"))
      (if (equal mh-unseen-seq "")
	  (setq mh-unseen-seq 'unseen)
	  (setq mh-unseen-seq (intern mh-unseen-seq))))))


(defun mh-expand-file-name (filename &optional default)
  "Just like expand-file-name, but also handles MH folder names.
Assumes that any filename that starts with '+' is a folder name."
   (if (string-equal (substring filename 0 1) "+")
       (expand-file-name (substring filename 1) mh-user-path)
     (expand-file-name filename default)))


(defun mh-get-field (field)
  ;; Find and return the value of field FIELD in the current buffer.
  ;; Returns the empty string if the field is not in the message.
  (let ((case-fold-search t))
    (goto-char (point-min))
    (cond ((not (search-forward field nil t)) "")
	  ((looking-at "[\t ]*$") "")
	  (t
	   (re-search-forward "[\t ]*\\([^\t \n].*\\)$" nil t)
	   (let ((field (buffer-substring (match-beginning 1)
					  (match-end 1)))
		 (end-of-match (point)))
	     (forward-line)
	     (while (looking-at "[ \t]") (forward-line 1))
	     (backward-char 1)
	     (if (<= (point) end-of-match)
		 field
		 (format "%s%s"
			 field
			 (buffer-substring end-of-match (point)))))))))


(defun mh-insert-fields (&rest name-values)
  ;; Insert the NAME-VALUE pairs in the current buffer.
  ;; Do not insert any pairs whose value is the empty string.
  (let ((case-fold-search t))
    (while name-values
      (let ((field-name (car name-values))
	    (value (car (cdr name-values))))
	(when (not (equal value ""))
	  (goto-char (point-min))
	  (cond ((not (re-search-forward (format "^%s" field-name) nil t))
		 (mh-goto-header-end 0)
		 (insert field-name " " value "\n"))
		(t
		 (end-of-line)
		 (insert " " value))))
	(setq name-values (cdr (cdr name-values)))))))


(defun mh-position-on-field (field set-mark)
  ;; Set point to the end of the line beginning with FIELD.
  ;; Set the mark to the old value of point, if SET-MARK is non-nil.
  (let ((case-fold-search t))
    (if set-mark (push-mark))
    (goto-char (point-min))
    (mh-goto-header-end 0)
    (if (re-search-backward (format "^%s" field) nil t)
	(progn (end-of-line) t)
	nil)))


(defun mh-goto-header-end (arg)
  ;; Find the end of the message header in the current buffer and position
  ;; the cursor at the ARG'th newline after the header.
  (if (re-search-forward "^$\\|^-+$" nil nil)
      (forward-line arg)))



;;; Build the folder-mode keymap:

(suppress-keymap mh-folder-mode-map)
(define-key mh-folder-mode-map "q" 'mh-restore-window-config)
(define-key mh-folder-mode-map "b" 'mh-restore-window-config)
(define-key mh-folder-mode-map "?" 'mh-msg-is-in-seq)
(define-key mh-folder-mode-map "%" 'mh-put-msg-in-seq)
(define-key mh-folder-mode-map "\ea" 'mh-edit-again)
(define-key mh-folder-mode-map "\e%" 'mh-delete-msg-from-seq)
(define-key mh-folder-mode-map "\C-Xn" 'mh-narrow-to-seq)
(define-key mh-folder-mode-map "\C-Xw" 'mh-widen)
(define-key mh-folder-mode-map "\eb" 'mh-burst-digest)
(define-key mh-folder-mode-map "\eu" 'mh-undo-folder)
(define-key mh-folder-mode-map "\e " 'mh-page-digest)
(define-key mh-folder-mode-map "\e\177" 'mh-page-digest-backwards)
(define-key mh-folder-mode-map "\ee" 'mh-extract-rejected-mail)
(define-key mh-folder-mode-map "\ef" 'mh-visit-folder)
(define-key mh-folder-mode-map "\ek" 'mh-kill-folder)
(define-key mh-folder-mode-map "\el" 'mh-list-folders)
(define-key mh-folder-mode-map "\ep" 'mh-pack-folder)
(define-key mh-folder-mode-map "\es" 'mh-search-folder)
(define-key mh-folder-mode-map "\er" 'mh-rescan-folder)
(define-key mh-folder-mode-map "l" 'mh-print-msg)
(define-key mh-folder-mode-map "t" 'mh-toggle-summarize)
(define-key mh-folder-mode-map "c" 'mh-copy-msg)
(define-key mh-folder-mode-map ">" 'mh-write-msg-to-file)
(define-key mh-folder-mode-map "i" 'mh-inc-folder)
(define-key mh-folder-mode-map "x" 'mh-execute-commands)
(define-key mh-folder-mode-map "e" 'mh-execute-commands)
(define-key mh-folder-mode-map "r" 'mh-redistribute)
(define-key mh-folder-mode-map "f" 'mh-forward)
(define-key mh-folder-mode-map "s" 'mh-send)
(define-key mh-folder-mode-map "m" 'mh-send)
(define-key mh-folder-mode-map "a" 'mh-reply)
(define-key mh-folder-mode-map "j" 'mh-goto-msg)
(define-key mh-folder-mode-map "g" 'mh-goto-msg)
(define-key mh-folder-mode-map "\177" 'mh-previous-page)
(define-key mh-folder-mode-map " " 'mh-page-msg)
(define-key mh-folder-mode-map "." 'mh-show)
(define-key mh-folder-mode-map "u" 'mh-undo)
(define-key mh-folder-mode-map "!" 'mh-refile-or-write-again)
(define-key mh-folder-mode-map "^" 'mh-refile-msg)
(define-key mh-folder-mode-map "d" 'mh-delete-msg)
(define-key mh-folder-mode-map "p" 'mh-previous-undeleted-msg)
(define-key mh-folder-mode-map "n" 'mh-next-undeleted-msg)


;;; Build the letter-mode keymap:

(define-key mh-letter-mode-map "\C-c\C-f\C-b" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-c" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-f" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-s" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-t" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-fb" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-fc" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-ff" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-fs" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-ft" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-q" 'mh-fully-kill-draft)
(define-key mh-letter-mode-map "\C-c\C-w" 'mh-check-whom)
(define-key mh-letter-mode-map "\C-c\C-i" 'mh-insert-letter)
(define-key mh-letter-mode-map "\C-c\C-y" 'mh-yank-cur-msg)
(define-key mh-letter-mode-map "\C-c\C-s" 'mh-insert-signature)
(define-key mh-letter-mode-map "\C-c\C-c" 'mh-send-letter)


;;; Build the pick-mode keymap:

(define-key mh-pick-mode-map "\C-c\C-c" 'mh-do-pick-search)
(define-key mh-pick-mode-map "\C-c\C-f\C-b" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-f\C-c" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-f\C-f" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-f\C-s" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-f\C-t" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-fb" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-fc" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-ff" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-fs" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-ft" 'mh-to-field)
(define-key mh-pick-mode-map "\C-c\C-w" 'mh-check-whom)

