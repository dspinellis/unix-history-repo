;;;  mh-e.el	(Version: 3.8 for GNU Emacs Version 18 and MH.5 and MH.6)

(defvar mh-e-RCS-id)
(setq mh-e-RCS-id "$Header: mh-e.el,v 3.6 92/01/21 18:34:50 gildea Exp $")
(setq mh-e-time-stamp "92/01/21 18:34:45 gildea")
(provide 'mh-e)

;;; Copyright (c) 1985,1986,1987,1988,1990,1992 Free Software Foundation
;;;     Maintainer:  Stephen Gildea <gildea@lcs.mit.edu>
;;;	Please send suggestions and corrections to the above address.
;;;
;;; This file contains mh-e, a GNU Emacs front end to the MH mail system.

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


;;;  Original version for Gosling emacs by Brian Reid, Stanford, 1982.
;;;  Modified by James Larus, BBN, July 1984 and UCB, 1984 & 1985.
;;;  Rewritten for GNU Emacs, James Larus 1985.  larus@ginger.berkeley.edu
;;;  Modified by Stephen Gildea, BBN, 1988, and MIT, 1990.  gildea@lcs.mit.edu


;;;  NB.  MH must have been compiled with the MHE compiler flag or several
;;;  features necessary mh-e will be missing from MH commands, specifically
;;;  the -build switch to repl and forw.

;;;  HOW TO USE:
;;;  M-x mh-rmail to read mail.  Type C-h m there for a list of commands.
;;;  C-u M-x mh-rmail to visit any folder.
;;;  M-x mh-smail to send mail.  From within the mail reader, "m" works, too.
;;;  Your .emacs might benefit from these bindings:
;;;  (global-set-key "\C-xm" 'mh-smail)
;;;  (global-set-key "\C-x4m" 'mh-smail-other-window)
;;;  (global-set-key "\C-xr" 'mh-rmail)	;clobbers copy-rectangle-to-register



;;; Constants:

;;; Set for local environment:
;;;* These are now in paths.el.
;;;(defvar mh-progs "/usr/new/mh/" "Directory containing MH commands.")
;;;(defvar mh-lib "/usr/new/lib/mh/" "Directory of MH library.")

(defvar mh-redist-full-contents nil
  "Non-nil if the `dist' command needs whole letter for redistribution.
This is the case when `send' is compiled with the BERK option.")


;;; Hooks:

(defvar mh-folder-mode-hook nil
  "Invoked in `mh-folder mode' on a new folder.")

(defvar mh-letter-mode-hook nil
  "Invoked in `mh-letter-mode' on a new letter.")

(defvar mh-compose-letter-function nil
  "Invoked in `mh-compose-and-send-mail' on a draft letter.
It is passed three arguments: TO recipients, SUBJECT, and CC recipients.")

(defvar mh-before-send-letter-hook nil
  "Invoked at the beginning of the \\[mh-send-letter] command.")

(defvar mh-inc-folder-hook nil
  "Invoked after incorporating mail into a folder with \\[mh-inc-folder].")

(defvar mh-before-quit-hook nil
  "Invoked by \\[mh-quit] before quitting mh-e.  See also  mh-quit-hook")

(defvar mh-quit-hook nil
  "Invoked after quitting mh-e by \\[mh-quit].  See also  mh-before-quit-hook")


(defvar mh-ins-string nil
  "Temporarily set by `mh-insert-prefix' prior to running `mh-yank-hooks'.")

(defvar mh-yank-hooks
  '(lambda ()
    (save-excursion
      (goto-char (point))
      (or (bolp) (forward-line 1))
      (while (< (point) (mark))
	(insert mh-ins-string)
	(forward-line 1))))
  "Hook to run citation function.
Expects POINT and MARK to be set to the region to cite.")


;;; Personal preferences:

(defvar mh-clean-message-header nil
  "*Non-nil means clean headers of messages that are displayed or inserted.
The variables `mh-visible-headers' and `mh-invisible-headers' control what
is removed.")

(defvar mh-visible-headers nil
  "*If non-nil, contains a regexp specifying the headers to keep when cleaning.
Only used if `mh-clean-message-header' is non-nil.  Setting this variable
overrides `mh-invisible-headers'.")

(defvar mhl-formfile nil
  "*Name of format file to be used by mhl to show messages.
A value of T means use the default format file.
Nil means don't use mhl to format messages.")

(defvar mh-lpr-command-format "lpr -p -J '%s'"
  "*Format for Unix command that prints a message.
The string should be a Unix command line, with the string '%s' where
the job's name (folder and message number) should appear.  The message text
is piped to this command.")

(defvar mh-print-background nil
  "*Print messages in the background if non-nil.
WARNING: do not delete the messages until printing is finished;
otherwise, your output may be truncated.")

(defvar mh-summary-height 4
  "*Number of lines in summary window (including the mode line).")

(defvar mh-recenter-summary-p nil
  "*Recenter summary window when the show window is toggled off if non-nil.")

(defvar mh-ins-buf-prefix "> "
  "*String to put before each non-blank line of a yanked or inserted message.
Used when the message is inserted in an outgoing letter.")

(defvar mh-do-not-confirm nil
  "*Non-nil means do not prompt for confirmation before some commands.
Only affects certain innocuous commands.")

(defvar mh-bury-show-buffer t
  "*Non-nil means that the displayed show buffer for a folder is buried.")

(defvar mh-delete-yanked-msg-window nil
  "*Controls window display when a message is yanked by \\[mh-yank-cur-msg].
If non-nil, yanking the current message into a draft letter deletes any
windows displaying the message.")

(defvar mh-yank-from-start-of-msg t
  "*Controls which part of a message is yanked by \\[mh-yank-cur-msg].
If non-nil, include the entire message.  If the symbol `body', then yank the
message minus the header.  If nil, yank only the portion of the message
following the point.  If the show buffer has a region, this variable is
ignored.")

(defvar mh-reply-default-reply-to nil
  "*Sets the person or persons to whom a reply will be sent.
If nil, prompt for recipient. If non-nil, then \\[mh-reply] will use this
value and it should be one of \"from\", \"to\", or \"cc\".")

(defvar mh-recursive-folders nil
  "*If non-nil, then commands which operate on folders do so recursively.")

(defvar mh-unshar-default-directory ""
  "*Default for directory name prompted for by mh-unshar-msg.")


;;; Parameterize mh-e to work with different scan formats.  The defaults work
;;; with the standard MH scan listings.

(defvar mh-cmd-note 4
  "Offset to insert notation.")

(defvar mh-note-repl "-"
  "String whose first character is used to notate replied to messages.")

(defvar mh-note-forw "F"
  "String whose first character is used to notate forwarded messages.")

(defvar mh-note-dist "R"
  "String whose first character is used to notate redistributed messages.")

(defvar mh-good-msg-regexp  "^....[^D^]"
  "Regexp specifiying the scan lines that are 'good' messages.")

(defvar mh-deleted-msg-regexp "^....D"
  "Regexp matching scan lines of deleted messages.")

(defvar mh-refiled-msg-regexp  "^....\\^"
  "Regexp matching scan lines of refiled messages.")

(defvar mh-valid-scan-line "^ *[0-9]"
  "Regexp matching scan lines for messages (not error messages).")

(defvar mh-msg-number-regexp "^ *\\([0-9]+\\)"
  "Regexp to find the number of a message in a scan line.
The message's number must be surrounded with \\( \\)")

(defvar mh-msg-search-regexp "^[^0-9]*%d[^0-9]"
  "Format string containing a regexp matching the scan listing for a message.
The desired message's number will be an argument to format.")

(defvar mh-flagged-scan-msg-regexp "^....\\D\\|^....\\^\\|^....\\+\\|^.....%"
  "Regexp matching flagged scan lines.
Matches lines marked as deleted, refiled, in a sequence, or the cur message.")

(defvar mh-cur-scan-msg-regexp "^....\\+"
  "Regexp matching scan line for the cur message.")

(defvar mh-show-buffer-mode-line-buffer-id "{%%b}  %s/%d"
  "Format string to produce `mode-line-buffer-id' for show buffers.
First argument is folder name.  Second is message number.")

(defvar mh-partial-folder-mode-line-annotation "select"
  "Annotation when displaying part of a folder.
The string is displayed after the folder's name.  NIL for no annotation.")


;;; Real constants:

(defvar mh-invisible-headers
  "^Received: \\|^Message-Id: \\|^Remailed-\\|^Via: \\|^Mail-from: \\|^Return-Path: \\|^In-Reply-To: \\|^Resent-"
  "Regexp matching lines in a message header that are not to be shown.
If `mh-visible-headers' is non-nil, it is used instead to specify what
to keep.")

(defvar mh-rejected-letter-start
  (concat "^   ----- Unsent message follows -----$" ;from mail system
	  "\\|^------- Unsent Draft$"	;from MH itself
	  "\\|^  --- The unsent message follows ---$") ;from AIX mail system
  "Regexp specifying the beginning of the wrapper around a returned letter.
This wrapper is generated by the mail system when rejecting a letter.")

(defvar mh-to-field-choices '((?t . "To:") (?s . "Subject:") (?c . "Cc:")
			      (?b . "Bcc:") (?f . "Fcc:"))
  "A-list of (character . field name) strings for mh-to-field.")


;;; Global variables:

(defvar mh-user-path  ""
  "User's mail folder.")

(defvar mh-last-destination nil
  "Destination of last refile or write command.")

(defvar mh-folder-mode-map (make-keymap)
  "Keymap for MH folders.")

(defvar mh-letter-mode-map (copy-keymap text-mode-map)
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

(defvar mh-previous-window-config nil
  "Window configuration before mh-e command.")

(defvar mh-previous-seq nil
  "Name of the sequence to which a message was last added.")


;;; Macros and generic functions:

(defmacro mh-push (v l)
  (list 'setq l (list 'cons v l)))


(defmacro mh-when (pred &rest body)
  (list 'cond (cons pred body)))


(defmacro with-mh-folder-updating (save-modification-flag-p &rest body)
  ;; Format is (with-mh-folder-updating (SAVE-MODIFICATION-FLAG-P) &body BODY).
  ;; Execute BODY, which can modify the folder buffer without having to
  ;; worry about file locking or the read-only flag, and return its result.
  ;; If SAVE-MODIFICATION-FLAG-P is non-nil, the buffer's modification
  ;; flag is unchanged, otherwise it is cleared.
  (setq save-modification-flag-p (car save-modification-flag-p)) ; CL style
  (` (let ((folder-updating-mod-flag (buffer-modified-p)))
       (prog1
	   (let ((buffer-read-only nil)
		 (buffer-file-name nil)) ; don't let the buffer get locked
	     (,@ body))
	 (, (if save-modification-flag-p
		'(mh-set-folder-modified-p folder-updating-mod-flag)
	      '(mh-set-folder-modified-p nil)))))))


(defun mh-mapc (func list)
  (while list
    (funcall func (car list))
    (setq list (cdr list))))



;;; Entry points:

;;;###autoload
(defun mh-rmail (&optional arg)
  "Inc(orporate) new mail (no arg) or scan a MH mail box (arg given).
This front end uses the MH mail system, which uses different conventions
from the usual mail system."
  (interactive "P")
  (mh-find-path)
  (if arg
      (call-interactively 'mh-visit-folder)
      (mh-inc-folder)))


;;;###autoload
(defun mh-smail ()
  "Compose and send mail with the MH mail system."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send))


(defun mh-smail-other-window ()
  "Compose and send mail in other window with the MH mail system."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send-other-window))



;;; User executable mh-e commands:

(defun mh-burst-digest ()
  "Burst apart the current message, which should be a digest.
The message is replaced by its table of contents and the letters from the
digest are inserted into the folder after that message."
  (interactive)
  (let ((digest (mh-get-msg-num t)))
    (mh-process-or-undo-commands mh-current-folder)
    (mh-set-folder-modified-p t)		; lock folder while bursting
    (message "Bursting digest...")
    (mh-exec-cmd "burst" mh-current-folder digest "-inplace")
    (mh-scan-folder mh-current-folder (format "%d-last" mh-first-msg-num))
    (message "Bursting digest...done")))


(defun mh-copy-msg (prefix-provided msg-or-seq dest)
  "Copy specified MESSAGE(s) to another FOLDER without deleting them.
Default is the displayed message.  If optional prefix argument is
provided, then prompt for the message sequence."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq-default "Copy" t)
			 (mh-get-msg-num t))
		     (mh-prompt-for-folder "Copy to" "" t)))
  (mh-exec-cmd "refile" msg-or-seq "-link" "-src" mh-current-folder dest)
  (if prefix-provided
      (mh-notate-seq msg-or-seq ?C mh-cmd-note)
      (mh-notate msg-or-seq ?C mh-cmd-note)))


(defun mh-delete-msg (msg-or-seq)
  "Mark the specified MESSAGE(s) for subsequent deletion and move to the next.
Default is the displayed message.  If optional prefix argument is
given then prompt for the message sequence."
  (interactive (list (if current-prefix-arg
			 (mh-read-seq-default "Delete" t)
			 (mh-get-msg-num t))))
  (if (numberp msg-or-seq)
      (mh-delete-a-msg msg-or-seq)
      (mh-map-to-seq-msgs 'mh-delete-a-msg msg-or-seq))
  (mh-next-msg))


(defun mh-delete-msg-no-motion (msg-or-seq)
  "Mark the specified MESSAGE(s) for subsequent deletion.
Default is the displayed message.  If optional prefix argument is
provided, then prompt for the message sequence."
  (interactive (list (if current-prefix-arg
			 (mh-read-seq-default "Delete" t)
			 (mh-get-msg-num t))))
  (if (numberp msg-or-seq)
      (mh-delete-a-msg msg-or-seq)
      (mh-map-to-seq-msgs 'mh-delete-a-msg msg-or-seq)))


(defun mh-delete-msg-from-seq (prefix-provided msg-or-seq &optional from-seq)
  "Delete MESSAGE (default: displayed message) from SEQUENCE.
If optional prefix argument provided, then delete all messages
from a sequence."
  (interactive (let ((argp current-prefix-arg))
		 (list argp
		       (if argp
			   (mh-read-seq-default "Delete" t)
			   (mh-get-msg-num t))
		       (if (not argp)
			   (mh-read-seq-default "Delete from" t)))))
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
		 (pop-to-buffer (find-file-noselect (mh-msg-filename msg)) t)
		 (rename-buffer (format "draft-%d" msg))
		 (buffer-name))
		(t
		 (mh-read-draft "clean-up" (mh-msg-filename msg) nil)))))
    (mh-clean-msg-header (point-min)
			 "^Date:\\|^Received:\\|^Message-Id:\\|^From:\\|^Delivery-Date:"
			 nil)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (mh-compose-and-send-mail draft "" from-folder nil nil nil nil nil nil
			      config)))


(defun mh-execute-commands ()
  "Process outstanding delete and refile requests."
  (interactive)
  (if mh-narrowed-to-seq (mh-widen))
  (mh-process-commands mh-current-folder)
  (mh-set-scan-mode)
  (mh-goto-cur-msg)			; after mh-set-scan-mode for efficiency
  (mh-make-folder-mode-line)
  t)					; return t for write-file-hooks


(defun mh-extract-rejected-mail (msg)
  "Extract a letter returned by the mail system and make it resendable.
Default is the displayed message."
  (interactive (list (mh-get-msg-num t)))
  (let ((from-folder mh-current-folder)
	(config (current-window-configuration))
	(draft (mh-read-draft "extraction" (mh-msg-filename msg) nil)))
    (goto-char (point-min))
    (cond ((re-search-forward mh-rejected-letter-start nil t)
	   (forward-char 1)
	   (delete-region (point-min) (point))
	   (mh-clean-msg-header (point-min)
				"^Date:\\|^Received:\\|^Message-Id:\\|^From:\\|^Sender:\\|^Return-Path:"
				nil))
	  (t
	   (message "Does not appear to be a rejected letter.")))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (mh-compose-and-send-mail draft "" from-folder msg (mh-get-field "To")
			      (mh-get-field "From") (mh-get-field "cc")
			      nil nil config)))


(defun mh-first-msg ()
  "Move to the first message."
  (interactive)
  (goto-char (point-min)))


(defun mh-forward (prefix-provided msg-or-seq to cc)
  "Forward MESSAGE(s) (default: displayed message).
If optional prefix argument provided, then prompt for the message sequence."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq-default "Forward" t)
			 (mh-get-msg-num t))
		     (read-string "To: ")
		     (read-string "Cc: ")))
  (let* ((folder mh-current-folder)
	 (config (current-window-configuration))
	 ;; forw always leaves file in "draft" since it doesn't have -draft
	 (draft-name (expand-file-name "draft" mh-user-path))
	 (draft (cond ((or (not (file-exists-p draft-name))
			   (y-or-n-p "The file 'draft' exists.  Discard it? "))
		       (mh-exec-cmd "forw"
				    "-build" mh-current-folder msg-or-seq)
		       (prog1
			   (mh-read-draft "" draft-name t)
			 (mh-insert-fields "To:" to "Cc:" cc)
			 (set-buffer-modified-p nil)))
		      (t
		       (mh-read-draft "" draft-name nil)))))
    (goto-char (point-min))
    (re-search-forward "^------- Forwarded Message")
    (forward-line -1)
    (narrow-to-region (point) (point-max))
    (let* ((subject (save-excursion (mh-get-field "From:")))
	   (trim (string-match "<" subject))
	   (forw-subject (save-excursion (mh-get-field "Subject:"))))
      (if trim
	  (setq subject (substring subject 0 (1- trim))))
      (widen)
      (save-excursion
	(mh-insert-fields "Subject:" (format "[%s: %s]" subject forw-subject)))
      (delete-other-windows)
      (if prefix-provided
	  (mh-add-msgs-to-seq (mh-seq-to-msgs msg-or-seq) 'forwarded t)
	  (mh-add-msgs-to-seq msg-or-seq 'forwarded t))
      (mh-compose-and-send-mail draft "" folder msg-or-seq
				to subject cc
				mh-note-forw "Forwarded:"
				config))))


(defun mh-goto-msg (number &optional no-error-if-no-message dont-show)
  "Position the cursor at message NUMBER.
Non-nil second argument means do not signal an error if message does not exist.
Non-nil third argument means not to show the message.
Return non-nil if cursor is at message."
  (interactive "NGoto message: ")
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
		  (goto-char (point-max))
		  (re-search-backward msg-pattern nil t)))
	    (beginning-of-line)
	    (if (not dont-show) (mh-maybe-show number))
	    t)
	  (t
	   (goto-char starting-place)
	   (if (not no-error-if-no-message)
	       (error "No message %d" number))
	   nil))))


(defun mh-inc-folder (&optional maildrop-name)
  "Inc(orporate) new mail into +inbox.
Optional prefix argument specifies an alternate maildrop from the default.
If this is given, incorporate mail into the current folder, rather
than +inbox.  Run `mh-inc-folder-hook' after incorporating new mail."
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
	(mh-set-folder-modified-p t)	; lock folder to kill it
	(mh-exec-cmd-daemon "rmf" folder)
	(mh-remove-folder-from-folder-list folder)
	(message "Folder %s removed" folder)
	(mh-set-folder-modified-p nil)	; so kill-buffer doesn't complain
	(if (get-buffer mh-show-buffer)
	    (kill-buffer mh-show-buffer))
	(kill-buffer folder))
      (message "Folder not removed")))


(defun mh-last-msg ()
  "Move to the last message."
  (interactive)
  (goto-char (point-max))
  (while (and (not (bobp)) (looking-at "^$"))
    (forward-line -1)))


(defun mh-list-folders ()
  "List mail folders."
  (interactive)
  (with-output-to-temp-buffer " *mh-temp*"
    (save-excursion
      (switch-to-buffer " *mh-temp*")
      (erase-buffer)
      (message "Listing folders...")
      (mh-exec-cmd-output "folders" t (if mh-recursive-folders
					  "-recurse"
					  "-norecurse"))
      (goto-char (point-min))
      (message "Listing folders...done"))))


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
  (let ((eob (point-max)))
    (with-mh-folder-updating (t)
      (cond ((mh-seq-to-msgs seq)
	     (mh-copy-seq-to-point seq eob)
	     (narrow-to-region eob (point-max))
	     (mh-make-folder-mode-line (symbol-name seq))
	     (mh-recenter nil)
	     (setq mh-narrowed-to-seq seq))
	    (t
	     (error "No messages in sequence `%s'" (symbol-name seq)))))))


(defun mh-next-undeleted-msg (&optional arg)
  "Move to next undeleted message in window."
  (interactive "P")
  (forward-line (prefix-numeric-value arg))
  (setq mh-next-direction 'forward)
  (cond ((re-search-forward mh-good-msg-regexp nil 0 arg)
	 (beginning-of-line)
	 (mh-maybe-show))
	(t
	 (forward-line -1)
	 (if (get-buffer mh-show-buffer)
	     (delete-windows-on mh-show-buffer)))))


(defun mh-pack-folder (range)
  "Renumber the messages of a folder to be 1..n.
First, offer to execute any outstanding commands for the current folder.
If optional prefix argument provided, prompt for the range of messages
to display after packing.  Otherwise, show the entire folder."
  (interactive (list (if current-prefix-arg
			 (mh-read-msg-range
			  "Range to scan after packing [all]? ")
			 "all")))
  (mh-pack-folder-1 range)
  (mh-goto-cur-msg)
  (message "Packing folder...done"))


(defun mh-pipe-msg (prefix-provided command)
  "Pipe the current message through the given shell COMMAND.
If optional prefix argument is provided, send the entire message.
Otherwise just send the message's body."
  (interactive
   (list current-prefix-arg (read-string "Shell command on message: ")))
  (save-excursion
    (mh-display-msg (mh-get-msg-num t) mh-current-folder) ;update show buffer
    (goto-char (point-min))
    (if (not prefix-provided) (search-forward "\n\n"))
    (shell-command-on-region (point) (point-max) command nil)))


(defun mh-refile-msg (prefix-provided msg-or-seq dest)
  "Refile MESSAGE(s) (default: displayed message) in FOLDER.
If optional prefix argument provided, then prompt for message sequence."
  (interactive
   (list current-prefix-arg
	 (if current-prefix-arg
	     (mh-read-seq-default "Refile" t)
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
  "Re-execute the last refile or write command on the given MESSAGE.
Default is the displayed message.  Use the same folder or file as the
previous refile or write command."
  (interactive (list (mh-get-msg-num t)))
  (if (null mh-last-destination)
      (error "No previous refile or write"))
  (cond ((eq (car mh-last-destination) 'refile)
	 (mh-refile-a-msg msg (cdr mh-last-destination))
	 (message "Destination folder: %s" (cdr mh-last-destination)))
	(t
	 (mh-write-msg-to-file msg (cdr mh-last-destination))
	 (message "Destination: %s" (cdr mh-last-destination))))
  (mh-next-msg))


(defun mh-reply (prefix-provided msg)
  "Reply to a MESSAGE (default: displayed message).
If optional prefix argument provided, then include the message in the reply
using filter mhl.reply in your MH directory."
  (interactive (list current-prefix-arg (mh-get-msg-num t)))
  (let ((minibuffer-help-form
	 "from => Sender only\nto => Sender and primary recipients\ncc or all => Sender and all recipients"))
    (let ((reply-to (or mh-reply-default-reply-to
			(completing-read "Reply to whom: "
					 '(("from") ("to") ("cc") ("all"))
					 nil
					 t)))
	  (folder mh-current-folder)
	  (show-buffer mh-show-buffer)
	  (config (current-window-configuration)))
      (message "Composing a reply...")
      (cond ((or (equal reply-to "from") (equal reply-to ""))
	     (apply 'mh-exec-cmd
		    "repl" "-build" "-noquery"
		    "-nodraftfolder" mh-current-folder
		    msg
		    "-nocc" "all"
		    (if prefix-provided
			(list "-filter" "mhl.reply"))))
	    ((equal reply-to "to")
	     (apply 'mh-exec-cmd
		    "repl" "-build" "-noquery"
		    "-nodraftfolder" mh-current-folder
		    msg
		    "-cc" "to"
		    (if prefix-provided
			(list "-filter" "mhl.reply"))))
	    ((or (equal reply-to "cc") (equal reply-to "all"))
	     (apply 'mh-exec-cmd
		    "repl" "-build" "-noquery"
		    "-nodraftfolder" mh-current-folder
		    msg
		    "-cc" "all" "-nocc" "me"
		    (if prefix-provided
			(list "-filter" "mhl.reply")))))

      (let ((draft (mh-read-draft "reply"
				  (expand-file-name "reply" mh-user-path)
				  t)))
	(delete-other-windows)
	(set-buffer-modified-p nil)

	(let ((to (mh-get-field "To:"))
	      (subject (mh-get-field "Subject:"))
	      (cc (mh-get-field "Cc:")))
	  (goto-char (point-min))
	  (mh-goto-header-end 1)
	  (if (not prefix-provided)
	      (mh-display-msg msg folder))
	  (mh-add-msgs-to-seq msg 'answered t)
	  (message "Composing a reply...done")
	  (mh-compose-and-send-mail draft "" folder msg to subject cc
				    mh-note-repl "Replied:" config))))))


(defun mh-quit ()
  "Quit mh-e.
Start by running mh-before-quit-hook.  Restore the previous window
configuration, if one exists.  Finish by running mh-quit-hook."
  (interactive)
  (run-hooks 'mh-before-quit-hook)
  (if mh-previous-window-config
      (set-window-configuration mh-previous-window-config))
  (run-hooks 'mh-quit-hook))


(defun mh-page-digest ()
  "Advance displayed message to next digested message."
  (interactive)
  (save-excursion
    (mh-show-message-in-other-window)
    ;; Go to top of screen (in case user moved point).
    (move-to-window-line 0)
    (let ((case-fold-search nil))
      ;; Search for blank line and then for From:
      (mh-when (not (and (search-forward "\n\n" nil t)
			 (search-forward "From:" nil t)))
	(other-window -1)
	(error "No more messages")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (search-backward "\n\n" nil t)
    (forward-line 2)
    (mh-recenter 0)
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
      (mh-when (not (and (search-backward "\n\n" nil t)
			 (search-backward "From:" nil t)))
	(other-window -1)
	(error "No more messages")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (search-backward "\n\n" nil t)
    (forward-line 2)
    (mh-recenter 0)
    (other-window -1)))


(defun mh-page-msg (&optional arg)
  "Page the displayed message forwards.
Scrolls ARG lines or a full screen if no argument is supplied."
  (interactive "P")
  (scroll-other-window arg))


(defun mh-previous-page (&optional arg)
  "Page the displayed message backwards.
Scrolls ARG lines or a full screen if no argument is supplied."
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
  (beginning-of-line)
  (cond ((re-search-backward mh-good-msg-regexp nil 0 arg)
	 (mh-maybe-show))
	(t
	 (if (get-buffer mh-show-buffer)
	     (delete-windows-on mh-show-buffer)))))


(defun mh-print-msg (prefix-provided msg-or-seq)
  "Print MESSAGE(s) (default: displayed message) on a line printer.
If optional prefix argument provided, then prompt for the message sequence."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (reverse (mh-seq-to-msgs
				   (mh-read-seq-default "Print" t)))
			 (mh-get-msg-num t))))
  (if prefix-provided
      (message "Printing sequence...")
      (message "Printing message..."))
  (let ((print-command
	 (if prefix-provided
	     (format "(scan -clear %s ; %s -nobell -clear %s %s) | %s"
		     (mapconcat (function (lambda (msg) msg)) msg-or-seq " ")
		     (expand-file-name "mhl" mh-lib)
		     (if (stringp mhl-formfile)
			 (format "-form %s" mhl-formfile)
		         "")
		     (mh-msg-filenames msg-or-seq)
		     (format mh-lpr-command-format
			     (if prefix-provided
				 (format "Sequence from %s" mh-current-folder)
				 (format "%s/%d" mh-current-folder
					 msg-or-seq))))
	     (format "%s -nobell -clear %s %s | %s"
		     (expand-file-name "mhl" mh-lib)
		     (mh-msg-filename msg-or-seq)
		     (if (stringp mhl-formfile)
			 (format "-form %s" mhl-formfile)
		         "")
		     (format mh-lpr-command-format
			     (if prefix-provided
				 (format "Sequence from %s" mh-current-folder)
				 (format "%s/%d" mh-current-folder
					 msg-or-seq)))))))
    (if mh-print-background
	(mh-exec-cmd-daemon shell-file-name "-c" print-command)
	(call-process shell-file-name nil nil nil "-c" print-command))
    (if prefix-provided
	(mh-notate-seq msg-or-seq ?P mh-cmd-note)
	(mh-notate msg-or-seq ?P mh-cmd-note))
    (mh-add-msgs-to-seq msg-or-seq 'printed t)
    (if prefix-provided
	(message "Printing sequence...done")
        (message "Printing message...done"))))


(defun mh-put-msg-in-seq (prefix-provided from to)
  "Add MESSAGE(s) (default: displayed message) to SEQUENCE.
If optional prefix argument provided, then prompt for the message sequence."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-seq-to-msgs
			   (mh-read-seq-default "Add messages from" t))
			 (mh-get-msg-num t))
		     (mh-read-seq-default "Add to" nil)))
  (setq mh-previous-seq to)
  (mh-add-msgs-to-seq from to))


(defun mh-rescan-folder (&optional range)
  "Rescan a folder after optionally processing the outstanding commands.
If optional prefix argument is provided, prompt for the range of
messages to display.  Otherwise show the entire folder."
  (interactive (list (if current-prefix-arg
			 (mh-read-msg-range "Range to scan [all]? ")
			 nil)))
  (setq mh-next-direction 'forward)
  (mh-scan-folder mh-current-folder (or range "all")))


(defun mh-redistribute (to cc msg)
  "Redistribute a letter.
Depending on how your copy of MH was compiled, you may need to change the
setting of the variable mh-redist-full-contents.  See its documentation."
  (interactive (list (read-string "Redist-To: ")
		     (read-string "Redist-Cc: ")
		     (mh-get-msg-num t)))
  (save-window-excursion
    (let ((folder mh-current-folder)
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
				(expand-file-name "send" mh-progs)
				(buffer-file-name)))
	  (call-process "/bin/sh" nil 0 nil "-c"
			(format "mhdist=1 mhaltmsg=%s mhannotate=1 %s -push %s"
				(mh-msg-filename msg folder)
				(expand-file-name "send" mh-progs)
				(buffer-file-name))))
      (mh-annotate-msg msg folder mh-note-dist
		       "-component" "Resent:"
		       "-text" (format "\"%s %s\"" to cc))
      (kill-buffer draft)
      (message "Redistributing...done"))))


(defun mh-write-msg-to-file (msg file)
  "Append MESSAGE to the end of a FILE."
  (interactive
   (list (mh-get-msg-num t)
	 (let ((default-dir (if (eq 'write (car mh-last-destination))
				(file-name-directory (cdr mh-last-destination))
				default-directory)))
	   (read-file-name "Save message in file: " default-dir
			   (expand-file-name "mail.out" default-dir)))))
  (let ((file-name (mh-msg-filename msg))
	(output-file (mh-expand-file-name file)))
    (setq mh-last-destination (cons 'write file))
    (save-excursion
      (set-buffer (get-buffer-create " *mh-temp*"))
      (erase-buffer)
      (insert-file-contents file-name)
      (append-to-file (point-min) (point-max) output-file))))


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
  "Compose and send a letter.
The letter is composed in mh-letter-mode; see its documentation for more
details.  If `mh-compose-letter-function' is defined, it is called on the
draft and passed three arguments: to, subject, and cc."
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
  (let ((folder mh-current-folder)
	(msg-num (mh-get-msg-num nil)))
    (message "Composing a message...")
    (let ((draft (mh-read-draft
		  "message"
		  (if (file-exists-p
		       (expand-file-name "components" mh-user-path))
		      (expand-file-name "components" mh-user-path)
		      (if (file-exists-p
			   (expand-file-name "components" mh-lib))
			  (expand-file-name "components" mh-lib)
			  (error "Can't find components file")))
		  nil)))
      (mh-insert-fields "To:" to "Subject:" subject "Cc:" cc)
      (set-buffer-modified-p nil)
      (goto-char (point-max))
      (message "Composing a message...done")
      (mh-compose-and-send-mail draft "" folder msg-num
				to subject cc
				nil nil config))))


(defun mh-show (&optional msg)
  "Show MESSAGE (default: displayed message).
Forces a two-window display with the folder window on top (size
mh-summary-height) and the show buffer below it."
  (interactive)
  (if (not msg)
      (setq msg (mh-get-msg-num t)))
  (setq mh-showing t)
  (mh-set-mode-name "mh-e show")
  (if (not (eql (next-window (minibuffer-window)) (selected-window)))
      (delete-other-windows))		; force ourself to the top window
  (let ((folder mh-current-folder))
    (mh-show-message-in-other-window)
    (mh-display-msg msg folder))
  (other-window -1)
  (if (not (= (1+ (window-height)) (screen-height))) ;not horizontally split
      (shrink-window (- (window-height) mh-summary-height)))
  (mh-recenter nil)
  (if (not (memq msg mh-seen-list)) (mh-push msg mh-seen-list)))


(defun mh-sort-folder ()
  "Sort the messages in the current folder by date."
  (interactive)
  (mh-process-or-undo-commands mh-current-folder)
  (setq mh-next-direction 'forward)
  (mh-set-folder-modified-p t)		; lock folder while sorting
  (message "Sorting folder...")
  (mh-exec-cmd "sortm" mh-current-folder)
  (message "Sorting folder...done")
  (mh-scan-folder mh-current-folder "all"))


(defun mh-toggle-showing ()
  "Toggle the scanning mode/showing mode of displaying messages."
  (interactive)
  (if mh-showing
      (mh-set-scan-mode)
      (mh-show)))


(defun mh-undo (prefix-provided msg-or-seq)
  "Undo the deletion or refile of the specified MESSAGE(s).
Default is the displayed message.  If optional prefix argument is
provided, then prompt for the message sequence."
  (interactive (list current-prefix-arg
		     (if current-prefix-arg
			 (mh-read-seq-default "Undo" t)
			 (mh-get-msg-num t))))
  (cond (prefix-provided
	 (mh-mapc (function mh-undo-msg) (mh-seq-to-msgs msg-or-seq)))
	(t
	 (let ((original-position (point)))
	   (beginning-of-line)
	   (while (not (or (looking-at mh-deleted-msg-regexp)
			   (looking-at mh-refiled-msg-regexp)
			   (and (eq mh-next-direction 'forward) (bobp))
			   (and (eq mh-next-direction 'backward)
				(save-excursion (forward-line) (eobp)))))
	     (forward-line (if (eq mh-next-direction 'forward) -1 1)))
	   (if (or (looking-at mh-deleted-msg-regexp)
		   (looking-at mh-refiled-msg-regexp))
	       (progn
		 (mh-undo-msg (mh-get-msg-num t))
		 (mh-maybe-show))
	       (goto-char original-position)
	       (error "Nothing to undo")))))
  ;; update the mh-refile-list so mh-outstanding-commands-p will work
  (mh-mapc (function
	    (lambda (elt)
	      (if (not (mh-seq-to-msgs elt))
		  (setq mh-refile-list (delq elt mh-refile-list)))))
	   mh-refile-list)
  (if (not (mh-outstanding-commands-p))
      (mh-set-folder-modified-p nil)))


(defun mh-undo-msg (msg)
  ;; Undo the deletion or refile of one MESSAGE.
  (cond ((memq msg mh-delete-list)
	 (setq mh-delete-list (delq msg mh-delete-list))
	 (mh-remove-msg-from-seq msg 'deleted t))
	(t
	 (mh-mapc (function (lambda (dest)
			      (mh-remove-msg-from-seq msg dest t)))
		  mh-refile-list)))
  (mh-notate msg ?  mh-cmd-note))


(defun mh-undo-folder (&rest ignore)
  "Undo all commands in current folder."
  (interactive)
  (cond ((or mh-do-not-confirm
	     (yes-or-no-p "Undo all commands in folder? "))
	 (setq mh-delete-list nil
	       mh-refile-list nil
	       mh-seq-list nil
	       mh-next-direction 'forward)
	 (with-mh-folder-updating (nil)
	   (mh-unmark-all-headers t)))
	(t
	 (message "Commands not undone.")
	 (sit-for 2))))


(defun mh-unshar-msg (dir)
  "Unpack the shar file contained in the current message into directory DIR."
  (interactive (list (read-file-name "Unshar message in directory: "
				     mh-unshar-default-directory
				     mh-unshar-default-directory nil)))
  (mh-display-msg (mh-get-msg-num t) mh-current-folder) ;update show buffer
  (mh-unshar-buffer dir))

(defun mh-unshar-buffer (dir)
  ;; Unpack the shar file contained in the current buffer into directory DIR.
  (goto-char (point-min))
  (if (or (re-search-forward "^#![ \t]*/bin/sh" nil t)
	  (and (re-search-forward "^[^a-z0-9\"]*cut here\b" nil t)
	       (forward-line 1))
	  (re-search-forward "^#" nil t)
	  (re-search-forward "^: " nil t))
      (let ((default-directory (expand-file-name dir))
	    (start (progn (beginning-of-line) (point)))
	    (log-buffer (get-buffer-create "*Unshar Output*")))
	(save-excursion
	  (set-buffer log-buffer)
	  (setq default-directory (expand-file-name dir))
	  (erase-buffer)
	  (if (file-directory-p default-directory)
	      (insert "cd " dir "\n")
	    (insert "mkdir " dir "\n")
	    (call-process "mkdir" nil log-buffer t default-directory)))
	(set-window-start (display-buffer log-buffer) 0) ;so can watch progress
	(call-process-region start (point-max) "sh" nil log-buffer t))
    (error "Cannot find start of shar.")))
	

(defun mh-visit-folder (folder &optional range)
  "Visit FOLDER and display RANGE of messages.
Assumes mh-e has already been initialized."
  (interactive (list (mh-prompt-for-folder "Visit" "+inbox" t)
		     (mh-read-msg-range "Range [all]? ")))
  (let ((config (current-window-configuration)))
    (mh-scan-folder folder (or range "all"))
    (setq mh-previous-window-config config))
  nil)


(defun mh-widen ()
  "Remove restrictions from the current folder, thereby showing all messages."
  (interactive)
  (if mh-narrowed-to-seq
      (with-mh-folder-updating (t)
	(delete-region (point-min) (point-max))
	(widen)
	(mh-make-folder-mode-line)))
  (setq mh-narrowed-to-seq nil))



;;; Support routines.

(defun mh-delete-a-msg (msg)
  ;; Delete the MESSAGE.
  (save-excursion
    (mh-goto-msg msg nil t)
    (if (looking-at mh-refiled-msg-regexp)
	(error "Message %d is refiled.  Undo refile before deleting." msg))
    (if (looking-at mh-deleted-msg-regexp)
	nil
	(mh-set-folder-modified-p t)
	(mh-push msg mh-delete-list)
	(mh-add-msgs-to-seq msg 'deleted t)
	(mh-notate msg ?D mh-cmd-note))))


(defun mh-refile-a-msg (msg destination)
  ;; Refile MESSAGE in FOLDER.  FOLDER is a symbol, not a string.
  (save-excursion
    (mh-goto-msg msg nil t)
    (cond ((looking-at mh-deleted-msg-regexp)
	   (error "Message %d is deleted.  Undo delete before moving." msg))
	  ((looking-at mh-refiled-msg-regexp)
	   (if (y-or-n-p
		(format "Message %d already refiled.  Copy to %s as well? "
			msg destination))
	       (mh-exec-cmd "refile" (mh-get-msg-num t) "-link"
			    "-src" mh-current-folder
			    (symbol-name destination))
	       (message "Message not copied.")))
	  (t
	   (mh-set-folder-modified-p t)
	   (if (not (memq destination mh-refile-list))
	       (mh-push destination mh-refile-list))
	   (if (not (memq msg (mh-seq-to-msgs destination)))
	       (mh-add-msgs-to-seq msg destination t))
	   (mh-notate msg ?^ mh-cmd-note)))))


(defun mh-display-msg (msg-num folder)
  ;; Display message NUMBER of FOLDER.
  ;; Sets the current buffer to the show buffer.
  (set-buffer folder)
  ;; Bind variables in folder buffer in case they are local
  (let ((formfile mhl-formfile)
	(clean-message-header mh-clean-message-header)
	(invisible-headers mh-invisible-headers)
	(visible-headers mh-visible-headers)
	(msg-filename (mh-msg-filename msg-num))
	(show-buffer mh-show-buffer)
	(folder mh-current-folder))
    (if (not (file-exists-p msg-filename))
	(error "Message %d does not exist" msg-num))
    (switch-to-buffer show-buffer)
    (if mh-bury-show-buffer (bury-buffer (current-buffer)))
    (mh-when (not (equal msg-filename buffer-file-name))
      ;; Buffer does not yet contain message.
      (clear-visited-file-modtime)
      (unlock-buffer)
      (setq buffer-file-name nil)	; no locking during setup
      (erase-buffer)
      (if formfile
	  (if (stringp formfile)
	      (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
				      "-form" formfile msg-filename)
	      (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
				      msg-filename))
	  (insert-file-contents msg-filename))
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
	       (mh-recenter 0))))
      (set-buffer-modified-p nil)
      (setq buffer-file-name msg-filename)
      (set-mark nil)
      (setq mode-line-buffer-identification
	    (list (format mh-show-buffer-mode-line-buffer-id
			  folder msg-num))))))


(defun mh-invalidate-show-buffer ()
  ;; Invalidate the show buffer so we must update it to use it.
  (if (get-buffer mh-show-buffer)
      (save-excursion
	(set-buffer mh-show-buffer)
	(setq buffer-file-name nil))))


(defun mh-show-message-in-other-window ()
  (switch-to-buffer-other-window mh-show-buffer)
  (if mh-bury-show-buffer (bury-buffer (current-buffer))))


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
	  (backward-char 1))
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
	 (let ((orig-default-dir default-directory))
	   (pop-to-buffer (find-file-noselect (mh-new-draft-name)) t)
	   (rename-buffer (format "draft-%s" (buffer-name)))
	   (setq default-directory orig-default-dir)))
	(t
	 (let ((draft-name (expand-file-name "draft" mh-user-path)))
	   (pop-to-buffer "draft")	; Create if necessary
	   (if (buffer-modified-p)
	       (if (y-or-n-p "Draft has been modified; kill anyway? ")
		   (set-buffer-modified-p nil)
		   (error "Draft preserved")))
	   (setq buffer-file-name draft-name)
	   (clear-visited-file-modtime)
	   (unlock-buffer)
	   (mh-when (and (file-exists-p draft-name)
			 (not (equal draft-name initial-contents)))
	     (insert-file-contents draft-name)
	     (delete-file draft-name)))))
  (mh-when (and initial-contents
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
    (buffer-substring (point) (1- (mark)))))


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
  (setq mh-showing nil)
  (if mh-recenter-summary-p
      (mh-recenter nil)))


(defun mh-maybe-show (&optional msg)
  ;; If in showing mode, then display the message pointed to by the cursor.
  (if mh-showing (mh-show msg)))


(defun mh-set-mode-name (mode-name-string)
  ;; Set the mode-name and ensure that the mode line is updated.
  (setq mode-name mode-name-string)
  ;; Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p)))



;;; The folder data abstraction.

(defvar mh-current-folder nil "Name of current folder, a string.")
(defvar mh-show-buffer nil "Buffer that displays mesage for this folder.")
(defvar mh-folder-filename nil "Full path of directory for this folder.")
(defvar mh-showing nil "If non-nil, show the message in a separate window.")
(defvar mh-next-seq-num nil "Index of free sequence id.")
(defvar mh-delete-list nil "List of msg numbers to delete.")
(defvar mh-refile-list nil "List of folder names in mh-seq-list.")
(defvar mh-seq-list nil "Alist of (seq . msgs) numbers.")
(defvar mh-seen-list nil "List of displayed messages.")
(defvar mh-next-direction 'forward "Direction to move to next message.")
(defvar mh-narrowed-to-seq nil "Sequence display is narrowed to.")
(defvar mh-first-msg-num nil "Number of first msg in buffer.")
(defvar mh-last-msg-num nil "Number of last msg in buffer.")


(defun mh-make-folder (name)
  ;; Create and initialize a new mail folder called NAME and make it the
  ;; current folder.
  (switch-to-buffer name)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq buffer-read-only t)
  (mh-folder-mode)
  (mh-set-folder-modified-p nil)
  (setq buffer-file-name mh-folder-filename)
  (mh-set-mode-name "mh-e scan"))


;;; Don't use this mode when creating buffers if default-major-mode is nil.
(put 'mh-folder-mode 'mode-class 'special)

(defun mh-folder-mode ()
  "Major mode for \"editing\" an MH folder scan listing.
Messages can be marked for refiling and deletion.  However, both actions
are deferred until you request execution with \\[mh-execute-commands].
\\{mh-folder-mode-map}
  A prefix argument (\\[universal-argument]) to delete, refile, list, or undo
applies the action to a message sequence.

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
    Number of lines in the summary window including the mode line.

 mh-ins-buf-prefix (\"> \")
    String to insert before each non-blank line of a message as it is
    inserted in a draft letter.

The value of mh-folder-mode-hook is called when a new folder is set up."

  (kill-all-local-variables)
  (use-local-map mh-folder-mode-map)
  (setq major-mode 'mh-folder-mode)
  (mh-set-mode-name "mh-e folder")
  (make-local-vars
   'mh-current-folder (buffer-name)	; Name of folder, a string
   'mh-show-buffer (format "show-%s" (buffer-name)) ; Buffer that displays msgs
   'mh-folder-filename			; e.g. "/usr/foobar/Mail/inbox/"
   (file-name-as-directory (mh-expand-file-name (buffer-name)))
   'mh-showing nil			; Show message also?
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
  (setq truncate-lines t)
  (auto-save-mode -1)
  (setq buffer-offer-save t)
  (make-local-variable 'write-file-hooks)
  (setq write-file-hooks '(mh-execute-commands))
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'mh-undo-folder)
  (run-hooks 'mh-folder-mode-hook))


(defun make-local-vars (&rest pairs)
  ;; Take VARIABLE-VALUE pairs and makes local variables initialized to the
  ;; value.
  (while pairs
    (make-variable-buffer-local (car pairs))
    (set (car pairs) (car (cdr pairs)))
    (setq pairs (cdr (cdr pairs)))))


(defun mh-scan-folder (folder range)
  ;; Scan the FOLDER over the RANGE.  Return in the folder's buffer.
  (cond ((null (get-buffer folder))
	 (mh-make-folder folder))
	(t
	 (mh-process-or-undo-commands folder)
	 (switch-to-buffer folder)))
  (mh-regenerate-headers range)
  (mh-when (zerop (buffer-size))
    (if (equal range "all")
	(message "Folder %s is empty" folder)
	(message "No messages in %s, range %s" folder range))
    (sit-for 5))
  (mh-goto-cur-msg))


(defun mh-regenerate-headers (range)
  ;; Replace buffer with scan of its contents over range RANGE.
  (let ((folder mh-current-folder))
    (message "Scanning %s..." folder)
    (with-mh-folder-updating (nil)
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
      (setq mh-seq-list (mh-read-folder-sequences folder nil))
      (mh-notate-user-sequences)
      (mh-make-folder-mode-line (if (equal range "all")
				    nil
				    mh-partial-folder-mode-line-annotation)))
    (message "Scanning %s...done" folder)))


(defun mh-get-new-mail (maildrop-name)
  ;; Read new mail from a maildrop into the current buffer.
  ;; Return T if there was new mail, NIL otherwise.  Return in the current
  ;; buffer.
  (let ((point-before-inc (point))
	(folder mh-current-folder)
	(return-value t))
    (with-mh-folder-updating (t)
      (message (if maildrop-name
		   (format "inc %s -file %s..." folder maildrop-name)
		   (format "inc %s..." folder)))
      (mh-unmark-all-headers nil)
      (setq mh-next-direction 'forward)
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
	(goto-char start-of-inc)
	(cond ((looking-at "inc: no mail")
	       (keep-lines mh-valid-scan-line) ; Flush random scan lines
	       (goto-char point-before-inc)
	       (message "No new mail%s%s" (if maildrop-name " in " "")
			(if maildrop-name maildrop-name "")))
	      ((re-search-forward "^inc:" nil t) ; Error messages
	       (error "inc error"))
	      (t
	       (mh-delete-seq-locally 'cur) ; To pick up new one
	       (setq mh-seq-list (mh-read-folder-sequences folder t))
	       (mh-notate-user-sequences)
	       (keep-lines mh-valid-scan-line)
	       (mh-make-folder-mode-line)
	       (mh-goto-cur-msg)
	       (setq return-value t))))
      return-value)))


(defun mh-make-folder-mode-line (&optional annotation)
  ;; Set the fields of the mode line for a folder buffer.
  ;; The optional ANNOTATION string is displayed after the folder's name.
  (save-excursion
    (mh-first-msg)
    (setq mh-first-msg-num (mh-get-msg-num nil))
    (mh-last-msg)
    (setq mh-last-msg-num (mh-get-msg-num nil))
    (let ((lines (count-lines (point-min) (point-max))))
      (setq mode-line-buffer-identification
	    (list (format "{%%b%s}  %d msg%s"
			  (if annotation (format "/%s" annotation) "")
			  lines
			  (if (zerop lines)
			      "s"
			      (if (> lines 1)
				  (format "s (%d-%d)" mh-first-msg-num
					  mh-last-msg-num)
				  (format " (%d)" mh-first-msg-num)))))))))


(defun mh-unmark-all-headers (remove-all-flags)
  ;; Remove all '+' flags from the headers, and if called with a non-nil
  ;; argument, remove all 'D', '^' and '%' flags too.
  ;; Optimized for speed (i.e., no regular expressions).
  (save-excursion
    (let ((case-fold-search nil)
	  (last-line (- (point-max) mh-cmd-note))
	  char)
      (mh-first-msg)
      (while (<= (point) last-line)
	(forward-char mh-cmd-note)
	(setq char (following-char))
	(if (or (and remove-all-flags
		     (or (eql char ?D)
			 (eql char ?^)
			 (eql char ?%)))
		(eql char ?+))
	    (progn
	      (delete-char 1)
	      (insert " ")))
	(forward-line)))))


(defun mh-goto-cur-msg ()
  ;; Position the cursor at the current message.
  (let ((cur-msg (car (mh-seq-to-msgs 'cur))))
    (cond ((and cur-msg
		(mh-goto-msg cur-msg t nil))
	   (mh-notate nil ?+ mh-cmd-note)
	   (mh-recenter 0)
	   (mh-maybe-show cur-msg))
	  (t
	   (mh-last-msg)
	   (message "No current message")))))


(defun mh-pack-folder-1 (range)
  ;; Close and pack the current folder.
  (mh-process-or-undo-commands mh-current-folder)
  (message "Packing folder...")
  (mh-set-folder-modified-p t)		; lock folder while packing
  (save-excursion
    (mh-exec-cmd-quiet " *mh-temp*" "folder" mh-current-folder "-pack"))
  (mh-regenerate-headers range))


(defun mh-process-or-undo-commands (folder)
  ;; If FOLDER has outstanding commands, then either process or discard them.
  (set-buffer folder)
  (if (mh-outstanding-commands-p)
      (if (or mh-do-not-confirm
	      (y-or-n-p
		"Process outstanding deletes and refiles (or lose them)? "))
	  (mh-process-commands folder)
	  (mh-undo-folder))
      (mh-invalidate-show-buffer)))


(defun mh-process-commands (folder)
  ;; Process outstanding commands for the folder FOLDER.
  (message "Processing deletes and refiles for %s..." folder)
  (set-buffer folder)
  (with-mh-folder-updating (nil)
    ;; Update the unseen sequence if it exists
    (if (and mh-seen-list (mh-seq-to-msgs mh-unseen-seq))
	(mh-undefine-sequence mh-unseen-seq mh-seen-list))

    ;; Then refile messages
    (mh-mapc
     (function
      (lambda (dest)
	(let ((msgs (mh-seq-to-msgs dest)))
	  (mh-when msgs
	    (apply 'mh-exec-cmd "refile"
		   "-src" folder (symbol-name dest) msgs)
	    (mh-delete-scan-msgs msgs)))))
     mh-refile-list)

    ;; Now delete messages
    (mh-when mh-delete-list
      (apply 'mh-exec-cmd "rmm" folder mh-delete-list)
      (mh-delete-scan-msgs mh-delete-list))

    ;; Don't need to remove sequences since delete and refile do so.

    ;; Mark cur message
    (if (> (buffer-size) 0)
	(mh-define-sequence 'cur (list (or (mh-get-msg-num nil) "last"))))

    (mh-invalidate-show-buffer)

    (setq mh-delete-list nil
	  mh-refile-list nil
	  mh-seq-list (mh-read-folder-sequences mh-current-folder nil)
	  mh-seen-list nil)
    (mh-unmark-all-headers t)
    (mh-notate-user-sequences)
    (message "Processing deletes and refiles for %s...done" folder)))


(defun mh-delete-scan-msgs (msgs)
  ;; Delete the scan listing lines for each of the msgs in the LIST.
  ;; Optimized for speed (i.e., no regular expressions).
  (setq msgs (sort msgs (function <)))	;okay to clobber msgs
  (save-excursion
    (mh-first-msg)
    (while (and msgs (< (point) (point-max)))
      (cond ((equal (mh-get-msg-num nil) (car msgs))
	     (delete-region (point) (save-excursion (forward-line) (point)))
	     (setq msgs (cdr msgs)))
	    (t
	     (forward-line))))))


(defun mh-set-folder-modified-p (flag)
  "Mark current folder as modified or unmodified according to FLAG."
  (set-buffer-modified-p flag))


(defun mh-outstanding-commands-p ()
  ;; Returns non-nil if there are outstanding deletes or refiles.
  (or mh-delete-list mh-refile-list))



;;; Mode for composing and sending a draft message.

(defvar mh-sent-from-folder nil
  "Folder of msg associated with this letter.")

(defvar mh-sent-from-msg nil
  "Number of msg associated with this letter.")

(defvar mh-send-args nil
  "Extra arguments to pass to \"send\" command.")

(defvar mh-annotate-char nil
  "Character to use to annotate mh-sent-from-msg.")

(defvar mh-annotate-field nil
  "Field name for message annotation.")

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
  (make-local-variable 'mh-annotate-field)
  (make-local-variable 'mh-previous-window-config)
  (make-local-variable 'mh-sent-from-folder)
  (make-local-variable 'mh-sent-from-msg)
  (use-local-map mh-letter-mode-map)
  (setq major-mode 'mh-letter-mode)
  (mh-set-mode-name "mh-e letter")
  (set-syntax-table mh-letter-mode-syntax-table)
  (run-hooks 'text-mode-hook 'mh-letter-mode-hook)
  (mh-when auto-fill-hook
    (make-local-variable 'auto-fill-hook)
    (setq auto-fill-hook 'mh-auto-fill-for-letter)))


(defun mh-auto-fill-for-letter ()
  ;; Auto-fill in letters treats the header specially by inserting a tab
  ;; before continuation line.
  (do-auto-fill)
  (if (mh-in-header-p)
      (save-excursion
	(beginning-of-line nil)
	(insert-char ?\t 1))))


(defun mh-in-header-p ()
  ;; Return non-nil if the point is in the header of a draft message.
  (save-excursion
    (let ((cur-point (point)))
      (goto-char (dot-min))
      (re-search-forward "^--------" nil t)
      (< cur-point (point)))))


(defun mh-to-field ()
  "Move point to the end of a specified header field.
The field is indicated by the previous keystroke.  Create the field if
it does not exist.  Set the mark to point before moving."
  (interactive)
  (expand-abbrev)
  (let ((target (cdr (assoc (logior last-input-char ?`) mh-to-field-choices)))
	(case-fold-search t))
    (cond ((mh-position-on-field target t)
	   (let ((eol (point)))
	     (skip-chars-backward " \t")
	     (delete-region (point) eol))
	   (if (and (not (eq (logior last-input-char ?`) ?s))
		    (save-excursion
		      (backward-char 1)
		      (not (looking-at "[:,]"))))
	       (insert ", ")
	       (insert " ")))
	  (t
	   (goto-char (dot-min))
	   (re-search-forward "^To:")
	   (forward-line 1)
	   (while (looking-at "^[ \t]") (forward-line 1))
	   (insert (format "%s \n" target))
	   (backward-char 1)))))


(defun mh-to-fcc ()
  "Insert an Fcc: field in the current message.
Prompt for the field name with a completion list of the current folders."
  (interactive)
  (let ((last-input-char ?\C-f)
        (folder (mh-prompt-for-folder "Fcc" "" t)))
    (expand-abbrev)
    (save-excursion
      (mh-to-field)
      (insert (substring folder 1 nil)))))


(defun mh-insert-signature ()
  "Insert the file ~/.signature at the current point."
  (interactive)
  (insert-file-contents "~/.signature")
  (set-buffer-modified-p (buffer-modified-p))) ; force mode line update


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

(defvar mh-searching-folder nil "Folder this pick is searching.")


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
  "Find messages that match the qualifications in the current pattern buffer.
Messages are searched for in the folder named in mh-searching-folder.
Put messages found in a sequence named `search'."
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
    (mh-notate-seq 'search ?% (1+ mh-cmd-note))))


(defun mh-next-pick-field (buffer)
  ;; Return the next piece of a pick argument that can be extracted from the
  ;; BUFFER.  Returns nil if no pieces remain.
  (set-buffer buffer)
  (let ((case-fold-search t))
    (cond ((eobp)
	   nil)
	  ((re-search-forward "^\\([a-z].*\\):[ \t]*\\([a-z0-9].*\\)$" nil t)
	   (let* ((component
		   (format "--%s"
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
  ;; The TO, SUBJECT, and CC fields are passed to the
  ;; mh-compose-letter-function.
  ;; If ANNOTATE-CHAR is non-null, it is used to notate the scan listing of the
  ;; message.  In that case, the ANNOTATE-FIELD is used to build a string
  ;; for mh-annotate-msg.
  ;; CONFIG is the window configuration to restore after sending the letter.
  (pop-to-buffer draft)
  (mh-letter-mode)
  (setq mh-sent-from-folder sent-from-folder)
  (setq mh-sent-from-msg sent-from-msg)
  (setq mh-send-args send-args)
  (setq mh-annotate-char annotate-char)
  (setq mh-annotate-field annotate-field)
  (setq mh-previous-window-config config)
  (setq mode-line-buffer-identification (list "{%b}"))
  (if (and (boundp 'mh-compose-letter-function)
	   (symbol-value 'mh-compose-letter-function))
      ;; run-hooks will not pass arguments.
      (let ((value (symbol-value 'mh-compose-letter-function)))
	(if (and (listp value) (not (eq (car value) 'lambda)))
	    (while value
	      (funcall (car value) to subject cc)
	      (setq value (cdr value)))
	    (funcall mh-compose-letter-function to subject cc)))))


(defun mh-send-letter (&optional arg)
  "Send the draft letter in the current buffer.
If optional prefix argument is provided, monitor delivery.
Run mh-before-send-letter-hook before doing anything."
  (interactive "P")
  (run-hooks 'mh-before-send-letter-hook)
  (set-buffer-modified-p t)		; Make sure buffer is written
  (save-buffer)
  (message "Sending...")
  (let ((draft-buffer (current-buffer))
	(file-name (buffer-file-name))
	(config mh-previous-window-config))
    (cond (arg
	   (pop-to-buffer "MH mail delivery")
	   (erase-buffer)
	   (if mh-send-args
	       (mh-exec-cmd-output "send" t "-watch" "-nopush"
				   "-nodraftfolder" mh-send-args file-name)
	       (mh-exec-cmd-output "send" t "-watch" "-nopush"
				   "-nodraftfolder" file-name))
	   (goto-char (point-max))	; show the interesting part
	   (recenter -1)
	   (set-buffer draft-buffer))	; for annotation below
	  (mh-send-args
	   (mh-exec-cmd-daemon "send" "-nodraftfolder" "-noverbose"
			       mh-send-args file-name))
	  (t
	   (mh-exec-cmd-daemon "send" "-nodraftfolder" "-noverbose"
			       file-name)))

    (if mh-annotate-char
	(mh-annotate-msg mh-sent-from-msg
			 mh-sent-from-folder
			 mh-annotate-char
			 "-component" mh-annotate-field
			 "-text" (format "\"%s %s\""
					 (mh-get-field "To:")
					 (mh-get-field "Cc:"))))

    (mh-when (or (not arg)
		 (y-or-n-p "Kill draft buffer? "))
      (kill-buffer draft-buffer)
      (if config
	  (set-window-configuration config)))
    (message "Sending...done")))


(defun mh-insert-letter (prefix-provided folder msg)
  "Insert a message from any folder into the current letter.
Removes the message's headers using mh-invisible-headers.
Prefixes each non-blank line with mh-ins-buf-prefix (default \">> \").
If optional prefix argument provided, do not indent and do not delete
headers.  Leaves the mark before the letter and point after it."
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
      (if (equal msg "") (setq msg (int-to-string mh-sent-from-msg)))
      (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
			      (expand-file-name msg
						(mh-expand-file-name folder)))
      (mh-when (not prefix-provided)
	(mh-clean-msg-header start mh-invisible-headers mh-visible-headers)
	(set-mark start)		; since mh-clean-msg-header moves it
	(mh-insert-prefix-string mh-ins-buf-prefix)))))


(defun mh-yank-cur-msg ()
  "Insert the current message into the draft buffer.
Prefix each non-blank line in the message with the string in
`mh-ins-buf-prefix'.  If a region is set in the message's buffer, then
only the region will be inserted.  Otherwise, the entire message will
be inserted if `mh-yank-from-start-of-msg' is non-nil.  If this variable
is nil, the portion of the message following the point will be yanked.
If `mh-delete-yanked-msg-window' is non-nil, any window displaying the
yanked message will be deleted."
  (interactive)
  (if (and mh-sent-from-folder mh-sent-from-msg)
      (let ((to-point (point))
	    (to-buffer (current-buffer)))
	(set-buffer mh-sent-from-folder)
	(if mh-delete-yanked-msg-window
	    (delete-windows-on mh-show-buffer))
	(set-buffer mh-show-buffer)	; Find displayed message
	(let ((mh-ins-str (cond ((mark)
				 (buffer-substring (region-beginning)
						   (region-end)))
				((eq 'body mh-yank-from-start-of-msg)
				 (buffer-substring
				  (save-excursion
				    (goto-char (point-min))
				    (mh-goto-header-end 1)
				    (point))
				  (point-max)))
				(mh-yank-from-start-of-msg
				 (buffer-substring (point-min) (point-max)))
				(t
				 (buffer-substring (point) (point-max))))))
	  (set-buffer to-buffer)
	  (narrow-to-region to-point to-point)
	  (push-mark)
	  (insert mh-ins-str)
	  (mh-insert-prefix-string mh-ins-buf-prefix)
	  (insert "\n")
	  (widen)))
      (error "There is no current message")))


(defun mh-insert-prefix-string (mh-ins-string)
  ;; Run MH-YANK-HOOK to insert a prefix string before each line in the buffer.
  ;; Generality for supercite users.
  (save-excursion
    (set-mark (point-max))
    (goto-char (point-min))
    (run-hooks 'mh-yank-hooks)))


(defun mh-fully-kill-draft ()
  "Kill the draft message file and the draft message buffer.
Use \\[kill-buffer] if you don't want to delete the draft message file."
  (interactive)
  (if (y-or-n-p "Kill draft message? ")
      (let ((config mh-previous-window-config))
	(if (file-exists-p (buffer-file-name))
	    (delete-file (buffer-file-name)))
	(set-buffer-modified-p nil)
	(kill-buffer (buffer-name))
	(message "")
	(if config
	    (set-window-configuration config)))
    (error "Message not killed")))


(defun mh-recenter (arg)
  ;; Like recenter but with two improvements: nil arg means recenter,
  ;; and only does anything if the current buffer is in the selected
  ;; window.  (Commands like save-some-buffers can make this false.)
  (if (eql (get-buffer-window (current-buffer))
	   (selected-window))
      (recenter (if arg arg '(t)))))



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


(defun mh-read-seq-default (prompt not-empty)
  ;; Read and return sequence name with default narrowed or previous sequence.
  (mh-read-seq prompt not-empty (or mh-narrowed-to-seq mh-previous-seq)))


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
    (mh-when define-sequences
      (mh-define-sequences mh-seq-list)
      (mh-mapc (function (lambda (seq)	; Save the internal sequences
			   (if (mh-folder-name-p (mh-seq-name seq))
			       (mh-push seq seqs))))
	       mh-seq-list))
    (save-excursion
      (mh-exec-cmd-quiet " *mh-temp*" "mark" folder "-list")
      (goto-char (point-min))
      ;; look for name in line of form "cur: 4" or "myseq (private): 23"
      (while (re-search-forward "^[^: ]+" nil t)
	(mh-push (mh-make-seq (intern (buffer-substring (match-beginning 0)
							(match-end 0)))
			      (mh-read-msg-list))
		 seqs))
      (delete-region (point-min) (point))) ; avoid race with mh-process-daemon
    seqs))


(defun mh-seq-names (seq-list)
  ;; Return an alist containing the names of the SEQUENCES.
  (mapcar (function (lambda (entry) (list (symbol-name (mh-seq-name entry)))))
	  seq-list))


(defun mh-seq-from-command (folder seq seq-command)
  ;; In FOLDER, make a sequence named SEQ by executing COMMAND.
  ;; COMMAND is a list.  The first element is a program name
  ;; and the subsequent elements are its arguments, all strings.
  (let ((msg)
	(msgs ())
	(case-fold-search t))
    (save-excursion
      (save-window-excursion
	(apply 'mh-exec-cmd-quiet " *mh-temp*" seq-command)
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
	(end-of-line (save-excursion (end-of-line) (point)))
	num)
    (while (re-search-forward "[0-9]+" end-of-line t)
      (setq num (string-to-int (buffer-substring (match-beginning 0)
						 (match-end 0))))
      (cond ((looking-at "-")		; Message range
	     (forward-char 1)
	     (re-search-forward "[0-9]+" end-of-line t)
	     (let ((num2 (string-to-int (buffer-substring (match-beginning 0)
							  (match-end 0)))))
	       (if (< num2 num)
		   (error "Bad message range: %d-%d" num num2))
	       (while (<= num num2)
		 (mh-push num msgs)
		 (setq num (1+ num)))))
	    ((not (zerop num)) (mh-push num msgs))))
    msgs))


(defun mh-remove-seq (seq)
  ;; Delete the SEQUENCE.
  (mh-map-to-seq-msgs 'mh-notate-if-in-one-seq seq ?  (1+ mh-cmd-note) seq)
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
    (mh-when entry
      (mh-notate-if-in-one-seq msg ?  (1+ mh-cmd-note) (mh-seq-name entry))
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
    (mh-when (not internal-flag)
      (mh-add-to-sequence seq msgs)
      (mh-notate-seq seq ?% (1+ mh-cmd-note)))))


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
  (let ((seqs mh-seq-list)
	name)
    (while seqs
      (setq name (mh-seq-name (car seqs)))
      (if (not (mh-internal-seq name))
	  (mh-notate-seq name ?% (1+ mh-cmd-note)))
      (setq seqs (cdr seqs)))))


(defun mh-internal-seq (name)
  ;; Return non-NIL if NAME is the name of an internal mh-e sequence.
  (or (memq name '(answered cur deleted forwarded printed))
      (eq name mh-unseen-seq)
      (mh-folder-name-p name)))


(defun mh-folder-name-p (name)
  ;; Return non-NIL if NAME is possibly the name of a folder.
  ;; A name (a string or symbol) can be a folder name if it begins with "+".
  (if (symbolp name)
      (eql (aref (symbol-name name) 0) ?+)
      (eql (aref name 0) ?+)))


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
	    (apply func (car msgs) args))
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
  (if (not (mh-folder-name-p seq))
      (if msgs
	  (apply 'mh-exec-cmd "mark" mh-current-folder
		 "-sequence" (symbol-name seq)
		 "-add" msgs))))


(defun mh-define-sequence (seq msgs)
  ;; Define the SEQUENCE to contain the list of MSGS.  Do not mark
  ;; pseudo-sequences or empty sequences.
  (if (and msgs
	   (not (mh-folder-name-p seq)))
      (save-excursion
	(apply 'mh-exec-cmd "mark" mh-current-folder
	       "-sequence" (symbol-name seq)
	       "-add" "-zero" (mh-list-to-string msgs)))))


(defun mh-undefine-sequence (seq msgs)
  ;; Remove from the SEQUENCE the list of MSGS.
  (apply 'mh-exec-cmd "mark" mh-current-folder
	 "-sequence" (symbol-name seq)
	 "-delete" msgs))


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
  ;; Execute MH command COMMAND with ARGS.
  ;; Any output is assumed to be an error and is shown to the user.
  (save-excursion
    (set-buffer " *mh-temp*")
    (erase-buffer)
    (apply 'call-process
	   (expand-file-name command mh-progs) nil t nil
	   (mh-list-to-string args))
    (if (> (buffer-size) 0)
	(save-window-excursion
	  (switch-to-buffer-other-window " *mh-temp*")
	  (sit-for 5)))))


(defun mh-exec-cmd-quiet (buffer command &rest args)
  ;; In BUFFER, execute MH command COMMAND with ARGS.
  ;; ARGS is a list of strings.  Return in BUFFER, if one exists.
  (mh-when (stringp buffer)
    (set-buffer buffer)
    (erase-buffer))
  (apply 'call-process
	 (expand-file-name command mh-progs) nil buffer nil
	 args))


(defun mh-exec-cmd-output (command display &rest args)
  ;; Execute MH command COMMAND with DISPLAY flag and ARGS putting the output
  ;; into buffer after point.  Set mark after inserted text.
  (push-mark (point) t)
  (apply 'call-process
	 (expand-file-name command mh-progs) nil t display
	 (mh-list-to-string args))
  (exchange-point-and-mark))


(defun mh-exec-cmd-daemon (command &rest args)
  ;; Execute MH command COMMAND with ARGS.  Any output from command is
  ;; displayed in an asynchronous pop-up window.
  (save-excursion
    (set-buffer (get-buffer-create " *mh-temp*"))
    (erase-buffer))
  (let* ((process-connection-type nil)
	 (process (apply 'start-process
			 command nil
			 (expand-file-name command mh-progs)
			 (mh-list-to-string args))))
    (set-process-filter process 'mh-process-daemon)))


(defun mh-process-daemon (process output)
  ;; Process daemon that puts output into a temporary buffer.
  (set-buffer (get-buffer-create " *mh-temp*"))
  (insert-before-markers output)
  (display-buffer " *mh-temp*"))


(defun mh-exec-lib-cmd-output (command &rest args)
  ;; Execute MH library command COMMAND with ARGS.
  ;; Put the output into buffer after point.  Set mark after inserted text.
  (push-mark (point) t)
  (apply 'call-process
	 (expand-file-name command mh-lib) nil t nil
	 (mh-list-to-string args))
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
	    (t (error "Bad element in mh-list-to-string: %s" (car l))))
      (setq l (cdr l)))
    (nreverse new-list)))



;;; Commands to annotate a message.

(defun mh-annotate-msg (msg buffer note &rest args)
  ;; Mark the MESSAGE in BUFFER listing with the character NOTE and annotate
  ;; the saved message with ARGS.
  (apply 'mh-exec-cmd "anno" buffer msg args)
  (save-excursion
    (cond ((get-buffer buffer)		; Buffer may be deleted
	   (set-buffer buffer)
	   (if (symbolp msg)
	       (mh-notate-seq msg note (1+ mh-cmd-note))
	       (mh-notate msg note (1+ mh-cmd-note)))))))


(defun mh-notate (msg notation offset)
  ;; Marks MESSAGE with the character NOTATION at position OFFSET.
  ;; Null MESSAGE means the message that the cursor points to.
  (save-excursion
    (if (or (null msg)
	    (mh-goto-msg msg t t))
	(with-mh-folder-updating (t)
	  (beginning-of-line)
	  (forward-char offset)
	  (delete-char 1)
	  (insert notation)))))



;;; User prompting commands.

(defun mh-prompt-for-folder (prompt default can-create)
  ;; Prompt for a folder name with PROMPT.  Returns the folder's name as a
  ;; string.  DEFAULT is used if the folder exists and the user types return.
  ;; If the CAN-CREATE flag is t, then a non-existent folder is made.
  (let* ((prompt (format "%s folder%s" prompt
			 (if (equal "" default)
			     "? "
			     (format " [%s]? " default))))
	 name)
    (if (null mh-folder-list)
	(mh-set-folder-list))
    (while (and (setq name (completing-read prompt mh-folder-list
					    nil nil "+"))
		(equal name "")
		(equal default "")))
    (cond ((or (equal name "") (equal name "+"))
	   (setq name default))
	  ((not (mh-folder-name-p name))
	   (setq name (format "+%s" name))))
    (let ((new-file-p (not (file-exists-p (mh-expand-file-name name)))))
      (cond ((and new-file-p
		  (y-or-n-p
		   (format "Folder %s does not exist. Create it? " name)))
	     (message "Creating %s" name)
	     (call-process "mkdir" nil nil nil (mh-expand-file-name name))
	     (message "Creating %s...done" name)
	     (mh-push (list name) mh-folder-list))
	    (new-file-p
	     (error "Folder %s is not created" name))
	    (t
	     (mh-when (null (assoc name mh-folder-list))
	       (mh-push (list name) mh-folder-list)))))
    name))


(defun mh-set-folder-list ()
  "Sets mh-folder-list correctly.
A useful function for the command line or for when you need to sync by hand."
  (setq mh-folder-list (mh-make-folder-list)))


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
    (let ((list nil)
	  start)
      (while (not (eobp))
	(setq start (point))
	(forward-line 1)
	(mh-push (list (format "+%s" (buffer-substring start (1- (point)))))
		 list))
      (message "Collecting folder names...done")
      list)))


(defun mh-remove-folder-from-folder-list (folder)
  ;; Remove FOLDER from the list of folders.
  (setq mh-folder-list
	(delq (assoc folder mh-folder-list) mh-folder-list)))


(defun mh-read-msg-range (prompt)
  ;; Read a list of blank-separated items.
  (let* ((buf (read-string prompt))
	 (buf-size (length buf))
	 (start 0)
	 (input ()))
    (while (< start buf-size)
      (let ((next (read-from-string buf start buf-size)))
	(mh-push (car next) input)
	(setq start (cdr next))))
    (nreverse input)))



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


(defun mh-msg-filename (msg &optional folder)
  ;; Return the file name of MESSAGE in FOLDER (default current folder).
  (expand-file-name (int-to-string msg)
		    (if folder
			(mh-expand-file-name folder)
			mh-folder-filename)))


(defun mh-msg-filenames (msgs &optional folder)
  ;; Return a list of file names for MSGS in FOLDER (default current folder).
  (mapconcat (function (lambda (msg) (mh-msg-filename msg folder))) msgs " "))


(defun mh-expand-file-name (filename &optional default)
  "Just like `expand-file-name', but also handles MH folder names.
Assumes that any filename that starts with '+' is a folder name."
   (if (mh-folder-name-p filename)
       (expand-file-name (substring filename 1) mh-user-path)
     (expand-file-name filename default)))


(defun mh-find-path ()
  ;; Set mh-user-path, mh-draft-folder, and mh-unseen-seq from profile file.
  (save-excursion
    ;; Be sure profile is fully expanded before switching buffers
    (let ((profile (expand-file-name (or (getenv "MH") "~/.mh_profile"))))
      (if (not (file-exists-p profile))
	  (error "Cannot find MH profile %s" profile))
      (set-buffer (get-buffer-create " *mh-temp*"))
      (erase-buffer)
      (insert-file-contents profile)
      (setq mh-draft-folder (mh-get-field "Draft-Folder:"))
      (cond ((equal mh-draft-folder "")
	     (setq mh-draft-folder nil))
	    ((not (mh-folder-name-p mh-draft-folder))
	     (setq mh-draft-folder (format "+%s" mh-draft-folder))))
      (setq mh-user-path (mh-get-field "Path:"))
      (if (equal mh-user-path "")
	  (setq mh-user-path "Mail"))
      (setq mh-user-path
	    (file-name-as-directory
	     (expand-file-name mh-user-path (expand-file-name "~"))))
      (if (and mh-draft-folder
	       (not (file-exists-p (mh-expand-file-name mh-draft-folder))))
	  (error "Draft folder %s does not exist.  Create it and try again."
		 mh-draft-folder))
      (setq mh-unseen-seq (mh-get-field "Unseen-Sequence:"))
      (if (equal mh-unseen-seq "")
	  (setq mh-unseen-seq 'unseen)
	  (setq mh-unseen-seq (intern mh-unseen-seq))))))


(defun mh-get-field (field)
  ;; Find and return the value of field FIELD in the current buffer.
  ;; Returns the empty string if the field is not in the message.
  (let ((case-fold-search t))
    (goto-char (point-min))
    (cond ((not (re-search-forward (format "^%s" field) nil t)) "")
	  ((looking-at "[\t ]*$") "")
	  (t
	   (re-search-forward "[\t ]*\\([^\t \n].*\\)$" nil t)
	   (let ((start (match-beginning 1)))
	     (forward-line 1)
	     (while (looking-at "[ \t]")
	       (forward-line 1))
	     (buffer-substring start (1- (point))))))))


(defun mh-insert-fields (&rest name-values)
  ;; Insert the NAME-VALUE pairs in the current buffer.
  ;; Do not insert any pairs whose value is the empty string.
  (let ((case-fold-search t))
    (while name-values
      (let ((field-name (car name-values))
	    (value (car (cdr name-values))))
	(mh-when (not (equal value ""))
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
  ;; Returns non-nil iff the field was found.
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
(define-key mh-folder-mode-map "q" 'mh-quit)
(define-key mh-folder-mode-map "b" 'mh-quit)
(define-key mh-folder-mode-map "?" 'mh-msg-is-in-seq)
(define-key mh-folder-mode-map "%" 'mh-put-msg-in-seq)
(define-key mh-folder-mode-map "|" 'mh-pipe-msg)
(define-key mh-folder-mode-map "\ea" 'mh-edit-again)
(define-key mh-folder-mode-map "\e%" 'mh-delete-msg-from-seq)
(define-key mh-folder-mode-map "\C-xn" 'mh-narrow-to-seq)
(define-key mh-folder-mode-map "\C-xw" 'mh-widen)
(define-key mh-folder-mode-map "\eb" 'mh-burst-digest)
(define-key mh-folder-mode-map "\eu" 'mh-undo-folder)
(define-key mh-folder-mode-map "\e " 'mh-page-digest)
(define-key mh-folder-mode-map "\e\177" 'mh-page-digest-backwards)
(define-key mh-folder-mode-map "\ee" 'mh-extract-rejected-mail)
(define-key mh-folder-mode-map "\ef" 'mh-visit-folder)
(define-key mh-folder-mode-map "\ek" 'mh-kill-folder)
(define-key mh-folder-mode-map "\el" 'mh-list-folders)
(define-key mh-folder-mode-map "\eo" 'mh-write-msg-to-file)
(define-key mh-folder-mode-map "\ep" 'mh-pack-folder)
(define-key mh-folder-mode-map "\es" 'mh-search-folder)
(define-key mh-folder-mode-map "\er" 'mh-rescan-folder)
(define-key mh-folder-mode-map "l" 'mh-print-msg)
(define-key mh-folder-mode-map "t" 'mh-toggle-showing)
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
(define-key mh-folder-mode-map "<" 'mh-first-msg)
(define-key mh-folder-mode-map "g" 'mh-goto-msg)
(define-key mh-folder-mode-map "\177" 'mh-previous-page)
(define-key mh-folder-mode-map " " 'mh-page-msg)
(define-key mh-folder-mode-map "." 'mh-show)
(define-key mh-folder-mode-map "u" 'mh-undo)
(define-key mh-folder-mode-map "!" 'mh-refile-or-write-again)
(define-key mh-folder-mode-map "^" 'mh-refile-msg)
(define-key mh-folder-mode-map "d" 'mh-delete-msg)
(define-key mh-folder-mode-map "\C-d" 'mh-delete-msg-no-motion)
(define-key mh-folder-mode-map "p" 'mh-previous-undeleted-msg)
(define-key mh-folder-mode-map "n" 'mh-next-undeleted-msg)
(define-key mh-folder-mode-map "o" 'mh-refile-msg)


;;; Build the letter-mode keymap:

(define-key mh-letter-mode-map "\C-c\C-f\C-b" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-c" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-f" 'mh-to-fcc)
(define-key mh-letter-mode-map "\C-c\C-f\C-s" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-f\C-t" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-fb" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-fc" 'mh-to-field)
(define-key mh-letter-mode-map "\C-c\C-ff" 'mh-to-fcc)
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



;;; For Gnu Emacs.
;;; Local Variables: ***
;;; eval: (put 'mh-when 'lisp-indent-hook 1) ***
;;; eval: (put 'with-mh-folder-updating 'lisp-indent-hook 1) ***
;;; End: ***

