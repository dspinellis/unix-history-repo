;;;  mh-e.el	(Version: 3.3c for GNU Emacs Version 17 and MH.5 and MH.6)

;;;  Copyright (C) James Larus (larus@kim.berkeley.edu, ucbvax!larus), 1985
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
;;;  Rewritten for GNU Emacs, James Larus 1985.


;;;  NB MH must have been compiled with the MHE compiler flag or several
;;;  features necessary to this program will be missing.



;;; Constants:

;;; Set for local environment:
;;;* These are now in paths.el.
;;;(defvar mh-progs "/usr/new/mh/"     "Directory containing MH commands")
;;;(defvar mh-lib   "/usr/new/lib/mh/" "Directory of MH library")


;;; Mode hooks:

(defvar mh-folder-mode-hook nil
  "*Invoked in mh-folder-mode on a new folder.")
(defvar mh-letter-mode-hook nil
  "*Invoked in mh-letter-mode on a new letter.")
(defvar mh-compose-letter-hook nil
  "*Invoked in mh-compose-and-send-mail on an outgoing letter.  It is passed
three arguments: TO recipients, SUBJECT, and CC recipients.")


;;; Personal preferences:

(defvar mh-auto-fill-letters t
  "*Non-nil means invoke auto-fill-mode in draft messages.")
(defvar mh-clean-message-header nil
  "*Non-nil means remove invisible header lines in messages.")
(defvar mh-use-mhl nil
  "*Non-nil means use mhl to format messages.")
(defvar mh-lpr-command-format "lpr -p -J '%s'"
  "*Format for Unix command line to print a message. The format should be
a unix command line, with the string \"%s\" where the folder and message
number should appear.")
(defvar mh-summary-height 4
  "*Number of lines in summary window.")
(defvar mh-ins-buf-prefix ">> "
  "*String to put before each non-blank line of the the current message
as it is inserted in an outgoing letter.")


;;; Real constants:

(defvar mh-cmd-note 4		       "Offset to insert notation")
(defvar mh-invisible-headers
  "^Received: \\|^Message-Id: \\|^Remailed-\\|^Via: \\|^Mail-from: \\|\^Return-Path: \\|^In-Reply-To: \\|^Resent-"
  "Regexp specifying headers that are not to be shown.")
(defvar mh-rejected-letter-start "^   ----- Unsent message follows -----$"
  "Regexp specifying the beginning of the wrapper around a letter returned
by the mail system.")
(defvar mh-good-msg-regexp  "^....[^D^]"
  "Regexp specifiying the scan lines that are 'good' messages")

;;; Global variables:

(defvar mh-user-path  ""	     "User's mail folder.")
(defvar mh-last-destination nil	     "Destination of last "move" command.")
(defvar mh-folder-mode-map (make-sparse-keymap) "Keymap for MH folders.")
(defvar mh-letter-mode-map (make-sparse-keymap) "Keymap for composing mail.")
(defvar mh-pick-mode-map (make-sparse-keymap) "Keymap for searching folder.")
(defvar mh-folder-list nil	     "List of folder names for completion.")

;;; Macros and generic functions:

(defmacro push (v l)
  (list 'setq l (list 'cons v l)))

(defmacro caar (l)
  (list 'car (list 'car l)))

(defmacro cadr (l)
  (list 'car (list 'cdr l)))

(defmacro cdar (l)
  (list 'cdr (list 'car l)))

(defmacro cddr (l)
  (list 'cdr (list 'cdr l)))

(defmacro when (pred &rest body)
  (list 'cond (cons pred body)))

(defun mapc (func list)
  (while list
    (funcall func (car list))
    (setq list (cdr list))))



;;; Entry points:

(defun mh-rmail (&optional arg)
  "Inc(orporate) new mail if optional ARG omitted, or scan a MH mail box
if arg is present.  This front end uses the MH mail system, which uses
different conventions from the usual mail system."
  (interactive "P")
  (mh-find-path)
  (if (null mh-folder-list)
      (setq mh-folder-list (mh-make-folder-list)))
  (cond (arg
	 (let ((folder (mh-prompt-for-folder "mh" "+inbox" t))
	       (range (read-string "Range [all]? ")))
	   (mh-scan-folder folder (if (equal range "") "all" range))
	   (delete-other-windows)))
	(t
	 (mh-inc-folder))))


(defun mh-smail ()
  "Send mail using the MH mail system."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send))



;;; User executable mh-e commands:


(defun mh-answer (&optional arg)
  "Answer a letter.  If given a non-nil ARGUMENT, then include the current
message in the reply."
  (interactive "P")
  (let ((msg-filename (mh-msg-filename))
	(msg (mh-get-msg-num t))
	(minibuffer-help-form
"from => Sender\n  to => Sender and primary recipients\n  cc => Sender and all recipients")
	(folder mh-current-folder)
	(show-buffer mh-show-buffer))
    (let ((reply-to (completing-read
		     "Reply to whom: " '(("from") ("to") ("cc")) nil t)))
    (message "Composing a reply...")
    (cond ((or (equal reply-to "from") (equal reply-to ""))
	   (apply 'mh-exec-cmd
		  (nconc
		   (list "repl" "-build" mh-current-folder msg "-nocc" "all")
		   (if arg (list "-filter" "mhl.reply")))))
	  ((equal reply-to "to")
	   (apply 'mh-exec-cmd
		  (nconc
		   (list "repl" "-build" mh-current-folder msg "-cc" "to")
		   (if arg (list "-filter" "mhl.reply")))))
	  ((equal reply-to "cc")
	   (apply 'mh-exec-cmd
		  (nconc
		   (list "repl" "-build" mh-current-folder msg "-cc" "all")
		   (if arg (list "-filter" "mhl.reply"))))))

    (mh-read-draft)
    (delete-other-windows)
    (when (or (zerop (buffer-size))
	      (not (y-or-n-p "The file 'draft' exists.  Use for reply? ")))
      (erase-buffer)
      (insert-file-contents (format "%sreply" mh-user-path))
      (delete-file (format "%sreply" mh-user-path)))
    (set-buffer-modified-p nil)

    (let ((to (mh-get-field "To:"))
	  (subject (mh-get-field "Subject:"))
	  (cc (mh-get-field "Cc:")))
      (goto-char (point-min))
      (re-search-forward "^$" (point-max) nil)
      (if (not arg)
	(mh-display-msg msg msg-filename show-buffer))
      (mh-add-msg-to-seq msg "answered" t)
      (message "Composing a reply...done")
      (mh-compose-and-send-mail "" folder to subject cc "-" "Replied:")))))


(defun my-apply-command-to-seq (command)
  "Applies the next command to all messages in a sequence to be prompted for."
  (interactive "k")
  (funcall (key-binding command) (mh-read-seq "Apply to" mh-narrowed-to-seq)))


(defun mh-copy-msg (&optional seq)
  "Copy specified message(s) to another folder without deleting them.
Optional argument is a SEQUENCE name to copy."
  (interactive "P")
  (let ((msgs (if seq seq (mh-get-msg-num t))))
    (mh-exec-cmd "refile" msgs "-link" "-src" mh-current-folder
		 (mh-prompt-for-folder "Copy to" "" t))
    (if seq
	(mh-notate-seq msgs ?C mh-cmd-note)
	(mh-notate ?C mh-cmd-note))))


(defun mh-delete-msg (&optional seq)
  "Marks the specified message(s) for later deletion.  Optional argument is a
SEQUENCE name to  delete."
  (interactive "P")
  (if seq
      (mh-map-to-seq-msgs 'mh-delete-a-msg seq)
      (mh-delete-a-msg))
  (mh-next-msg))


(defun mh-delete-msg-from-seq (&optional arg)
  "Deletes a message from a sequence or, if optional ARG is non-nil, deletes
the sequence."
  (interactive "P")
  (if arg
      (mh-remove-seq (mh-read-seq "Delete"))
      (mh-remove-msg-from-seq (mh-get-msg-num t) (mh-read-seq "Delete from")))
  (mh-next-msg))


(defun mh-execute-commands ()
  "Process outstanding delete and move commands."
  (interactive)
  (if mh-narrowed-to-seq (mh-widen))
  (save-excursion
    (mh-process-commands mh-current-folder))
  (delete-other-windows)
  (setq mh-summarize t)
  (setq mode-name "Mh-Summary")
  (setq mode-line-format (mh-make-mode-line)))


(defun mh-extract-rejected-mail ()
  "Extract a letter returned by the mail system and make it resendable."
  (interactive "")
  (let ((msg-filename (format "%s%d" mh-folder-filename (mh-get-msg-num t))))
    (mh-read-draft)
    (when (or (zerop (buffer-size))
	      (not (y-or-n-p "The file 'draft' exists.  Use it? ")))
      (erase-buffer)
      (insert-file-contents msg-filename))
    (goto-char (point-min))
    (re-search-forward mh-rejected-letter-start)
    (forward-char 1)
    (kill-region (point-min) (point))
    (let ((mh-invisible-headers "^Date:\\|^Received:\\|^Message-Id:\\|^From:"))
      (mh-clean-msg-header (point-min)))
    (goto-char (point-min))
    (switch-to-buffer mh-current-folder)
    (mh-compose-and-send-mail "" mh-current-folder (mh-get-field "To")
			      (mh-get-field "From") (mh-get-field "cc"))))


(defun mh-forward (&optional seq)
  "Forward a letter.  Optional argument is a SEQUENCE of messages to forward."
  (interactive "P")
  (let ((to (read-string "To: "))
	(cc (read-string "Cc: "))
	(msg-filename (mh-msg-filename))
	(msg (if seq seq (mh-get-msg-num t)))
	(folder mh-current-folder))
    (cond ((or (not (file-exists-p (format "%sdraft" mh-user-path)))
	       (y-or-n-p "The file 'draft' exists.  Discard it? "))
	   (mh-exec-cmd "forw" "-build" mh-current-folder msg)
	   (mh-read-draft)
	   (mh-insert-fields "To:" to "Cc:" cc)
	   (set-buffer-modified-p nil))
	  (t
	   (mh-read-draft)))
    (goto-char (point-min))
    (re-search-forward "^------- Forwarded Message")
    (previous-line 1)
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
      (if seq
	  (mh-add-msg-list-to-seq (mh-seq-to-msgs seq) "forwarded" t)
	  (mh-add-msg-to-seq msg "forwarded" t))
      (mh-compose-and-send-mail "" folder to subject cc "F" "Forwarded:"))))


(defun mh-goto-msg (number &optional no-error-if-no-message)
  "Position the cursor at message NUMBER.  Do not signal an error if optional
ARG is t.  Return non-nil if cursor is at message."
  (interactive "nMessage number? ")
  (let ((starting-place (point)))
    (goto-char (point-min))
    (cond ((not (re-search-forward (mh-msg-search-pat number) nil t))
	   (goto-char starting-place)
	   (if (not no-error-if-no-message) (error "No message %d " number))
	   nil)
	  (t
	   (beginning-of-line)
	   (mh-maybe-show)
	   t))))


(defun mh-inc-folder ()
  "inc(orporate) new mail into inbox."
  (interactive)
  (pop-to-buffer "+inbox")
  (if (or (not (boundp 'mh-current-folder)) (null mh-current-folder))
      (mh-make-folder "+inbox"))
  (if (mh-get-new-mail)
      (mh-show)))


(defun mh-kill-folder ()
  "Removes the current folder."
  (interactive)
  (if (yes-or-no-p (format "Remove folder %s? " mh-current-folder))
      (let ((folder mh-current-folder))
	(switch-to-buffer-other-window " *mh-temp*")
	(mh-exec-cmd "rmf" "-nointeractive" folder)
	(mh-remove-folder-from-folder-list folder)
	(message "Folder removed")
	(kill-buffer folder))
      (message "Folder not removed")))


(defun mh-list-folders ()
  "List mail folders."
  (interactive)
  (message "listing folders...")
  (switch-to-buffer-other-window " *mh-temp*")
  (erase-buffer)
  (mh-exec-cmd-output "folders")
  (goto-char (point-min))
  (message "listing folders...done"))


(defun mh-msg-is-in-seq ()
  "Displays the sequences that the current messages is in."
  (interactive)
  (let ((msg (mh-get-msg-num t))
	(l mh-seq-list)
	(seqs ""))
      (while l
	(if (memq msg (cdar l))
	    (setq seqs (format "%s %s" (symbol-name (caar l)) seqs)))
	(setq l (cdr l)))
      (message "Message %d is in sequences: %s" msg seqs)))


(defun mh-move-msg (&optional seq)
  "Move specified message(s) to another folder.  Optional argument is a
SEQUENCE of messages to refile."
  (interactive "P")
  (setq mh-last-destination
	(cons 'move (intern (mh-prompt-for-folder "Destination" "" t))))
  (if seq
      (mh-map-to-seq-msgs 'mh-move-a-msg seq (cdr mh-last-destination))
      (mh-move-a-msg (cdr mh-last-destination)))
  (mh-next-msg))


(defun mh-move-or-write-again ()
  "Move or write the current message to same folder or file as the last move
or write."
  (interactive)
  (if (null mh-last-destination)
      (error "No previous move"))
  (cond ((eq (car mh-last-destination) 'move)
	 (mh-move-a-msg (cdr mh-last-destination))
	 (message "Destination folder: %s" (cdr mh-last-destination)))
	(t
	 (mh-write-msg-to-file (cdr mh-last-destination))
	 (message "Destination: %s" (cdr mh-last-destination))))
  (sit-for 3)
  (mh-next-msg))


(defun mh-narrow-to-seq ()
  "Restrict the display of the current folder to the messages in the sequence
to be prompted for.  Use \\[mh-widen] to undo this command."
  (interactive "")
  (let ((seq (mh-read-seq "Narrow to"))
	(eob (point-max))
	(buffer-read-only nil))
    (cond ((mh-seq-to-msgs seq)
	   (mh-copy-seq-to-point seq eob)
	   (narrow-to-region eob (point-max))
	   (setq mode-line-format (mh-make-mode-line (symbol-name seq)))
	   (recenter)
	   (setq mh-narrowed-to-seq seq))
	  (t
	   (error "No messages in sequence `%s'" (symbol-name seq))))))


(defun mh-next-line (&optional arg)
  "Move to next undeleted message in window and display body if summary
flag set."
  (interactive "p")
  (forward-line (if arg arg 1))
  (setq mh-next-direction 'forward)
  (cond ((re-search-forward mh-good-msg-regexp nil 0 arg)
	 (beginning-of-line)
	 (mh-maybe-show))
	(t
	 (forward-line -1)
	 (sit-for 2)
	 (message "No more messages"))))


(defun mh-renumber-folder ()
  "Renumber messages in folder to be 1..N."
  (interactive)
  (message "packing buffer...")
  (mh-pack-folder)
  (mh-unmark-all-headers nil)
  (mh-goto-cur-msg)
  (message "packing buffer...done"))


(defun mh-page-digest ()
  "Advance displayed message to next digested message."
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window mh-show-buffer)
    (move-to-window-line nil)
    (let ((case-fold-search nil))
      (when (not (search-forward "\nFrom:" nil t))
	(other-window -1)
	(error "No more messages")))
    (recenter 0)
    (other-window -1)))


(defun mh-page-msg (&optional arg)
  (interactive "P")
  (scroll-other-window arg))


(defun mh-previous-line (&optional arg)
  "Move to previous message in window and display body if summary flag set."
  (interactive "p")
  (setq mh-next-direction 'backward)
  (if (not (re-search-backward mh-good-msg-regexp nil 0 arg))
      (message "Beginning of messages")
      (mh-maybe-show)))


(defun mh-previous-page ()
  "Page the displayed message backwards."
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window mh-show-buffer)
    (unwind-protect
	(scroll-down nil)
      (other-window -1))))


(defun mh-print-msg (&optional seq)
  "Print specified message(s) on a line printer.  Optional argument is a
SEQUENCE of messages to print."
  (interactive "P")
  (let ((msgs (if seq
		  (reverse (mh-seq-to-msgs seq))
		  (list (mh-get-msg-num t)))))
    (message "printing message...")
    (call-process shell-file-name nil 0 nil "-c"
		  (if seq
		      (format "(scan -clear %s ; %smhl -nobell -clear %s) | %s"
			      (mapconcat (function (lambda (msg) msg)) msgs " ")
			      mh-lib
			      (mh-msg-filenames msgs mh-folder-filename)
			      (format mh-lpr-command-format
				      (if seq
					  "Mail"
					  (format "%s/%d" mh-current-folder
						  (mh-get-msg-num t)))))
		      (format "%smhl -nobell -clear %s | %s"
			      mh-lib
			      (mh-msg-filenames msgs mh-folder-filename)
			      (format mh-lpr-command-format
				      (if seq
					  "Mail"
					  (format "%s/%d" mh-current-folder
						  (mh-get-msg-num t)))))))
    (if seq
	(mh-notate-seq msgs ?P mh-cmd-note)
	(mh-notate ?P mh-cmd-note))
    (mh-add-msg-list-to-seq msgs 'printed t)
    (message "printing message...done")))


(defun mh-put-msg-in-seq (&optional arg)
  "Add a message to a sequence or, if optional ARG is non-nil, add the
messages from a sequence to another sequence."
  (interactive "P")
  (if arg
      (mh-add-msg-list-to-seq (mh-seq-to-msgs
			       (mh-read-seq "Add messages from"))
			      (mh-read-seq "to"))
      (mh-add-msg-to-seq (mh-get-msg-num t) (mh-read-seq "Add to")))
  (mh-next-msg))


(defun mh-rescan-folder (&optional arg)
  "Rescan a folder after optionally processing the outstanding commands.  If
the optional argument is non-nil, then prompt for the range of messages to
display, otherwise assume the whole buffer."
  (interactive "P")
  (if (and (or mh-delete-list mh-move-list)
	   (y-or-n-p "Process commands? "))
      (mh-process-commands mh-current-folder))
  (setq mh-next-direction 'forward)
  (mh-scan-folder mh-current-folder
		  (if arg (read-string "Range [all]? ") "all")))


(defun mh-redistribute (to cc)
  "Redistribute a letter."
  (interactive "sRedist-To: \nsRedist-Cc: ")
  (let ((msg-filename (mh-msg-filename))
	(msg (mh-get-msg-num t))
	(folder mh-current-folder))
    (save-window-excursion
      (mh-read-draft)
      (when (or (zerop (buffer-size))
		(not (y-or-n-p
		      "The file 'draft' exists.  Redistribute old version? ")))
	(erase-buffer)
	(insert-file-contents msg-filename))
      (re-search-forward "^$\\|^---")
      (insert "Resent-To: " to "\n")
      (if (not (equal cc ""))
	  (insert "Resent-cc: " cc "\n"))
      (let ((mh-invisible-headers "^Message-Id:\\|^Received:\\|Return-Path:"))
	(mh-clean-msg-header (point-min)))
      (save-buffer)
      (message "Redistributing...")
      (call-process "/bin/sh" nil 0 nil "-c"
       (format "mhdist=1 mhaltmsg=%s %s/send -push %s/draft"
	       msg-filename mh-progs mh-user-path))
      (mh-annotate-msg msg folder "R"
		       "-component" "Resent:"
		       "-text" (format "\"%s %s\"" to cc))
      (message "Redistributing...done"))))


(defun mh-write-msg-to-file (file)
  "Append the current message to the end of a file."
  (interactive "FSave message in file: ")
  (setq mh-last-destination (cons 'write file))
  (call-process shell-file-name nil 0 nil "-c"
		(format "cat %s >> %s " (mh-msg-filename) file)))


(defun mh-search-folder ()
  "Search the current folder for messages matching a pattern."
  (interactive)
  (let ((folder mh-current-folder))
    (switch-to-buffer-other-window "pick-pattern")
    (if (or (zerop (buffer-size))
	    (not (y-or-n-p "Reuse pattern? ")))
	(mh-make-pick-template)
	(message ""))
    (setq mh-searching-folder folder)))


(defun mh-send (to cc subject)
  "Compose and send a letter."
  (interactive "sTo: \nsCc: \nsSubject: ")
  (let ((folder (if (boundp 'mh-current-folder) mh-current-folder)))
    (message "Composing a message...")
    (mh-read-draft)
    (delete-other-windows)
    (when (or (zerop (buffer-size))
	      (not (y-or-n-p "The file 'draft' exists.  Use it? ")))
      (erase-buffer)
      (if (file-exists-p (format "%scomponents" mh-user-path))
	  (insert-file-contents (format "%scomponents" mh-user-path))
	  (if (file-exists-p (format "%scomponents" mh-lib))
	      (insert-file-contents (format "%scomponents" mh-lib))
	      (error "Can't find components")))
      (mh-insert-fields "To:" to "Subject:" subject "Cc:" cc)
      (set-buffer-modified-p nil)
      (goto-char (point-max))
      (message "Composing a message...done"))
    (mh-compose-and-send-mail "" folder to subject cc)))


(defun mh-show ()
  "Show message indicated by cursor in scan buffer."
  (interactive)
  (setq mh-summarize nil)
  (setq mode-name "Mh-Show")
  (let ((msg-num (mh-get-msg-num t))
	(folder mh-current-folder))
    (mh-display-msg msg-num (mh-msg-filename) mh-show-buffer)

    ;; These contortions are to force the summary line to be the top window.
    (switch-to-buffer-other-window folder)
    (delete-other-windows)
    (switch-to-buffer-other-window mh-show-buffer)
    (switch-to-buffer-other-window folder)
    (shrink-window (- (window-height) mh-summary-height))
    (recenter 1)
    (push msg-num mh-seen-list)))


(defun mh-sort-folder ()
  "Sort the messages in the current folder by date."
  (interactive "")
  (mh-process-commands mh-current-folder)
  (setq mh-next-direction 'forward)
  (message "sorting folder...")
  (mh-exec-cmd "sortm" mh-current-folder)
  (message "sorting folder...done")
  (mh-scan-folder mh-current-folder "all"))


(defun mh-toggle-summarize ()
  "Turn the summary mode of displaying messages on or off."
  (interactive)
  (setq mh-summarize (not mh-summarize))
  (cond (mh-summarize
	 (delete-other-windows)
	 (setq mode-name "Mh-Summarize")
	 (recenter (/ (window-height) 2)))
	(t
	 (setq mode-name "Mh-Show")
	 (mh-show))))


(defun mh-undo (&optional seq)
  "Undo the deletion or move of the specified message(s).  Optional argument
is a name of a sequence of messages to apply undo to."
  (interactive "P")
  (cond ((looking-at "^....D")
	 (cond (seq
		(mapc (function (lambda (msg)
				  (setq mh-delete-list
					(delq msg mh-delete-list))
				  (mh-remove-msg-from-seq msg 'deleted)))
		      (mh-seq-to-msgs seq))
		(mh-notate-seq seq ?  mh-cmd-note))
	       (t
		(let ((msg (mh-get-msg-num t)))
		  (setq mh-delete-list (delq msg mh-delete-list))
		  (mh-remove-msg-from-seq msg 'deleted)
		  (mh-notate ?  mh-cmd-note)))))

	((looking-at "^....\\^")
	 (cond (seq
		(mapc (function (lambda (msg)
				  (mapc (function
					 (lambda (dest)
					   (mh-remove-msg-from-seq msg dest)))
					mh-move-list)))
		      (mh-seq-to-msgs seq))
		(mh-notate-seq seq ?  mh-cmd-note))
	       (t
		(let ((msg (mh-get-msg-num t)))
		  (mapc (function (lambda (dest)
				    (mh-remove-msg-from-seq msg dest)))
			mh-move-list)
		  (mh-notate ?  mh-cmd-note)))))

	(t nil)))


(defun mh-undo-folder ()
  "Undo all commands in current folder."
  (interactive "")
  (cond ((yes-or-no-p "Undo all commands in folder? ")
	 (setq mh-delete-list nil
	       mh-move-list nil
	       mh-seq-list nil
	       mh-next-direction 'forward)
	 (mh-unmark-all-headers t))
	(t
	 (message "Commands not undone."))))


(defun mh-visit-folder (&optional arg)
  "Visit a new folder.  If optional argument is non-nil, then prompt for the
range of messages, otherwise scan the whole buffer."
  (interactive "p")
  (let ((folder (mh-prompt-for-folder "Visit" "" t))
	(range (if arg (read-string "Range [all]? ") "all")))
    (mh-scan-folder folder (if (equal range "") "all" range))
    (delete-other-windows)))


(defun mh-widen ()
  "Remove restrictions from the current folder, thereby showing all messages."
  (interactive "")
  (let ((buffer-read-only nil))
    (delete-region (point-min) (point-max))
    (widen)
    (setq mode-line-format (mh-make-mode-line)))
  (setq mh-narrowed-to-seq nil))



;;; Support routines.

(defun mh-delete-a-msg ()
  "Delete the message pointed to by the cursor."
  (let ((msg (mh-get-msg-num t)))
    (if (looking-at "....\\^")
	(error "Message %d already moved.  Undo move before deleting." msg))
    (push msg mh-delete-list)
    (mh-add-msg-to-seq msg 'deleted t)
    (mh-notate ?D mh-cmd-note)))


(defun mh-move-a-msg (destination)
  "Move the message pointed to by the cursor."
  (if (looking-at "....D")
      (error "Message %d is already deleted.  Undo delete before moving."
	     (mh-get-msg-num nil))
      (let ((msg (mh-get-msg-num t)))
	(if (not (memq destination mh-move-list))
	    (push destination mh-move-list))
	(mh-add-msg-to-seq msg destination t)
	(mh-notate ?^ mh-cmd-note))))


(defun mh-display-msg (msg-num msg-filename show-buffer)
  "Displays the message NUMBER and PATHNAME in BUFFER."
  (if (not (file-exists-p msg-filename))
      (error "Message %d does not exist." msg-num))
  (switch-to-buffer show-buffer)
  (buffer-flush-undo (current-buffer))
  (when (not (equal msg-filename buffer-file-name))
    ;; Buffer does not yet contain message.
    (setq buffer-file-name msg-filename)
    (erase-buffer)
    (unlock-buffer)
    (if mh-use-mhl
	(mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear" msg-filename)
	(insert-file-contents msg-filename))
    (goto-char (point-min))
    (cond (mh-clean-message-header
	   (mh-clean-msg-header (point-min))
	   (goto-char (point-min)))
	  (t
	   (let ((case-fold-search t))
	     (re-search-forward "^To:\\|^From:\\|^Subject:\\|^Date:" nil t)
	     (beginning-of-line)
	     (recenter 0))))
    (set-buffer-modified-p nil)
    (setq mode-line-format
	  (concat "{%b}	%[%p of " folder "/" msg-num "%]	%M"))))


(defun mh-clean-msg-header (start)
  "Flush extraneous lines in a message header.  The variable
mh-invisible-headers contains a regular expression specifying these lines."
  (save-restriction
    (goto-char start)
    (search-forward "\n\n" nil t)
    (narrow-to-region start (point))
    (goto-char (point-min))
    (while (re-search-forward mh-invisible-headers nil t)
      (beginning-of-line)
      (kill-line 1)
      (while (looking-at "^[ \t]+")
	(beginning-of-line)
	(kill-line 1)))
    (unlock-buffer)))


(defun mh-read-draft ()
  "Read draft file into draft buffer.  Avoids errors even if disk file has been
modified underneath the buffer.  Causes an error if the folder is modified and
the user does not want to change it."
  (switch-to-buffer "draft")
  (if (buffer-modified-p)
      (if (y-or-n-p "Draft is modified; kill anyways? ")
	  (set-buffer-modified-p nil)
	  (error "Draft is not killed.")))
  (kill-buffer "draft")
  (switch-to-buffer-other-window
   (find-file-noselect (format "%sdraft" mh-user-path))))


(defun mh-next-msg ()
  "Move backward or forward to the next message in the buffer."
  (if (eq mh-next-direction 'forward)
      (mh-next-line 1)
      (mh-previous-line 1)))


(defun mh-maybe-show ()
  "If the scan listing is not summarized, show the message pointed to
by the cursor."
  (if (not mh-summarize) (mh-show)))



;;; The folder data abstraction.

(defun mh-make-folder (name)
  "Create and initialize a new mail folder called NAME and make it the
current folder."
  (switch-to-buffer name)
  (buffer-flush-undo (current-buffer))
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (make-local-vars
   'mh-current-folder name		;Name of folder
   'mh-show-buffer (format "show-%s" name) ; Buffer that displays messages
   'mh-folder-filename			; e.g. /usr/foobar/Mail/inbox/
     (format "%s%s/" mh-user-path (substring name 1))
   'mh-summarize t			; Show scan list only?
   'mh-next-seq-num 0			; Index of free sequence id
   'mh-delete-list nil			; List of msgs nums to delete
   'mh-move-list nil			; List of folder names in mh-seq-list
   'mh-seq-list nil			; Alist of seq . msgs nums
   'mh-seen-list nil			; List of displayed messages
   'mh-next-direction 'forward		; Direction to move to next message
   'mh-narrowed-to-seq nil)		; Sequence display is narrowed to
  (mh-folder-mode)
  (setq buffer-read-only t)
  (setq mode-name "Mh-Summarize"))


(defun make-local-vars (&rest pairs)
  "Takes VARIABLE-VALUE pairs and makes local variables initialized to the
value."
  (while pairs
    (make-local-variable (car pairs))
    (set (car pairs) (cadr pairs))
    (setq pairs (cddr pairs))))


(defun mh-folder-mode ()
  "Major mode for \"editing\" an MH folder scan listing.
Messages can be marked for refiling and deletion.  However, both actions
are defered until you request execution with \\[mh-execute-commands].
\\{mh-folder-mode-map}
  A prefix argument (\\[universal-argument]) to delete, move, list, or undo applies the action to a message sequence.

Variables controlling mh-e operation are (defaults in parentheses):

 mh-auto-fill-letters (t)
    Non-nil means invoke auto-fill-mode in draft messages.

 mh-clean-message-header (nil)
    Non-nil means remove header lines matching the regular expression
    specified in mh-invisible-headers from messages.

 mh-use-mhl (nil)
    Non-nil means use mhl to format displayed messages.

 mh-lpr-command-format (\"lpr -p -J '%s'\")
    Format for command used to print a message on a system printer.

 mh-summary-height (4)
    Number of lines in the summary window.

 mh-ins-buf-prefix (\">> \")
    String to insert before each non-blank line of a message as it is
    inserted in a letter being composed."

  (use-local-map mh-folder-mode-map)
  (setq major-mode 'mh-folder-mode)
  (setq mode-name "mh-folder")
  (if (and (boundp 'mh-folder-mode-hook) mh-folder-mode-hook)
      (funcall mh-folder-mode-hook)))


(defun mh-scan-folder (folder range)
  "Scan the folder FOLDER over the range RANGE.  Return in the folder."
  (cond ((null (get-buffer folder))
	 (mh-make-folder folder))
	(t
	 (if (or mh-delete-list mh-move-list mh-seq-list)
	     (if (y-or-n-p "Process commands?")
		 (mh-process-commands folder)
		 (mh-undo-folder)))
	 (switch-to-buffer-other-window folder)))
  (mh-regenerate-headers range)
  (when (= (count-lines (point-min) (point-max)) 0)
    (if (equal range "all")
	(message  "Folder %s is empty" folder)
	(message  "No messages in %s, range %s" folder range))
    (sit-for 5))
  (setq mode-line-format (mh-make-mode-line))
  (mh-unmark-all-headers nil)
  (mh-goto-cur-msg))


(defun mh-regenerate-headers (range)
  "Replace buffer with scan of its contents over range RANGE."
  (let ((buffer-read-only nil))
    (message (format "scanning %s..." (buffer-name)))
    (delete-other-windows)
    (erase-buffer)
    (mh-exec-cmd-output "scan" "-noclear" "-noheader" (buffer-name) range)
    (goto-char (point-min))
    (cond ((looking-at "scan: no messages in")
	   (keep-lines "^[ ]*[0-9]"))	; Flush random scan lines
	  ((looking-at "scan: "))	; Keep error messages
	  (t
	   (keep-lines "^[ ]*[0-9]")))	; Flush random scan lines
    (message (format "scanning %s...done" (buffer-name)))))


(defun mh-get-new-mail ()
  "Read new mail into the current buffer.  Return t if there was new mail,
nil otherwise.  Leave cursor in current buffer."
  (let ((buffer-read-only nil)
	(point-before-inc (point)))
    (message (format "inc %s..." (buffer-name)))
    (mh-unmark-all-headers nil)
    (setq mh-next-direction 'forward)
    (flush-lines "^inc:\\|^scan:")	; Kill old error messages
    (goto-char (point-max))
    (let ((start-of-inc (point)))
      (mh-exec-cmd-output "inc")
      (goto-char start-of-inc)
      (message (format "inc %s...done" (buffer-name)))
      (cond ((looking-at "inc: no mail")
	     (keep-lines "^[ ]*[0-9]")	; Flush random scan lines
	     (setq mode-line-format (mh-make-mode-line))
	     (goto-char point-before-inc)
	     (message "No new mail.")
	     nil)
	    ((looking-at "inc:")	; Error messages
	     (setq mode-line-format (mh-make-mode-line))
	     (goto-char point-before-inc)
	     (message "inc error")
	     nil)
	    (t
	     (keep-lines "^[ ]*[0-9]")
	     (setq mode-line-format (mh-make-mode-line))
	     (mh-goto-cur-msg)
	     t)))))


(defun mh-make-mode-line (&optional annotation)
  "Returns a string for mode-line-format.  The optional ANNOTATION string is
displayed after the folder's name."
  (save-excursion
    (goto-char (point-min))
    (let ((lines (count-lines (point-min) (point-max))))
      (let* ((first (mh-get-msg-num nil))
	     (case-fold-search nil)
	     (current (and (re-search-forward "....\\+" nil t)
			   (mh-get-msg-num nil))))
	(goto-char (point-max))
	(previous-line 1)
	(let ((last (mh-get-msg-num nil)))
	  (format "{%%b%s}  [%d messages%s%s]  (%%p%%%% - %%m)  %%M"
		  (if annotation (format "/%s" annotation) "")
		  lines
		  (if (> lines 0)
		      (format "  (%d - %d)" first last)
		      "")
		  (if current
		      (format "  cur = %d" current)
		      "")))))))


(defun mh-unmark-all-headers (remove-all-flags)
  "This function removes all + flags from the headers, and if called
  with a non-nil argument, removes all D and ^ flags too."
  (let ((buffer-read-only nil)
	(case-fold-search nil))
    (goto-char (point-min))
    (while (if remove-all-flags
	       (re-search-forward "^....\\D\\|^....\\^\\|^....\\+\\|.....%"
				  nil t)
	       (re-search-forward "^....\\+" nil t))
      (delete-backward-char 1)
      (insert " "))))


(defun mh-goto-cur-msg ()
  "Position the cursor at the current message."
  (let ((curmsg (mh-get-cur-msg mh-current-folder)))
    (cond ((or (zerop curmsg) (not (mh-goto-msg curmsg t)))
	   (goto-char (point-max))
	   (forward-line -1)
	   (mh-maybe-show)
	   (message "No current message"))
	  (t
	   (mh-notate ?+ 4)
	   (recenter 0)))))


(defun mh-pack-folder ()
  "Closes and packs the current folder."
  (let ((buffer-read-only nil))
    (message "closing folder...")
    (mh-process-commands mh-current-folder)
    (message "packing folder...")
    (mh-exec-cmd-quiet 0 "folder" mh-current-folder "-pack")
    (mh-regenerate-headers "all")
    (message "packing done"))
  (setq mode-line-format (mh-make-mode-line)))


(defun mh-process-commands (buffer)
  "Process outstanding commands for the buffer BUFFER."
  (message "Processing deletes and moves...")
  (switch-to-buffer buffer)
  (let ((buffer-read-only nil))
    ;; Sequences must be first
    (mh-process-seq-commands mh-seq-list)
    ;; Update the unseen sequence
    (if mh-seen-list
	(let ((unseen-seq (mh-get-profile-field "Unseen-Sequence:")))
	  (if (null unseen-seq)		; For MH.5
	      (setq unseen-seq "unseen"))
	  (apply 'mh-exec-cmd-quiet
		 (nconc (list 0 "mark" mh-current-folder)
			mh-seen-list
			(list "-sequence" unseen-seq "-delete")))))

    ;; Then refile messages
    (mapc (function
	   (lambda (dest)
	     (let ((msgs (mh-seq-to-msgs dest)))
	       (when msgs
		 (mh-delete-scan-msgs msgs)
		 (apply 'mh-exec-cmd
			(nconc (cons "refile" msgs)
			       (list "-src" buffer (symbol-name dest))))))))
	  mh-move-list)

    ;; Now delete messages
    (when mh-delete-list
      (apply 'mh-exec-cmd
	     (nconc (list "rmm" (format "%s" buffer)) mh-delete-list))
      (mh-delete-scan-msgs mh-delete-list))

    ;; Mark as cur message.
    (cond ((mh-get-msg-num nil)
	   (mh-exec-cmd "mark" mh-current-folder (mh-get-msg-num nil)
			"-seq" "cur" "-add" "-zero"))
	  ((> (buffer-size) 0)		; Some messages left in folder.
	   (mh-exec-cmd "mark" mh-current-folder
			"-seq" "cur" "-delete" "all")))

    (save-excursion
      (switch-to-buffer mh-show-buffer)
      (setq buffer-file-name nil))	; Invalidate buffer file cache

    (setq mh-delete-list nil
	  mh-move-list nil
	  mh-seq-list nil
	  mh-seen-list nil))
  (message "Processing deletes and moves...done"))


(defun mh-delete-scan-msgs (msgs)
  "Delete the scan listing lines for each of the msgs in the LIST."
  (save-excursion
    (goto-char (point-min))
    (while msgs
      (flush-lines (mh-msg-search-pat (car msgs)))
      (setq msgs (cdr msgs)))))



;;; A mode for composing and sending a message.

(defun mh-letter-mode ()
    "Mode for composing letters in mh-e.
\\{mh-letter-mode-map}"
  (text-mode)
  (if mh-auto-fill-letters
      (auto-fill-mode 1))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-start))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	(concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-separate))
  (use-local-map mh-letter-mode-map)
  (setq major-mode 'mh-letter-mode)
  (setq mode-name "mh-letter")
  (if (and (boundp 'mh-letter-mode-hook) mh-letter-mode-hook)
      (funcall mh-letter-mode-hook)))


(defun mh-to-to ()
  "Move point to end of To: field."
  (interactive)
  (expand-abbrev)
  (mh-position-on-field "To:" t))


(defun mh-to-subject ()
  "Move point to end of Subject: field.  Creates the field if necessary"
  (interactive)
  (expand-abbrev)
  (when (not (mh-position-on-field "Subject:" t))
    (mh-position-on-field "To:" nil)
    (insert-string "\nSubject: ")))


(defun mh-to-cc ()
  "Move point to end of Cc: field.  Creates the field if necessary"
  (interactive)
  (expand-abbrev)
  (when (not (mh-position-on-field "Cc:" t))
    (mh-position-on-field "To:" nil)
    (insert-string "\nCc: ")))


(defun mh-to-bcc ()
  "Move point to end of Bcc: field.  Creates the field if necessary"
  (interactive)
  (expand-abbrev)
  (when (not (mh-position-on-field "Bcc:" t))
    (mh-position-on-field "To:" nil)
    (insert-string "\nBcc: ")))


(defun mh-to-fcc ()
  "Move point to end of Fcc: field.  Creates the field if necessary"
  (interactive)
  (expand-abbrev)
  (when (not (mh-position-on-field "Fcc:" t))
    (mh-position-on-field "To:" nil)
    (insert-string "\nFcc: ")))


(defun mh-check-whom ()
  "List recipients of the current message."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (set-buffer-modified-p t)		; Force writing of contents
    (save-buffer)
    (message "Checking recipients...")
    (switch-to-buffer-other-window "*Mail Recipients*")
    (bury-buffer (current-buffer))
    (erase-buffer)
    (mh-exec-cmd-output "whom" file-name)
    (other-window -1)
    (message "Checking recipients...done")))



;;; Routines to make a search pattern and search for a message.

(defun mh-make-pick-template ()
  "Initialize a buffer with a template for a pick pattern."
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
  (setq mode-line-format "{%b}\tPick Pattern\t(^C^C to do search)")
  (goto-char (point-min))
  (end-of-line))


(defun mh-do-pick-search ()
  "Search the current folder for the messages matching the qualification
in the current buffer and make them into a sequence called `search'."
  (interactive)
  (let* ((pattern-buffer (buffer-name))
	 (searching-buffer mh-searching-folder)
	 (range "all")
	 (pattern nil))
    (message "Searching...")
    (goto-char (point-min))
    (while (setq pattern (mh-next-pick-field pattern-buffer))
      (setq msgs
	    (mh-seq-from-command searching-buffer
				 'search
				 (nconc (cons "pick" pattern)
					(list searching-buffer
					      range
					      "-sequence" "search"
					      "-list"))))
      (setq range "search"))
    (message "Searching...done")
    (switch-to-buffer searching-buffer)
    (mh-notate-seq 'search ?% (+ mh-cmd-note 1))))


(defun mh-next-pick-field (buffer)
  "Return the next piece of a pick argument that can be extracted from the
BUFFER.  Returns nil if no pieces remain."
  (switch-to-buffer buffer)
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



;;; Routines compose and send a letter.

(defun mh-compose-and-send-mail (send-args sent-from-folder to subject cc
					   &optional annotate-char
					   annotate-field search-prefix)
  "Edit and compose a draft message and send or save it.
SENT-FROM-FOLDER is buffer containing summary of current folder, if any.
SEND-ARGS is an optional argument passed to the send command.
The TO, SUBJECT, and CC fields are passed to the mh-compose-letter-hook.
If ANNOTATE-CHAR is non-null, it is used to notate the scan listing of the
current message.  In that case, the ANNOTATE-FIELD is used to build a string
for mh-annotate-msg."
  (let ((sent-from-msg))
    (save-window-excursion
      (when sent-from-folder
	(switch-to-buffer sent-from-folder)
	(setq sent-from-msg (mh-get-msg-num nil))))
    (pop-to-buffer "draft")
    (mh-letter-mode)
    (make-local-vars
     'mh-send-args send-args
     'mh-sent-from-folder sent-from-folder
     'mh-sent-from-msg sent-from-msg
     'mh-annotate-field annotate-field
     'mh-annotate-char annotate-char
     'mh-annotate-search-prefix (if search-prefix search-prefix ""))
    (setq mode-line-format "{%b}  %[Mail/draft%] (%p - %m) (^C^C to send) %M")
    (if (and (boundp 'mh-compose-letter-hook) mh-compose-letter-hook)
	(funcall mh-compose-letter-hook to subject cc))))


(defun mh-send-letter (&optional arg)
  "Send the letter in the current buffer.  If given an ARGUMENT, the delivery
process is monitored and displayed."
  (interactive "P")
  (save-buffer)
  (message "Sending...")
  (if arg
      (let ((from-buffer (buffer-name))
	    (file-name (buffer-file-name)))
	(pop-to-buffer " *mh-temp*")
	(erase-buffer)
	(if mh-send-args
	    (mh-exec-cmd-output "send" "-watch" "-unique" mh-send-args
				file-name)
	    (mh-exec-cmd-output "send" "-watch" "-unique" file-name))
	(pop-to-buffer from-buffer))
      (if mh-send-args
	  (mh-exec-cmd-quiet 0 "send" "-push" "-unique" mh-send-args
			     (buffer-file-name))
	  (mh-exec-cmd-quiet 0 "send" "-push" "-unique" (buffer-file-name))))
  (if mh-annotate-char
      (mh-annotate-msg mh-sent-from-msg
		       mh-sent-from-folder
		       mh-annotate-char
		       "-component" mh-annotate-field
		       "-text" (format "\"%s %s\""
				       (mh-get-field
					(format "%s%s"
						mh-annotate-search-prefix
						"To:"))
				       (mh-get-field
 					(format "%s%s"
						mh-annotate-search-prefix
						"Cc:")))))
  (message "Sending...done")
  (kill-buffer (buffer-name)))


(defun mh-insert-letter (&optional arg)
  "Insert a message in the current letter, asking for folder and number.
Removes headers using mh-invisible-headers.
Prefixes each non-blank line with mh-ins-buf-prefix (default \">> \").
Just \\[universal-argument] means do not indent and do not delete any
header fields.  Leaves point before the text and mark after it."
  (interactive "p")
  (let ((folder (mh-prompt-for-folder "Message from" mh-sent-from-folder nil))
	(message (read-input (format "Message number%s: "
				     (if mh-sent-from-msg
					 (format " [%d]" mh-sent-from-msg)
					 ""))))
	(start (point)))
    (if (equal message "") (setq message (format "%d" mh-sent-from-msg)))
    (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
			    (format "%s%s/%s" mh-user-path
				    (substring folder 1) message))
    (when (not (equal arg 4))
      (mh-clean-msg-header start)
      (narrow-to-region start (mark))
      (mh-insert-prefix-string mh-ins-buf-prefix)
      (widen))
    (exchange-point-and-mark)))


(defun mh-insert-cur-msg ()
  "Inserts the currently displayed message into the current draft buffer.
Prefixes each non-blank line with the string mh-ins-buf-prefix.
If there is a region set in the  message's buffer,only the region will
be inserted.  Otherwise, the region from (point) to the end will be grabbed."
  (interactive)
  (let ((to-point (point))
	(to-buffer (current-buffer)))
    (set-buffer mh-sent-from-folder)
    (set-buffer mh-show-buffer)		; Find displayed message
    (let  ((mh-ins-str (if (mark)
			   (buffer-substring (point) (mark))
			   (buffer-substring (point) (point-max)))))
      (set-buffer to-buffer)
      (narrow-to-region to-point to-point)
      (insert-string mh-ins-str)
      (mh-insert-prefix-string mh-ins-buf-prefix)
      (widen))))


(defun mh-insert-prefix-string (ins-string)
"Preface each line in the current buffer with STRING."
  (goto-char (point-min))
  (replace-regexp "^." (concat ins-string "\\&") nil)
  (goto-char (point-min)))


(defun mh-fully-kill-draft ()
  "Kill the draft message file and the draft message buffer.  Use kill-buffer
if you don't want to delete the draft message file."
  (interactive "")
  (if (file-exists-p (buffer-file-name))
      (delete-file (buffer-file-name)))
  (kill-buffer (buffer-name)))



;;; Commands to manipulate sequences.  Sequences are stored in an alist
;;; of the form:
;;;	((seq-name msgs ...) (seq-name msgs ...) ...)


(defmacro mh-seq-name (pair)
  (list 'car pair))

(defmacro mh-seq-msgs (pair)
  (list 'cdr pair))


(defun mh-seq-to-msgs (seq)
  "Returns the messages in sequence SEQ."
  (mh-seq-msgs (assoc seq mh-seq-list)))


(defun mh-msg-to-seq (msg)
  "Given a MESSAGE number, return the first sequence in which it occurs."
  (let ((l mh-seq-list))
    (while (and l (not (memq msg (cdar l))))
      (setq l (cdr l)))
    (caar l)))


(defun mh-read-seq (prompt &optional seq)
  "Read and return a sequence name from the minibuffer, prompting with 
the string PROMPT and supplying the optional DEFAULT.
% defaults to the sequences that the current message is in.
Makes sure that the sequence is known to MH commands."
  (let ((input (completing-read
		(format "%s %s %s" prompt "sequence:"
			(if seq (format "[%s] " (symbol-name seq)) ""))
		(mh-seq-names mh-seq-list))))
    (let ((seq (cond ((equal input "%") (mh-msg-to-seq (mh-get-msg-num t)))
		     ((equal input "") seq)
		     (t (intern input)))))
      (mh-process-seq seq (mh-seq-to-msgs seq))
      seq)))


(defun mh-seq-names (seq-list)
  "Returns an alist of the names of the SEQUENCES."
  (mapcar (function (lambda (entry) (cons (symbol-name (car entry)) nil)))
	  seq-list))


(defun mh-seq-from-command (folder seq command)
  "In FOLDER, make a sequence named SEQ by executing COMMAND."
  (let ((msgs ())
	(case-fold-search t))
    (save-excursion
      (save-window-excursion
	(apply 'mh-exec-cmd-quiet (cons " *mh-temp*" command))
	(goto-char (point-min))
	(while (re-search-forward "\\([0-9]+\\)" nil t)
	  (let ((num (string-to-int (buffer-substring (match-beginning 1)
						      (match-end 1)))))
	    (if (not (zerop num))
		(push num msgs)))))
      (switch-to-buffer folder)
      (push (cons seq msgs) mh-seq-list)
      msgs)))


(defun mh-remove-seq (seq)
  "Delete the sequence SEQ."
  (let ((entry (assoc seq mh-seq-list)))
    (setq mh-seq-list (delq (car entry) mh-seq-list))
    (mh-notate-seq (mh-seq-msgs (car entry)) ?  (+ mh-cmd-note 1))))


(defun mh-remove-msg-from-seq (msg-num seq &optional do-not-mark)
  "Remove a message MSG-NUM from the sequence SEQ.  If optional FLAG is
non-nil, do not mark the message as being part of a sequence."
  (let ((seq (assoc seq mh-seq-list)))
    (if seq
	(setcdr seq (delq msg-num (mh-seq-msgs seq)))))
  (if (not do-not-mark) (mh-notate ? (+ mh-cmd-note 1))))


(defun mh-add-msg-to-seq (msg-num seq &optional do-not-mark)
  "Add the message MSG-NUM to the SEQUENCE.  If optional FLAG is non-nil,
do not mark the message as being part of a sequence."
  (let ((seq-list (assoc seq mh-seq-list)))
    (if (not do-not-mark) (mh-notate ?% (+ mh-cmd-note 1)))
    (if (null seq-list)
	(push (cons seq (list msg-num)) mh-seq-list)
	(setcdr seq-list (cons msg-num (cdr seq-list))))))


(defun mh-add-msg-list-to-seq (msgs seq &optional do-not-mark)
  "Add the messages in LIST to the SEQUENCE.  If optional FLAG is non-nil,
do not mark the messages as being part of a sequence."
  (mapc (function (lambda (msg) (mh-add-msg-to-seq msg seq do-not-mark)))
	msgs))


(defun mh-rename-seq (seq new-name)
  "Rename a SEQUENCE to have a new NAME."
  (interactive "SOld sequence name: \nSNew name: ")
  (let ((old-seq (assoc seq mh-seq-list)))
    (if old-seq
	(rplaca old-seq new-name)
	(error "Sequence %s does not exists" (symbol-name seq)))))


(defun mh-notate-seq (seq notation offset)
  "Mark all messages in the sequence SEQ with the NOTATION at character
OFFSET."
  (mh-map-to-seq-msgs 'mh-notate seq notation offset))


(defun mh-map-to-seq-msgs (func seq &rest args)
  "Invoke the function FUNC at each message in the sequence SEQ, passing
the remaining ARGS as arguments."
  (let ((msgs (mh-seq-to-msgs seq)))
    (while msgs
      (mh-goto-msg (car msgs))
      (apply func args)
      (setq msgs (cdr msgs)))))


(defun mh-map-over-seqs (func seq-list)
  "Apply the function FUNC to each element in the sequence LIST,
passing the sequence name and a list of messages as arguments."
  (while seq-list
    (funcall func (caar seq-list) (cdar seq-list))
    (setq seq-list (cdr seq-list))))


(defun mh-process-seq-commands (seq-list)
  "Process outstanding sequence commands for the sequences in SEQ-LIST."
  (mh-map-over-seqs 'mh-process-seq seq-list))


(defun mh-process-seq (seq msgs)
  "Mark sequence SEQ to contain MSGS."
  ;; Do not mark pseudo-sequences.
  (if (not (equal (substring (symbol-name seq) 0 1) "+"))
      (apply 'mh-exec-cmd
	     (nconc (list "mark" "-zero" "-seq" (format "%s" seq) "-add")
		    msgs))))


(defun mh-copy-seq-to-point (seq location)
  "Copy the messages in SEQUENCE to after the LOCATION in the current buffer."
  (mh-map-to-seq-msgs 'mh-copy-line-to-point seq location))


(defun mh-copy-line-to-point (location)
  "Copy the current line to the LOCATION in the current buffer."
  (beginning-of-line)
  (let ((beginning-of-line (point)))
    (forward-line 1)
    (copy-region-as-kill beginning-of-line (point))
    (goto-char location)
    (yank)
    (goto-char beginning-of-line)))



;;; Issue commands to mh.

(defun mh-exec-cmd (command &rest args)
  "Execute MH command COMMAND with ARGS.  Any output is shown to the user."
  (save-excursion
    (switch-to-buffer-other-window " *mh-temp*")
    (erase-buffer)
    (apply 'call-process
	   (nconc (list (format "%s%s" mh-progs command) nil t nil)
		  (mh-list-to-string args)))
    (if (> (buffer-size) 0)
	(sit-for 5))))


(defun mh-exec-cmd-quiet (buffer command &rest args)
  "In BUFFER, execute MH command COMMAND with ARGS.  Return in buffer, if
one exists."
  (when (stringp buffer)
    (switch-to-buffer buffer)
    (erase-buffer))
  (apply 'call-process
	 (nconc (list (format "%s%s" mh-progs command) nil buffer nil)
		(mh-list-to-string args))))


(defun mh-exec-cmd-output (command &rest args)
  "Execute MH command COMMAND with ARGS putting the output into buffer after
point.  Set mark after inserted text."
  (set-mark (point))
  (apply 'call-process
	 (nconc (list (format "%s%s" mh-progs command) nil t nil)
		(mh-list-to-string args)))
  (exchange-point-and-mark))



(defun mh-exec-lib-cmd-output (command &rest args)
  "Execute MH library command COMMAND with ARGS.  Put the output into
buffer after point.  Set mark after inserted text."
  (set-mark (point))
  (apply 'call-process
	 (nconc (list (format "%s%s" mh-lib command) nil t nil)
		(mh-list-to-string args)))
  (exchange-point-and-mark))


(defun mh-list-to-string (l)
  "Flattens the list L and makes every element a string."
  (let ((new-list nil))
    (while l
      (cond ((symbolp (car l)) (push (format "%s" (car l)) new-list))
	    ((numberp (car l)) (push (format "%d" (car l)) new-list))
	    ((equal (car l) ""))
	    ((stringp (car l)) (push (car l) new-list))
	    ((null (car l)))
	    ((listp (car l)) (setq new-list
				   (nconc (mh-list-to-string (car l))
					  new-list)))
	    (t (error "Bad argument %s" (car l))))
      (setq l (cdr l)))
    (nreverse new-list)))



;;; Commands to annotate a message.

(defun mh-annotate-msg (msg buffer note &rest args)
  "Mark the MESSAGE in BUFFER listing with the character NOTE and annotate
the saved message with ARGS."
  ;; Wait for annotation to finish, to avoid race condition with reading msg.
  (apply 'mh-exec-cmd (cons "anno" (cons buffer (cons msg args))))
  (save-excursion
    (switch-to-buffer buffer)
    (if (mh-goto-msg msg t)
	(mh-notate note (+ mh-cmd-note 1)))))


(defun mh-notate (notation offset)
  "Marks the current message with the character NOTATION at position OFFSET."
  (let ((buffer-read-only nil))
    (beginning-of-line)
    (goto-char (+ (point) offset))
    (delete-char 1)
    (insert notation)
    (beginning-of-line)))



;;; User prompting commands.

(defun mh-prompt-for-folder (prompt default can-create)
  "Prompt for a folder name with PROMPT.  Returns the folder's name.
DEFAULT is used if the folder exists and the user types CR.
If the CAN-CREATE flag is t,then a non-existant folder is made."
  (let* ((prompt (format "%s folder%s" prompt
			 (if (equal "" default)
			     "? "
			     (format " [%s]? " default))))
	 name)
    (while (and (setq name (completing-read prompt mh-folder-list
					    nil (not can-create) "+"))
		(equal name "")
		(equal default "")))
    (cond ((or (equal name "") (equal name "+"))
	   (setq name default))
	  ((not (equal (substring name 0 1) "+"))
	   (setq name (format "+%s" name))))
    (let ((new-file-p
	   (not
	    (file-exists-p (format "%s%s" mh-user-path (substring name 1))))))
      (cond ((and new-file-p
		  (y-or-n-p
		   (format "Folder %s does not exist. Create it? " name)))
	     (message "Creating %s" name)
	     (call-process "mkdir" nil nil nil
			   (format "%s%s" mh-user-path (substring name 1)))
	     (message "Creating %s...done" name)
	     (push (list name) mh-folder-list)
	     (push (list (substring name 1 nil)) mh-folder-list))
	    (new-file-p
	     (error ""))
	    (t
	     (when (null (assoc name mh-folder-list))
	       (push (list name) mh-folder-list)
	       (push (list (substring name 1 nil)) mh-folder-list)))))
    name))


(defun mh-make-folder-list ()
  "Returns a list of the user's folders in a form suitable for completing
read."
  (interactive)
  (save-window-excursion
    (mh-exec-cmd-quiet " *mh-temp*" "folders" "-fast" "-norecurse")
    (goto-char (point-min))
    (let ((list nil))
      (while (not (eobp))
	(let ((start (point)))
	  (search-forward "\n" nil t)
	  (let ((folder (buffer-substring start (- (point) 1))))
	    (push (list (format "+%s" folder)) list))))
      list)))


(defun mh-remove-folder-from-folder-list (folder)
  "Remove FOLDER from the list of folders."
  (setq mh-folder-list
	(delq (assoc (substring folder 1 nil) mh-folder-list)
	      mh-folder-list)))



;;; Misc. functions.

(defun mh-get-msg-num (error-if-no-message)
  "Returns the message number of the current message.  If the argument
ERROR-IF-NO-MESSAGE is non-nil, then complain if the cursor is not 
pointing to a message."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "^[ ]*\\([0-9]+\\)")
	   (string-to-int (buffer-substring (match-beginning 1)
					    (match-end 1))))
	  (error-if-no-message
	   (error "Cursor not pointing to message"))
	  (t nil))))


(defun mh-msg-search-pat (n)
  "Returns a search pattern for message N in the scan listing."
  (cond ((< n 10) (format "^[^0-9][^0-9][^0-9]%d" n))
	((< n 100) (format "^[^0-9][^0-9]%d" n))
	((< n 1000) (format "^[^0-9]%d" n))
	(t (format "^%d" n))))


(defun mh-msg-filename ()
  "Returns a string containing the pathname for the file containing the
current message."
  (format "%s%d" mh-folder-filename (mh-get-msg-num t)))


(defun mh-msg-filenames (msgs folder)
  "Returns a string of filenames for MSGS in FOLDER."
  (mapconcat (function (lambda (msg) (concat folder msg))) msgs " "))


(defun mh-find-path ()
   "Set mh-user-path to the user's Mail directory from  ~/.mh_profile."
   (if (equal (setq mh-user-path (mh-get-profile-field "Path:")) "")
       (setq mh-user-path "Mail/")
       (setq mh-user-path (format "%s/" mh-user-path)))
   (if (not (equal (substring mh-user-path 0 1) "/"))
       (setq mh-user-path (format "%s/%s" (getenv "HOME") mh-user-path))))


(defun mh-get-profile-field (field)
  "Return FIELD from the user's .mh_profile file."
  (save-window-excursion
    (if (not (file-exists-p "~/.mh_profile"))
	(error "Cannot find .mh_profile file."))
    (switch-to-buffer " *mh_temp*")
    (erase-buffer)
    (insert-file-contents "~/.mh_profile")
    (mh-get-field field)))


(defun mh-get-cur-msg (folder)
  "Returns the number of the 'cur' message in FOLDER."
  (save-excursion
    (switch-to-buffer " *mh_temp*")
    (erase-buffer)
    (mh-exec-cmd-output "pick" folder "cur")
    (string-to-int (buffer-substring (point-min) (point-max)))))


(defun mh-get-field (field)
  "Find and return the value of field FIELD in the current buffer.
Returns the empty string if the field is not in the message."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (cond ((not (search-forward field nil t)) "")
	  ((looking-at "[\t ]*$") "")
	  (t
	   (re-search-forward "[\t ]*\\([a-zA-z0-9/\.].*\\)$" nil t)
	   (let ((field (buffer-substring (match-beginning 1)
					  (match-end 1)))
		 (end-of-match (point)))
	     (forward-line)
	     (while (looking-at "[ \t]") (forward-line 1))
	     (backward-char 1)
	     (format "%s%s" field (buffer-substring end-of-match (point))))))))


(defun mh-insert-fields (&rest name-values)
  "Insert the NAME-VALUE pairs in the current buffer."
  (let ((case-fold-search t))
    (while name-values
      (let ((field-name (car name-values))
	    (value (cadr name-values)))
	(goto-char (point-min))
	(cond ((not (re-search-forward (format "^%s" field-name) nil t))
	       (re-search-forward "^---\\|^$")
	       (beginning-of-line)
	       (insert field-name " " value "\n"))
	      (t
	       (end-of-line)
	       (insert " " value)))
	(setq name-values (cddr name-values))))))


(defun mh-position-on-field (field set-mark)
  "Set point to the end of the line beginning with FIELD.  Sets the mark
to the point, if SET-MARK is non-nil."
  (if set-mark (set-mark (point)))
  (goto-char (point-min))
  (if (re-search-forward (format "^%s" field) nil t)
      (progn (end-of-line) t)
      nil))



;;; Build the folder-mode keymap:

(define-key mh-folder-mode-map "?" 'mh-msg-is-in-seq)
(define-key mh-folder-mode-map "%" 'mh-put-msg-in-seq)
(define-key mh-folder-mode-map "\e%" 'mh-delete-msg-from-seq)
(define-key mh-folder-mode-map "\^xn" 'mh-narrow-to-seq)
(define-key mh-folder-mode-map "\^xw" 'mh-widen)

(define-key mh-folder-mode-map "\^c" 'my-apply-command-to-seq)
(define-key mh-folder-mode-map "\eu" 'mh-undo-folder)
(define-key mh-folder-mode-map "\e " 'mh-page-digest)
(define-key mh-folder-mode-map "\ee" 'mh-extract-rejected-mail)
(define-key mh-folder-mode-map "\ef" 'mh-visit-folder)
(define-key mh-folder-mode-map "\ek" 'mh-kill-folder)
(define-key mh-folder-mode-map "\el" 'mh-list-folders)
(define-key mh-folder-mode-map "\ep" 'mh-renumber-folder)
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
(define-key mh-folder-mode-map "a" 'mh-answer)
(define-key mh-folder-mode-map "g" 'mh-goto-msg)
(define-key mh-folder-mode-map "\177" 'mh-previous-page)
(define-key mh-folder-mode-map " " 'mh-page-msg)
(define-key mh-folder-mode-map "." 'mh-show)
(define-key mh-folder-mode-map "u" 'mh-undo)
(define-key mh-folder-mode-map "!" 'mh-move-or-write-again)
(define-key mh-folder-mode-map "^" 'mh-move-msg)
(define-key mh-folder-mode-map "d" 'mh-delete-msg)
(define-key mh-folder-mode-map "p" 'mh-previous-line)
(define-key mh-folder-mode-map "n" 'mh-next-line)


;;; Build the letter-mode keymap:

(define-key mh-letter-mode-map "\^Cb" 'mh-to-bcc)
(define-key mh-letter-mode-map "\^Cw" 'mh-check-whom)
(define-key mh-letter-mode-map "\^Cc" 'mh-to-cc)
(define-key mh-letter-mode-map "\^Cf" 'mh-to-fcc)
(define-key mh-letter-mode-map "\^Cs" 'mh-to-subject)
(define-key mh-letter-mode-map "\^Ct" 'mh-to-to)
(define-key mh-letter-mode-map "\^Cq" 'mh-fully-kill-draft)
(define-key mh-letter-mode-map "\^Cy" 'mh-insert-cur-msg)
(define-key mh-letter-mode-map "\^C\^Y" 'mh-insert-letter)
(define-key mh-letter-mode-map "\^C\^C" 'mh-send-letter)

;;; Build the pick-mode keymap:

(define-key mh-pick-mode-map "\^C\^C" 'mh-do-pick-search)
(define-key mh-pick-mode-map "\^Cb" 'mh-to-bcc)
(define-key mh-pick-mode-map "\^Cc" 'mh-to-cc)
(define-key mh-pick-mode-map "\^Cf" 'mh-to-fcc)
(define-key mh-pick-mode-map "\^Cs" 'mh-to-subject)
(define-key mh-pick-mode-map "\^Ct" 'mh-to-to)
(define-key mh-pick-mode-map "\^Cw" 'mh-check-whom)
