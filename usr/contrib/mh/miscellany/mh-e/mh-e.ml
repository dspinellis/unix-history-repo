;;;  mh-e.el	(Version: 3.1a for GNU Emacs Version 17)

;;;  Copyright (C) James Larus (larus@berkeley.arpa, ucbvax!larus), 1985
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
(defvar mh-progs "/usr/new/mh/"     "Directory containing MH commands")
(defvar mh-lib   "/usr/new/lib/mh/" "Directory of MH library")


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


;;; Global variables:

(defvar mh-user-path  ""	     "User's mail folder.")
(defvar mh-last-destination nil	     "Destination of last "move" command.")
(defvar mh-folder-mode-map (make-sparse-keymap) "Keymap for MH folders.")
(defvar mh-letter-mode-map (make-sparse-keymap) "Keymap for composing mail.")
(defvar mh-pick-mode-map (make-sparse-keymap) "Keymap for searching folder.")
(defvar mh-folder-list nil	     "List of folder names for completion.")

;;; Macros:

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
	 (let ((folder (mh-get-folder-name "mh" "+inbox" t))
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
  "Answer a letter.  If given an argument, then include the current message
in the reply."
  (interactive "P")
  (let ((msg-filename (mh-msg-filename))
	(msg (mh-get-msg-num t))
	(minibuffer-help-form "from => Sender\n  to => Sender and primary recipients\n  cc => Sender and all recipients")
	(folder mh-current-folder))
    (let ((reply-to (completing-read "Reply to whom: "
				     '(("from") ("to") ("cc"))
				      nil t)))
    (message "Composing a reply...")
    (cond ((or (equal reply-to "from") (equal reply-to ""))
	   (apply 'mh-exec-cmd
		  (nconc
		   (list "repl" "-build" mh-current-folder msg "-nocc" "all")
		   (if arg (list "-filter" "mhl.reply")))))
	  ((equal reply-to "to")
	   (apply 'mh-exec-cmd
		  (nconc
		   (list "repl" "-build" mh-current-folder msg "-cc" "to"
			 "-nocc" "me")
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
      (when (not arg)
	(switch-to-buffer-other-window "*message*")
	(erase-buffer)
	(if (file-exists-p msg-filename)
	    (insert-file-contents msg-filename)
	    (error "File %s does not exist" msg-filename))
	(set-buffer-modified-p nil)
	(goto-char (point-min))
	(re-search-forward "^$\\|^-*$")
	(recenter 0))
      (message "Composing a reply...done")
      (mh-compose-and-send-mail "" folder to subject cc "-" "Replied:")))))


(defun mh-copy-msg (&optional arg)
  "Copy specified message(s) to another folder without deleting them."
  (interactive "P")
  (let ((msgs (if arg
		  (mh-seq-to-msgs (mh-read-seq "Copy"))
		  (mh-get-msg-num t))))
    (mh-exec-cmd-no-wait "refile" msgs "-link" "-src"
			 mh-current-folder
			 (mh-get-folder-name "Copy to" "" t))
    (if arg
	(mh-notate-seq msgs ?C mh-cmd-note)
	(mh-notate ?C mh-cmd-note))))


(defun mh-delete-msg (&optional arg)
  "Marks the specified message(s) for later deletion."
  (interactive "P")
  (if arg
      (mh-apply-to-seq (mh-read-seq "Delete") 'mh-delete-one-msg)
      (mh-delete-one-msg))
  (mh-next-message))


(defun mh-execute-commands ()
  "Process outstanding delete and move commands."
  (interactive)
  (save-excursion
    (mh-process-commands mh-current-folder))
  (delete-other-windows)
  (setq mh-summarize t)
  (setq mode-name "Mh-Summary")
  (setq mode-line-format (mh-make-mode-line)))


(defun mh-forward (to cc subject)
  "Forward a letter."
  (interactive "sTo: \nsCc: \nsSubject: ")
  (let ((msg-filename (mh-msg-filename))
	(msg (mh-get-msg-num t))
	(folder mh-current-folder))
    (cond ((or (not (file-exists-p (format "%sdraft" mh-user-path)))
	       (y-or-n-p "The file 'draft' exists.  Discard it? "))
	   (mh-exec-cmd "forw" "-build" mh-current-folder msg)
	   (mh-read-draft)
	   (mh-insert-fields "To:" to "Subject:" subject "Cc:" cc))
	  (t
	   (mh-read-draft)))

    (goto-char (point-min))
    (delete-other-windows)
    (mh-compose-and-send-mail "" folder to subject cc "F" "Forwarded-To:")))


(defun mh-goto (number &optional no-error-if-no-message)
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
  (switch-to-buffer-other-window "+inbox")
  (if (or (not (boundp 'mh-current-folder)) (null mh-current-folder))
      (mh-make-folder "+inbox"))
  (if (mh-get-new-mail)
      (mh-show)))


(defun mh-indicate-seq (&optional arg)
  "Add the specified message(s) to a sequence."
  (interactive "P")
  (let ((new-seq (mh-letter-to-seq last-input-char))
	(old-seq (if (looking-at "^[0-9a-z]")
		     (mh-letter-to-seq (char-after (point))))))
    (if old-seq
	(if arg
	    (mh-remove-seq old-seq)
	    (mh-remove-msg-from-seq (mh-get-msg-num t) old-seq)))
    (if (and (not arg)
	     (or (not old-seq) (not (equal new-seq old-seq))))
	(mh-add-msg-to-seq (mh-get-msg-num t) new-seq)))
    (mh-next-message))


(defun mh-kill-folder ()
  "Removes the current folder."
  (interactive)
  (if (yes-or-no-p (format "Remove folder %s? " mh-current-folder))
      (let ((buffer mh-current-folder))
	(switch-to-buffer-other-window " *mh-temp*")
	(mh-exec-cmd "rmf" buffer)
	(mh-remove-folder-from-folder-list buffer)
	(message "Folder removed")
	(kill-buffer buffer))
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


(defun mh-move-msg (&optional arg)
  "Move specified message(s) to another folder."
  (interactive "P")
  (setq mh-last-destination (mh-get-folder-name "Destination" "" t))
  (if arg
      (mh-apply-to-seq (mh-read-seq "Move") 'mh-move-one-msg)
      (mh-move-one-msg))
  (mh-next-message))


(defun mh-next-line (&optional arg)
  "Move to next undeleted message in window and display body if summary
flag set."
  (interactive "p")
  (forward-line (if arg arg 1))
  (setq mh-next-direction 'forward)
  (cond ((re-search-forward "^....[^D^]" nil 0 arg)
	 (beginning-of-line)
	 (mh-maybe-show))
	(t
	 (forward-line -1)
	 (message "No more messages"))))


(defun mh-renumber-folder ()
  "Renumber messages in folder to be 1..N."
  (interactive)
  (message "packing buffer...")
  (mh-pack-folder)
  (mh-unmark-all-headers nil)
  (mh-position-to-current)
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
  (if (not (re-search-backward "^....[^D^]" nil 0 arg))
      (message "Beginning of messages")
      (mh-maybe-show)))


(defun mh-previous-page ()
  "Page the displayed message backwards."
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window mh-show-buffer)
    (scroll-down nil)
    (other-window -1)))


(defun mh-print-msg (&optional arg)
  "Print specified message(s) on a line printer."
  (interactive "P")
  (let ((msgs (if arg
		  (reverse (mh-seq-to-msgs (mh-read-seq "Print")))
		  (list (mh-get-msg-num t)))))
    (message "printing message...")
    (call-process "/bin/sh" nil 0 nil "-c"
		  (format "%smhl -nobell -clear %s | %s" mh-lib
			  (mh-msg-filenames msgs mh-folder-filename)
			  (format mh-lpr-command-format
				  (if arg
				      "Mail"
				      (format "%s/%d" mh-current-folder
					      (mh-get-msg-num t))))))
    (message "printing message...done")))


(defun mh-rescan-folder (&optional arg)
  "Optionally process commands in current folder and (re)scan it."
  (interactive "P")
  (if (and (or mh-delete-list mh-move-list)
	   (y-or-n-p "Process commands? "))
      (mh-process-commands mh-current-folder))
  (setq mh-next-direction 'forward)
  (mh-scan-folder mh-current-folder
		  (if arg (read-string "Range [all]? ") "all")))


(defun mh-redistribute (to cc)
  "Redistribute a letter."
  (interactive "sTo: \nsCc: ")
  (let ((msg-filename (mh-msg-filename))
	(msg (mh-get-msg-num t))
	(folder mh-current-folder))
    (mh-read-draft)
    (delete-other-windows)
    (when (or (zerop (buffer-size))
	      (not (y-or-n-p
		    "The file 'draft' exists.  Redistribute old version? ")))
      (erase-buffer)
      (insert-file-contents msg-filename)
      (goto-char (point-min))
      (insert "Resent-To: " to "\n")
      (if (not (equal cc ""))
	  (insert "Resent-cc: " cc "\n")))
    (mh-compose-and-send-mail "-dist" folder to (mh-get-field "Subject:") cc
			      "F" "Distributed-to:")))


(defun mh-re-move ()
  "Move specified message to same folder as last move."
  (interactive)
  (if (null mh-last-destination)
      (error "No previous move"))
  (mh-move-one-msg)
  (mh-next-message))


(defun mh-save-message (file)
  "Append the current message to the end of a file."
  (interactive "FSave message in file: ")
  (let ((msg-filename (mh-msg-filename)))
    (call-process "/bin/csh" nil 0 nil "-c"
		  (format "cat %s >> %s " msg-filename file))))

    
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
      (goto-char (point-max))
      (message "Composing a message...done"))
    (mh-compose-and-send-mail "" folder to subject cc)))


(defun mh-show ()
  "Show message indicated by cursor in scan buffer."
  (interactive)
  (setq mh-summarize nil)
  (setq mode-name "Mh-Show")
  (let ((msgn (mh-get-msg-num t))
	(msg-filename (mh-msg-filename))
	(folder mh-current-folder))
    (if (not (file-exists-p msg-filename))
	(error "Message %d does not exist." msgn))
    (switch-to-buffer mh-show-buffer)
    (erase-buffer)
    (if mh-use-mhl
	(mh-exec-lib-cmd-output "mhl" "-nobell" msg-filename)
	(insert-file-contents msg-filename))
    (setq buffer-file-name msg-filename)
    (goto-char (point-min))
    (cond (mh-clean-message-header
	   (mh-clean-message-header (point-min))
	   (goto-char (point-min)))
	  (t
	   (let ((case-fold-search t))
	     (re-search-forward "^To:\\|^From:\\|^Subject:\\|^Date:" nil t)
	     (beginning-of-line)
	     (recenter 0))))
    (set-buffer-modified-p nil)
    (setq mode-line-format
	  (concat "{%b}	%[%p of " folder "/" msgn "%]	%M"))
    ;; These contortions are to force the summary line to be the top window.
    (switch-to-buffer-other-window folder)
    (delete-other-windows)
    (switch-to-buffer-other-window mh-show-buffer)
    (switch-to-buffer-other-window folder)
    (shrink-window (- (window-height) mh-summary-height))
    (recenter 1)
    ;; Remove from unseen seq.
    (mh-exec-cmd-no-wait "mark" mh-current-folder msgn "-seq" "unseen"
			 "-delete" "-nolist")))


(defun mh-sort-folder (&optional arg)
  "Sort the messages in the current folder by date."
  (interactive "P")
  (mh-process-commands mh-current-folder)
  (setq mh-next-direction 'forward)
  (message "sorting folder...")
  (mh-exec-cmd "sortm" mh-current-folder)
  (message "sorting folder...done")
  (mh-scan-folder mh-current-folder "all"))


(defun mh-summary ()
  "Show a summary of mh-e commands."
  (interactive)
  (message
"Next Prev Go Del ^ ! Copy Undo . Toggle Ans Forw Redist Send List Execute")
  (sit-for 5))


(defun mh-toggle-summarize ()
  "Turn the summary mode of displaying messages on or off."
  (interactive)
  (setq mh-summarize (not mh-summarize))
  (cond (mh-summarize
	 (delete-other-windows)
	 (setq mode-name "Mh-Summarize")
	 (recenter))
	(t
	 (setq mode-name "Mh-Show")
	 (mh-show))))


(defun mh-undo (&optional arg)
  "Undo the deletion or move of the specified message(s)."
  (interactive "P")
  (cond ((looking-at "^....D")
	 (let ((msgs (if arg (mh-read-seq "Undelete") (mh-get-msg-num t))))
	   (setq mh-delete-list (delq msgs mh-delete-list))
	   (if arg
	       (mh-notate-seq msgs ?  mh-cmd-note)
	       (mh-notate ?  mh-cmd-note))))

	((looking-at "^....^")
	 (let ((msgs (if arg (mh-read-seq "Unmove") (mh-get-msg-num t))))
	   (mapcar
	    (function (lambda (move) (setcdr move (delq msgs (cdr move)))))
	    mh-move-list)
	   (if arg
	       (mh-notate-seq msgs ?  mh-cmd-note)
	       (mh-notate ?  mh-cmd-note))))

	(t nil)))


(defun mh-visit-folder (&optional arg)
  "Visit a new folder."
  (interactive "p")
  (let ((folder (mh-get-folder-name "Visit" "" t))
	(range (if arg (read-string "Range [all]? ") "all")))
    (mh-scan-folder folder (if (equal range "") "all" range))
    (delete-other-windows)))



;;; Support routines.

(defun mh-delete-one-msg ()
  "Delete the message pointed to by the cursor."
  (if (looking-at "....^")
      (error "Message %d already moved.  Undo move before deleting."
	     (mh-get-msg-num t)))
  (push (mh-get-msg-num t) mh-delete-list)
  (mh-notate ?D mh-cmd-note))


(defun mh-move-one-msg ()
  "Move the message pointed to by the cursor."
  (if (looking-at "....D")
      (error "Message %d is already deleted.  Undo delete before moving."
	     (mh-get-msg-num nil))
      (let ((others (assoc mh-last-destination mh-move-list))
	    (msg (mh-get-msg-num t)))
	(if others
	    (setcdr others (cons msg (cdr others)))
	    (push (cons mh-last-destination (list msg)) mh-move-list))
	(mh-notate ?^ mh-cmd-note))))


(defun mh-clean-message-header (start)
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
	(kill-line 1)))))


(defun mh-read-draft ()
  "Read draft file into buffer draft.  No errors if disk file has been
modified."
  (switch-to-buffer "draft")
  (set-buffer-modified-p nil)
  (kill-buffer "draft")
  (switch-to-buffer-other-window
   (find-file-noselect (format "%sdraft" mh-user-path))))


(defun mh-next-message ()
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
  "Create and initialize a new mail folder called NAME and make
it the current folder."
  (switch-to-buffer name)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (make-local-variable 'mh-current-folder) ; Name of folder
  (setq mh-current-folder name)
  (make-local-variable 'mh-show-buffer) ; Buffer that displays messages
  (setq mh-show-buffer (format "show-%s" mh-current-folder))
  (make-local-variable 'mh-folder-filename) ; e.g. /usr/foobar/Mail/inbox/
  (setq mh-folder-filename (format "%s%s/" mh-user-path (substring name 1)))
  (make-local-variable 'mh-summarize)	 ; Show scan list only?
  (setq mh-summarize t)
  (make-local-variable 'mh-next-seq-num)  ; Index of free sequence id
  (setq mh-next-seq-num 0)
  (make-local-variable 'mh-delete-list)	 ; List of msgs nums to delete
  (setq mh-delete-list nil)
  (make-local-variable 'mh-move-list)	 ; Alist of dest . msgs nums
  (setq mh-move-list nil)
  (make-local-variable 'mh-seq-list)	 ; Alist of seq . msgs nums
  (setq mh-seq-list nil)
  (make-local-variable 'mh-next-direction) ; Direction to move to next message
  (setq mh-next-direction 'forward)
  (mh-folder-mode)
  (setq buffer-read-only t)
  (setq mode-name "Mh-Summarize"))


(defun mh-folder-mode ()
  "    \\[mh-next-line]: next message			\\[mh-previous-line]: previous message
    \\[mh-delete-msg]: delete (mark for deletion)	\\[mh-move-msg]: put (mark for moving)
    \\[mh-undo]: undo last delete or mark		\\[mh-re-move]: repeat last ^ command
    \\[mh-copy-msg]: copy message to another folder
    \\[mh-show]: type message			\\[mh-toggle-summarize]: toggle summarize mode
    \\[mh-page-msg] page message			\\[mh-previous-page]: page message backwards
    \\[mh-print-msg]: print message			\\[mh-goto]: goto a message
    \\[mh-execute-commands]: execute pending delete and move commands
    \\[mh-send]: send a message			\\[mh-redistribute]: redistribute a message
    \\[mh-answer]: answer a message			\\[mh-forward]: forward a message
    \\[mh-visit-folder]: visit folder			\\[mh-inc-folder]: inc mail
    \\[mh-kill-folder]: kill folder			\\[mh-list-folders]: list folders
    \\[mh-renumber-folder]: pack folder			\\[mh-rescan-folder]: rescan folder
    \\[mh-search-folder]: search folder			\\[mh-sort-folder]: sorts the letters in the folder

    0..9 Add a message to a numbered sequence

A prefix argument to delete, move, list, or undo applies to a sequence.

Edit the scan list, marking messages. Moving and deleting messages is
deferred until you type \\[mh-execute-commands]."
  (use-local-map mh-folder-mode-map)
  (setq major-mode 'mh-folder-mode)
  (setq mode-name "mh-folder")
  (if (and (boundp 'mh-folder-mode-hook) mh-folder-mode-hook)
      (funcall mh-folder-mode-hook)))


(defun mh-scan-folder (folder range)
  "Scan the folder FOLDER over the range RANGE.  Return in the folder."
  (if (null (get-buffer folder))
      (mh-make-folder folder)
      (switch-to-buffer-other-window folder))
  (mh-regenerate-headers range)
  (when (looking-at "scan: no messages ")
      (let ((buffer-read-only nil))
	(erase-buffer))
      (if (equal range "all")
	  (message  "Folder %s is empty" folder)
	  (message  "No messages in %s, range %s" folder range))
      (sit-for 5))
  (setq mode-line-format (mh-make-mode-line))
  (mh-unmark-all-headers nil)
  (mh-position-to-current))


(defun mh-regenerate-headers (range)
  "Replace buffer with scan of its contents over range RANGE."
  (let ((buffer-read-only nil))
    (message (format "scanning %s..." (buffer-name)))
    (delete-other-windows)
    (erase-buffer)
    (mh-exec-cmd-output "scan" (buffer-name) range)
    (goto-char (point-min))
    (message (format "scanning %s...done" (buffer-name)))
    ))


(defun mh-get-new-mail ()
  "Read new mail into the current buffer.  Return t if there was new mail,
nil otherwise.  Return in the current buffer."
  (let ((buffer-read-only nil))
    (message (format "inc %s..." (buffer-name)))
    (mh-unmark-all-headers nil)
    (setq mh-next-direction 'forward)
    (goto-char (point-max))
    (let ((start-of-inc (point)))
      (mh-exec-cmd-output "inc")
      (message (format "inc %s...done" (buffer-name)))
      (goto-char start-of-inc)
      (cond ((looking-at "inc: no mail")
	     (kill-line 1)
	     (setq mode-line-format (mh-make-mode-line))
	     (previous-line 1)
	     (message "No new mail")
	     (sit-for 5)
	     nil)
	    (t
	     (kill-line 2)
	     (setq mode-line-format (mh-make-mode-line))
	     t)))))


(defun mh-make-mode-line ()
  "Returns a string for mode-line-format."
  (save-excursion
    (let ((lines (count-lines (point-min) (point-max))))
      (goto-char (point-min))
      (let* ((first (mh-get-msg-num nil))
	     (case-fold-search nil)
	     (current (and (re-search-forward "....\\+" nil t)
			   (mh-get-msg-num nil))))
	(goto-char (point-max))
	(previous-line 1)
	(let ((last (mh-get-msg-num nil)))
	  (concat "{%b} %[" lines " messages"
		  (if (> lines 0)
		      (format " (%d - %d)" first last)
		      "")
		  (if current
		      (format " cur = %d" current)
		      "")
		  "%]	(%p%% - %m)"))))))


(defun mh-unmark-all-headers (remove-all-flags)
  "This function removes all + flags from the headers, and if called
  with a non-nil argument, removes all D and ^ flags too."
  (let ((buffer-read-only nil)
	(case-fold-search nil))
    (goto-char (point-min))
    (while (if remove-all-flags
	       (re-search-forward "^....\\D\\|^....\\^\\|^....\\+" nil t)
	       (re-search-forward "^....\\+" nil t))
      (delete-backward-char 1)
      (insert " "))))


(defun mh-position-to-current ()
  "Position the cursor at the current message."
  (let ((curmsg (mh-get-cur-msg mh-folder-filename)))
    (cond ((or (zerop curmsg) (mh-goto curmsg t))
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
    (mh-exec-cmd-quiet "folder" mh-current-folder "-pack")
    (mh-regenerate-headers "all")
    (message "packing done"))
  (setq mode-line-format (mh-make-mode-line)))


(defun mh-apply-to-message-list (func list)
  "Apply function FUNC to each item in a message-list LIST,
passing the name and list of messages as arguments."
  (mapcar (function (lambda (l) (apply func (list (car l) (cdr l))))) list))


(defun mh-process-commands (buffer)
  "Process outstanding commands for the buffer BUFFER."
  (message "Processing deletes and moves...")
  (switch-to-buffer buffer)
  (let ((buffer-read-only nil))
    ;; Sequences must be first
    (mh-process-seq-commands mh-seq-list)

    ;; Then refile messages
    (mh-apply-to-message-list
     (function (lambda (dest msgs)
		 (apply 'mh-exec-cmd
			(nconc (cons "refile" msgs)
			       (list "-src" (format "%s" buffer) dest)))))
     mh-move-list)

    ;; Now delete messages
    (if mh-delete-list
	(apply 'mh-exec-cmd
	       (nconc (list "rmm" (format "%s" buffer)) mh-delete-list)))

    ;; Mark as cur message.
    (if (mh-get-msg-num nil)
	(mh-exec-cmd-no-wait "mark" mh-current-folder (mh-get-msg-num nil)
			     "-seq" "cur" "-add" "-zero" "-nolist")
	(mh-exec-cmd-no-wait "mark" mh-current-folder "-seq" "cur" "-delete"
			     "all" "-nolist"))

    (switch-to-buffer buffer)
    (goto-char (point-min))
    (flush-lines "^....D")
    (goto-char (point-min))
    (flush-lines "^....^")
    (setq mh-delete-list nil
	  mh-move-list nil
	  mh-seq-list nil))
  (message "Processing deletes and moves...done"))



;;; A mode for composing and sending a message.

(defun mh-letter-mode ()
  "Mode for composing letters in mh-e.
Like text-mode, but with these additional commands:
    \\[mh-send-letter]: sends the message.
    \\[mh-insert-letter]: inserts a message into the current letter.
    \\[mh-to-to]: move to the To: field		\\[mh-to-subject]: move to the Subject: field
    \\[mh-to-cc]: move to the Cc: field		\\[mh-to-bcc]: move to the Bcc: field
    \\[mh-to-fcc]: move to the Fcc: field
    \\[mh-check-whom]: report who a message will go to
    \\[kill-buffer]: quit draft and delete it."
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
  "Move point to end of Subject: field."
  (interactive)
  (expand-abbrev)
  (mh-position-on-field "Subject:" t))


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
    (save-buffer)
    (message "Checking recipients...")
    (switch-to-buffer-other-window "*Mail Recipients*")
    (erase-buffer)
    (mh-exec-cmd-output "whom" file-name)
    (previous-window)))



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
  "Search for the messages in the current folder meeting the qualification
in the current buffer and make them into a sequence."
  (interactive)
  (let* ((pattern-buffer (buffer-name))
	 (searching-buffer mh-searching-folder)
	 (range "all")
	 (seq (mh-new-seq mh-searching-folder))
	 (pattern nil))
    (message "Searching...")
    (goto-char (point-min))
    (while (setq pattern (mh-next-pick-field pattern-buffer))
      (setq msgs
	    (mh-seq-from-command searching-buffer
				 seq
				 (nconc (cons "pick" pattern)
					(list searching-buffer
					      range
					      "-sequence" seq "-list"))))
      (setq range seq))
    (message "Searching...done")
    (switch-to-buffer searching-buffer)
    (mh-notate-seq seq (mh-seq-to-notation seq) 0)))


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
					   annotate-field)
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
    (make-local-variable 'mh-send-args)
    (setq mh-send-args send-args)
    (make-local-variable 'mh-sent-from-folder)
    (setq mh-sent-from-folder sent-from-folder)
    (make-local-variable 'mh-sent-from-msg)
    (setq mh-sent-from-msg sent-from-msg)
    (make-local-variable 'mh-annotate-field)
    (setq mh-annotate-field annotate-field)
    (make-local-variable 'mh-annotate-char)
    (setq mh-annotate-char annotate-char)
    (setq mode-line-format "{%b}  %[Mail/draft%] (%p - %m) (^C^C to send) %M")
    (if (and (boundp 'mh-compose-letter-hook) mh-compose-letter-hook)
	(funcall mh-compose-letter-hook to subject cc))))


(defun mh-send-letter ()
  "Send the letter in the current buffer."
  (interactive)
  (save-buffer)
  (message "Sending...")
  (if mh-send-args
      (mh-exec-cmd-no-wait "send" "-push" "-unique" mh-send-args
			   (buffer-file-name))
      (mh-exec-cmd-no-wait "send" "-push" "-unique" (buffer-file-name)))
  (if mh-annotate-char
      (mh-annotate-msg mh-sent-from-msg mh-sent-from-folder
		       mh-annotate-char
		       "-component" mh-annotate-field
		       "-text" (format "\"%s %s\"" (mh-get-field "To:")
				      (mh-get-field "Cc:"))))
  (message "Sending...done")
  (kill-buffer (buffer-name)))


(defun mh-insert-letter (&optional arg)
  "Insert a message in the current letter, asking for folder and number.
Removes headers using mh-invisible-headers.
Prefixes each non-blank line with mh-ins-buf-prefix (default \">> \").
Just \\[universal-argument] means do not indent and do not delete any
header fields.  Leaves point before the text and mark after it."
  (interactive "p")
  (let ((folder (mh-get-folder-name "Message from" mh-sent-from-folder nil))
	(message (read-input (format "Message number%s: "
				     (if mh-sent-from-msg
					 (format " [%d]" mh-sent-from-msg)
					 ""))))
	(start (point)))
    (if (equal message "") (setq message (format "%d" mh-sent-from-msg)))
    (mh-exec-lib-cmd-output "mhl" "-nobell"
			    (format "%s%s/%s" mh-user-path
				    (substring folder 1) message))
    (when (not (equal arg 4))
      (mh-clean-message-header start)
      (narrow-to-region start (mark))
      (mh-insert-prefix-string mh-ins-buf-prefix)
      (widen))
    (exchange-point-and-mark)))


(defun mh-insert-cur-message ()
  "Inserts the currently visible message into the current buffer.
Prefixes the string mh-ins-buf-prefix to each non-blank line
of the inserted text.  If there is a region set in the
currently visible message's buffer, only the region will be grabbed.
Otherwise, the region from (point) to the end will be grabbed."
  (interactive)
  (let ((to-point (point))
	(to-buffer (current-buffer)))
    (set-buffer "*message*")
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
  (replace-regexp "^.." (concat ins-string "\\&") nil)
  (goto-char (point-min)))



;;; Commands to manipulate sequences.

(defmacro mh-seq-name (pair)
  (list 'car pair))

(defmacro mh-seq-msgs (pair)
  (list 'cdr pair))


(defun mh-seq-to-msgs (seq)
  "Returns the list of messages in sequence SEQ."
  (mh-seq-msgs (assoc seq mh-seq-list)))


(defun mh-read-seq (prompt)
  "Prompt the user with PROMPT and read a sequence name."
  (mh-letter-to-seq
   (string-to-char (read-string (format "%s %s" prompt "sequence: ")))))


(defun mh-seq-from-command (folder seq command)
  "In FOLDER, make a sequence named SEQ by executing COMMAND."
  (let ((msgs ())
	(case-fold-search t))
    (save-excursion
      (save-window-excursion
	(apply 'mh-exec-cmd-quiet command)
	(switch-to-buffer " *mh-temp*")
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
    (mh-notate-seq (mh-seq-msgs (car entry)) ?  0)))


(defun mh-remove-msg-from-seq (msg-num seq)
  "Remove a message MSG-NUM from the sequence SEQ."
  (let ((seq (assoc seq mh-seq-list)))
    (if seq
	(setcdr seq (delq msg-num (mh-seq-msgs seq)))))
  (mh-notate ? 0))


(defun mh-add-msg-to-seq (msg-num seq)
  "Add a message MSG-NUM to a sequence SEQ."
  (let ((seq-list (assoc seq mh-seq-list)))
    (mh-notate (mh-seq-to-notation seq) 0)
    (if (null seq-list)
	(push (cons seq (list msg-num)) mh-seq-list)
	(setcdr seq-list (cons msg-num (cdr seq-list))))))



(defun mh-new-seq (folder)
  "Return a new sequence name for FOLDER."
  (save-excursion
    (switch-to-buffer folder)
    (if (= mh-next-seq-num 10)
	(error "No more sequences"))
    (setq mh-next-seq-num (+ mh-next-seq-num 1))
    (mh-letter-to-seq (+ (1- mh-next-seq-num) ?a))))


(defun mh-letter-to-seq (letter)
  "Given a LETTER, return a string that is a valid sequence name."
  (cond ((and (>= letter ?0) (<= letter ?9))
	 (intern (format "mhe%c" letter)))
	((and (>= letter ?a) (<= letter ?z))
	 (intern (format "mhe%c" letter)))
	(t
	 (error "A sequence is named 0...9"))))


(defun mh-seq-to-notation (seq)
  "Return the string used to indicate sequence SEQ in a scan listing."
  (string-to-char (substring (symbol-name seq) 3 4)))


(defun mh-notate-seq (seq notation offset)
  "Mark all messages in the sequence SEQ with the NOTATION at character
OFFSET."
  (mh-apply-to-seq seq 'mh-notate notation offset))


(defun mh-apply-to-seq (seq function &rest args)
  "For each message in sequence SEQ, evaluate the FUNCTION with ARGS."
  (mapcar (function (lambda (msg) (mh-goto msg) (apply function args)))
	  (mh-seq-to-msgs seq)))


(defun mh-process-seq-commands (seq-list)
  "Process outstanding sequence commands for the sequences in SEQ-LIST."
  (mh-apply-to-message-list
   (function (lambda (seq msgs)
	       (apply 'mh-exec-cmd-quiet
		      (nconc (list "mark" "-zero" "-seq" (format "%s" seq)
				   "-add" "-nolist")
			     msgs))))
   seq-list))



;;; Issue commands to mh.

(defun mh-exec-cmd (command &rest args)
  "Execute MH command COMMAND with ARGS.  Any output is shown to the user."
  (save-excursion
    (switch-to-buffer-other-window " *mh-temp*")
    (erase-buffer)
    (apply 'call-process (nconc (list (format "%s%s" mh-progs command)
				      nil t nil)
				(mh-list-to-string args)))
    (if (> (buffer-size) 0)
	(sit-for 5))))


(defun mh-exec-cmd-quiet (command &rest args)
  "Execute MH command COMMAND with ARGS.  Output is collected, but not shown
 to the user."
  (save-excursion
    (switch-to-buffer " *mh-temp*")
    (erase-buffer)
    (apply 'call-process (nconc (list (format "%s%s" mh-progs command)
				      nil t nil)
				(mh-list-to-string args)))))


(defun mh-exec-cmd-output (command &rest args)
  "Execute MH command COMMAND with ARGS putting the output into buffer after
point.  Set mark after inserted text."
  (set-mark (point))
  (apply 'call-process (nconc (list (format "%s%s" mh-progs command) nil t nil)
			      (mh-list-to-string args)))
  (exchange-point-and-mark))


(defun mh-exec-cmd-no-wait (command &rest args)
  "Execute MH command COMMAND with ARGS and do not wait until it finishes."
  (apply 'call-process (nconc (list (format "%s%s" mh-progs command) nil 0 nil)
			      (mh-list-to-string args))))



(defun mh-exec-lib-cmd-output (command &rest args)
  "Execute MH library command COMMAND with ARGS.  Put the output into
buffer after point.  Set mark after inserted text."
  (set-mark (point))
  (apply 'call-process (nconc (list (format "%s%s" mh-lib command) nil t nil)
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
  (apply 'mh-exec-cmd (cons "anno" (nconc (list buffer msg) args)))
  (save-excursion
    (switch-to-buffer buffer)
    (if (mh-goto msg t)
	(mh-notate note 5))))


(defun mh-notate (notation offset)
  "Marks the current message with the character NOTATION at position OFFSET."
  (let ((buffer-read-only nil))
    (beginning-of-line)
    (goto-char (+ (point) offset))
    (delete-char 1)
    (insert notation)
    (beginning-of-line)))



;;; User prompting commands.

(defun mh-get-folder-name (prompt default can-create)
  "Prompt for a folder name with PROMPT.  DEFAULT is used if the folder
exists and the user types CR.  If the CAN-CREATE flag is t,
then a non-existant folder is made."
  (let* ((prompt (format "%s folder%s" prompt
			 (if (equal "" default)
			     "? "
			     (format " [%s]? " default))))
	 name)
    (while (and (setq name (completing-read prompt mh-folder-list
					    nil (not can-create)))
		(equal name "")
		(equal default "")))
    (cond ((equal name "")
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
    (mh-exec-cmd-quiet "folders" "-fast" "-norecurse")
    (switch-to-buffer " *mh-temp*")
    (goto-char (point-min))
    (let ((list nil))
      (while (not (eobp))
	(let ((start (point)))
	  (search-forward "\n" nil t)
	  (let ((folder (buffer-substring start (- (point) 1))))
	    (push (list folder) list)
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
ERROR-IF-NO-MESSAGE is t, then complain if the cursor is not pointing to a
message."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "^[0-9a-z]?[ ]+\\([0-9]+\\)")
	   (string-to-int (buffer-substring (match-beginning 1)
					    (match-end 1))))
	  ((looking-at "^\\([0-9]+\\)")
	   (string-to-int (buffer-substring (match-beginning 1)
					    (match-end 1))))
	  (error-if-no-message
	   (error "Cursor not pointing to message"))
	  (t nil))))


(defun mh-msg-search-pat (n)
  "Returns a search pattern for message N in the scan listing."
  (cond ((< n 10) (format "^...%d" n))
	((< n 100) (format "^..%d" n))
	((< n 1000) (format "^.%d" n))
	(t (format "^%d" n))))


(defun mh-msg-filename ()
  "Returns a string containing the pathname for the file containing the
current message."
  (format "%s%d" mh-folder-filename (mh-get-msg-num t)))


(defun mh-msg-filenames (msgs folder)
  "Returns a string of filenames specifying MSGS in FOLDER."
  (if msgs
      (let ((args ""))
	(while (cdr msgs)
	  (setq args (format "%s%s%d " args folder (car msgs)))
	  (setq msgs (cdr msgs)))
	(format "%s%s%d" args folder (car msgs)))
      ""))


(defun mh-find-path ()
  "Set mh_path from  ~/.mh_profile."
  (save-window-excursion
    (if (not (file-exists-p "~/.mh_profile"))
	(error "Cannot find .mh_profile file."))
    (switch-to-buffer " *mh_profile*")
    (erase-buffer)
    (insert-file-contents "~/.mh_profile")
    (if (equal (setq mh-user-path (mh-get-field "Path:")) "")
	(setq mh-user-path "Mail/")
	(setq mh-user-path (format "%s/" mh-user-path)))
    (if (not (equal (substring mh-user-path 0 1) "/"))
	(setq mh-user-path (format "%s/%s" (getenv "HOME") mh-user-path)))))


(defun mh-get-cur-msg (folder)
  "Returns the number of the 'cur' message in FOLDER."
  (save-excursion
    (switch-to-buffer " *mh_temp*")
    (erase-buffer)
    (mh-exec-cmd-output "pick" folder "cur")
    (string-to-int (buffer-substring (point-min) (point)))))


(defun mh-get-field (field)
  "Find and return the value of field FIELD in the current buffer.
Returns the empty string if the field is not in the message."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (cond ((not (search-forward field nil t)) "")
	  ((looking-at "[\t ]*$") "")
	  (t
	   (re-search-forward "[\t ]*\\([a-zA-z0-9/].*\\)$" nil t)
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

(define-key mh-folder-mode-map " " 'mh-page-msg)
(define-key mh-folder-mode-map "!" 'mh-re-move)
(define-key mh-folder-mode-map "." 'mh-show)
(define-key mh-folder-mode-map "0" 'mh-indicate-seq)
(define-key mh-folder-mode-map "1" 'mh-indicate-seq)
(define-key mh-folder-mode-map "2" 'mh-indicate-seq)
(define-key mh-folder-mode-map "3" 'mh-indicate-seq)
(define-key mh-folder-mode-map "4" 'mh-indicate-seq)
(define-key mh-folder-mode-map "5" 'mh-indicate-seq)
(define-key mh-folder-mode-map "6" 'mh-indicate-seq)
(define-key mh-folder-mode-map "7" 'mh-indicate-seq)
(define-key mh-folder-mode-map "8" 'mh-indicate-seq)
(define-key mh-folder-mode-map "9" 'mh-indicate-seq)
(define-key mh-folder-mode-map ">" 'mh-save-message)
(define-key mh-folder-mode-map "?" 'mh-summary)
(define-key mh-folder-mode-map "\177" 'mh-previous-page)
(define-key mh-folder-mode-map "\e " 'mh-page-digest)
(define-key mh-folder-mode-map "\ef" 'mh-visit-folder)
(define-key mh-folder-mode-map "\ei" 'mh-inc-folder)
(define-key mh-folder-mode-map "\ek" 'mh-kill-folder)
(define-key mh-folder-mode-map "\el" 'mh-list-folders)
(define-key mh-folder-mode-map "\ep" 'mh-renumber-folder)
(define-key mh-folder-mode-map "\er" 'mh-rescan-folder)
(define-key mh-folder-mode-map "\es" 'mh-search-folder)
(define-key mh-folder-mode-map "^" 'mh-move-msg)
(define-key mh-folder-mode-map "a" 'mh-answer)
(define-key mh-folder-mode-map "c" 'mh-copy-msg)
(define-key mh-folder-mode-map "d" 'mh-delete-msg)
(define-key mh-folder-mode-map "e" 'mh-execute-commands)
(define-key mh-folder-mode-map "f" 'mh-forward)
(define-key mh-folder-mode-map "g" 'mh-goto)
(define-key mh-folder-mode-map "l" 'mh-print-msg)
(define-key mh-folder-mode-map "n" 'mh-next-line)
(define-key mh-folder-mode-map "p" 'mh-previous-line)
(define-key mh-folder-mode-map "r" 'mh-redistribute)
(define-key mh-folder-mode-map "s" 'mh-send)
(define-key mh-folder-mode-map "t" 'mh-toggle-summarize)
(define-key mh-folder-mode-map "u" 'mh-undo)
(define-key mh-folder-mode-map "x" 'mh-execute-commands)

;;; Build the letter-mode keymap:

(define-key mh-letter-mode-map "\^C\^C" 'mh-send-letter)
(define-key mh-letter-mode-map "\^C\^Y" 'mh-insert-letter)
(define-key mh-letter-mode-map "\^Cb" 'mh-to-bcc)
(define-key mh-letter-mode-map "\^Cc" 'mh-to-cc)
(define-key mh-letter-mode-map "\^Cf" 'mh-to-fcc)
(define-key mh-letter-mode-map "\^Cq" 'kill-buffer)
(define-key mh-letter-mode-map "\^Cs" 'mh-to-subject)
(define-key mh-letter-mode-map "\^Ct" 'mh-to-to)
(define-key mh-letter-mode-map "\^Cw" 'mh-check-whom)
(define-key mh-letter-mode-map "\^Cy" 'mh-insert-cur-message)

;;; Build the pick-mode keymap:

(define-key mh-pick-mode-map "\^C\^C" 'mh-do-pick-search)
(define-key mh-pick-mode-map "\^Cb" 'mh-to-bcc)
(define-key mh-pick-mode-map "\^Cc" 'mh-to-cc)
(define-key mh-pick-mode-map "\^Cf" 'mh-to-fcc)
(define-key mh-pick-mode-map "\^Cs" 'mh-to-subject)
(define-key mh-pick-mode-map "\^Ct" 'mh-to-to)
(define-key mh-pick-mode-map "\^Cw" 'mh-check-whom)

