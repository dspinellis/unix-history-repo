;;;  mh-e.el	(Version: 2.7)

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
;;;  Rewritten for Gnu Emacs, James Larus 1985.


;;;  NB MH must have been compiled with the MHE compiler flag or several
;;;  features necessary to this program will be missing.



;;; Constants:

;;; Set for local environment:

(defvar mh-progs "/usr/local/mh/"     "Directory containing MH commands")
(defvar mh-lib   "/usr/local/lib/mh/" "Directory of MH library")


;;; Mode hooks:

(defvar mh-folder-mode-hook nil	    "Invoked in mh-folder-mode")
(defvar mh-letter-mode-hook nil     "Invoked in mh-letter-mode")


;;; Personal preferences:

(defvar mh-auto-fill-letters t	    "Invoke auto-fill-mode in letters")
(defvar mh-clean-message-header nil
  "Remove invisible header lines in messages")
(defvar mh-lpr-command-format "lpr -p -J '%s'"
"Format for Unix command line to print a message. The format should be
a unix command line, with the string "%s" where the folder and message
number should appear.")
(defvar mh-summary-height 4 	"Number of lines in summary window")

;;; Real constants:

(defvar mh-cmd-note 4		       "Offset to insert notation")
(defvar mh-invisible-headers
  "^Received: \\|^Message-Id: \\|^Remailed-\\|^Via: \\|^Mail-from: \\|\^Return-
Path: \\|^In-Reply-To: \\|^Resent-"
  "Regexp specifying headers that are not to be shown.")


;;; Global variables:

(defvar mh-user-path  ""	     "User's mail folder")
(defvar mh-last-folder "inbox"	     "Last folder read by mh-rmail")
(defvar mh-last-destination nil	     "Destination of last "move" command")
(defvar	mh-current-folder nil	     "Currently active folder")
(defvar	mh-folder-buffer nil	     "Buffer name of currently active folder")
(defvar mh-show-buffer nil	     "Name of buffer that displays messages")
(defvar mh-letter-mode-map nil	     "Command map for composing mail")

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
  (let ((make-backup-files nil)
	(pop-up-windows t)
	mh-current-folder
	mh-folder-buffer)

    (mh-find-path)
    (save-window-excursion
      (cond (arg
	     (let ((folder (mh-get-folder-name "mh" mh-last-folder t))
		   (range (read-string "range [all]? ")))
	       (mh-scan-folder folder (if (string= range "") "all" range))))
	    (t
	     (mh-make-folder "inbox")
	     (mh-inc-folder)))

      (let ((mh-show-buffer (concat "show-" mh-current-folder)))
	(pop-to-buffer mh-show-buffer)
	(unwind-protect
	    (mh-command-loop)
	  (kill-buffer mh-folder-buffer)
	  (kill-buffer mh-show-buffer)
	  (setq mh-last-folder mh-current-folder))))))


(defun mh-smail ()
  "Send mail using the MH mail system."
  (interactive)
  (let ((make-backup-files nil)
	(pop-up-windows t))
    (mh-find-path)
    (call-interactively 'mh-send)))



;;; User executable mh-e commands:

(defun mh-answer ()
  "Answer a letter."
  (interactive)
  (save-window-excursion
    (let ((msg-filename (mh-msg-filename))
	  (msg (mh-get-msg-num t))
	  (reply-to
	   (mh-get-response
	    "Reply to (f, t, c, ?): "
	    '(?f ?t ?c)
	    "Reply to F(rom), T(o + From), C(c + To + From): ")))
      (message "Composing a reply...")
      (cond ((equal reply-to ?f)
	     (mh-exec-cmd "repl" "-build" mh-folder-buffer msg "-nocc" "all"))
	    ((equal reply-to ?t)
	     (mh-exec-cmd "repl" "-build" mh-folder-buffer msg "-cc" "to"
			  "-nocc" "me"))
	    ((equal reply-to ?c)
	     (mh-exec-cmd "repl" "-build" mh-folder-buffer msg "-cc" "all"
			  "-nocc" "me")))

      (mh-read-file (concat mh-user-path "draft") "draft")
      (delete-other-windows)
      (when (or (zerop (buffer-size))
		(not (y-or-n-p "The file 'draft' exists.  Use for reply? ")))
	  (erase-buffer)
	  (insert-file-contents (concat mh-user-path "reply"))
	  (delete-file (concat mh-user-path "reply")))

      (let ((to-names (mh-get-field "To:"))
	    (cc-names (mh-get-field "Cc:")))
	(goto-char (dot-max))
	(pop-to-buffer "*message*")
	(erase-buffer)
	(if (file-exists-p msg-filename)
	    (insert-file-contents msg-filename)
	    (error "File %s does not exist" msg-filename))
	(goto-char (dot-min))
	(let ((case-fold-search nil))
	  (re-search-forward "^$\\|^-*$"))
	(recenter 0)
	(message "Composing a reply...done")
	(if (mh-compose-and-send-mail "")
	    (mh-annotate "R" mh-folder-buffer msg
			 "-component" "Replied-To:"
			 "-text" (concat to-names
					 (if (string= cc-names "")
					     ""
					     (concat ", " cc-names)))))))))


(defun mh-close-folder ()
  "Process the outstanding delete and move commands in the current folder."
  (interactive)
  (message "closing folder...")
  (mh-process-commands mh-folder-buffer)
  (mh-unmark-all-headers t)
  (mh-regenerate-headers "all")
  (setq mode-line-format (mh-make-mode-line))
  (message "closing folder...done"))


(defun mh-copy-msg (&optional arg)
  "Copy specified message(s) to another folder without deleting them."
  (interactive "P")
  (let ((msgs (if arg
		  (mh-seq-to-msgs (mh-read-seq "Copy"))
		  (mh-get-msg-num t))))
    (mh-exec-cmd-no-wait "refile" msgs "-link" "-src"
			 mh-folder-buffer
			 (format "+%s" (mh-get-folder-name "Copy to" "" t)))))


(defun mh-delete-msg (&optional arg)
  "Marks the specified message(s) for later deletion."
  (interactive "P")
  (let ((msgs (if arg (mh-read-seq "Delete") (mh-get-msg-num t))))
    (push msgs mh-delete-list)
    (if arg
	(mh-notate-seq msgs ?D mh-cmd-note)
	(mh-notate ?D mh-cmd-note))
    (mh-next-line 1)))


(defun mh-exit ()
  "Exit mh-e and process outstanding delete and move commands."
  (interactive)
  (cond ((not (or mh-delete-list mh-move-list))
	 (throw 'exit nil))
	((yes-or-no-p "Exit? ")
	 (mh-process-commands mh-folder-buffer)
	 (throw 'exit nil))))


(defun mh-forward (to subject cc)
  "Forward a letter."
  (interactive "sTo: \nsSubject: \nsCc: ")
  (save-window-excursion
    (let ((msg-filename (mh-msg-filename))
	  (msg (mh-get-msg-num t)))
      (cond ((or (not (file-exists-p (concat mh-user-path "draft")))
		 (y-or-n-p "The file 'draft' exists.  Discard it? "))
	     (mh-exec-cmd "forw" "-build" mh-folder-buffer msg)
	     (mh-read-file (concat mh-user-path "draft") "draft")
	     (mh-insert-fields "To:" to "Subject:" subject "Cc:" cc))
	    (t
	     (mh-read-file (concat mh-user-path "draft") "draft")))

      (goto-char (dot-min))
      (delete-other-windows)
      (if (mh-compose-and-send-mail "")
	  (mh-annotate "F" mh-folder-buffer msg
		       "-component" "Forwared-To:"
		       "-text" (concat to
				       (if (string= cc "")
					   ""
					   (concat ", " cc))))))))


(defun mh-goto (number &optional no-error-if-no-message)
  "Position the cursor at a particular message."
  (interactive "nMessage number? ")
  (pop-to-buffer mh-folder-buffer)
  (let ((starting-place (dot)))
    (goto-char (dot-min))
    (cond ((not (re-search-forward (concat "^\+?[0-9a-z]*[ ]*" number) nil t))
	   (goto-char starting-place)
	   (if (not no-error-if-no-message) (error "No message %d " number)))
	  (t
	   (beginning-of-line)
	   (if (not mh-summarize) (mh-show))))))


(defun mh-inc-folder ()
  "inc(orporate) new mail in the current folder."
  (interactive)
  (mh-get-new-mail))


(defun mh-indicate-seq (&optional arg)
  "Add the specified message(s) to a sequence."
  (interactive "P")
  (let ((seq (mh-letter-to-seq last-input-char)))
    (if (looking-at "^[0-9a-j]")
	(if arg
	    (mh-remove-seq seq)
	    (mh-remove-msg-from-seq (mh-get-msg-num t) seq))
	(mh-add-msg-to-seq (mh-get-msg-num t) seq))))


(defun mh-kill-folder ()
  "Removes the current folder."
  (interactive)
  (cond ((yes-or-no-p "Remove current folder ")
	 (pop-to-buffer " *mh-temp*")
	 (mh-exec-cmd "rmf" (buffer-name))
	 (message "Folder removed")
	 (throw 'exit nil))
	(t
	 (message "Folder not removed"))))


(defun mh-list-folders ()
  "List mail folders."
  (interactive)
  (message "listing folders...")
  (pop-to-buffer " *mh-temp*")
  (erase-buffer)
  (mh-exec-cmd-output "folders")
  (goto-char (dot-min))
  (message "listing folders...done"))


(defun mh-print-msg (&optional arg)
  "Print specified message(s) on a line printer."
  (interactive "P")
  (let ((msgs (if arg
		  (reverse (mh-seq-to-msgs (mh-read-seq "Print")))
		  (list (mh-get-msg-num t)))))
    (message "printing message...")
    (shell-command
     (concat mh-lib "mhl -noclear -nobell "
	     (mh-msg-filenames msgs mh-folder-filename) " | "
	     (format mh-lpr-command-format
		     (if arg
			 "Mail"
			 (concat mh-current-folder "/" (mh-get-msg-num t))))))
    (message "printing message...done")))


(defun mh-move-msg (&optional arg)
  "Move specified message(s) to another folder."
  (interactive "P")
  (let ((msgs (if arg (mh-read-seq "Move") (mh-get-msg-num t))))
    (setq mh-last-destination (mh-get-folder-name "Destination" "" t))
    (mh-refile msgs mh-last-destination)
    (mh-next-line 1)))


(defun mh-next-line (&optional arg)
  "Move to next undeleted message in window and display body if summary
flag set."
  (interactive "p")
  (pop-to-buffer mh-folder-buffer)
  (forward-line (if arg arg 1))
  (if (not (re-search-forward "^....[^D^]" nil 0 arg))
      (progn
	(forward-line -1)
	(message "No more messages"))
      (beginning-of-line))
  (if (not mh-summarize) (mh-show)))


(defun mh-renumber-folder ()
  "Renumber messages in folder to be 1..N."
  (interactive)
  (message "packing buffer...")
  (pop-to-buffer mh-folder-buffer)
  (mh-pack-folder)
  (mh-unmark-all-headers nil)
  (mh-position-to-current)
  (message "packing buffer...done"))


(defun mh-page-digest ()
  "Advance displayed message to next digested message."
  (interactive)
  (save-excursion
    (pop-to-buffer mh-show-buffer)
    (move-to-window-line nil)
    (let ((case-fold-search nil))
      (when (not (search-forward "\nFrom:" nil t))
	(other-window -1)
	(error "No more messages")))
    (recenter 0)
    (other-window -1)))


(defun mh-previous-line (&optional arg)
  "Move to previous message in window and display body if summary flag set."
  (interactive "p")
  (pop-to-buffer mh-folder-buffer)
  (forward-line (- (if arg arg 1)))
  (if (not (re-search-backward "^....[^D^]" nil 0 arg))
      (message "Beginning of messages")
      (if (not mh-summarize) (mh-show))))


(defun mh-previous-page ()
  "Page the displayed message backwards."
  (interactive)
  (save-excursion
    (pop-to-buffer mh-show-buffer)
    (scroll-down nil)
    (other-window -1)))


(defun mh-quit ()
  "Quit mh-e without processing outstanding delete and move commands."
  (interactive)
  (if (and (or mh-delete-list mh-move-list)
	   (not (yes-or-no-p "Quit without processing? ")))
      (mh-process-commands mh-folder-buffer))
  (throw 'exit nil))


(defun mh-rescan-folder (&optional arg)
  "Optionally process commands in current folder and (re)scan it."
  (interactive "P")
  (pop-to-buffer mh-folder-buffer)
  (if (and (or mh-delete-list mh-move-list)
	   (y-or-n-p "Process commands? "))
      (mh-process-commands mh-folder-buffer))
  (mh-regenerate-headers (if arg (read-string "Range? ") "all"))
  (setq mode-line-format (mh-make-mode-line))
  (mh-unmark-all-headers nil)
  (mh-position-to-current))


(defun mh-redistribute (to cc)
  "Redistribute a letter."
  (interactive "sTo: \nsCc: ")
  (save-window-excursion
    (let ((msg-filename (mh-msg-filename))
	  (msg (mh-get-msg-num t)))
      (mh-read-file (concat mh-user-path "draft") "draft")
      (delete-other-windows)
      (when (or (zerop (buffer-size))
		(not (y-or-n-p "The file 'draft' exists.  Redistribute? ")))
	  (erase-buffer)
	  (insert-file-contents msg-filename)
	  (goto-char (dot-min))
	  (insert "Resent-To: " to "\n")
	  (if (not (string= cc ""))
	      (insert "Resent-cc: " cc "\n")))

      (if (mh-compose-and-send-mail "-dist")
	  (mh-annotate "F" mh-folder-buffer msg
		       "-component" "Distributed-to:"
		       "-text" (concat to
				       (if (string= cc "")
					   ""
					   (concat ", " cc))))))))


(defun mh-re-move ()
  "Move specified message to same folder as last move."
  (interactive)
  (if (null mh-last-destination)
      (error "No previous move")
      (mh-refile (mh-get-msg-num t) mh-last-destination)))


(defun mh-search-folder ()
  "Search folder for letters matching a pattern."
  (interactive)
  (let* ((range "all")
	 (seq (mh-new-seq))
	 (pattern nil))
    (mh-get-pick-pattern " *pattern*")
    (while (setq pattern (mh-next-pick-field " *pattern*"))
      (setq msgs
	    (mh-seq-from-command seq
				 (nconc (cons "pick" pattern)
					(list (concat "+" mh-current-folder)
					      range
					      "-sequence" seq "-list"))))
      (setq range seq))
    (mh-apply-to-seq seq 'mh-notate  (mh-seq-to-notation seq) 0)))


(defun mh-send (to subject cc)
  "Compose and send a letter."
  (interactive "sTo: \nsSubject: \nsCc: ")
  (message "Composing a message...")
  (save-window-excursion
    (mh-read-file (concat mh-user-path "draft") "draft")
    (delete-other-windows)
    (when (or (zerop (buffer-size))
	      (not (y-or-n-p "The file 'draft' exists.  Use it? ")))
	(erase-buffer)
	(if (file-exists-p (concat mh-user-path "components"))
	    (insert-file-contents (concat mh-user-path "components"))
	    (if (file-exists-p (concat mh-lib "components"))
		(insert-file-contents (concat mh-lib "components"))
		(error "Can't find components")))
	(mh-insert-fields "To:" to "Subject:" subject "Cc:" cc)
	(goto-char (dot-max))
	(message "Composing a message...done"))
    (mh-compose-and-send-mail "")))


(defun mh-show ()
  "Show message indicated by cursor in scan buffer."
  (interactive)
  (setq mh-summarize nil)
  (pop-to-buffer mh-folder-buffer)
  (let ((msgn (mh-get-msg-num t))
	(msg-filename (mh-msg-filename))
	(folder mh-current-folder))
    (if (not (file-exists-p msg-filename))
	(error "Message %d does not exist." msgn))
    (push msgn mh-shown-msgs)
    (switch-to-buffer mh-show-buffer)
    (erase-buffer)
    (insert-file-contents msg-filename)
    (setq buffer-file-name msg-filename)
    (mh-letter-mode)
    (cond (mh-clean-message-header
	   (mh-clean-message-header)
	   (goto-char (dot-min)))
	  (t
	   (let ((case-fold-search nil))
	     (re-search-forward "^To:\\|^From:\\|^Subject:" nil t)
	     (beginning-of-line)
	     (recenter 0))))
    (set-buffer-modified-p nil)
    (setq mode-line-format
	  (concat "{%b}	%[%p of +" folder "/" msgn "%]	%M"))
    ;; These contortions are to force the summary line to be the top window.
    (pop-to-buffer mh-folder-buffer)
    (delete-other-windows)
    (pop-to-buffer mh-show-buffer)
    (pop-to-buffer mh-folder-buffer)
    (shrink-window (- (window-height) mh-summary-height))
    (recenter 1)))


(defun mh-summary ()
  "Show a summary of mh-e commands."
  (interactive)
  (message
"Next Prev Go Del ^ ! Copy Undo . Toggle Ans Forw Redist Send List Quit Exit")
  (sit-for 5))


(defun mh-toggle-summarize ()
  "Turn the summary mode of displaying messages on or off."
  (interactive)
  (setq mh-summarize (not mh-summarize))
  (if (not mh-summarize)
      (mh-show)
      (delete-other-windows)))


(defun mh-undo (&optional arg)
  "Undo the deletion or move of the specified message(s)."
  (interactive "P")
  (cond ((looking-at "^....D")
	 (let ((msgs (if arg (mh-read-seq "undelete") (mh-get-msg-num t))))
	   (setq mh-delete-list (delq msgs mh-delete-list))
	   (if arg
	       (mh-notate-seq msgs ?  mh-cmd-note)
	       (mh-notate ?  mh-cmd-note))))

	((looking-at "^....^")
	 (let ((msgs (if arg (mh-read-seq "unmove") (mh-get-msg-num t))))
	   (mapcar
	    (function (lambda (move) (setcdr msgs (delq msgs (cdr move)))))
	    mh-move-list)
	   (if arg
	       (mh-notate-seq msgs ?  mh-cmd-note)
	       (mh-notate ?  mh-cmd-note))))

	(t nil)))


(defun mh-visit-folder (&optional arg)
  "Visit a new folder."
  (interactive "p")
  (let* (mh-current-folder
	 mh-folder-buffer
	 (folder (mh-get-folder-name "visit" "" t))
	 (mh-show-buffer (concat "show-" folder)))
    (save-window-excursion
      (switch-to-buffer (concat "+" folder))
      (if (> (buffer-size) 0)
	  (error "folder +%s is open. close it before revisiting." folder))
      (mh-scan-folder folder (if arg (read-string "range? ") "all"))
      (pop-to-buffer mh-show-buffer)
      (unwind-protect
	  (mh-command-loop)
	(kill-buffer mh-show-buffer)
	(kill-buffer mh-folder-buffer)))))



;;; Support routines.

(defun mh-command-loop ()
  "Read and execute mh commands."
  (pop-to-buffer mh-folder-buffer)
  (delete-other-windows)
  (recursive-edit))


(defun mh-refile (msgs destination)
  "Refile the msgs in the folder called destination."
  (let ((others (assoc destination mh-move-list)))
    (if others
	(setcdr others (cons msgs (cdr others)))
	(push (cons destination (list msgs)) mh-move-list))
    (if (integerp msgs)
	(mh-notate ?^ mh-cmd-note)
	(mh-notate-seq msgs ?^ mh-cmd-note))))


(defun mh-clean-message-header ()
  "Flush extraneous lines in a message header.  The variable
mh-invisible-headers contains a regular expression specifying these lines."
  (save-restriction
    (goto-char (dot-min))
    (search-forward "\n\n" nil t)
    (narrow-to-region (dot-min) (dot))
    (goto-char (dot-min))
    (while (re-search-forward mh-invisible-headers nil t)
      (beginning-of-line)
      (kill-line 1)
      (while (looking-at "^[ \t]+")
	(beginning-of-line)
	(kill-line 1)))))


(defun mh-read-file (file-name buffer-name)
  "Read file FILE-NAME into buffer BUFFER-NAME.  No errors if disk file
has been modified."
  (pop-to-buffer buffer-name)
  (kill-buffer buffer-name)
  (pop-to-buffer buffer-name)
  (if (file-exists-p file-name)
      (insert-file-contents file-name t)
      (setq buffer-file-name file-name))
  (set-buffer-modified-p nil))



;;; The folder data abstraction.

(defun mh-make-folder (name)
  "Create and initialize a new mail folder called NAME and make
it the current folder."
  (setq mh-current-folder name)
  (setq mh-folder-buffer (concat "+" name))
  (switch-to-buffer mh-folder-buffer)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (mh-folder-mode)
  (erase-buffer)
  (make-variable-buffer-local 'mh-folder-filename)
	;; "e.g./usr/foldbar/Mail/inbox/"
  (setq mh-folder-filename (concat mh-user-path name "/"))
  (make-variable-buffer-local 'mh-summarize)	 ; Show scan list only?
  (setq mh-summarize t)
  (make-variable-buffer-local 'mh-next-seq-num)  ; Index of free sequence id
  (setq mh-next-seq-num 0)
  (make-variable-buffer-local 'mh-delete-list)	 ; List of msgs nums to delete
  (setq mh-delete-list nil)
  (make-variable-buffer-local 'mh-move-list)	 ; Alist of dest . msgs nums
  (setq mh-move-list nil)
  (make-variable-buffer-local 'mh-seq-list)	 ; Alist of seq . msgs nums
  (setq mh-seq-list nil)
  (make-variable-buffer-local 'mh-shown-msgs)	; List of msgs shown
  (setq mh-shown-msgs nil)
  (setq buffer-read-only t))


(defun mh-folder-mode ()
  "    \\[mh-next-line]: next message			\\[mh-previous-line]: p
revious message
    \\[mh-delete-msg]: delete (mark for deletion)	\\[mh-move-msg]: put (m
ark for moving)
    \\[mh-undo]: undo last delete or mark		\\[mh-re-move]: repeat 
last ^ command
    \\[mh-copy-msg]: copy message to another folder
    \\[mh-show]: type message			\\[mh-toggle-summarize]: toggle
 summarize mode
    \\[scroll-other-window]: page message		       \\[mh-previous-p
age]: page message backwards
    \\[mh-print-msg]: print message			\\[mh-goto]: goto a mes
sage
    \\[mh-exit]: exit				\\[mh-quit]: quit
    \\[mh-send]: send a message			\\[mh-redistribute]: redistribu
te a message
    \\[mh-answer]: answer a message 		\\[mh-forward]: forward a messa
ge
  \\[mh-visit-folder]: visit folder		      \\[mh-inc-folder]: inc ma
il
  \\[mh-close-folder]: close folder		      \\[mh-kill-folder]: kill 
folder
  \\[mh-list-folders]: list folders		      \\[mh-renumber-folder]: p
ack folder
  \\[mh-rescan-folder]: rescan folder		      \\[mh-search-folder]: sea
rch folder
Edit the scan list, marking messages.
When you are done, type 'e'.  The messages marked for deletion will be
deleted and messages marked for moving will be moved.
In any of the submodes, such as editing a message or composing a message,
exit with \\[exit-emacs]."
  (auto-save-mode -1)
  (use-local-map mh-keymap)
  (setq major-mode 'mh-folder-mode)
  (setq mode-name "mh-folder")
  (if (and (boundp 'mh-folder-mode-hook) mh-folder-mode-hook)
      (funcall mh-folder-mode-hook)))


(defun mh-scan-folder (folder range)
  "Scan the folder FOLDER over the range RANGE."
  (mh-make-folder folder)
  (mh-regenerate-headers range)
  (when (looking-at "scan: no messages ")
      (let ((buffer-read-only nil))
	(erase-buffer))
      (if (string= range "all")
	  (message  "Folder +%s is empty" folder)
	  (message  "No messages in +%s, range %s" folder range))
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
    (goto-char (dot-min))
    (message (format "scanning %s...done" (buffer-name)))
    ))


(defun mh-get-new-mail ()
  "Read new mail into the current buffer."
  (let ((buffer-read-only nil))
    (message (format "inc %s..." (buffer-name)))
    (goto-char (dot-max))
    (set-mark (dot))
    (mh-exec-cmd-output "inc")
    (message (format "inc %s...done" (buffer-name)))
    (goto-char (mark))
    (cond ((looking-at "inc: no mail")
	   (kill-line 1)
	   (message "No new mail")
	   (sit-for 5))
	  (t
	   (kill-line 2))))
    (setq mode-line-format (mh-make-mode-line)))


(defun mh-make-mode-line ()
  "Returns a string for mode-line-format."
  (save-excursion
   (goto-char (dot-min))
   (set-mark (dot))
   (goto-char (dot-max))
   (let ((lines (count-lines (dot) (mark))))
     (goto-char (dot-min))
     (let ((first (mh-get-msg-num nil))
	   (case-fold-search nil))
       (re-search-forward "[ ]*[0-9]*\\+" nil t)
       (let ((current (mh-get-msg-num nil)))
	 (goto-char (dot-max))
	 (previous-line 1)
	 (let ((last (mh-get-msg-num nil)))
	   (concat "{%b} %[" lines " messages"
		   (if (> lines 0)
		       (concat " (" first " - " last ")")
		       "")
		   (if current
		       (concat " cur = " current)
		       "")
		   "%] ")))))))


(defun mh-unmark-all-headers (remove-all-flags)
  "This function removes all + flags from the headers, and if called
  with a non-nil argument, removes all D and ^ flags too."
  (switch-to-buffer mh-folder-buffer)
  (let ((buffer-read-only nil)
	(case-fold-search nil))
    (goto-char (dot-min))
    (while (if remove-all-flags
	       (re-search-forward "^....\\+" nil t)
	       (re-search-forward "^\\D\\|^\\^\\|^....\\+" nil t))
      (delete-backward-char 1)
      (insert " "))))


(defun mh-position-to-current ()
  "Position the cursor at the current message."
  (let ((curmsg (mh-get-cur-msg mh-folder-filename)))
    (when (or (zerop curmsg) (mh-goto curmsg t))
	(message "No message %s" curmsg)
	(goto-char (dot-max))
	(forward-line -1))
    (when (looking-at "[ ]+[0-9]+")
	(mh-notate ?+ 0)
	(recenter 0))))


(defun mh-pack-folder ()
  "Closes and packs the current folder."
  (let ((buffer-read-only nil))
    (message "closing folder...")
    (mh-process-commands mh-folder-buffer)
    (message "packing folder...")
    (mh-exec-cmd "folder" mh-folder-buffer "-pack")
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
			       (list "-src" (format "%s" buffer)
				     (format "+%s" dest))))))
     mh-move-list)

    ;; Now delete messages
    (if mh-delete-list
	(apply 'mh-exec-cmd
	       (nconc (list "rmm" (format "%s" buffer)) mh-delete-list)))

    ;; Finally update unseen sequence
    (if mh-shown-msgs
	(apply 'mh-exec-cmd-no-wait
	       (nconc (list "show" (format "%s" buffer))
		      mh-shown-msgs
		      (list "-noformat"))))

    (setq mh-delete-list nil
	  mh-move-list nil
	  mh-seq-list nil
	  mh-shown-msgs nil))
  (message "Processing deletes and moves...done"))



;;; Routines for editing a message.

(defun mh-letter-mode ()
  "Mode for composing letters in mh.
^N and ^P work normally in the body of a letter but hug the end of field names
in the header.
^X^C exits and sends a letter."
  (text-mode)
  (if mh-auto-fill-letters
      (auto-fill-mode 1))
  (setq paragraph-separate "^[- \t\^L]*$")
  (setq paragraph-start "^$\\|^\^L\\|^-+$")
  (when (not mh-letter-mode-map)
      (setq mh-letter-mode-map (copy-sequence text-mode-map))
      (define-key mh-letter-mode-map "\^N" 'mh-header-next)
      (define-key mh-letter-mode-map "\^P" 'mh-header-previous))
  (use-local-map mh-letter-mode-map)
  (setq major-mode 'mh-letter-mode)
  (setq mode-name "mh-letter")
  (if (and (boundp 'mh-letter-mode-hook) mh-letter-mode-hook)
      (funcall mh-letter-mode-hook)))


(defun mh-header-next (&optional arg)
  "Modified ^N command that skips to end of header field names."
  (interactive "p")
  (next-line (if arg arg 1))
  (mh-header-line-position))


(defun mh-header-previous (&optional arg)
  "Modified ^P command that skips to end of header field names."
  (interactive "p")
  (previous-line (if arg arg 1))
  (mh-header-line-position))


(defun mh-dot-in-header ()
    "t iff cursor in message header."
    (save-excursion
      (let ((wasdot (dot))
	    (case-fold-search nil))
	(goto-char (dot-min))
	(re-search-forward "^-*$" nil t)
	(beginning-of-line)
	(backward-char 1)
	(>= (dot) wasdot))))


(defun mh-header-line-position ()
  "Position cursor at end of field name when in header."
  (if (mh-dot-in-header)
      (when (save-excursion (beginning-of-line) (not (looking-at " \\|\t")))
	  (beginning-of-line)
	  (search-forward ":" nil t)
	  (if (eolp)
	      (insert " ")
	      (while (looking-at "[ \t]") (forward-char 1))))))



;;; Routines to make a search pattern and search for a message.

(defun mh-get-pick-pattern (buffer)
  "Prompt the user for a pattern to search for in messages.  Upon return,
current buffer contains the filled-in template."
  (save-window-excursion
    (pop-to-buffer buffer)
    (if (or (zerop (buffer-size))
	    (not (y-or-n-p "Reuse pattern? ")))
	(mh-pick-template)
	(message ""))
    (local-set-key "\^X\^C" 'mh-make-pick-pattern)
    (let ((mode-line-format "{%b}\tPick Pattern\t^X^C to exit and search"))
      (catch 'mh-pattern (recursive-edit)))))


(defun mh-make-pick-pattern ()
  (interactive)
  (goto-char (dot-min))
  (throw 'mh-pattern nil))


(defun mh-pick-template ()
  (erase-buffer)
  (insert "From: \n"
	  "To: \n"
	  "Cc: \n"
	  "Date: \n"
	  "Subject: \n"
	  "---------\n")
  (goto-char (dot-min))
  (end-of-line)
  (mh-letter-mode))


(defun mh-next-pick-field (buffer)
  "Return a pattern to search for messages containing the next field, or NIL
if no fields remain."
  (save-excursion
    (pop-to-buffer buffer)
    (let ((pat ())
	  (case-fold-search t))
      (cond ((re-search-forward "^\\([a-z].*\\):[ \t]*\\([a-z0-9].*\\)$" nil t)
	     (region-around-match 1)
	     (let ((component (concat "-" (downcase (region-to-string)))))
	       (region-around-match 2)
	       (setq pat (nconc (list component (region-to-string)) pat)))
	     (forward-line 1)
	     pat)
	    ((re-search-forward "^-*$" nil t)
	     (forward-char 1)
	     (set-mark (dot))
	     (goto-char (dot-max))
	     (let ((body (region-to-string)))
	       (if (> (length body) 0)
		   (list "-search" body)
		   nil)))
	    (t
	     nil)))))



;;; Routines compose and send a letter.

(defun mh-compose-and-send-mail (send-args)
  "Edit a draft message and send or save it.  SEND-ARGS is passed to the
send command.  Returns t if mail is being sent."
  (save-window-excursion
    (pop-to-buffer "draft")
    (mh-letter-mode)
    (local-set-key "\^X\^C" 'mh-send-letter)
    (local-set-key "\^X\^Y" 'mh-insert-letter)
    (mh-header-line-position)
    (let ((mode-line-format
	   "{%b}  %[Mail/draft%] (%p - %m)  (^X^C to finish  ^X^Y to yank msg) 
 %M"))
      (catch 'mail-send (recursive-edit)))))


(defun mh-send-letter ()
  "Prompt the user as to the disposition of the just-composed letter."
  (interactive)
  (save-buffer)
  (let ((mode-line-format "{%b}  %[Mail/draft%]  (%p - %m)  %M")
	(action (mh-get-response
		 "Ready to send. Action (s, q, e, ?): "
		 '(?s ?b ?q ?e ?\^C)
		 "S-end, Q-uit, E-dit ")))
    (cond ((equal action ?s)
	   (message "Sending...")
	   (mh-exec-cmd-no-wait "send" "-push" "-unique" send-args
				(buffer-file-name))
	   (message "Sending...done")
	   (throw 'mail-send t))

	  ((equal action ?q)
	   (message "Not sent... Message remains in buffer draft")
	   (throw 'mail-send nil)))))


(defun mh-insert-letter ()
  "Insert a message in the current letter."
  (interactive)
  (let ((folder (mh-get-folder-name "Message from" mh-current-folder nil))
	(message (string-to-int (read-input "Message number: " ""))))
    (insert-file-contents (concat mh-user-path folder "/" message))))



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


(defun mh-seq-from-command (seq command)
  "Make a sequence called SEQ by executing the form COMMAND."
  (let ((msgs ())
	(case-fold-search t))
    (save-window-excursion
      (apply 'mh-exec-cmd-quiet command)
      (pop-to-buffer " *mh-temp*")
      (goto-char (dot-min))
      (while (re-search-forward "\\([0-9]+\\)" nil t)
	(region-around-match 0)		; BUG in GNU EMACS: should be 1!
	(let ((num (string-to-int (region-to-string))))
	  (if (not (zerop num))
	      (push num msgs)))))

      (push (cons seq msgs) mh-seq-list)
      msgs))


(defun mh-remove-seq (seq)
  "Delete the sequence SEQ."
  (let ((entry (assoc seq mh-seq-list)))
    (setq mh-seq-list (delq (car entry) mh-seq-list))
    (mh-apply-to-seq (mh-seq-msgs (car entry)) 'mh-notate ?  0)))


(defun mh-remove-msg-from-seq (msg-num seq)
  "Remove a message MSG-NUM from the sequence SEQ."
  (let ((seq (assoc seq mh-seq-list)))
    (setcdr (car seq) (delq msg-num (mh-seq-msgs (car seq)))))
  (mh-notate ?  mh-cmd-note))


(defun mh-add-msg-to-seq (msg-num seq)
  "Add a message MSG-NUM to a sequence SEQ."
  (let ((seq-list (assoc seq mh-seq-list)))
    (mh-notate (mh-seq-to-notation seq) 0)
    (if (null seq-list)
	(push (cons seq (list msg-num)) mh-seq-list)
	(setcdr seq-list (cons msg-num (cdr seq-list))))))



(defun mh-new-seq ()
  "Return a new sequence name."
  (save-excursion
    (switch-to-buffer mh-folder-buffer)
    (if (= mh-next-seq-num 10)
	(error "No more sequences"))
    (setq mh-next-seq-num (+ mh-next-seq-num 1))
    (mh-letter-to-seq (+ (1- mh-next-seq-num) ?a))))


(defun mh-letter-to-seq (letter)
  "Given a LETTER, return a string that is a valid sequence name."
  (cond ((and (>= letter ?0) (< letter ?9))
	 (intern (concat "mhe" (char-to-string letter))))
	((and (>= letter ?a) (< letter ?z))
	 (intern (concat "mhe" (char-to-string letter))))
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
				   "-add")
			     msgs))))
   seq-list))



;;; Issue commands to mh.

(defun mh-exec-cmd (command &rest args)
  "Execute MH command COMMAND with ARGS.  Any output is shown to the user."
  (save-excursion
    (pop-to-buffer " *mh-temp*")
    (erase-buffer)
    (set-mark (dot))
    (apply 'call-process (nconc (list (concat mh-progs command) nil t nil)
				(mh-list-to-string args)))
    (when (> (buffer-size) 0)
	(message "%s" (region-to-string))
	(sit-for 5))))


(defun mh-exec-cmd-quiet (command &rest args)
  "Execute MH command COMMAND with ARGS.  Output is collected, but not shown
 to the user."
  (pop-to-buffer " *mh-temp*")
  (erase-buffer)
  (set-mark (dot))
  (apply 'call-process (nconc (list (concat mh-progs command) nil t nil)
			      (mh-list-to-string args))))


(defun mh-exec-cmd-output (command &rest args)
  "Execute MH command COMMAND with ARGS putting the output into the current
buffer."
  (apply 'call-process (nconc (list (concat mh-progs command) nil t nil)
			      (mh-list-to-string args))))


(defun mh-exec-cmd-no-wait (command &rest args)
  "Execute MH command COMMAND with ARGS and do not wait until it finishes."
  (apply 'call-process (nconc (list (concat mh-progs command) nil 0 nil)
			      (mh-list-to-string args))))


(defun mh-list-to-string (l)
  "Flattens the list L and makes every element a string."
  (let ((new-list nil))
    (while l
      (cond ((symbolp (car l)) (push (format "%s" (car l)) new-list))
	    ((numberp (car l)) (push (format "%d" (car l)) new-list))
	    ((string= (car l) ""))
	    ((stringp (car l)) (push (car l) new-list))
	    ((null (car l)))
	    ((listp (car l)) (setq new-list
				   (nconc (mh-list-to-string (car l))
					  new-list)))
	    (t (error "Bad argument %s" (car l))))
      (setq l (cdr l)))
    (nreverse new-list)))



;;; Commands to annotate a message.

(defun mh-annotate (note &rest args)
  "Mark the current message with the character NOTE and annotate the message
with ARGS."
  (apply 'mh-exec-cmd-no-wait (cons "anno" args))
  (mh-notate note 5))


(defun mh-notate (notation offset)
  "Marks the current message with the character NOTATION at position OFFSET."
  (save-excursion
    (pop-to-buffer mh-folder-buffer)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (goto-char (+ (dot) offset))
      (delete-char 1)
      (insert notation)
      (beginning-of-line))))



;;; User prompting commands.

(defun mh-get-folder-name (prompt default can-create)
  "Prompt for a folder name with PROMPT.  DEFAULT is the default folder.
If the CAN-CREATE flag is t, then the folder can be made if it does not exist."
  (let ((exists nil)
	(prompt (concat prompt " folder"
			(if (string= "" default)
			    "? "
			    (concat " [" default "]? "))))
	(file-name))
    (let ((name))
      (while (not exists)
	(setq name (read-string prompt))
	(if (string= name "")
	    (setq name default))
	(if (string= (substring name 0 1) "+")
	    (setq name (substring name 1)))
	(if (not (string= (substring name 0 1) "/"))
	    (setq file-name (concat mh-user-path name))
	    (setq file-name name))
	(setq exists (file-exists-p file-name))
	(if (not exists)
	    (cond ((and can-create
			(y-or-n-p (concat "Folder +" name
					  " does not exist. Create it? ")))
		   (message "Creating %s" name)
		   (call-process "mkdir" nil nil nil file-name)
		   (message "Creating %s...done" name)
		   (setq exists t))

		  (can-create
		   (error ""))

		  (t
		   (setq prompt (concat "Sorry, no such folder as `" name
					"'  Folder name? "))))))
      name)))


(defun mh-get-response (prompt possibilities help)
  "Prints PROMPT, reads a character, and checks it against the list
of POSSIBILITIES. Returns the character if it is legal.  The HELP string
is displayed if the character is not legal."
  (let ((ok nil)
	(first-char))
    (while (not ok)
      (let ((pos possibilities))
	(message prompt)
	(setq first-char (read-char))
	(while (and (not ok) pos)
	  (if (equal first-char (car pos))
	      (setq ok t))
	  (setq pos (cdr pos)))

	(cond ((equal first-char ??)
	       (message help)
	       (sit-for 5))
	      ((not ok)
	       (message "Illegal response '%c'" first-char)
	       (sit-for 5)))))
    first-char))



;;; Misc. functions.

(defun mh-get-msg-num (error-if-no-message)
  "Returns the message number of the current message.  If the argument
ERROR-IF-NO-MESSAGE is t, then complain if the cursor is not pointing to a
message."
  (save-excursion
    (switch-to-buffer mh-folder-buffer)
    (beginning-of-line)
    (cond ((looking-at "^\+?\\([0-9]+\\)")
	   (region-around-match 1)
	   (string-to-int (region-to-string)))
	  ((looking-at "^\+?[0-9a-z]?[ ]+\\([0-9]+\\)")
	   (region-around-match 1)
	   (string-to-int (region-to-string)))
	  (error-if-no-message
	   (error "Cursor not pointing to message"))
	  (t nil))))


(defun mh-msg-filename ()
  "Returns a string containing the pathname for a message."
  (save-excursion
    (switch-to-buffer mh-folder-buffer)
    (concat mh-folder-filename (mh-get-msg-num t))))


(defun mh-msg-filenames (msgs folder)
  "Returns an arglist for ls specifying the messages MSGS in folder FOLDER."
  (if msgs
      (let ((args (concat folder "{")))
	(while (cdr msgs)
	  (setq args (concat args (car msgs) ","))
	  (setq msgs (cdr msgs)))
	(concat args (car msgs) "}"))
      ""))


(defun mh-find-path ()
  "Set mh_path from  ~/.mh_profile."
  (save-window-excursion
    (if (not (file-exists-p "~/.mh_profile"))
	(error "Can find .mh_profile file."))
    (switch-to-buffer " *mh_profile*")
    (erase-buffer)
    (insert-file-contents "~/.mh_profile")
    (if (string= (setq mh-user-path (mh-get-field "Path:")) "")
	(setq mh-user-path "Mail/")
	(setq mh-user-path (concat mh-user-path "/")))
    (if (not (string= (substring mh-user-path 0 1) "/"))
	(setq mh-user-path (concat (getenv "HOME") "/" mh-user-path)))))


(defun mh-get-cur-msg (folder)
  "Returns the cur message from FOLDER."
  (let ((seq-filename (concat folder ".mh_sequences")))
    (save-window-excursion
      (cond ((file-exists-p seq-filename)
	     (switch-to-buffer " *mh_sequences*")
	     (erase-buffer)
	     (insert-file-contents seq-filename)
	     (string-to-int (mh-get-field "cur: ")))
	    (t 0)))))


(defun mh-get-field (field)
  "Get the value of field FIELD in the current buffer."
  (let ((case-fold-search t))
    (goto-char (dot-min))
    (cond ((not (search-forward field nil t)) "")
	  (t
	   (re-search-forward "[\t ]*\\([a-zA-z0-9/].*\\)$" nil t)
	   (region-around-match 1)
	   (let ((field (region-to-string)))
	     (set-mark (dot))
	     (forward-line)
	     (while (looking-at "[ \t]") (forward-line 1))
	     (backward-char 1)
	     (concat field (region-to-string)))))))


(defun mh-insert-fields (&rest name-values)
  "Insert the NAME-VALUE pairs in the current buffer."
  (let ((case-fold-search t))
    (while name-values
      (let ((field-name (car name-values))
	    (value (cadr name-values)))
	(goto-char (dot-min))
	(cond ((not (search-forward (concat "\n" field-name) nil t))
	       (search-forward "---")
	       (beginning-of-line)
	       (insert field-name " " value "\n"))
	      (t
	       (end-of-line)
	       (insert " " value)))
	(setq name-values (cddr name-values))))))



;;; Build the keymap for mh:

(defvar mh-keymap (make-sparse-keymap))

(define-key mh-keymap "?" 'mh-summary)
(define-key mh-keymap  "c" 'mh-copy-msg)
(define-key mh-keymap  "d" 'mh-delete-msg)
(define-key mh-keymap  "^" 'mh-move-msg)
(define-key mh-keymap  "!" 'mh-re-move)
(define-key mh-keymap  "u" 'mh-undo)
(define-key mh-keymap "l" 'mh-print-msg)
(define-key mh-keymap  "p" 'mh-previous-line)
(define-key mh-keymap  "n" 'mh-next-line)
(define-key mh-keymap  "g" 'mh-goto)
(define-key mh-keymap  " " 'scroll-other-window)
(define-key mh-keymap  "\e " 'mh-page-digest)
(define-key mh-keymap  "\^H" 'mh-previous-page)
(define-key mh-keymap  "t" 'mh-toggle-summarize)
(define-key mh-keymap  "." 'mh-show)
(define-key mh-keymap  "a" 'mh-answer)
(define-key mh-keymap  "f" 'mh-forward)
(define-key mh-keymap  "r" 'mh-redistribute)
(define-key mh-keymap  "s" 'mh-send)
(define-key mh-keymap  "\^X\^C" 'mh-quit)
(define-key mh-keymap  "q" 'mh-quit)
(define-key mh-keymap  "e" 'mh-exit)
(define-key mh-keymap "0" 'mh-indicate-seq)
(define-key mh-keymap "1" 'mh-indicate-seq)
(define-key mh-keymap "2" 'mh-indicate-seq)
(define-key mh-keymap "3" 'mh-indicate-seq)
(define-key mh-keymap "4" 'mh-indicate-seq)
(define-key mh-keymap "5" 'mh-indicate-seq)
(define-key mh-keymap "6" 'mh-indicate-seq)
(define-key mh-keymap "7" 'mh-indicate-seq)
(define-key mh-keymap "8" 'mh-indicate-seq)
(define-key mh-keymap "9" 'mh-indicate-seq)
(define-key mh-keymap "\ef" 'mh-visit-folder)
(define-key mh-keymap "\ei" 'mh-inc-folder)
(define-key mh-keymap "\ec" 'mh-close-folder)
(define-key mh-keymap "\ek" 'mh-kill-folder)
(define-key mh-keymap "\el" 'mh-list-folders)
(define-key mh-keymap "\ep" 'mh-renumber-folder)
(define-key mh-keymap "\er" 'mh-rescan-folder)
(define-key mh-keymap "\es" 'mh-search-folder)
