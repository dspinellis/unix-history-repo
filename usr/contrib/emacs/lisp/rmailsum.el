;; "RMAIL" mail reader for Emacs.
;; Copyright (C) 1985 Richard M. Stallman.

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


;; summary things

(defun rmail-summary ()
  "Display a summary of all messages, one line per message."
  (interactive)
  (rmail-new-summary "Full summary" nil))

(defun rmail-summary-by-labels (labels)
  "Display a summary of all messages with one or more LABELS.
LABELS should be a string containing the desired labels, separated by commas."
  (interactive "sLabels to summarize by: ")
  (if (string= labels "")
      (setq labels (or rmail-last-multi-labels
		       (error "No label specified"))))
  (setq rmail-last-multi-labels labels)
  (rmail-new-summary (concat "Summary of " labels)
		     'rmail-message-labels-p
		     (concat ",\\(" (mail-comma-list-regexp labels) "\\),")))

(defun rmail-summary-by-recipients (recipients &optional primary-only)
  "Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers;
but if PRIMARY-ONLY is non-nil (prefix arg given),
 only look in the To and From fields.
RECIPIENTS is a string of names separated by commas."
  (interactive "sRecipients to summarize by:  \nP")
  (rmail-new-summary
   (concat "Summary of " recipients)
   'rmail-message-recipients-p
   (mail-comma-list-regexp recipients) primary-only))

(defun rmail-message-recipients-p (msg recipients &optional primary-only)
  (save-restriction
    (goto-char (rmail-msgbeg msg))
    (search-forward "\n*** EOOH ***\n")
    (narrow-to-region (point) (progn (search-forward "\n\n") (point)))
    (or (string-match recipients (or (mail-fetch-field "To") ""))
	(string-match recipients (or (mail-fetch-field "From") ""))
	(if (not primary-only)
	    (string-match recipients (or (mail-fetch-field "Cc") ""))))))

(defun rmail-new-summary (description function &rest args)
  "Create a summary of selected messages.
DESCRIPTION makes part of the mode line of the summary buffer.
For each message, FUNCTION is applied to the message number and ARGS...
and if the result is non-nil, that message is included.
nil for FUNCTION means all messages."
  (message "Computing summary lines...")
  (or (and rmail-summary-buffer
	   (buffer-name rmail-summary-buffer))
      (setq rmail-summary-buffer
	    (generate-new-buffer (concat (buffer-name) "-summary"))))
  (let (summary-msgs)
    (let ((msgnum 1)
	  (new-summary-line-count 0)
	  (buffer-read-only nil))
      (save-restriction
	(save-excursion
	  (widen)
	  (goto-char (point-min))
	  (while (>= rmail-total-messages msgnum)
	    (if (or (null function)
		    (apply function (cons msgnum args)))
		(setq summary-msgs
		      (cons (rmail-make-summary-line msgnum)
			    summary-msgs)))
	    (setq msgnum (1+ msgnum))))))
    (let ((sbuf rmail-summary-buffer)
	  (rbuf (current-buffer))
	  (total rmail-total-messages)
	  (mesg rmail-current-message))
      (pop-to-buffer sbuf)
      (let (buffer-read-only)
	(erase-buffer)
	(cond (summary-msgs
	       (princ (nreverse summary-msgs) sbuf)
	       (delete-char -1)
	       (subst-char-in-region 1 2 ?\( ?\ ))))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (rmail-summary-mode)
      (rmail-summary-mode-line description)
      (setq rmail-buffer rbuf
	    rmail-total-messages total)
      (if (> total 0)
	  (rmail-summary-goto-msg mesg t))))
  (message "Computing summary lines...done"))

(defun rmail-make-summary-line (msg)
  (let ((line (or (aref rmail-summary-vector (1- msg))
		  (progn
		    (setq new-summary-line-count
			  (1+ new-summary-line-count))
		    (if (zerop (% new-summary-line-count 10))
			(message "Computing summary lines...%d"
				 new-summary-line-count))
		    (rmail-make-summary-line-1 msg)))))
    ;; Fix up the part of the summary that says "deleted" or "unseen".
    (aset line 4
	  (if (rmail-message-deleted-p msg) ?\D
	    (if (= ?0 (char-after (+ 3 (rmail-msgbeg msg))))
		?\- ?\ )))
    line))

(defun rmail-make-summary-line-1 (msg)
  (goto-char (rmail-msgbeg msg))
  (let* ((lim (save-excursion (forward-line 2) (point)))
	 pos
	 (labels
	  (progn
	    (forward-char 3)
	    (concat
;	     (if (save-excursion (re-search-forward ",answered," lim t))
;		 "*" "")
;	     (if (save-excursion (re-search-forward ",filed," lim t))
;		 "!" "")
	     (if (progn (search-forward ",,") (eolp))
		 ""
	       (concat "{"
		       (buffer-substring (point)
					 (progn (end-of-line) (point)))
		       "} ")))))
	 (line
	  (progn
	    (forward-line 1)
	    (if (looking-at "Summary-line: ")
		(progn
		  (goto-char (match-end 0))
		  (setq line
			(buffer-substring (point)
					  (progn (forward-line 1) (point)))))))))
    ;; Obsolete status lines lacking a # should be flushed.
    (and line
	 (not (string-match "#" line))
	 (progn
	   (delete-region (point)
			  (progn (forward-line -1) (point)))
	   (setq line nil)))
    ;; If we didn't get a valid status line from the message,
    ;; make a new one and put it in the message.
    (or line
	(let* ((case-fold-search t)
	       (next (rmail-msgend msg))
	       (beg (if (progn (goto-char (rmail-msgbeg msg))
			       (search-forward "\n*** EOOH ***\n" next t))
			(point)
		      (forward-line 1)
		      (point)))
	       (end (progn (search-forward "\n\n" nil t) (point))))
	  (save-restriction
	    (narrow-to-region beg end)
	    (goto-char beg)
	    (setq line (rmail-make-basic-summary-line)))
	  (goto-char (rmail-msgbeg msg))
	  (forward-line 2)
	  (insert "Summary-line: " line)))
    (setq pos (string-match "#" line))
    (aset rmail-summary-vector (1- msg)
	  (concat (format "%4d  " msg)
		  (substring line 0 pos)
		  labels
		  (substring line (1+ pos))))))

(defun rmail-make-basic-summary-line ()
  (goto-char (point-min))
  (concat (save-excursion
	    (if (not (search-forward "Date:" nil t))
		"      "
	      (if (re-search-forward
		   "\\([^0-9:]\\)\\([0-3]?[0-9]\\)\\([ \t-_]+\\)\\([a-z][a-z][a-z]\\)"
		   (save-excursion (end-of-line) (point)) t)
		  (format "%2s-%3s"
			  (buffer-substring
			   (match-beginning 2) (match-end 2))
			  (buffer-substring
			   (match-beginning 4) (match-end 4)))
		(if (not (re-search-forward
			  "\\([^a-z]\\)\\([a-z][a-z][a-z]\\)\\([a-z \t-_]*\\)\\([0-9][0-9]?\\)"
			  (save-excursion (end-of-line) (point)) t))
		    "??????"
		  (format "%2s-%3s"
			  (buffer-substring
			   (match-beginning 4) (match-end 4))
			  (buffer-substring
			   (match-beginning 2) (match-end 2)))))))
	  "  "
	  (save-excursion
	    (if (not (search-forward "From:" nil t))
		"                         "
	      (progn (skip-chars-forward " \t")
		     (let* ((from (mail-strip-quoted-names
				   (buffer-substring
				    (1- (point))
				    (progn (end-of-line)
					   (skip-chars-backward " ")
					   (point)))))
			    (len (length from))
			    (mch (string-match "[@%]" from))
			    lo)
		       (format "%25s"
			       (if (or (not mch) (<= len 25))
				   (substring from (max 0 (- len 25)))
				 (substring from
					    (setq lo (cond ((< (- mch 9) 0) 0)
							   ((< len (+ mch 16))
							    (- len 25))
							   (t (- mch 9))))
					    (min len (+ lo 25)))))))))
	  "  #"
	  (if (search-forward "Subject:" nil t)
	      (progn (skip-chars-forward " \t")
		     (buffer-substring (point)
				       (progn (end-of-line)
					      (point))))
	    (re-search-forward "[\n][\n]+" nil t)
	    (buffer-substring (point) (progn (end-of-line) (point))))
	  "\n"))

(defun rmail-summary-next-all (&optional number)
  (interactive "p")
  (forward-line (if number number 1))
  (rmail-summary-goto-msg))

(defun rmail-summary-previous-all (&optional number)
  (interactive "p")
  (forward-line (- (if number number 1)))
  (rmail-summary-goto-msg))

(defun rmail-summary-next-msg (&optional number)
  (interactive "p")
  (forward-line 0)
  (and (> number 0) (forward-line 1))
  (let ((count (if (< number 0) (- number) number))
	(search (if (> number 0) 're-search-forward 're-search-backward))
	end)
    (while (and (> count 0) (funcall search "^.....[^D]" nil t))
      (setq count (1- count)))
    (rmail-summary-goto-msg)))

(defun rmail-summary-previous-msg (&optional number)
  (interactive "p")
  (rmail-summary-next-msg (- (if number number 1))))

(defun rmail-summary-delete-forward ()
  (interactive)
  (let (end)
    (rmail-summary-goto-msg)
    (pop-to-buffer rmail-buffer)
    (rmail-delete-message)
    (pop-to-buffer rmail-summary-buffer)
    (let ((buffer-read-only nil))
      (skip-chars-forward " ")
      (skip-chars-forward "[0-9]")
      (delete-char 1)
      (insert "D"))
    (rmail-summary-next-msg 1)))

(defun rmail-summary-undelete ()
  (interactive)
  (let ((buffer-read-only nil))
    (end-of-line)
    (cond ((re-search-backward "\\(^ *[0-9]*\\)\\(D\\)" nil t)
	   (replace-match "\\1 ")
	   (rmail-summary-goto-msg)
	   (pop-to-buffer rmail-buffer)
	   (and (rmail-message-deleted-p rmail-current-message)
		(rmail-undelete-previous-message))
	   (pop-to-buffer rmail-summary-buffer))
	  (t
	   (rmail-summary-goto-msg)))))

(defun rmail-summary-mode ()
  "RMAIL Summary Mode.
A subset of the Rmail mode commands are supported in this mode. 
As commands are issued in the summary buffer the corresponding
mail message is displayed in the rmail buffer.

n       Move to next undeleted message, or arg messages.
p       Move to previous undeleted message, or arg messages.
C-n	Move to next, or forward arg messages.
C-p	Move to previous, or previous arg messages.
j       Jump to the message at the cursor location.
d       Delete the message at the cursor location and move to next message.
u	Undelete this or previous deleted message.
q	Quit Rmail.
x	Exit and kill the summary window.
space   Scroll message in other window forward.
delete  Scroll message backward.

Entering this mode calls value of hook variable rmail-summary-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'rmail-buffer)
  (make-local-variable 'rmail-total-messages)
  (setq major-mode 'rmail-summary-mode)
  (setq mode-name "RMAIL Summary")
  (use-local-map rmail-summary-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'rmail-summary-mode-hook))

(defun rmail-summary-mode-line (description)
  (setq mode-line-format
	(concat "---Emacs: %17b   %M   %[(" description ")%]----%3p-%-")))

(defun rmail-summary-goto-msg (&optional n nowarn)
  (interactive "P")
  (if (consp n) (setq n (prefix-numeric-value n)))
  (if (eobp) (forward-line -1))
  (beginning-of-line)
  (let ((buf rmail-buffer)
	(cur (point))
	(curmsg (string-to-int (buffer-substring (point) (+ 5 (point))))))
    (if (not n)
	(setq n curmsg)
      (if (< n 1)
	  (progn (message "No preceding message")
		 (setq n 1)))
      (if (> n rmail-total-messages)
	  (progn (message "No following message")
		 (goto-char (point-max))
		 (rmail-summary-goto-msg)))
      (goto-char (point-min))
      (if (not (re-search-forward (concat "^ *" (int-to-string n)) nil t))
	  (progn (or nowarn (message "Message %d not found" n))
		 (setq n curmsg)
		 (goto-char cur))))
    (beginning-of-line)
    (skip-chars-forward " ")
    (skip-chars-forward "0-9")
    (save-excursion (if (= (following-char) ?-)
			(let ((buffer-read-only nil))
			  (delete-char 1)
			  (insert " "))))
    (beginning-of-line)
    (pop-to-buffer buf)
    (rmail-show-message n)
    (pop-to-buffer rmail-summary-buffer)))

(defvar rmail-summary-mode-map nil)

(if rmail-summary-mode-map
    nil
  (setq rmail-summary-mode-map (make-keymap))
  (suppress-keymap rmail-summary-mode-map)
  (define-key rmail-summary-mode-map "j" 'rmail-summary-goto-msg)
  (define-key rmail-summary-mode-map "n" 'rmail-summary-next-msg)
  (define-key rmail-summary-mode-map "p" 'rmail-summary-previous-msg)
  (define-key rmail-summary-mode-map "\C-n" 'rmail-summary-next-all)
  (define-key rmail-summary-mode-map "\C-p" 'rmail-summary-previous-all)
  (define-key rmail-summary-mode-map " " 'rmail-summary-scroll-msg-up)
  (define-key rmail-summary-mode-map "q" 'rmail-summary-quit)
  (define-key rmail-summary-mode-map "u" 'rmail-summary-undelete)
  (define-key rmail-summary-mode-map "x" 'rmail-summary-exit)
  (define-key rmail-summary-mode-map "\177" 'rmail-summary-scroll-msg-down)
  (define-key rmail-summary-mode-map "d" 'rmail-summary-delete-forward))

(defun rmail-summary-scroll-msg-up (&optional dist)
  "Scroll message in top window forward"
  (interactive "P")
  (scroll-other-window (if (null dist)
			   nil
			 (prefix-numeric-value dist))))

(defun rmail-summary-scroll-msg-down (&optional dist)
  "Scroll message in top window backward"
  (interactive "P")
  (other-window 1)
  (scroll-down (if (null dist)
		   nil
		 (prefix-numeric-value dist)))
  (other-window 1))

(defun rmail-summary-quit ()
  "Quit out of rmail and rmail summary"
  (interactive)
  (rmail-summary-exit)
  (rmail-quit))

(defun rmail-summary-exit ()
  "Exit rmail summary, remaining within rmail."
  (interactive)
  ;; Switch to the rmail buffer after burying this one.
  ;; Tricky since variable rmail-buffer is local.
  (pop-to-buffer (prog1 rmail-buffer (bury-buffer (current-buffer))))
  (delete-other-windows))
