;; "RMAIL" mail reader for Emacs.
;; Copyright (C) 1985, 1988 Free Software Foundation, Inc.

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


;; Global to all RMAIL buffers.  It exists primarily for the sake of
;; completion.  It is better to use strings with the label functions
;; and let them worry about making the label.

(defvar rmail-label-obarray (make-vector 47 0))

;; Named list of symbols representing valid message attributes in RMAIL.

(defconst rmail-attributes
  (cons 'rmail-keywords
	(mapcar '(lambda (s) (intern s rmail-label-obarray))
		'("deleted" "answered" "filed" "forwarded" "unseen" "edited"))))

(defconst rmail-deleted-label (intern "deleted" rmail-label-obarray))

;; Named list of symbols representing valid message keywords in RMAIL.

(defvar rmail-keywords nil)

(defun rmail-add-label (string)
  "Add LABEL to labels associated with current RMAIL message.
Completion is performed over known labels when reading."
  (interactive (list (rmail-read-label "Add label")))
  (rmail-set-label string t))

(defun rmail-kill-label (string)
  "Remove LABEL from labels associated with current RMAIL message.
Completion is performed over known labels when reading."
  (interactive (list (rmail-read-label "Remove label")))
  (rmail-set-label string nil))

(defun rmail-read-label (prompt)
  (if (not rmail-keywords) (rmail-parse-file-keywords))
  (let ((result
	 (completing-read (concat prompt
				  (if rmail-last-label
				      (concat " (default "
					      (symbol-name rmail-last-label)
					      "): ")
				    ": "))
			  rmail-label-obarray
			  nil
			  nil)))
    (if (string= result "")
	rmail-last-label
      (setq rmail-last-label (rmail-make-label result t)))))

(defun rmail-set-label (l state &optional n)
  (rmail-maybe-set-message-counters)
  (if (not n) (setq n rmail-current-message))
  (aset rmail-summary-vector (1- n) nil)
  (let* ((attribute (rmail-attribute-p l))
	 (keyword (and (not attribute)
		       (or (rmail-keyword-p l)
			   (rmail-install-keyword l))))
	 (label (or attribute keyword)))
    (if label
	(let ((omax (- (buffer-size) (point-max)))
	      (omin (- (buffer-size) (point-min)))
	      (buffer-read-only nil)
	      (case-fold-search t))
	  (unwind-protect
	      (save-excursion
		(widen)
		(goto-char (rmail-msgbeg n))
		(forward-line 1)
		(if (not (looking-at "[01],"))
		    nil
		  (let ((start (1+ (point)))
			(bound))
		    (narrow-to-region (point) (progn (end-of-line) (point)))
		    (setq bound (point-max))
		    (search-backward ",," nil t)
		    (if attribute
			(setq bound (1+ (point)))
		      (setq start (1+ (point))))
		    (goto-char start)
;		    (while (re-search-forward "[ \t]*,[ \t]*" nil t)
;		      (replace-match ","))
;		    (goto-char start)
		    (if (re-search-forward
			   (concat ", " (rmail-quote-label-name label) ",")
			   bound
			   'move)
			(if (not state) (replace-match ","))
		      (if state (insert " " (symbol-name label) ",")))
		    (if (eq label rmail-deleted-label)
			(rmail-set-message-deleted-p n state)))))
	    (narrow-to-region (- (buffer-size) omin) (- (buffer-size) omax))
	    (if (= n rmail-current-message) (rmail-display-labels)))))))

;; Commented functions aren't used by RMAIL but might be nice for user
;; packages that do stuff with RMAIL.  Note that rmail-message-labels-p
;; is in rmailsum now.

;(defun rmail-message-attribute-p (attribute &optional n)
;  "Returns t if ATTRIBUTE on NTH or current message."
;  (rmail-message-labels-p (rmail-make-label attribute t) n))

;(defun rmail-message-keyword-p (keyword &optional n)
;  "Returns t if KEYWORD on NTH or current message."
;  (rmail-message-labels-p (rmail-make-label keyword t) n t))

;(defun rmail-message-label-p (label &optional n)
;  "Returns symbol if LABEL (attribute or keyword) on NTH or current message."
;  (rmail-message-labels-p (rmail-make-label label t) n 'all))

;; Not used by RMAIL but might be nice for user package.

;(defun rmail-parse-message-labels (&optional n)
;  "Returns labels associated with NTH or current RMAIL message.
;Results is a list of two lists.  The first is the message attributes
;and the second is the message keywords.  Labels are represented as symbols."
;  (let ((omin (- (buffer-size) (point-min)))
;	(omax (- (buffer-size) (point-max)))
;	(result))	
;    (unwind-protect
;	(save-excursion
;	  (let ((beg (rmail-msgbeg (or n rmail-current-message))))
;	    (widen)
;	    (goto-char beg)
;	    (forward-line 1)
;	    (if (looking-at "[01],")
;		(save-restriction
;		  (narrow-to-region (point) (save-excursion (end-of-line) (point)))
;		  (rmail-nuke-whitespace)
;		  (goto-char (1+ (point-min)))
;		  (list (mail-parse-comma-list) (mail-parse-comma-list))))))
;      (narrow-to-region (- (buffer-size) omin)
;    			 (- (buffer-size) omax))
;      nil)))

(defun rmail-attribute-p (s)
  (let ((symbol (rmail-make-label s)))
    (if (memq symbol (cdr rmail-attributes)) symbol)))

(defun rmail-keyword-p (s)
  (let ((symbol (rmail-make-label s)))
    (if (memq symbol (cdr (rmail-keywords))) symbol)))

(defun rmail-make-label (s &optional forcep)
  (cond ((symbolp s) s)
	(forcep (intern (downcase s) rmail-label-obarray))
	(t  (intern-soft (downcase s) rmail-label-obarray))))

(defun rmail-force-make-label (s)
  (intern (downcase s) rmail-label-obarray))

(defun rmail-quote-label-name (label)
  (regexp-quote (symbol-name (rmail-make-label label t))))

;; Motion on messages with keywords.

(defun rmail-previous-labeled-message (n label)
  "Show previous message with LABEL.  Defaults to last labels used.
With prefix argument N moves backward N messages with these labels."
  (interactive "p\nsMove to previous msg with labels: ")
  (rmail-next-labeled-message (- n) label))

(defun rmail-next-labeled-message (n labels)
  "Show next message with LABEL.  Defaults to last labels used.
With prefix argument N moves forward N messages with these labels."
  (interactive "p\nsMove to next msg with labels: ")
  (if (string= labels "")
      (setq labels rmail-last-multi-labels))
  (or labels
      (error "No labels to find have been specified previously"))
  (setq rmail-last-multi-labels labels)
  (rmail-maybe-set-message-counters)
  (let ((lastwin rmail-current-message)
	(current rmail-current-message)
	(regexp (concat ", ?\\("
			(mail-comma-list-regexp labels)
			"\\),")))
    (save-restriction
      (widen)
      (while (and (> n 0) (< current rmail-total-messages))
	(setq current (1+ current))
	(if (rmail-message-labels-p current regexp)
	    (setq lastwin current n (1- n))))
      (while (and (< n 0) (> current 1))
	(setq current (1- current))
	(if (rmail-message-labels-p current regexp)
	    (setq lastwin current n (1+ n)))))
    (rmail-show-message lastwin)
    (if (< n 0)
	(message "No previous message with labels %s" labels))
    (if (> n 0)
	(message "No following message with labels %s" labels))))

;;; Manipulate the file's Labels option.

;; Return a list of symbols for all
;; the keywords (labels) recorded in this file's Labels option.
(defun rmail-keywords ()
  (or rmail-keywords (rmail-parse-file-keywords)))

;; Set rmail-keywords to a list of symbols for all
;; the keywords (labels) recorded in this file's Labels option.
(defun rmail-parse-file-keywords ()
  (save-restriction
    (save-excursion
      (widen)
      (goto-char 1)
      (setq rmail-keywords
	    (if (search-forward "\nLabels:" (rmail-msgbeg 1) t)
		(progn
		  (narrow-to-region (point) (progn (end-of-line) (point)))
		  (goto-char (point-min))
		  (cons 'rmail-keywords
			(mapcar 'rmail-force-make-label
				(mail-parse-comma-list)))))))))

;; Add WORD to the list in the file's Labels option.
;; Any keyword used for the first time needs this done.
(defun rmail-install-keyword (word)
  (let ((keyword (rmail-make-label word t))
	(keywords (rmail-keywords)))
    (if (not (or (rmail-attribute-p keyword)
		 (rmail-keyword-p keyword)))
	(let ((omin (- (buffer-size) (point-min)))
	      (omax (- (buffer-size) (point-max))))
	  (unwind-protect
	      (save-excursion
		(widen)
		(goto-char 1)
		(let ((case-fold-search t)
		      (buffer-read-only nil))
		  (or (search-forward "\nLabels:" nil t)
		      (progn
			(end-of-line)
			(insert "\nLabels:")))
		  (delete-region (point) (progn (end-of-line) (point)))
		  (setcdr keywords (cons keyword (cdr keywords)))
		  (while (setq keywords (cdr keywords))
		    (insert (symbol-name (car keywords)) ","))
		  (delete-char -1)))
	    (narrow-to-region (- (buffer-size) omin)
			      (- (buffer-size) omax)))))
    keyword))
