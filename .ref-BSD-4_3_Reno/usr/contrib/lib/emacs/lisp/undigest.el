;; "RMAIL" mail reader for Emacs.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

;; note Interent RFP934

(defun undigestify-rmail-message ()
  "Break up a digest message into its constituent messages.
Leaves original message, deleted, before the undigestified messages."
  (interactive)
  (widen)
  (let ((buffer-read-only nil)
	(msg-string (buffer-substring (rmail-msgbeg rmail-current-message)
				      (rmail-msgend rmail-current-message))))
    (goto-char (rmail-msgend rmail-current-message))
    (narrow-to-region (point) (point))
    (insert msg-string)
    (narrow-to-region (point-min) (1- (point-max))))
  (let ((error t)
	(buffer-read-only nil))
    (unwind-protect
	(progn
	  (save-restriction
	    (goto-char (point-min))
	    (delete-region (point-min)
			   (progn (search-forward "\n*** EOOH ***\n")
				  (point)))
	    (insert "\^_\^L\n0, unseen,,\n*** EOOH ***\n")
	    (narrow-to-region (point)
			      (point-max))
	    (let* ((fill-prefix "")
		   (case-fold-search t)
		   (digest-name
		    (mail-strip-quoted-names
		     (or (save-restriction
			   (search-forward "\n\n")
			   (narrow-to-region (point-min) (point))
			   (goto-char (point-max))
			   (or (mail-fetch-field "Reply-To")
			       (mail-fetch-field "To")
			       (mail-fetch-field "Apparently-To")))
			 (error "Message is not a digest")))))
	      (save-excursion
		(goto-char (point-max))
		(skip-chars-backward " \t\n")
		(let ((count 10) found)
		  ;; compensate for broken un*x digestifiers.  Sigh Sigh.
		  (while (and (> count 0) (not found))
		    (forward-line -1)
		    (setq count (1- count))
		    (if (looking-at (concat "End of.*Digest.*\n"
					    (regexp-quote "*********") "*"
					    "\\(\n------*\\)*"))
			(setq found t)))
		  (if (not found) (error "Message is not a digest"))))
	      (re-search-forward (concat "^" (make-string 55 ?-) "-*\n*"))
	      (replace-match "\^_\^L\n0, unseen,,\n*** EOOH ***\n")
	      (save-restriction
		(narrow-to-region (point)
				  (progn (search-forward "\n\n")
					 (point)))
		(if (mail-fetch-field "To") nil
		  (goto-char (point-min))
		  (insert "To: " digest-name "\n")))
	      (while (re-search-forward
		      (concat "\n\n" (make-string 27 ?-) "-*\n*")
		      nil t)
		(replace-match "\n\n\^_\^L\n0, unseen,,\n*** EOOH ***\n")
		(save-restriction
		  (if (looking-at "End ")
		      (insert "To: " digest-name "\n\n")
		    (narrow-to-region (point)
				      (progn (search-forward "\n\n"
							     nil 'move)
					     (point))))
		  (if (mail-fetch-field "To") nil
		    (goto-char (point-min))
		    (insert "To: " digest-name "\n"))))))
	  (setq error nil)
	  (message "Message successfully undigestified")
	  (let ((n rmail-current-message))
	    (rmail-forget-messages)
	    (rmail-show-message n)
	    (rmail-delete-forward)))
      (cond (error
	     (narrow-to-region (point-min) (1+ (point-max)))
	     (delete-region (point-min) (point-max))
	     (rmail-show-message rmail-current-message))))))

