;; Copyright (C) 1985 Free Software Foundation, Inc.

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


(defun set-rmail-inbox-list (file-name)
  "Set the inbox list of the current RMAIL file to FILE-NAME.  This may be
a list of file names separated by commas.  If FILE-NAME is empty, remove
any inbox list."
  (interactive "sSet mailbox list to (comma-separated list of filenames): ")
  (save-excursion
    (let ((names (rmail-parse-file-inboxes))
	  (standard-output nil))
      (if (or (not names)
	      (y-or-n-p (concat "Replace "
				(mapconcat 'identity names ", ")
				"? ")))
	  (let ((buffer-read-only nil))
	    (widen)
	    (goto-char (point-min))
	    (search-forward "\n\^_")
	    (re-search-backward "^Mail" nil t)
	    (forward-line 0)
	    (if (looking-at "Mail:")
		(delete-region (point)
			       (progn (forward-line 1)
				      (point))))
	    (if (not (string= file-name ""))
		(insert "Mail: " file-name "\n"))))))
  (setq rmail-inbox-list (rmail-parse-file-inboxes))
  (rmail-show-message rmail-current-message))
