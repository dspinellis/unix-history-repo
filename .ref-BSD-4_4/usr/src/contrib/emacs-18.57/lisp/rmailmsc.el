;; Copyright (C) 1985 Free Software Foundation, Inc.

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
