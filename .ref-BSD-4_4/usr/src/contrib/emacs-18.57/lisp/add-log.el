;; Change log maintenance commands for Emacs
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


(defun add-change-log-entry (whoami file-name &optional other-window)
  "Find change log file and add an entry for today.
First arg (interactive prefix) non-nil means prompt for user name and site.
Second arg is file name of change log.
Optional third arg OTHER-WINDOW non-nil means visit in other window."
  (interactive
   (list current-prefix-arg
	 (let ((default
		 (if (eq system-type 'vax-vms) "$CHANGE_LOG$.TXT" "ChangeLog")))
	   (expand-file-name
	    (read-file-name (format "Log file (default %s): " default)
			    nil default)))))
  (let* ((default
	   (if (eq system-type 'vax-vms) "$CHANGE_LOG$.TXT" "ChangeLog"))
	 (full-name (if whoami
			(read-input "Full name: " (user-full-name))
		      (user-full-name)))
	 ;; Note that some sites have room and phone number fields in
	 ;; full name which look silly when inserted.  Rather than do
	 ;; anything about that here, let user give prefix argument so that
	 ;; s/he can edit the full name field in prompter if s/he wants.
	 (login-name (if whoami
			 (read-input "Login name: " (user-login-name))
		       (user-login-name)))
	 (site-name (if whoami
			(read-input "Site name: " (system-name))
		      (system-name))))
    (if (file-directory-p file-name)
	(setq file-name (concat (file-name-as-directory file-name)
				default)))
    (if other-window (find-file-other-window file-name) (find-file file-name))
    (or (eq major-mode 'indented-text-mode)
	(progn
	  (indented-text-mode)
	  (setq left-margin 8)
	  (setq fill-column 74)))
    (auto-fill-mode 1)
    (undo-boundary)
    (goto-char (point-min))
    (if (not (and (looking-at (substring (current-time-string) 0 10))
		  (save-excursion (re-search-forward "(.* at")
				  (skip-chars-backward "^(")
				  (looking-at login-name))))
	(progn (insert (current-time-string)
		       "  " full-name
		       "  (" login-name
		       " at " site-name ")\n\n")))
    (goto-char (point-min))
    (forward-line 1)
    (while (looking-at "\\sW")
      (forward-line 1))
    (delete-region (point)
		   (progn
		     (skip-chars-backward "\n")
		     (point)))
    (open-line 3)
    (forward-line 2)
    (indent-to left-margin)
    (insert "* ")))

(defun add-change-log-entry-other-window ()
  "Find change log file in other window, and add an entry for today."
  (interactive)
  (add-change-log-entry nil default-directory t))
