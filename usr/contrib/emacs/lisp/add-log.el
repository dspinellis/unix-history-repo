;; Change log maintenance commands for Emacs
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


;; >> Potential lossage if two people edit change log at same time.
;; >> Maybe should lock file and unlock when written out.

(defun add-change-log-entry (&optional whoami)
  "Find change log file and add an entry for today.
With ARG, prompt for name and site of person."
  (interactive "P")
  (let* ((full-name (if whoami
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
    (find-file (expand-file-name
		(read-file-name "Log file (default ChangeLog): "
				nil "ChangeLog")))
    (or (eq major-mode 'indented-text-mode)
	(progn
	  (indented-text-mode)
	  (setq left-margin 8)
	  (setq fill-column 74)))
    (auto-fill-mode 1)
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


