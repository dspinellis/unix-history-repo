;; "RMAIL edit mode"  Edit the current message.
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


(require 'rmail)

(defvar rmail-edit-map nil)
(if rmail-edit-map
    nil
  (setq rmail-edit-map (copy-keymap text-mode-map))
  (define-key rmail-edit-map "\C-c\C-c" 'rmail-cease-edit)
  (define-key rmail-edit-map "\C-c\C-]" 'rmail-abort-edit))

(defun rmail-edit-mode ()
  "Major mode for editing the contents of an RMAIL message.
The editing commands are the same as in Text mode, together with two commands
to return to regular RMAIL:
  *  rmail-abort-edit cancels the changes
     you have made and returns to RMAIL
  *  rmail-cease-edit makes them permanent.
\\{rmail-edit-map}"
  (use-local-map rmail-edit-map)
  (setq major-mode 'rmail-edit-mode)
  (setq mode-name "RMAIL Edit")
  (setq mode-line-format default-mode-line-format)
  (run-hooks 'text-mode-hook 'rmail-edit-mode-hook))

(defun rmail-edit-current-message ()
  "Edit the contents of this message."
  (interactive)
  (rmail-edit-mode)
  (make-local-variable 'rmail-old-text)
  (setq rmail-old-text (buffer-substring (point-min) (point-max)))
  (setq buffer-read-only nil)
  (set-buffer-modified-p (buffer-modified-p))
  ;; Make mode line update.
  (if (and (eq (key-binding "\C-c\C-c") 'rmail-cease-edit)
	   (eq (key-binding "\C-c\C-]") 'rmail-abort-edit))
      (message "Editing: Type C-c C-c to return to Rmail, C-c C-] to abort")
    (message (substitute-command-keys
	       "Editing: Type \\[rmail-cease-edit] to return to Rmail, \\[rmail-abort-edit] to abort"))))

(defun rmail-cease-edit ()
  "Finish editing message; switch back to Rmail proper."
  (interactive)
  ;; Make sure buffer ends with a newline.
  (save-excursion
    (goto-char (point-max))
    (if (/= (preceding-char) ?\n)
	(insert "\n"))
    ;; Adjust the marker that points to the end of this message.
    (set-marker (aref rmail-message-vector (1+ rmail-current-message))
		(point)))
  ;; Update the mode line.
  (set-buffer-modified-p (buffer-modified-p))
  (rmail-mode-1)
  (cond ((boundp 'rmail-summary-vector)
	 (aset rmail-summary-vector (1- rmail-current-message) nil)
	 (save-excursion
	   (rmail-widen-to-current-msgbeg
	    (function (lambda ()
			(forward-line 2)
			(if (looking-at "Summary-line: ")
			    (let ((buffer-read-only nil))
			      (delete-region (point)
					     (progn (forward-line 1) (point))))))))
	   (rmail-show-message))))
  (setq buffer-read-only t))

(defun rmail-abort-edit ()
  "Abort edit of current message; restore original contents."
  (interactive)
  (delete-region (point-min) (point-max))
  (insert rmail-old-text)
  (rmail-cease-edit))

