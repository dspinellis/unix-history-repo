;; Lisp interface between GNU Emacs and MEDIT package. Emacs under MDL.
;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Principal author K. Shane Hartman

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


;; >> This package depends on two MDL packages: MEDIT and FORKS which
;; >> can be obtained from the public (network) library at mit-ajax.

(require 'mim-mode)

(defconst medit-zap-file (concat "/tmp/" (getenv "USER") ".medit.mud")
  "File name for data sent to MDL by Medit.")
(defconst medit-buffer "*MEDIT*"
  "Name of buffer in which Medit accumulates data to send to MDL.")
(defconst medit-save-files t
  "If non-nil, Medit offers to save files on return to MDL.")
  
(defun medit-save-define ()
  "Mark the previous or surrounding toplevel object to be sent back to MDL."
  (interactive)
  (save-excursion
      (beginning-of-DEFINE)
      (let ((start (point)))
	(forward-mim-object 1)
	(append-to-buffer medit-buffer start (point))
	(goto-char start)
	(message (buffer-substring start (progn (end-of-line) (point)))))))

(defun medit-save-region (start end)
  "Mark the current region to be sent to back to MDL."
  (interactive "r")
  (append-to-buffer medit-buffer start end)
  (message "Current region saved for MDL."))

(defun medit-save-buffer ()
  "Mark the current buffer to be sent back to MDL."
  (interactive)
  (append-to-buffer medit-buffer (point-min) (point-max))
  (message "Current buffer saved for MDL."))

(defun medit-zap-define-to-mdl ()
  "Return to MDL with surrounding or previous toplevel MDL object."
  (indetarctive)
  (medit-save-defun)
  (medit-go-to-mdl))

(defun medit-zap-region-mdl (start end)
  "Return to MDL with current region."
  (interactive)
  (medit-save-region start end)
  (medit-go-to-mdl))

(defun medit-zap-buffer ()
  "Return to MDL with current buffer."
  (interactive)
  (medit-save-buffer)
  (medit-go-to-mdl))

(defun medit-goto-mdl ()
  "Return from Emacs to superior MDL, sending saved code.
Optionally, offers to save changed files."
  (interactive)
  (let ((buffer (get-buffer medit-buffer)))
  (if buffer
      (save-excursion
	(set-buffer buffer)
	(if (buffer-modified-p buffer)
	    (write-region (point-min) (point-max) medit-zap-file))
	(set-buffer-modified-p nil)
	(erase-buffer)))
  (if medit-save-files (save-some-buffers))
  ;; Note could handle parallel fork by giving argument "%xmdl".  Then
  ;; mdl would have to invoke with "%emacs".
  (suspend-emacs)))

(defconst medit-mode-map nil)
(if (not medit-mode-map)
    (progn
      (setq medit-mode-map (copy-alist mim-mode-map))
      (define-key medit-mode-map "\e\z" 'medit-save-define)
      (define-key medit-mode-map "\e\^z" 'medit-save-buffer)
      (define-key medit-mode-map "\^xz" 'medit-goto-mdl)
      (define-key medit-mode-map "\^xs" 'medit-zap-buffer)))

(defconst medit-mode-hook (and (boundp 'mim-mode-hook) mim-mode-hook) "")
(setq mim-mode-hook '(lambda () (medit-mode)))
	 
(defun medit-mode (&optional state)
  "Major mode for editing text and returning it to a superior MDL.
Like Mim mode, plus these special commands:
\\{medit-mode-map}"
  (interactive)
  (use-local-map medit-mode-map)
  (run-hooks 'medit-mode-hook)
  (setq major-mode 'medit-mode)
  (setq mode-name "Medit"))

(mim-mode)


