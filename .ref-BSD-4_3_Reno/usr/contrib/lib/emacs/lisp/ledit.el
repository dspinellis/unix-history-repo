;; Emacs side of ledit interface
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


;;; To do:
;;; o lisp -> emacs side of things (grind-definition and find-definition)

(defvar ledit-mode-map nil)

(defconst ledit-zap-file (concat "/tmp/" (getenv "USER") ".l1")
  "File name for data sent to Lisp by Ledit.")
(defconst ledit-read-file (concat "/tmp/" (getenv "USER") ".l2")
  "File name for data sent to Ledit by Lisp.")
(defconst ledit-compile-file 
  (concat "/tmp/" (getenv "USER") ".l4")
  "File name for data sent to Lisp compiler by Ledit.")
(defconst ledit-buffer "*LEDIT*"
  "Name of buffer in which Ledit accumulates data to send to Lisp.")
;These are now in loaddefs.el
;(defconst ledit-save-files t
;  "*Non-nil means Ledit should save files before transferring to Lisp.")
;(defconst ledit-go-to-lisp-string "%?lisp"
;  "*Shell commands to execute to resume Lisp job.")
;(defconst ledit-go-to-liszt-string "%?liszt"
;  "*Shell commands to execute to resume Lisp compiler job.")

(defun ledit-save-defun ()
  "Save the current defun in the ledit buffer"
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (append-to-buffer ledit-buffer (point) end))
   (message "Current defun saved for Lisp")))

(defun ledit-save-region (beg end)
  "Save the current region in the ledit buffer"
  (interactive "r")
  (append-to-buffer ledit-buffer beg end)
  (message "Region saved for Lisp"))

(defun ledit-zap-defun-to-lisp ()
  "Carry the current defun to lisp"
  (interactive)
  (ledit-save-defun)
  (ledit-go-to-lisp))

(defun ledit-zap-defun-to-liszt ()
  "Carry the current defun to liszt"
  (interactive)
  (ledit-save-defun)
  (ledit-go-to-liszt))

(defun ledit-zap-region-to-lisp (beg end)
  "Carry the current region to lisp"
  (interactive "r")
  (ledit-save-region beg end)
  (ledit-go-to-lisp))

(defun ledit-go-to-lisp ()
  "Suspend Emacs and restart a waiting Lisp job."
  (interactive)
  (if ledit-save-files
      (save-some-buffers))
  (if (get-buffer ledit-buffer)
      (save-excursion
       (set-buffer ledit-buffer)
       (goto-char (point-min))
       (write-region (point-min) (point-max) ledit-zap-file)
       (erase-buffer)))
  (suspend-emacs ledit-go-to-lisp-string)
  (load ledit-read-file t t))

(defun ledit-go-to-liszt ()
  "Suspend Emacs and restart a waiting Liszt job."
  (interactive)
  (if ledit-save-files
      (save-some-buffers))
  (if (get-buffer ledit-buffer)
      (save-excursion
       (set-buffer ledit-buffer)
       (goto-char (point-min))
       (insert "(declare (macros t))\n")
       (write-region (point-min) (point-max) ledit-compile-file)
       (erase-buffer)))
  (suspend-emacs ledit-go-to-liszt-string)
  (load ledit-read-file t t))

(defun ledit-setup ()
  "Set up key bindings for the Lisp / Emacs interface"
  (if (not ledit-mode-map)
      (progn (setq ledit-mode-map (make-sparse-keymap))
	     (lisp-mode-commands ledit-mode-map)))
  (define-key ledit-mode-map "\e\^d" 'ledit-save-defun)
  (define-key ledit-mode-map "\e\^r" 'ledit-save-region)
  (define-key ledit-mode-map "\^xz" 'ledit-go-to-lisp)
  (define-key ledit-mode-map "\e\^c" 'ledit-go-to-liszt))

(ledit-setup)

(defun ledit-mode ()
  "Major mode for editing text and stuffing it to a Lisp job.
Like Lisp mode, plus these special commands:
  M-C-d	-- record defun at or after point
	   for later transmission to Lisp job.
  M-C-r -- record region for later transmission to Lisp job.
  C-x z -- transfer to Lisp job and transmit saved text.
  M-C-c -- transfer to Liszt (Lisp compiler) job
	   and transmit saved text.
\\{ledit-mode-map}
To make Lisp mode automatically change to Ledit mode,
do (setq lisp-mode-hook 'ledit-from-lisp-mode)"
  (interactive)
  (lisp-mode)
  (ledit-from-lisp-mode))

(defun ledit-from-lisp-mode ()
  (use-local-map ledit-mode-map)
  (setq mode-name "Ledit")
  (setq major-mode 'ledit-mode)
  (run-hooks 'ledit-mode-hook))
