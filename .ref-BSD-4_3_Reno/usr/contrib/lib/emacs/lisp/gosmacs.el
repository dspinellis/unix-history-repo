;; Rebindings to imitate Gosmacs.
;; Copyright (C) 1986 Free Software Foundation, Inc.

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


(defvar non-gosmacs-binding-alist nil)

(defun set-gosmacs-bindings ()
  "Rebind some keys globally to make GNU Emacs resemble Gosling Emacs.
Use \\[set-gnu-bindings] to restore previous global bindings."
  (interactive)
  (setq non-gosmacs-binding-alist
	(rebind-and-record
	 '(("\C-x\C-e" compile)
	   ("\C-x\C-f" save-buffers-kill-emacs)
	   ("\C-x\C-i" insert-file)
	   ("\C-x\C-m" save-some-buffers)
	   ("\C-x\C-n" next-error)
	   ("\C-x\C-o" switch-to-buffer)
	   ("\C-x\C-r" insert-file)
	   ("\C-x\C-u" undo)
	   ("\C-x\C-v" find-file-other-window)
	   ("\C-x\C-z" shrink-window)
	   ("\C-x!" shell-command)
	   ("\C-xd" delete-window)
	   ("\C-xn" gosmacs-next-window)
	   ("\C-xp" gosmacs-previous-window)
	   ("\C-xz" enlarge-window)
	   ("\C-z" scroll-one-line-up)
	   ("\e\C-c" save-buffers-kill-emacs)
	   ("\e!" line-to-top-of-window)
	   ("\e(" backward-paragraph)
	   ("\e)" forward-paragraph)
	   ("\e?" apropos)
	   ("\eh" delete-previous-word)
	   ("\ej" indent-sexp)
	   ("\eq" query-replace)
	   ("\er" replace-string)
	   ("\ez" scroll-one-line-down)
	   ("\C-_" suspend-emacs)))))

(defun rebind-and-record (bindings)
  "Establish many new global bindings and record the bindings replaced.
Arg is an alist whose elements are (KEY DEFINITION).
Value is a similar alist whose elements describe the same KEYs
but each with the old definition that was replaced,"
  (let (old)
    (while bindings
      (let* ((this (car bindings))
	     (key (car this))
	     (newdef (nth 1 this)))
	(setq old (cons (list key (lookup-key global-map key)) old))
	(global-set-key key newdef))
      (setq bindings (cdr bindings)))
    (nreverse old)))

(defun set-gnu-bindings ()
  "Restore the global bindings that were changed by \\[set-gosmacs-bindings]."
  (interactive)
  (rebind-and-record non-gosmacs-binding-alist))

(defun gosmacs-previous-window ()
  "Select the window above or to the left of the window now selected.
From the window at the upper left corner, select the one at the lower right."
  (interactive)
  (select-window (previous-window)))

(defun gosmacs-next-window ()
  "Select the window below or to the right of the window now selected.
From the window at the lower right corner, select the one at the upper left."
  (interactive)
  (select-window (next-window)))

(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive "p")
  (scroll-up (or arg 1)))

(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive "p")
  (scroll-down (or arg 1)))

(defun line-to-top-of-window ()
  "Scroll the selected window up so that the current line is at the top."
  (interactive)
  (recenter 0))
