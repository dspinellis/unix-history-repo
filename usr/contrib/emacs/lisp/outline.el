;; Outline mode commands for Emacs
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


(defvar outline-mode-map nil "")

(if outline-mode-map
    nil
  (setq outline-mode-map (copy-alist text-mode-map))
  (define-key outline-mode-map "\e}" 'next-visible-heading)
  (define-key outline-mode-map "\e{" 'previous-visible-heading)
  (define-key outline-mode-map "\C-c\t" 'show-children)
  (define-key outline-mode-map "\C-c\C-s" 'show-subtree)
  (define-key outline-mode-map "\C-c\C-h" 'hide-subtree))

(defun outline-mode ()
  "Set major mode for editing outlines with selective display.
Headings should be lines starting with one or more asterisks.
Major headings have one asterisk, subheadings two, etc.
Lines not starting with asterisks are body lines.

You can make the body text under a heading, or the subheadings
under a heading, temporarily invisible, or visible again.
Invisible lines are attached to the end of the previous line
so they go with it if you kill it and yank it back.

Commands:
Meta-}   next-visible-heading      move by visible headings
Meta-{   previous-visible-heading  move by visible headings

Meta-x hide-body	make all text invisible (not headings).
Meta-x show-all		make everything in buffer visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
C-c C-h   hide-subtree	make body and subheadings invisible.
C-c C-s   show-subtree	make body and subheadings visible.
C-c C-i   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
hide-entry	make immediately following body invisible.
show-entry	make it visible.
hide-leaves	make body under heading and under its subheadings invisible.
		 The subheadings remain visible.
show-branches	make all subheadings at all levels visible."
  (interactive)
  (kill-all-local-variables)
  (setq selective-display t)
  (use-local-map outline-mode-map)
  (setq mode-name "Outline")
  (setq major-mode 'outline-mode)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat paragraph-start "\\|*"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat paragraph-separate "\\|*"))
  (run-hooks 'text-mode-hook 'outline-mode-hook))

(defun outline-level ()
  (save-excursion
   (- (- (point) (progn (skip-chars-forward "^ \t") (point))))))

(defun next-heading-preface ()
  (if (re-search-forward "[\n\^M]\\*"
			 nil 'move)
      (goto-char (match-beginning 0)))
  (if (memq (preceding-char) '(?\n ?\^M))
      (forward-char -1)))

(defun next-heading ()
  "Move to the next heading line (a line starting with *'s)."
  (interactive)
  (if (re-search-forward "[\n\^M]\\*"
			 nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defun next-visible-heading (arg)
  "Move to the next visible heading line (a line starting with *'s).
With argument, repeats or can move backward if negative."
  (interactive "p")
  (if (< arg 0)
      (beginning-of-line)
    (forward-line 1))
  (re-search-forward "^\\*" nil nil arg)
  (beginning-of-line))

(defun previous-visible-heading (arg)
  "Move to the previous heading line (a line starting with *'s).
With argument, repeats or can move forward if negative."
  (interactive "p")
  (if (> arg 0)
      (beginning-of-line)
    (forward-line 1))
  (re-search-backward "^\\*" nil nil arg)
  (beginning-of-line))

(defun flag-lines-in-region (from to flag)
  (let ((modp (buffer-modified-p)))
    (unwind-protect
     (subst-char-in-region from to
			   (if (= flag ?\n) ?\^M ?\n)
			   flag t)
     (set-buffer-modified-p modp))))

(defun hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (save-excursion
   (flag-lines-in-region (point) (progn (next-heading-preface) (point)) ?\^M)))

(defun show-entry ()
  "Show the body directly following this heading."
  (interactive)
  (save-excursion
   (flag-lines-in-region (point) (progn (next-heading-preface) (point)) ?\n)))

(defun hide-body ()
  "Hide all of buffer except headings."
  (interactive)
  (hide-region-body (point-min) (point-max)))

(defun hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
   (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (not (eobp))
     (flag-lines-in-region (point) (progn (next-heading-preface) (point)) ?\^M)
     (forward-char
      (if (looking-at "[\n\^M][\n\^M]")
	  2 1))))))

(defun show-all ()
  "Show all of the body in the buffer."
  (interactive)
  (flag-lines-in-region (point-min) (point-max) ?\n))

(defun hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (flag-subtree ?\^M))

(defun hide-leaves ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (hide-region-body (point) (progn (end-of-subtree) (point))))

(defun show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (flag-subtree ?\n))

(defun flag-subtree (flag)
  (save-excursion
   (flag-lines-in-region (point)
			 (progn (end-of-subtree) (point))
			 flag)))

(defun end-of-subtree ()
  (beginning-of-line)
  (let ((opoint (point))
	(first t)
	(level (outline-level)))
    (while (and (not (eobp))
		(or first (> (outline-level) level)))
      (setq first nil)
      (next-heading))
    (forward-char -1)
    (if (memq (preceding-char) '(?\n ?\^M))
	(forward-char -1))))

(defun show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (show-children 1000))

(defun show-children (&optional level)
  "Show all direct subheadings of this heading."
  (interactive "p")
  (or level (setq level 1))
  (save-excursion
   (save-restriction
    (beginning-of-line)
    (setq level (+ level (outline-level)))
    (narrow-to-region (point)
		      (progn (end-of-subtree) (1+ (point))))
    (goto-char (point-min))
    (while (and (not (eobp))
		(progn
		 (next-heading)
		 (not (eobp))))
      (if (<= (outline-level) level)
	  (save-excursion
	   (let ((end (1+ (point))))
	     (forward-char -1)
	     (if (memq (preceding-char) '(?\n ?\^M))
		 (forward-char -1))
	     (flag-lines-in-region (point) end ?\n))))))))

