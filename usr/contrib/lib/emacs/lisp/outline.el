;; Outline mode commands for Emacs
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

;; Jan '86, Some new features added by Peter Desnoyers and rewritten by RMS.
  
(defvar outline-regexp "[*\^l]+"
  "*Regular expression to match the beginning of a heading line.
Any line whose beginning matches this regexp is considered a heading.
The recommended way to set this is with a Local Variables: list
in the file it applies to.")

(defvar outline-mode-map nil "")

(if outline-mode-map
    nil
  (setq outline-mode-map (copy-keymap text-mode-map))
  (define-key outline-mode-map "\C-c\C-n" 'outline-next-visible-heading)
  (define-key outline-mode-map "\C-c\C-p" 'outline-previous-visible-heading)
  (define-key outline-mode-map "\C-c\C-i" 'show-children)
  (define-key outline-mode-map "\C-c\C-s" 'show-subtree)
  (define-key outline-mode-map "\C-c\C-h" 'hide-subtree)
  (define-key outline-mode-map "\C-c\C-u" 'outline-up-heading)
  (define-key outline-mode-map "\C-c\C-f" 'outline-forward-same-level)
  (define-key outline-mode-map "\C-c\C-b" 'outline-backward-same-level))

(defun outline-mode ()
  "Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines. 

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end 
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:
C-c C-n   outline-next-visible-heading      move by visible headings
C-c C-p   outline-previous-visible-heading
C-c C-f   outline-forward-same-level        similar but skip subheadings
C-c C-b   outline-backward-same-level
C-c C-u   outline-up-heading		    move from subheading to heading

Meta-x hide-body	make all text invisible (not headings).
Meta-x show-all		make everything in buffer visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
C-c C-h   hide-subtree	make body and subheadings invisible.
C-c C-s   show-subtree	make body and subheadings visible.
C-c C-i   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
M-x hide-entry	   make immediately following body invisible.
M-x show-entry	   make it visible.
M-x hide-leaves	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
M-x show-branches  make all subheadings at all levels visible.

The variable outline-regexp can be changed to control what is a heading.
A line is a heading if outline-regexp matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of text-mode-hook and then of
outline-mode-hook, if they are non-nil."
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
  (setq paragraph-start (concat paragraph-start "\\|^\\("
				outline-regexp "\\)"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat paragraph-separate "\\|^\\("
				   outline-regexp "\\)"))
  (run-hooks 'text-mode-hook 'outline-mode-hook))

(defun outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.
This is actually the length of whatever outline-regexp matches."
  (save-excursion
    (looking-at outline-regexp)
    (- (match-end 0) (match-beginning 0))))

(defun outline-next-preface ()
  "Skip forward to just before the next heading line."
  (if (re-search-forward (concat "[\n\^M]\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (match-beginning 0)))
  (if (memq (preceding-char) '(?\n ?\^M))
      (forward-char -1)))

(defun outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  (if (re-search-forward (concat "[\n\^M]\\(" outline-regexp "\\)")
			 nil 'move)
      (goto-char (1+ (match-beginning 0)))))

(defun outline-back-to-heading ()
  "Move to previous (possibly invisible) heading line,
or to beginning of this line if it is a heading line."
  (beginning-of-line)
  (or (outline-on-heading-p)
      (re-search-backward (concat "^\\(" outline-regexp "\\)") nil 'move)))

(defun outline-on-heading-p ()
  "Return T if point is on a header line."
  (save-excursion
    (beginning-of-line)
    (and (eq (preceding-char) ?\n)
	 (looking-at outline-regexp))))

(defun outline-next-visible-heading (arg)
  "Move to the next visible heading line.
With argument, repeats or can move backward if negative.
A heading line is one that starts with a `*' (or that outline-regexp matches)."
  (interactive "p")
  (if (< arg 0)
      (beginning-of-line)
    (end-of-line))
  (re-search-forward (concat "^\\(" outline-regexp "\\)") nil nil arg)
  (beginning-of-line))

(defun outline-previous-visible-heading (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that outline-regexp matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg)))

(defun outline-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is `\\n' (newline character) then text is shown,
while if FLAG is `\\^M' (control-M) the text is hidden."
  (let ((modp (buffer-modified-p)))
    (unwind-protect
        (subst-char-in-region from to
			      (if (= flag ?\n) ?\^M ?\n)
			      flag t)
     (set-buffer-modified-p modp))))

(defun hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (outline-back-to-heading)
  (save-excursion
   (outline-flag-region (point) (progn (outline-next-preface) (point)) ?\^M)))

(defun show-entry ()
  "Show the body directly following this heading."
  (interactive)
  (save-excursion
   (outline-flag-region (point) (progn (outline-next-preface) (point)) ?\n)))

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
	(outline-flag-region (point) (progn (outline-next-preface) (point)) ?\^M)
	(if (not (eobp))
	    (forward-char
	     (if (looking-at "[\n\^M][\n\^M]")
		 2 1)))))))

(defun show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (outline-flag-region (point-min) (point-max) ?\n))

(defun hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree ?\^M))

(defun hide-leaves ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (outline-back-to-heading)
  (hide-region-body (point) (progn (outline-end-of-subtree) (point))))

(defun show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree ?\n))

(defun outline-flag-subtree (flag)
  (save-excursion
    (outline-back-to-heading)
    (outline-flag-region (point)
			  (progn (outline-end-of-subtree) (point))
			  flag)))

(defun outline-end-of-subtree ()
  (beginning-of-line)
  (let ((opoint (point))
	(first t)
	(level (outline-level)))
    (while (and (not (eobp))
		(or first (> (outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (forward-char -1)
    (if (memq (preceding-char) '(?\n ?\^M))
	(forward-char -1))))

(defun show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (show-children 1000))

(defun show-children (&optional level)
  "Show all direct subheadings of this heading.  Optional LEVEL specifies
how many levels below the current level should be shown."
  (interactive "p")
  (or level (setq level 1))
  (save-excursion
   (save-restriction
    (beginning-of-line)
    (setq level (+ level (outline-level)))
    (narrow-to-region (point)
		      (progn (outline-end-of-subtree) (1+ (point))))
    (goto-char (point-min))
    (while (and (not (eobp))
		(progn
		 (outline-next-heading)
		 (not (eobp))))
      (if (<= (outline-level) level)
	  (save-excursion
	   (let ((end (1+ (point))))
	     (forward-char -1)
	     (if (memq (preceding-char) '(?\n ?\^M))
		 (forward-char -1))
	     (outline-flag-region (point) end ?\n))))))))

(defun outline-up-heading (arg)
  "Move to the heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  (outline-back-to-heading)
  (if (eq (outline-level) 1)
      (error ""))
    (while (and (> (outline-level) 1)
		(> arg 0)
		(not (bobp)))
      (let ((present-level (outline-level)))
	(while (not (< (outline-level) present-level))
	  (outline-previous-visible-heading 1))
	(setq arg (- arg 1)))))

(defun outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading from here of the same level as the
present one. It stops at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-next-sibling))))  
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error ""))))))

(defun outline-get-next-sibling ()
  "Position the point at the next heading of the same level, 
and return that position or nil if it cannot be found."
  (let ((level (outline-level)))
    (outline-next-visible-heading 1)
    (while (and (> (outline-level) level)
		(not (eobp)))
      (outline-next-visible-heading 1))
    (if (< (outline-level) level)
	nil
      (point))))
	
(defun outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading from here of the same level as the
present one. It stops at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-last-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error ""))))))

(defun outline-get-last-sibling ()
  "Position the point at the previous heading of the same level, 
and return that position or nil if it cannot be found."
  (let ((level (outline-level)))
    (outline-previous-visible-heading 1)
    (while (and (> (outline-level) level)
		(not (bobp)))
      (outline-previous-visible-heading 1))
    (if (< (outline-level) level)
	nil
        (point))))

