;; GNU Emacs major mode for editing nroff source
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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



(defvar nroff-mode-abbrev-table nil
  "Abbrev table used while in nroff mode.")

(defvar nroff-mode-map nil
     "Major mode keymap for nroff-mode buffers")
(if (not nroff-mode-map)
    (progn
      (setq nroff-mode-map (make-sparse-keymap))
      (define-key nroff-mode-map "\t"  'tab-to-tab-stop)
      (define-key nroff-mode-map "\es" 'center-line)
      (define-key nroff-mode-map "\e?" 'count-text-lines)
      (define-key nroff-mode-map "\n"  'electric-nroff-newline)
      (define-key nroff-mode-map "\en" 'forward-text-line)
      (define-key nroff-mode-map "\ep" 'backward-text-line)))

(defun nroff-mode ()
  "Major mode for editing text intended for nroff to format.
\\{nroff-mode-map}
Turning on Nroff mode runs text-mode-hook, then nroff-mode-hook.
Also, try nroff-electric-mode, for automatically inserting
closing requests for requests that are used in matched pairs."
  (interactive)
  (kill-all-local-variables)
  (use-local-map nroff-mode-map)
  (setq mode-name "Nroff")
  (setq major-mode 'nroff-mode)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table nroff-mode-abbrev-table)
  (make-local-variable 'nroff-electric-mode)
  ;; now define a bunch of variables for use by commands in this mode
  (make-local-variable 'page-delimiter)
  (setq page-delimiter "^\\.\\(bp\\|SK\\|OP\\)")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[.']\\|" paragraph-start))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat "^[.']\\|" paragraph-separate))
  ;; comment syntax added by mit-erl!gildea 18 Apr 86
  (make-local-variable 'comment-start)
  (setq comment-start "\\\" ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\\\\"[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 24)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'nroff-comment-indent)
  (run-hooks 'text-mode-hook 'nroff-mode-hook))

;;; Compute how much to indent a comment in nroff/troff source.
;;; By mit-erl!gildea April 86
(defun nroff-comment-indent ()
  "Compute indent for an nroff/troff comment.
Puts a full-stop before comments on a line by themselves."
  (let ((pt (point)))
    (unwind-protect
	(progn
	  (skip-chars-backward " \t")
	  (if (bolp)
	      (progn
		(setq pt (1+ pt))
		(insert ?.)
		1)
	    (if (save-excursion
		  (backward-char 1)
		  (looking-at "^[.']"))
		1
	      (max comment-column
		   (* 8 (/ (+ (current-column)
			      9) 8)))))) ; add 9 to ensure at least two blanks
      (goto-char pt))))

(defun count-text-lines (start end &optional print)
  "Count lines in region, except for nroff request lines.
All lines not starting with a period are counted up.
Interactively, print result in echo area.
Noninteractively, return number of non-request lines from START to END."
  (interactive "r\np")
  (if print
      (message "Region has %d text lines" (count-text-lines start end))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(- (buffer-size) (forward-text-line (buffer-size)))))))

(defun forward-text-line (&optional cnt)
  "Go forward one nroff text line, skipping lines of nroff requests.
An argument is a repeat count; if negative, move backward."
  (interactive "p")
  (if (not cnt) (setq cnt 1))
  (while (and (> cnt 0) (not (eobp)))
    (forward-line 1)
    (while (and (not (eobp)) (looking-at "[.']."))
      (forward-line 1))
    (setq cnt (- cnt 1)))
  (while (and (< cnt 0) (not (bobp)))
    (forward-line -1)
    (while (and (not (bobp))
		(looking-at "[.']."))
      (forward-line -1))
    (setq cnt (+ cnt 1)))
  cnt)

(defun backward-text-line (&optional cnt)
  "Go backward one nroff text line, skipping lines of nroff requests.
An argument is a repeat count; negative means move forward."
  (interactive "p")
  (forward-text-line (- cnt)))

(defconst nroff-brace-table
  '((".(b" . ".)b")
    (".(l" . ".)l")
    (".(q" . ".)q")
    (".(c" . ".)c")
    (".(x" . ".)x")
    (".(z" . ".)z")
    (".(d" . ".)d")
    (".(f" . ".)f")
    (".LG" . ".NL")
    (".SM" . ".NL")
    (".LD" . ".DE")
    (".CD" . ".DE")
    (".BD" . ".DE")
    (".DS" . ".DE")
    (".DF" . ".DE")
    (".FS" . ".FE")
    (".KS" . ".KE")
    (".KF" . ".KE")
    (".LB" . ".LE")
    (".AL" . ".LE")
    (".BL" . ".LE")
    (".DL" . ".LE")
    (".ML" . ".LE")
    (".RL" . ".LE")
    (".VL" . ".LE")
    (".RS" . ".RE")
    (".TS" . ".TE")
    (".EQ" . ".EN")
    (".PS" . ".PE")
    (".BS" . ".BE")
    (".G1" . ".G2")			; grap
    (".na" . ".ad b")
    (".nf" . ".fi")
    (".de" . "..")))

(defun electric-nroff-newline (arg)
  "Insert newline for nroff mode; special if electric-nroff mode.
In electric-nroff-mode, if ending a line containing an nroff opening request,
automatically inserts the matching closing request after point."
  (interactive "P")
  (let ((completion (save-excursion
		      (beginning-of-line)
		      (and (null arg)
			   nroff-electric-mode
			   (<= (point) (- (point-max) 3))
			   (cdr (assoc (buffer-substring (point)
							 (+ 3 (point)))
				       nroff-brace-table)))))
	(needs-nl (not (looking-at "[ \t]*$"))))
    (if (null completion)
	(newline (prefix-numeric-value arg))
      (save-excursion
	(insert "\n\n" completion)
	(if needs-nl (insert "\n")))
      (forward-char 1))))

(defun electric-nroff-mode (&optional arg)
  "Toggle nroff-electric-newline minor mode
Nroff-electric-newline forces emacs to check for an nroff
request at the beginning of the line, and insert the
matching closing request if necessary.  
This command toggles that mode (off->on, on->off), 
with an argument, turns it on iff arg is positive, otherwise off."
  (interactive "P")
  (or (eq major-mode 'nroff-mode) (error "Must be in nroff mode"))
  (or (assq 'nroff-electric-mode minor-mode-alist)
      (setq minor-mode-alist (append minor-mode-alist
				     (list '(nroff-electric-mode
					     " Electric")))))
  (setq nroff-electric-mode
	(cond ((null arg) (null nroff-electric-mode))
	      (t (> (prefix-numeric-value arg) 0)))))

