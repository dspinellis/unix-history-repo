;; Copyright (C) 1985 N. Ziring and Richard M. Stallman.

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


;; GNU Emacs major mode for editing nroff source

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
  (setq page-delimiter "^\\.bp")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^\\.\\|" paragraph-start))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat "^\\.\\|" paragraph-separate))
  (run-hooks 'text-mode-hook 'nroff-mode-hook))

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
    (while (and (not (eobp)) (looking-at "\\.."))
      (forward-line 1))
    (setq cnt (- cnt 1)))
  (while (and (< cnt 0) (not (bobp)))
    (forward-line -1)
    (while (and (not (bobp))
		(looking-at "\\.."))
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
    (".DS" . ".DE")
    (".KS" . ".KE")
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
				       nroff-brace-table))))))
    (if (null completion)
	(newline (prefix-numeric-value arg))
      (insert "\n" completion "\n"))))

(defun electric-nroff-mode (arg)
  "Toggle nroff-electric-newline minor mode
Nroff-electric-newline forces emacs to check for an nroff
request at the beginning of the line, and insert the
matching closing request if necessary.  
This command toggles that mode (off->on, on->off), 
with an argument, turns it on iff arg is positive, otherwise off."
     (interactive "P")
     (setq nroff-electric-mode
	   (cond
	    ((null arg) (null nroff-electric-mode))
	    (t (> (prefix-numeric-value arg) 0))))
     (set-minor-mode 'electric-nroff-mode "electric" nroff-electric-mode))

