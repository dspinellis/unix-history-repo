;; Fill commands for Emacs
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


(defun set-fill-prefix ()
  "Set the fill-prefix to the current line up to point.
Filling expects lines to start with the fill prefix
and reinserts the fill prefix in each resulting line."
  (interactive)
  (setq fill-prefix (buffer-substring
		     (save-excursion (beginning-of-line) (point))
		     (point)))
  (if (equal fill-prefix "")
      (setq fill-prefix nil))
  (if fill-prefix
      (message "fill-prefix: \"%s\"" fill-prefix)
    (message "fill-prefix cancelled")))

(defun fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph: break lines to fit fill-column.
Prefix arg means justify too.
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (skip-chars-forward "\n")
    (narrow-to-region (point) (point-max))
    (setq from (point))
    (let ((fpre (and fill-prefix (not (equal fill-prefix ""))
		     (regexp-quote fill-prefix))))
      ;; Delete the fill prefix from every line except the first.
      ;; The first line may not even have a fill prefix.
      (and fpre
	   (progn
	     (if (>= (length fill-prefix) fill-column)
		 (error "fill-prefix too long for specified width"))
	     (goto-char (point-min))
	     (forward-line 1)
	     (while (not (eobp))
	       (if (looking-at fpre)
		   (delete-region (point) (match-end 0)))
	       (forward-line 1))
	     (goto-char (point-min))
	     (and (looking-at fpre) (forward-char (length fill-prefix)))
	     (setq from (point)))))
    ;; from is now before the text to fill,
    ;; but after any fill prefix on the first line.

    ;; Make sure sentences ending at end of line get an extra space.
    (goto-char from)
    (while (re-search-forward "[.?!][])""']*$" nil t)
      (insert ? ))
    ;; The change all newlines to spaces.
    (subst-char-in-region from (point-max) ?\n ?\ )
    ;; Flush excess spaces, except in the paragraph indentation.
    (goto-char from)
    (skip-chars-forward " \t")
    (while (re-search-forward "   *" nil t)
      (delete-region
       (+ (match-beginning 0)
	  (if (save-excursion
	       (skip-chars-backward " ])\"'")
	       (memq (preceding-char) '(?. ?? ?!)))
	      2 1))
       (match-end 0)))
    (goto-char (point-max))
    (delete-horizontal-space)
    (insert "  ")
    (goto-char (point-min))
    (let ((prefixcol 0))
      (while (not (eobp))
	(move-to-column (1+ fill-column))
	(if (eobp)
	    nil
	  (skip-chars-backward "^ \n")
	  (if (if (zerop prefixcol) (bolp) (>= prefixcol (current-column)))
	      (skip-chars-forward "^ \n")
	    (forward-char -1)))
	(delete-horizontal-space)
	(insert ?\n)
	(and (not (eobp)) fill-prefix (not (equal fill-prefix ""))
	     (progn
	       (insert fill-prefix)
	       (setq prefixcol (current-column))))
	(and justify-flag (not (eobp))
	     (progn
	       (forward-line -1)
	       (justify-current-line)
	       (forward-line 1)))))))

(defun fill-paragraph (arg)
  "Fill paragraph at or after point.
Prefix arg means justify as well."
  (interactive "P")
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)))
      (backward-paragraph)
      (fill-region-as-paragraph (point) end arg))))

(defun fill-region (from to &optional justify-flag)
  "Fill each of the paragraphs in the region.
Prefix arg (non-nil third arg, if called from program)
means justify as well."
  (interactive "r\nP")
  (save-restriction
   (narrow-to-region from to)
   (goto-char (point-min))
   (while (not (eobp))
     (let ((initial (point))
	   (end (progn
		 (forward-paragraph 1) (point))))
       (forward-paragraph -1)
       (if (>= (point) initial)
	   (fill-region-as-paragraph (point) end justify-flag)
	 (goto-char end))))))

(defun justify-current-line ()
  "Add spaces to line point is in, so it ends at fill-column."
  (interactive)
  (save-excursion
   (save-restriction
    (let (ncols beg)
      (beginning-of-line)
      (forward-char (length fill-prefix))
      (skip-chars-forward " \t")
      (setq beg (point))
      (end-of-line)
      (narrow-to-region beg (point))
      (goto-char beg)
      (while (re-search-forward "   *" nil t)
	(delete-region
	 (+ (match-beginning 0)
	    (if (save-excursion
		 (skip-chars-backward " ])\"'")
		 (memq (preceding-char) '(?. ?? ?!)))
		2 1))
	 (match-end 0)))
      (goto-char beg)
      (while (re-search-forward "[.?!][])""']*\n" nil t)
	(forward-char -1)
	(insert ? ))
      (goto-char (point-max))
      (setq ncols (- fill-column (current-column)))
      (if (search-backward " " nil t)
	  (while (> ncols 0)
	    (let ((nmove (+ 3 (% (random) 3))))
	      (while (> nmove 0)
		(or (search-backward " " nil t)
		    (progn
		     (goto-char (point-max))
		     (search-backward " ")))
		(skip-chars-backward " ")
		(setq nmove (1- nmove))))
	    (insert " ")
	    (skip-chars-backward " ")
	    (setq ncols (1- ncols))))))))

(defun fill-individual-paragraphs (min max &optional justifyp mailp)
  "Fill each paragraph in region according to its individual fill prefix.
Calling from a program, pass range to fill as first two arguments.
Optional third and fourth arguments JUSTIFY-FLAG and MAIL-FLAG:
JUSTIFY-FLAG to justify paragraphs (prefix arg),
MAIL-FLAG for a mail message, i. e. don't fill header lines."
  (interactive "r\nP")
  (let (fill-prefix)
    (save-restriction
      (save-excursion
	(narrow-to-region min max)
	(goto-char (point-min))
	(while (progn
		 (skip-chars-forward " \t\n")
		 (not (eobp)))
	  (setq fill-prefix (buffer-substring (point) (progn (beginning-of-line) (point))))
	  (let ((fin (save-excursion (forward-paragraph) (point)))
		(start (point)))
	    (if mailp
		(while (re-search-forward "^[ \t]*[^ \t\n]*:" fin t)
		  (forward-line 1)))
	    (cond ((= start (point))
		   (fill-region-as-paragraph (point) fin justifyp)
		   (goto-char fin)))))))))

