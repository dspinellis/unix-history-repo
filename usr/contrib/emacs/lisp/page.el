;; Page motion commands for emacs.
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


(defun forward-page (&optional count)
  "Move forward to page boundary.  With arg, repeat, or go back if negative.
A page boundary is any line whose beginning matches the regexp  page-delimiter."
  (interactive "p")
  (or count (setq count 1))
  (while (and (> count 0) (not (eobp)))
    (if (re-search-forward page-delimiter nil t)
	nil
      (goto-char (point-max)))
    (setq count (1- count)))
  (while (and (< count 0) (not (bobp)))
    (forward-char -1)
    (if (re-search-backward page-delimiter nil t)
	(goto-char (match-end 0))
      (goto-char (point-min)))
    (setq count (1+ count))))

(defun backward-page (&optional count)
  "Move backward to page boundary.  With arg, repeat, or go fwd if negative.
A page boundary is any line whose beginning matches the regexp  page-delimiter."
  (interactive "p")
  (or count (setq count 1))
  (forward-page (- count)))

(defun mark-page (&optional arg)
  "Put mark at end of page, point at beginning.
A numeric arg specifies to move forward or backward by that many pages,
thus marking a page other than the one point was originally in."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (if (> arg 0)
      (forward-page arg)
    (if (< arg 0)
        (forward-page (1- arg))))
  (forward-page)
  (set-mark (point))
  (forward-page -1))

(defun narrow-to-page (&optional arg)
  "Make text outside current page invisible.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
   (if (> arg 0)
       (forward-page arg)
     (if (< arg 0)
	 (forward-page (1- arg))))
   (forward-page)
   (beginning-of-line)
   (narrow-to-region (point)
		     (progn
		      (forward-page -1)
		      (point)))))

(defun count-lines-page ()
  "Report number of lines on current page, and how many are before or after point."
  (interactive)
  (save-excursion
    (let ((opoint (point)) beg end
	  total before after)
      (forward-page)
      (beginning-of-line)
      (setq end (point))
      (backward-page)
      (setq beg (point))
      (setq total (count-lines beg end)
	    before (count-lines beg opoint)
	    after (count-lines opoint end))
      (message "Page has %d lines (%d + %d)" total before after))))

(defun what-page ()
  "Print page and line number of point."
  (interactive)
  (let ((count 1) opoint)
    (save-restriction
      (widen)
      (save-excursion
	(beginning-of-line)
	(setq opoint (point))
	(goto-char 1)
	(while (re-search-forward page-delimiter opoint t)
	  (setq count (1+ count)))
	(message "Page %d, line %d" count (1+ (count-lines (point) opoint)))))))
