;; Tab conversion commands for Emacs
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


(defun untabify (start end)
  "Convert all tabs in region to multiple spaces, preserving columns.
The variable tab-width controls the action."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (search-forward "\t" nil t)	; faster than re-search
	(let ((start (point))
	      (column (current-column))
	      (indent-tabs-mode nil))
	  (skip-chars-backward "\t")
	  (delete-region start (point))
	  (indent-to column))))))

(defun tabify (start end)
  "Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs
when this can be done without changing the column they end at.
The variable tab-width controls the action."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (re-search-forward "[ \t][ \t][ \t]*" nil t)
	(let ((column (current-column))
	      (indent-tabs-mode t))
	  (delete-region (match-beginning 0) (point))
	  (indent-to column))))))
