;; List one abbrev table alphabetically ordered.
;; Copyright (C) 1986 Free Software Foundation, Inc.
;; Suggested by a previous version by Gildea.

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


(provide 'abbrevlist)

(defun list-one-abbrev-table (abbrev-table output-buffer)
  "Display alphabetical listing of ABBREV-TABLE in buffer BUFFER."
  (with-output-to-temp-buffer output-buffer
    (save-excursion
      (let ((abbrev-list nil) (first-column 0))
	(set-buffer standard-output)
	(mapatoms 
	  (function (lambda (abbrev)
		      (setq abbrev-list (cons abbrev abbrev-list))))
	  abbrev-table)
	(setq abbrev-list (sort abbrev-list 'string-lessp))
	(while abbrev-list
	  (if (> (+ first-column 40) (screen-width))
	      (progn
		(insert "\n")
		(setq first-column 0)))
	  (indent-to first-column)
	  (insert (symbol-name (car abbrev-list)))
	  (indent-to (+ first-column 8))
	  (insert (symbol-value (car abbrev-list)))
	  (setq first-column (+ first-column 40))
	  (setq abbrev-list (cdr abbrev-list)))))))
