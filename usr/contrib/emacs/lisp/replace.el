;; Replace commands for Emacs.
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


(fset 'delete-non-matching-lines 'keep-lines)
(defun keep-lines (regexp)
  "Delete lines not containing matches for REGEXP.
Applies to lines after point."
  (interactive "sKeep lines (containing match for regexp): ")
  (save-excursion
    (while (not (eobp))
      (let ((end (scan-buffer (point) 1 ?\n)))
	(if (re-search-forward regexp end t)
	    (goto-char end)
	  (delete-region (point)
			 (if (re-search-forward regexp nil t)
			     (progn (beginning-of-line) (point))
			   (point-max))))))))

(fset 'delete-matching-lines 'flush-lines)
(defun flush-lines (regexp)
  "Delete lines containing matches for REGEXP.
Applies to lines after point."
  (interactive "sFlush lines (containing match for regexp): ")
  (save-excursion
    (while (and (not (eobp))
		(re-search-forward regexp nil t))
      (beginning-of-line)
      (delete-region (point)
		     (progn (forward-line 1) (point))))))

(fset 'count-matches 'how-many)
(defun how-many (regexp)
  "Print number of matches for REGEXP following point."
  (interactive "sHow many (matches for regexp): ")
  (let ((count 0) opoint)
    (save-excursion
     (while (and (not (eobp))
		 (progn (setq opoint (point))
			(re-search-forward regexp nil t)))
       (if (= opoint (point))
	   (forward-char 1)
	 (setq count (1+ count))))
     (message "%d occurrences" count))))

(fset 'list-matching-lines 'occur)
(defun occur (regexp &optional nlines)
  "Show all lines containing of REGEXP following point.
Display each line with NLINES lines before and after.
NLINES defaults to 0.  Interactively it is the prefix arg."
  (interactive "sOccur (show lines matching regexp): \nP")
  (setq nlines (if nlines (prefix-numeric-value nlines) 0))
  (let ((first t))
   (with-output-to-temp-buffer "*Occur*"
    (save-excursion
     (while (re-search-forward regexp nil t)
      (let ((buffer (current-buffer))
	    (start
	     (save-excursion
	      (beginning-of-line)
	      (forward-line (- nlines))
	      (point)))
	    (end
	     (save-excursion
	      (forward-line (1+ nlines))
	      (point))))
	(save-excursion
	 (set-buffer standard-output)
	 (or first
	     (insert "--------\n"))
	 (setq first nil)
	 (insert-buffer-substring buffer start end))
	(forward-line 1)))))))

(defconst query-replace-help
  "Type Space to replace one match, Delete to skip to next,
ESC to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r to enter recursive edit (\\[exit-recursive-edit] to get out again),
C-w to delete match and recursive edit,
! to replace all remaining matches with no more questions,
^ to move point back to previous match."
  "Help message while in query-replace")

(defun perform-replace (from-string to-string
		        query-flag regexp-flag delimited-flag)
  (let ((nocasify (not (and case-fold-search case-replace
			    (string-equal from-string
					  (downcase from-string)))))
	(literal (not regexp-flag))
	(search-function (if regexp-flag 're-search-forward 'search-forward))
	(search-string from-string)
	(keep-going t)
	(lastrepl nil)			;Position after last match considered.
	(help-form
	 '(concat "Query replacing "
		  (if regexp-flag "regexp " "")
		  from-string " with " to-string ".\n\n"
		  (substitute-command-keys query-replace-help))))
    (if delimited-flag
	(setq search-function 're-search-forward
	      search-string (concat "\\b"
				    (if regexp-flag from-string
				      (regexp-quote from-string))
				    "\\b")))
    (push-mark)
    (push-mark)
    (while (and keep-going
		(not (eobp))
		(progn
		 (set-mark (point))
		 (funcall search-function search-string nil t)))
      ;; Don't replace the null string 
      ;; right after end of previous replacement.
      (if (eq lastrepl (point))
	  (forward-char 1)
	(undo-boundary)
	(if (not query-flag)
	    (replace-match to-string nocasify literal)
	  (let (done replaced)
	    (while (not done)
	      (message "Query replacing %s with %s: " from-string to-string)
	      ;; Preserve the match data.  Process filters and sentinels
	      ;; could run inside read-char..
	      (let ((data (match-data)))
		(setq char (read-char))
		(store-match-data data))
	      (cond ((not (memq char '(?\e ?\ ?\, ?\. ?! ?\177 ?\C-r ?\C-w ?^)))
		     (setq keep-going nil)
		     (setq unread-command-char char)
		     (setq done t))
		    ((= char ?\e)
		     (setq keep-going nil)
		     (setq done t))
		    ((= char ?^)
		     (goto-char (mark))
		     (setq replaced t))
		    ((= char ?\ )
		     (or replaced
			 (replace-match to-string nocasify literal))
		     (setq done t))
		    ((= char ?\.)
		     (or replaced
			 (replace-match to-string nocasify literal))
		     (setq keep-going nil)
		     (setq done t))
		    ((and (not replaced) (= char ?\,))
		     (replace-match to-string nocasify literal)
		     (setq replaced t))
		    ((= char ?!)
		     (or replaced
			 (replace-match to-string nocasify literal))
		     (setq done t query-flag nil))
		    ((= char ?\177)
		     (setq done t))
		    ((= char ?\C-r)
		     (store-match-data
		       (prog1 (match-data)
			 (save-excursion (recursive-edit)))))
		    ((= char ?\C-w)
		     (delete-region (match-beginning 0) (match-end 0))
		     (store-match-data
		       (prog1 (match-data)
			 (save-excursion (recursive-edit))))
		     (setq replaced t))))))
	(setq lastrepl (point))))
    (pop-mark)
    (message "Done")
    keep-going))
