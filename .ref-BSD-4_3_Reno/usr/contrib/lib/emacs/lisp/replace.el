;; Replace commands for Emacs.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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
  "Delete all lines except those containing matches for REGEXP.
A match split across lines preserves all the lines it lies in.
Applies to all lines after point."
  (interactive "sKeep lines (containing match for regexp): ")
  (save-excursion
    (or (bolp) (forward-line 1))
    (let ((start (point)))
      (while (not (eobp))
	;; Start is first char not preserved by previous match.
	(if (not (re-search-forward regexp nil 'move))
	    (delete-region start (point-max))
	  (let ((end (save-excursion (goto-char (match-beginning 0))
				     (beginning-of-line)
				     (point))))
	    ;; Now end is first char preserved by the new match.
	    (if (< start end)
		(delete-region start end))))
	(setq start (save-excursion (forward-line 1)
				    (point)))))))

(fset 'delete-matching-lines 'flush-lines)
(defun flush-lines (regexp)
  "Delete lines containing matches for REGEXP.
If a match is split across lines, all the lines it lies in are deleted.
Applies to lines after point."
  (interactive "sFlush lines (containing match for regexp): ")
  (save-excursion
    (while (and (not (eobp))
		(re-search-forward regexp nil t))
      (delete-region (save-excursion (goto-char (match-beginning 0))
				     (beginning-of-line)
				     (point))
		     (progn (forward-line 1) (point))))))

(fset 'count-matches 'how-many)
(defun how-many (regexp)
  "Print number of matches for REGEXP following point."
  (interactive "sHow many matches for (regexp): ")
  (let ((count 0) opoint)
    (save-excursion
     (while (and (not (eobp))
		 (progn (setq opoint (point))
			(re-search-forward regexp nil t)))
       (if (= opoint (point))
	   (forward-char 1)
	 (setq count (1+ count))))
     (message "%d occurrences" count))))

(defvar occur-mode-map ())
(if occur-mode-map
    ()
  (setq occur-mode-map (make-sparse-keymap))
  (define-key occur-mode-map "\C-c\C-c" 'occur-mode-goto-occurrence))

(defvar occur-buffer nil)
(defvar occur-nlines nil)
(defvar occur-pos-list nil)

(defun occur-mode ()
  "Major mode for output from \\[occur].
Move point to one of the occurrences in this buffer,
then use \\[occur-mode-goto-occurrence] to go to the same occurrence
in the buffer that the occurrences were found in.
\\{occur-mode-map}"
  (kill-all-local-variables)
  (use-local-map occur-mode-map)
  (setq major-mode 'occur-mode)
  (setq mode-name "Occur")
  (make-local-variable 'occur-buffer)
  (make-local-variable 'occur-nlines)
  (make-local-variable 'occur-pos-list))

(defun occur-mode-goto-occurrence ()
  "Go to the line this occurrence was found in, in the buffer it was found in."
  (interactive)
  (if (or (null occur-buffer)
	  (null (buffer-name occur-buffer)))
      (progn
	(setq occur-buffer nil
	      occur-pos-list nil)
	(error "Buffer in which occurences were found is deleted.")))
  (let* ((occur-number (/ (1- (count-lines (point-min) (point)))
			  (cond ((< occur-nlines 0)
				 (- 2 occur-nlines))
				((> occur-nlines 0)
				 (+ 2 (* 2 occur-nlines)))
				(t 1))))
	 (pos (nth occur-number occur-pos-list)))
    (pop-to-buffer occur-buffer)
    (goto-char (marker-position pos))))

(defvar list-matching-lines-default-context-lines 0
  "*Default number of context lines to include around a list-matching-lines
match.  A negative number means to include that many lines before the match.
A positive number means to include that many lines both before and after.")

(fset 'list-matching-lines 'occur)
(defun occur (regexp &optional nlines)
  "Show all lines following point containing a match for REGEXP.
Display each line with NLINES lines before and after,
 or -NLINES before if NLINES is negative.
NLINES defaults to list-matching-lines-default-context-lines.
Interactively it is the prefix arg.

The lines are shown in a buffer named *Occur*.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  (interactive "sList lines matching regexp: \nP")
  (setq nlines (if nlines (prefix-numeric-value nlines)
		 list-matching-lines-default-context-lines))
  (let ((first t)
	(buffer (current-buffer))
	linenum prevpos)
    (save-excursion
      (beginning-of-line)
      (setq linenum (1+ (count-lines (point-min) (point))))
      (setq prevpos (point)))
    (with-output-to-temp-buffer "*Occur*"
      (save-excursion
	(set-buffer standard-output)
	(insert "Lines matching ")
	(prin1 regexp)
	(insert " in buffer " (buffer-name buffer) ?. ?\n)
	(occur-mode)
	(setq occur-buffer buffer)
	(setq occur-nlines nlines)
	(setq occur-pos-list ()))
      (if (eq buffer standard-output)
	  (goto-char (point-max)))
      (save-excursion
	(while (re-search-forward regexp nil t)
	  (beginning-of-line 1)
	  (save-excursion
	    (setq linenum (+ linenum (count-lines prevpos (point))))
	    (setq prevpos (point)))
	  (let* ((start (save-excursion
			  (forward-line (if (< nlines 0) nlines (- nlines)))
			  (point)))
		 (end (save-excursion
			(if (> nlines 0)
			    (forward-line (1+ nlines))
			    (forward-line 1))
			(point)))
		 (tag (format "%3d" linenum))
		 (empty (make-string (length tag) ?\ ))
		 tem)
	    (save-excursion
	      (setq tem (make-marker))
	      (set-marker tem (point))
	      (set-buffer standard-output)
	      (setq occur-pos-list (cons tem occur-pos-list))
	      (or first (zerop nlines)
		  (insert "--------\n"))
	      (setq first nil)
	      (insert-buffer-substring buffer start end)
	      (backward-char (- end start))
	      (setq tem (if (< nlines 0) (- nlines) nlines))
	      (while (> tem 0)
		(insert empty ?:)
		(forward-line 1)
		(setq tem (1- tem)))
	      (insert tag ?:)
	      (forward-line 1)
	      (while (< tem nlines)
		(insert empty ?:)
		(forward-line 1)
		(setq tem (1+ tem))))				
	    (forward-line 1)))
	(set-buffer standard-output)
	;; Put positions in increasing order to go with buffer.
	(setq occur-pos-list (nreverse occur-pos-list))
	(if (interactive-p)
	    (message "%d matching lines." (length occur-pos-list)))))))

(defconst query-replace-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
ESC or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r to enter recursive edit (\\[exit-recursive-edit] to get out again),
C-w to delete match and recursive edit,
C-l to clear the screen, redisplay, and offer same replacement again,
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
	(lastrepl nil))			;Position after last match considered.
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
	      ;; Preserve the match data.  Process filters and sentinels
	      ;; could run inside read-char..
	      (let ((data (match-data))
		    (help-form
		     '(concat "Query replacing "
			      (if regexp-flag "regexp " "")
			      from-string " with " to-string ".\n\n"
			      (substitute-command-keys query-replace-help))))
		(setq char help-char)
		(while (= char help-char)
		  (message "Query replacing %s with %s: " from-string to-string)
		  (setq char (read-char))
		  (if (= char ??)
		      (setq unread-command-char help-char char help-char)))
		(store-match-data data))
	      (cond ((or (= char ?\e)
			 (= char ?q))
		     (setq keep-going nil)
		     (setq done t))
		    ((= char ?^)
		     (goto-char (mark))
		     (setq replaced t))
		    ((or (= char ?\ )
			 (= char ?y))
		     (or replaced
			 (replace-match to-string nocasify literal))
		     (setq done t))
		    ((= char ?\.)
		     (or replaced
			 (replace-match to-string nocasify literal))
		     (setq keep-going nil)
		     (setq done t))
		    ((= char ?\,)
		     (if (not replaced)
			 (progn
			   (replace-match to-string nocasify literal)
			   (setq replaced t))))
		    ((= char ?!)
		     (or replaced
			 (replace-match to-string nocasify literal))
		     (setq done t query-flag nil))
		    ((or (= char ?\177)
			 (= char ?n))
		     (setq done t))
		    ((= char ?\C-l)
		     (recenter nil))
		    ((= char ?\C-r)
		     (store-match-data
		       (prog1 (match-data)
			 (save-excursion (recursive-edit)))))
		    ((= char ?\C-w)
		     (delete-region (match-beginning 0) (match-end 0))
		     (store-match-data
		       (prog1 (match-data)
			 (save-excursion (recursive-edit))))
		     (setq replaced t))
		    (t
		     (setq keep-going nil)
		     (setq unread-command-char char)
		     (setq done t))))))
	(setq lastrepl (point))))
    (pop-mark)
    keep-going))

