;; Spelling correction interface for Emacs.
;; Copyright (C) 1985 Free Software Foundation, Inc.

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


(defvar spell-command "spell"
  "*Command to run the spell program.")

(defvar spell-filter nil
  "*Filter function to process text before passing it to spell program.
This function might remove text-processor commands.
nil means don't alter the text before checking it.")

(defun spell-buffer ()
  "Check spelling of every word in the buffer.
For each incorrect word, you are asked for the correct spelling
and then put into a query-replace to fix some or all occurrences.
If you do not want to change a word, just give the same word
as its \"correct\" spelling; then the query replace is skipped."
  (interactive)
  (spell-region (point-min) (point-max) "buffer"))

(defun spell-word ()
  "Check spelling of word at or before point.
If it is not correct, ask user for the correct spelling
and query-replace the entire buffer to substitute it."
  (interactive)
  (let (beg end spell-filter)
    (save-excursion
     (if (not (looking-at "\\<"))
	 (forward-word -1))
     (setq beg (point))
     (forward-word 1)
     (setq end (point)))
    (spell-region beg end (buffer-substring beg end))))

(defun spell-region (start end &optional description)
  "Like spell-buffer but applies only to region.
Used in a program, applies from START to END.
DESCRIPTION is an optional string naming the unit being checked:
for example, \"word\"."
  (interactive "r")
  (let ((filter spell-filter)
	(buf (get-buffer-create " *temp*")))
    (save-excursion
     (set-buffer buf)
     (widen)
     (erase-buffer))
    (message "Checking spelling of %s..." (or description "region"))
    (if (and (null filter) (= ?\n (char-after (1- end))))
	(if (string= "spell" spell-command)
	    (call-process-region start end "spell" nil buf)
	  (call-process-region start end shell-file-name
			       nil buf nil "-c" spell-command))
      (let ((oldbuf (current-buffer)))
	(save-excursion
	 (set-buffer buf)
	 (insert-buffer-substring oldbuf start end)
	 (or (bolp) (insert ?\n))
	 (if filter (funcall filter))
	 (if (string= "spell" spell-command)
	     (call-process-region (point-min) (point-max) "spell" t buf)
	   (call-process-region (point-min) (point-max) shell-file-name
				t buf nil "-c" spell-command)))))
    (message "Checking spelling of %s...%s"
	     (or description "region")
	     (if (save-excursion
		  (set-buffer buf)
		  (> (buffer-size) 0))
		 "not correct"
	       "correct"))
    (let (word newword
	  (case-fold-search t)
	  (case-replace t))
      (while (save-excursion
	      (set-buffer buf)
	      (> (buffer-size) 0))
	(save-excursion
	 (set-buffer buf)
	 (goto-char (point-min))
	 (setq word (downcase
		     (buffer-substring (point)
				       (progn (end-of-line) (point)))))
	 (forward-char 1)
	 (delete-region (point-min) (point))
	 (setq newword
	       (read-input (concat "`" word
				   "' not recognized; edit a replacement: ")
			   word))
	 (flush-lines (concat "^" (regexp-quote word) "$")))
	(if (not (equal word newword))
	    (progn
	     (goto-char (point-min))
	     (query-replace-regexp (concat "\\b" (regexp-quote word) "\\b")
				   newword)))))))


(defun spell-string (string)
  "Check spelling of string supplied as argument."
  (interactive "sSpell string: ")
  (let ((buf (get-buffer-create " *temp*")))
    (save-excursion
     (set-buffer buf)
     (widen)
     (erase-buffer)
     (insert string "\n")
     (if (string= "spell" spell-command)
	 (call-process-region (point-min) (point-max) "spell"
			      t t)
       (call-process-region (point-min) (point-max) shell-file-name
			    t t nil "-c" spell-command))
     (if (= 0 (buffer-size))
	 (message "%s is correct" string)
       (goto-char (point-min))
       (while (search-forward "\n" nil t)
	 (replace-match " "))
       (message "%sincorrect" (buffer-substring 1 (point-max)))))))
