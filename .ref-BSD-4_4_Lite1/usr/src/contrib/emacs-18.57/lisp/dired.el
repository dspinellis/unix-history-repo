;;; Missing: P command, sorting, setting file modes.
;;; Dired buffer containing multiple directories gets totally confused
;;; Implement insertion of subdirectories in situ --- tree dired

;; DIRED commands for Emacs
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


;In loaddefs.el
;(defvar dired-listing-switches "-al"
;  "Switches passed to ls for dired. MUST contain the 'l' option.
;CANNOT contain the 'F' option.")

(defun dired-readin (dirname buffer)
  (save-excursion
    (message "Reading directory %s..." dirname)
    (set-buffer buffer)
    (let ((buffer-read-only nil))
      (widen)
      (erase-buffer)
      (setq dirname (expand-file-name dirname))
      (if (file-directory-p dirname)
	  (call-process "ls" nil buffer nil
			dired-listing-switches dirname)
	(let ((default-directory (file-name-directory dirname)))
	  (call-process shell-file-name nil buffer nil
			"-c" (concat "ls " dired-listing-switches " "
				     (file-name-nondirectory dirname)))))
      (goto-char (point-min))
      (while (not (eobp))
	(insert "  ")
	(forward-line 1))
      (goto-char (point-min)))
    (message "Reading directory %s...done" dirname)))

(defun dired-find-buffer (dirname)
  (let ((blist (buffer-list))
	found)
    (while blist
      (save-excursion
        (set-buffer (car blist))
	(if (and (eq major-mode 'dired-mode)
		 (equal dired-directory dirname))
	    (setq found (car blist)
		  blist nil)
	  (setq blist (cdr blist)))))
    (or found
	(create-file-buffer (directory-file-name dirname)))))

(defun dired (dirname)
  "\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Dired displays a list of files in DIRNAME.
You can move around in it with the usual commands.
You can flag files for deletion with C-d
and then delete them by typing `x'.
Type `h' after entering dired for more info."
  (interactive (list (read-file-name "Dired (directory): "
				     nil default-directory nil)))
  (switch-to-buffer (dired-noselect dirname)))

(defun dired-other-window (dirname)
  "\"Edit\" directory DIRNAME.  Like M-x dired but selects in another window."
  (interactive (list (read-file-name "Dired in other window (directory): "
				     nil default-directory nil)))
  (switch-to-buffer-other-window (dired-noselect dirname)))

(defun dired-noselect (dirname)
  "Like M-x dired but returns the dired buffer as value, does not select it."
  (or dirname (setq dirname default-directory))
  (setq dirname (expand-file-name (directory-file-name dirname)))
  (if (file-directory-p dirname)
      (setq dirname (file-name-as-directory dirname)))
  (let ((buffer (dired-find-buffer dirname)))
    (save-excursion
      (set-buffer buffer)
      (dired-readin dirname buffer)
      (dired-move-to-filename)
      (dired-mode dirname))
    buffer))

(defun dired-revert (&optional arg noconfirm)
  (let ((opoint (point))
	(ofile (dired-get-filename t t))
	(buffer-read-only nil))
    (erase-buffer)
    (dired-readin dired-directory (current-buffer))
    (or (and ofile (re-search-forward (concat " " (regexp-quote ofile) "$")
				      nil t))
	(goto-char opoint))
    (beginning-of-line)))

(defvar dired-mode-map nil "Local keymap for dired-mode buffers.")
(if dired-mode-map
    nil
  (setq dired-mode-map (make-keymap))
  (suppress-keymap dired-mode-map)
  (define-key dired-mode-map "r" 'dired-rename-file)
  (define-key dired-mode-map "\C-d" 'dired-flag-file-deleted)
  (define-key dired-mode-map "d" 'dired-flag-file-deleted)
  (define-key dired-mode-map "v" 'dired-view-file)
  (define-key dired-mode-map "e" 'dired-find-file)
  (define-key dired-mode-map "f" 'dired-find-file)
  (define-key dired-mode-map "o" 'dired-find-file-other-window)
  (define-key dired-mode-map "u" 'dired-unflag)
  (define-key dired-mode-map "x" 'dired-do-deletions)
  (define-key dired-mode-map "\177" 'dired-backup-unflag)
  (define-key dired-mode-map "?" 'dired-summary)
  (define-key dired-mode-map "c" 'dired-copy-file)
  (define-key dired-mode-map "#" 'dired-flag-auto-save-files)
  (define-key dired-mode-map "~" 'dired-flag-backup-files)
  (define-key dired-mode-map "." 'dired-clean-directory)
  (define-key dired-mode-map "h" 'describe-mode)
  (define-key dired-mode-map " "  'dired-next-line)
  (define-key dired-mode-map "\C-n" 'dired-next-line)
  (define-key dired-mode-map "\C-p" 'dired-previous-line)
  (define-key dired-mode-map "n" 'dired-next-line)
  (define-key dired-mode-map "p" 'dired-previous-line)
  (define-key dired-mode-map "g" 'revert-buffer)
  (define-key dired-mode-map "C" 'dired-compress)
  (define-key dired-mode-map "U" 'dired-uncompress)
  (define-key dired-mode-map "B" 'dired-byte-recompile)
  (define-key dired-mode-map "M" 'dired-chmod)
  (define-key dired-mode-map "G" 'dired-chgrp)
  (define-key dired-mode-map "O" 'dired-chown))


;; Dired mode is suitable only for specially formatted data.
(put 'dired-mode 'mode-class 'special)

(defun dired-mode (&optional dirname)
  "Mode for \"editing\" directory listings.
In dired, you are \"editing\" a list of the files in a directory.
You can move using the usual cursor motion commands.
Letters no longer insert themselves.
Instead, type d to flag a file for Deletion.
Type u to Unflag a file (remove its D flag).
  Type Rubout to back up one line and unflag.
Type x to eXecute the deletions requested.
Type f to Find the current line's file
  (or Dired it, if it is a directory).
Type o to find file or dired directory in Other window.
Type # to flag temporary files (names beginning with #) for Deletion.
Type ~ to flag backup files (names ending with ~) for Deletion.
Type . to flag numerical backups for Deletion.
  (Spares dired-kept-versions or its numeric argument.)
Type r to rename a file.
Type c to copy a file.
Type v to view a file in View mode, returning to Dired when done.
Type g to read the directory again.  This discards all deletion-flags.
Space and Rubout can be used to move down and up by lines.
Also: C -- compress this file.  U -- uncompress this file.
      B -- byte compile this file.
 M, G, O -- change file's mode, group or owner.
\\{dired-mode-map}"
  (interactive)
  (kill-all-local-variables)    
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'dired-revert)
  (setq major-mode 'dired-mode)
  (setq mode-name "Dired")
  (make-local-variable 'dired-directory)
  (setq dired-directory (or dirname default-directory))
  (if dirname
      (setq default-directory 
	    (if (file-directory-p dirname)
		dirname (file-name-directory dirname))))
  (setq mode-line-buffer-identification '("Dired: %17b"))
  (setq case-fold-search nil)
  (setq buffer-read-only t)
  (use-local-map dired-mode-map)
  (run-hooks 'dired-mode-hook))

(defun dired-repeat-over-lines (arg function)
  (beginning-of-line)
  (while (and (> arg 0) (not (eobp)))
    (setq arg (1- arg))
    (save-excursion
      (beginning-of-line)
      (and (bobp) (looking-at "  total")
	   (error "No file on this line"))
      (funcall function))
    (forward-line 1)
    (dired-move-to-filename))
  (while (and (< arg 0) (not (bobp)))
    (setq arg (1+ arg))
    (forward-line -1)
    (dired-move-to-filename)
    (save-excursion
      (beginning-of-line)
      (funcall function))))

(defun dired-flag-file-deleted (arg)
  "In dired, flag the current line's file for deletion.
With arg, repeat over several lines."
  (interactive "p")
  (dired-repeat-over-lines arg
    '(lambda ()
       (let ((buffer-read-only nil))
	 (delete-char 1)
	 (insert "D")))))

(defun dired-summary ()
  (interactive)
  ;>> this should check the key-bindings and use substitute-command-keys if non-standard
  (message
   "d-elete, u-ndelete, x-ecute, f-ind, o-ther window, r-ename, c-opy, v-iew"))

(defun dired-unflag (arg)
  "In dired, remove the current line's delete flag then move to next line."
  (interactive "p")
  (dired-repeat-over-lines arg
    '(lambda ()
       (let ((buffer-read-only nil))
	 (delete-char 1)
	 (insert " ")
	 (forward-char -1)))))

(defun dired-backup-unflag (arg)
  "In dired, move up a line and remove deletion flag there."
  (interactive "p")
  (dired-unflag (- arg)))

(defun dired-next-line (arg)
  "Move down ARG lines then position at filename."
  (interactive "p")
  (next-line arg)
  (dired-move-to-filename))

(defun dired-previous-line (arg)
  "Move up ARG lines then position at filename."
  (interactive "p")
  (previous-line arg)
  (dired-move-to-filename))

(defun dired-find-file ()
  "In dired, visit the file or directory named on this line."
  (interactive)
  (find-file (dired-get-filename)))

(defun dired-view-file ()
  "In dired, examine a file in view mode, returning to dired when done."
  (interactive)
  (if (file-directory-p (dired-get-filename))
      (dired (dired-get-filename))
    (view-file (dired-get-filename))))
	    
(defun dired-find-file-other-window ()
  "In dired, visit this file or directory in another window."
  (interactive)
  (find-file-other-window (dired-get-filename)))

(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In dired, return name of file mentioned on this line.
Value returned normally includes the directory name.
A non-nil 1st argument means do not include it.  A non-nil 2nd argument
says return nil if no filename on this line, otherwise an error occurs."
  (let (eol)
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (if (re-search-forward
	   "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ ]+[0-9]+"
	   eol t)
	  (progn (skip-chars-forward " ")
		 (skip-chars-forward "^ " eol)
		 (skip-chars-forward " " eol)
		 (let ((beg (point)))
		   (skip-chars-forward "^ \n")
		   (if localp
		       (buffer-substring beg (point))
		     ;; >> uses default-directory, could lose on cd, multiple.
		     (concat default-directory (buffer-substring beg (point))))))
	(if no-error-if-not-filep nil
	  (error "No file on this line"))))))

(defun dired-move-to-filename ()
  "In dired, move to first char of filename on this line.
Returns position (point) or nil if no filename on this line."
  (let ((eol (progn (end-of-line) (point))))
    (beginning-of-line)
    (if (re-search-forward
	 "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ ]+[0-9]+"
	 eol t)
	(progn
	  (skip-chars-forward " ")
	  (skip-chars-forward "^ " eol)
	  (skip-chars-forward " " eol)
	  (point)))))

(defun dired-map-dired-file-lines (fn)
  "perform fn with point at the end of each non-directory line:
arguments are the short and long filename"
  (save-excursion
    (let (filename longfilename (buffer-read-only nil))
      (goto-char (point-min))
      (while (not (eobp))
	(save-excursion
	  (and (not (looking-at "  d"))
	       (not (eolp))
	       (setq filename (dired-get-filename t t)
		     longfilename (dired-get-filename nil t))
	       (progn (end-of-line)
		      (funcall fn filename longfilename))))
	(forward-line 1)))))

(defun dired-flag-auto-save-files ()
  "Flag for deletion files whose names suggest they are auto save files."
  (interactive)
  (save-excursion
   (let ((buffer-read-only nil))
     (goto-char (point-min))
     (while (not (eobp))
       (and (not (looking-at "  d"))
	    (not (eolp))
	    (if (fboundp 'auto-save-file-name-p)
		(let ((fn (dired-get-filename t t)))
		  (if fn (auto-save-file-name-p fn)))
	      (if (dired-move-to-filename)
		  (looking-at "#")))
	    (progn (beginning-of-line)
		   (delete-char 1)
		   (insert "D")))
       (forward-line 1)))))

(defun dired-clean-directory (keep)
  "Flag numerical backups for Deletion.
Spares dired-kept-versions latest versions, and kept-old-versions oldest.
Positive numeric arg overrides dired-kept-versions;
negative numeric arg overrides kept-old-versions with minus the arg."
  (interactive "P")
  (setq keep (if keep (prefix-numeric-value keep) dired-kept-versions))
  (let ((early-retention (if (< keep 0) (- keep) kept-old-versions))
	(late-retention (if (<= keep 0) dired-kept-versions keep))
	(file-version-assoc-list ()))
    ;; Look at each file.
    ;; If the file has numeric backup versions,
    ;; put on file-version-assoc-list an element of the form
    ;; (FILENAME . VERSION-NUMBER-LIST)
    (dired-map-dired-file-lines 'dired-collect-file-versions)
    ;; Sort each VERSION-NUMBER-LIST,
    ;; and remove the versions not to be deleted.
    (let ((fval file-version-assoc-list))
      (while fval
	(let* ((sorted-v-list (cons 'q (sort (cdr (car fval)) '<)))
	       (v-count (length sorted-v-list)))
	  (if (> v-count (+ early-retention late-retention))
	      (rplacd (nthcdr early-retention sorted-v-list)
		      (nthcdr (- v-count late-retention)
			      sorted-v-list)))
	  (rplacd (car fval)
		  (cdr sorted-v-list)))
	(setq fval (cdr fval)))) 
    ;; Look at each file.  If it is a numeric backup file,
    ;; find it in a VERSION-NUMBER-LIST and maybe flag it for deletion.
    (dired-map-dired-file-lines 'dired-trample-file-versions)))

(defun dired-collect-file-versions (ignore fn)
  "If it looks like fn has versions, we make a list of the versions.
We may want to flag some for deletion."
    (let* ((base-versions
	    (concat (file-name-nondirectory fn) ".~"))
	   (bv-length (length base-versions))
	   (possibilities (file-name-all-completions
			   base-versions
			   (file-name-directory fn)))
	   (versions (mapcar 'backup-extract-version possibilities)))
      (if versions
	  (setq file-version-assoc-list (cons (cons fn versions)
					      file-version-assoc-list)))))

(defun dired-trample-file-versions (ignore fn)
  (let* ((start-vn (string-match "\\.~[0-9]+~$" fn))
	 base-version-list)
    (and start-vn
	 (setq base-version-list	; there was a base version to which 
	       (assoc (substring fn 0 start-vn)	; this looks like a 
		      file-version-assoc-list))	; subversion
	 (not (memq (string-to-int (substring fn (+ 2 start-vn)))
		    base-version-list))	; this one doesn't make the cut
	 (dired-flag-this-line-for-DEATH))))

(defun dired-flag-this-line-for-DEATH ()
  (beginning-of-line)
  (delete-char 1)
  (insert "D"))

(defun dired-flag-backup-files ()
  "Flag all backup files (names ending with ~) for deletion."
  (interactive)
  (save-excursion
   (let ((buffer-read-only nil))
     (goto-char (point-min))
     (while (not (eobp))
       (and (not (looking-at "  d"))
	    (not (eolp))
	    (if (fboundp 'backup-file-name-p)
		(let ((fn (dired-get-filename t t)))
		  (if fn (backup-file-name-p fn)))
	      (end-of-line)
	      (forward-char -1)
	      (looking-at "~"))
	    (progn (beginning-of-line)
		   (delete-char 1)
		   (insert "D")))
       (forward-line 1)))))

(defun dired-flag-backup-and-auto-save-files ()
  "Flag all backup and temporary files for deletion.
Backup files have names ending in ~.  Auto save file names usually
start with #."
  (interactive)
  (dired-flag-backup-files)
  (dired-flag-auto-save-files))

(defun dired-rename-file (to-file)
  "Rename this file to TO-FILE."
  (interactive
   (list (read-file-name (format "Rename %s to: "
				 (file-name-nondirectory (dired-get-filename)))
			 nil (dired-get-filename))))
  (setq to-file (expand-file-name to-file))
  (rename-file (dired-get-filename) to-file)
  (let ((buffer-read-only nil))
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (setq to-file (expand-file-name to-file))
    (dired-add-entry (file-name-directory to-file)
		     (file-name-nondirectory to-file))))

(defun dired-copy-file (to-file)
  "Copy this file to TO-FILE."
  (interactive "FCopy to: ")
  (copy-file (dired-get-filename) to-file)
  (setq to-file (expand-file-name to-file))
  (dired-add-entry (file-name-directory to-file)
		   (file-name-nondirectory to-file)))

(defun dired-add-entry (directory filename)
  ;; If tree dired is implemented, this function will have to do
  ;; something smarter with the directory.  Currently, just check
  ;; default directory, if same, add the new entry at point.  With tree
  ;; dired, should call 'dired-current-directory' or similar.  Note
  ;; that this adds the entry 'out of order' if files sorted by time,
  ;; etc.
  (if (string-equal directory default-directory)
      (let ((buffer-read-only nil))
	(beginning-of-line)
	(call-process "ls" nil t nil
		      "-d" dired-listing-switches (concat directory filename))
	(forward-line -1)
	(insert "  ")
	(dired-move-to-filename)
	(let* ((beg (point))
	       (end (progn (end-of-line) (point))))
	  (setq filename (buffer-substring beg end))
	  (delete-region beg end)
	  (insert (file-name-nondirectory filename)))
	(beginning-of-line))))

(defun dired-compress ()
  "Compress this file."
  (interactive)
  (let* ((buffer-read-only nil)
	 (error-buffer (get-buffer-create " *Dired compress output*"))
	 (from-file (dired-get-filename))
	 (to-file (concat from-file ".Z")))
    (if (string-match "\\.Z$" from-file)
	(error "%s is already compressed!" from-file))
    (message "Compressing %s..." from-file)
    (unwind-protect
	(progn
	  (save-excursion
	    (set-buffer error-buffer)
	    (erase-buffer))
	  ;; Must have default-directory of dired buffer in call-process
	  (call-process "compress" nil error-buffer nil "-f" from-file)
	  (if (save-excursion
		(set-buffer error-buffer)
		(= 0 (buffer-size)))
	      (progn
		(message "Compressing %s... done" from-file)
		(kill-buffer error-buffer))
	    (display-buffer error-buffer)
	    (setq error-buffer nil)
	    (error "Compress error on %s." from-file)))
      (if error-buffer (kill-buffer error-buffer)))
    (dired-redisplay to-file)))

(defun dired-uncompress ()
  "Uncompress this file."
  (interactive)
  (let* ((buffer-read-only nil)
	 (error-buffer (get-buffer-create " *Dired compress output*"))
	 (from-file (dired-get-filename))
	 (to-file (substring from-file 0 -2)))
    (if (string-match "\\.Z$" from-file) nil
	(error "%s is not compressed!" from-file))
    (message "Uncompressing %s..." from-file)
    (unwind-protect
	(progn
	  (save-excursion
	    (set-buffer error-buffer)
	    (erase-buffer))
	  ;; Must have default-directory of dired buffer in call-process
	  (call-process "uncompress" nil error-buffer nil "-f" from-file)
	  (if (save-excursion
		(set-buffer error-buffer)
		(= 0 (buffer-size)))
	      (progn
		(message "Uncompressing %s... done" from-file)
		(kill-buffer error-buffer))
	    (display-buffer error-buffer)
	    (setq error-buffer nil)
	    (error "Uncompress error on %s." from-file)))
      (if error-buffer (kill-buffer error-buffer)))
    (dired-redisplay to-file)))

(defun dired-byte-recompile ()
  "Byte recompile this file."
  (interactive)
  (let* ((buffer-read-only nil)
	 (from-file (dired-get-filename))
	 (to-file (substring from-file 0 -3)))
    (if (string-match "\\.el$" from-file) nil
	(error "%s is uncompilable!" from-file))
    (byte-compile-file from-file)))

(defun dired-chmod (mode)
  "Change mode of this file."
  (interactive "sChange to Mode: ")
  (let ((buffer-read-only nil)
	(file (dired-get-filename)))
    (call-process "/bin/chmod" nil nil nil mode file)
    (dired-redisplay file)))

(defun dired-chgrp (group)
  "Change group of this file."
  (interactive "sChange to Group: ")
  (let ((buffer-read-only nil)
	(file (dired-get-filename)))
    (call-process "/bin/chgrp" nil nil nil group file)
    (dired-redisplay file)))

(defun dired-chown (owner)
  "Change Owner of this file."
  (interactive "sChange to Owner: ")
  (let ((buffer-read-only nil)
	(file (dired-get-filename)))
    (call-process "/etc/chown" nil nil nil owner file)
    (dired-redisplay file)))

(defun dired-redisplay (file) "Redisplay this line."
  (beginning-of-line)
  (delete-region (point) (progn (forward-line 1) (point)))
  (if file (dired-add-entry (file-name-directory    file)
			    (file-name-nondirectory file)))
  (dired-move-to-filename))

(defun dired-do-deletions ()
  "In dired, delete the files flagged for deletion."
  (interactive)
  (let (delete-list answer)
    (save-excursion
     (goto-char 1)
     (while (re-search-forward "^D" nil t)
       (setq delete-list
	     (cons (cons (dired-get-filename t) (1- (point)))
		   delete-list))))
    (if (null delete-list)
	(message "(No deletions requested)")
      (save-window-excursion
       (switch-to-buffer " *Deletions*")
       (erase-buffer)
       (setq fill-column 70)
       (let ((l (reverse delete-list)))
	 ;; Files should be in forward order for this loop.
	 (while l
	   (if (> (current-column) 59)
	       (insert ?\n)
	     (or (bobp)
		 (indent-to (* (/ (+ (current-column) 19) 20) 20) 1)))
	   (insert (car (car l)))
	   (setq l (cdr l))))
       (goto-char (point-min))
       (setq answer (yes-or-no-p "Delete these files? ")))
      (if answer
	  (let ((l delete-list)
		failures)
	    ;; Files better be in reverse order for this loop!
	    ;; That way as changes are made in the buffer
	    ;; they do not shift the lines still to be changed.
	    (while l
	      (goto-char (cdr (car l)))
	      (let ((buffer-read-only nil))
		(condition-case ()
		    (let ((fn (concat default-directory (car (car l)))))
		      (if (file-directory-p fn)
			  (progn
			    (call-process "rmdir" nil nil nil fn)
			    (if (file-exists-p fn) (delete-file fn)))
			(delete-file fn))
		      (delete-region (point)
				     (progn (forward-line 1) (point))))
		  (error (delete-char 1)
			 (insert " ")
			 (setq failures (cons (car (car l)) failures)))))
	      (setq l (cdr l)))
	    (if failures
		(message "Deletions failed: %s"
			 (prin1-to-string failures))))))))

(provide 'dired)
