;;; Missing: P command, sorting, setting file modes.
;;; Dired buffer containing multiple directories gets totally confused
;;; Implement insertion of subdirectories in situ --- tree dired

;; DIRED commands for Emacs
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


;Will be in loaddefs.el for versions 16.2 and up.
;(defvar dired-listing-switches "-al"
;  "Switches passed to ls for dired. MUST contain the 'l' option.
;CANNOT contain the 'F' option.")

(defun dired-readin (dirname buffer)
  (save-excursion
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
      (goto-char (point-min)))))

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
	(progn (if (string-match "/$" dirname)
		   (setq dirname (substring dirname 0 -1)))
	       (create-file-buffer (file-name-nondirectory dirname))))))

(defun dired (dirname)
  "\"Edit\" directory DIRNAME.  Delete some files in it.
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
  (if (string-match "./$" dirname)
      (setq dirname (substring dirname 0 -1)))
  (setq dirname (expand-file-name dirname))
  (and (not (string-match "/$" dirname))
       (file-directory-p dirname)
       (setq dirname (concat dirname "/")))
  (let ((buffer (dired-find-buffer dirname)))
    (save-excursion
      (set-buffer buffer)
      (dired-readin dirname buffer)
      (dired-move-to-filename)
      (dired-mode dirname))
    buffer))

(defun dired-revert (&optional arg)
  (let ((opoint (point))
	(ofile (dired-get-filename t t))
	(buffer-read-only nil))
    (erase-buffer)
    (dired-readin dired-directory (current-buffer))
    (or (and ofile (re-search-forward (concat " " (regexp-quote ofile) "$") nil t))
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
  (define-key dired-mode-map "p" 'dired-previous-line))

(defun dired-mode (dirname)
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
Space can be used to move down and up by lines.
\\{dired-mode-map}"
  (kill-all-local-variables)    
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'dired-revert)
  (setq major-mode 'dired-mode)
  (setq mode-name "Dired")
  (make-local-variable 'dired-directory)
  (setq dired-directory dirname)
  (setq default-directory 
	(if (file-directory-p dirname)
	    dirname (file-name-directory dirname)))
  (setq case-fold-search nil)
  (setq mode-line-format
	(concat "--Directory " dirname
		"      %M   %[(%m)%]----%p--%-"))
  (setq buffer-read-only t)
  (use-local-map dired-mode-map))

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
	 (if (looking-at "  d")
	     nil
	   (delete-char 1)
	   (insert "D"))))))

(defun dired-summary ()
  (interactive)
  ;>> this should check the key-bindings and use substitute-command-keys if non-standard
  (message
   "d-elete, u-ndelete, x-ecute, f-ind, o-ther window, r-ename, c-opy, v-iew"))

(defun dired-unflag (arg)
  "In dired, flag the current line's file for deletion."
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
  "In dired, visit the file named on this line."
  (interactive)
  (if (save-excursion
	(beginning-of-line)
	(looking-at "  d"))
      (dired (dired-get-filename))
    (find-file (dired-get-filename))))

(defun dired-view-file ()
  "In dired, examine a file in view mode, returning to dired when done."
  (interactive)
  (if (save-excursion
	(beginning-of-line)
	(looking-at "  d"))
      (dired (dired-get-filename))
    (view-file (dired-get-filename))))
	    
(defun dired-find-file-other-window ()
  "In dired, visit this file in another window."
  (interactive)
  (if (save-excursion
	(beginning-of-line)
	(looking-at "  d"))
      (dired-other-window (dired-get-filename))
    (find-file-other-window (dired-get-filename))))

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
	    (progn (end-of-line) (forward-char -1) (looking-at "~"))
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
  (interactive "FRename to: ")
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
		    (progn (delete-file (concat default-directory
						(car (car l))))
			   (delete-region (point)
					  (progn (forward-line 1) (point))))
		  (error (delete-char 1)
			 (insert " ")
			 (setq failures (cons (car (car l)) failures)))))
	      (setq l (cdr l)))
	    (if failures
		(message "Deletions failed: %s"
			 (prin1-to-string failures))))))))


  
