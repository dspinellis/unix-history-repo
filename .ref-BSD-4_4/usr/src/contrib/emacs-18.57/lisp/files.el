;; File input and output commands for Emacs
;; Copyright (C) 1985, 1986, 1987, 1990 Free Software Foundation, Inc.

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


(defconst delete-auto-save-files t
  "*Non-nil means delete a buffer's auto-save file
when the buffer is saved for real.")

;(make-variable-buffer-local 'buffer-backed-up)
;(defvar buffer-backed-up nil
;  "Non-nil if this buffer's file has been backed up.
;Backing up is done before the first time the file is saved.")

;;; Turn off backup files on VMS since it has version numbers.
(defconst make-backup-files (not (eq system-type 'vax-vms))
  "*Create a backup of each file when it is saved for the first time.
This can be done by renaming the file or by copying.

Renaming means that Emacs renames the existing file so that it is a
backup file, then writes the buffer into a new file.  Any other names
that the old file had will now refer to the backup file.
The new file is owned by you and its group is defaulted.

Copying means that Emacs copies the existing file into the backup file,
then writes the buffer on top of the existing file.  Any other names
that the old file had will now refer to the new (edited) file.
The file's owner and group are unchanged.

The choice of renaming or copying is controlled by the variables
backup-by-copying, backup-by-copying-when-linked and
backup-by-copying-when-mismatch.")

(defconst backup-by-copying nil
 "*Non-nil means always use copying to create backup files.
See documentation of variable  make-backup-files.")

(defconst backup-by-copying-when-linked nil
 "*Non-nil means use copying to create backups for files with multiple names.
This causes the alternate names to refer to the latest version as edited.
This variable is relevant only if  backup-by-copying  is nil.")

(defconst backup-by-copying-when-mismatch nil
  "*Non-nil means create backups by copying if this preserves owner or group.
Renaming may still be used (subject to control of other variables)
when it would not result in changing the owner or group of the file;
that is, for files which are owned by you and whose group matches
the default for a new file created there by you.
This variable is relevant only if  backup-by-copying  is nil.")

(defconst buffer-offer-save nil
  "*Non-nil in a buffer means offer to save the buffer on exit
even if the buffer is not visiting a file.  Automatically local in
all buffers.")
(make-variable-buffer-local 'buffer-offer-save)

(defconst file-precious-flag nil
  "*Non-nil means protect against I/O errors while saving files.
Some modes set this non-nil in particular buffers.")

(defvar version-control nil
  "*Control use of version numbers for backup files.
t means make numeric backup versions unconditionally.
nil means make them for files that have some already.
never means do not make them.")

(defvar dired-kept-versions 2
  "*When cleaning directory, number of versions to keep.")

(defvar trim-versions-without-asking nil
  "*If true, deletes excess backup versions silently.
Otherwise asks confirmation.")

(defvar kept-old-versions 2
  "*Number of oldest versions to keep when a new numbered backup is made.")

(defvar kept-new-versions 2
  "*Number of newest versions to keep when a new numbered backup is made.
Includes the new backup.  Must be > 0")

(defconst require-final-newline nil
  "*t says silently put a newline at the end whenever a file is saved.
Non-nil but not t says ask user whether to add a newline in each such case.
nil means don't add newlines.")

(defconst auto-save-default t
  "*t says by default do auto-saving of every file-visiting buffer.")

(defconst auto-save-visited-file-name nil
  "*t says auto-save a buffer in the file it is visiting, when practical.
Normally auto-save files are written under other names.")

(defconst save-abbrevs nil
  "*Non-nil means save word abbrevs too when files are saved.
Loading an abbrev file sets this to t.")

(defconst find-file-run-dired t
  "*Non-nil says run dired if find-file is given the name of a directory.")

(defvar find-file-not-found-hooks nil
  "List of functions to be called for find-file on nonexistent file.
These functions are called as soon as the error is detected.
buffer-file-name is already set up.
The functions are called in the order given,
until one of them returns non-nil.")

(defvar find-file-hooks nil
  "List of functions to be called after a buffer is loaded from a file.
The buffer's local variables (if any) will have been processed before the
functions are called.")

(defvar write-file-hooks nil
  "List of functions to be called before writing out a buffer to a file.
If one of them returns non-nil, the file is considered already written
and the rest are not called.")

(defconst inhibit-local-variables nil
  "*Non-nil means query before obeying a file's local-variables list.
This applies when the local-variables list is scanned automatically
after you find a file.  If you explicitly request such a scan with
\\[normal-mode], there is no query, regardless of this variable.")

;; Avoid losing in versions where CLASH_DETECTION is disabled.
(or (fboundp 'lock-buffer)
    (fset 'lock-buffer 'ignore))
(or (fboundp 'unlock-buffer)
    (fset 'unlock-buffer 'ignore))

(defun pwd ()
  "Show the current default directory."
  (interactive nil)
  (message "Directory %s" default-directory))

(defun cd (dir)
  "Make DIR become the current buffer's default directory."
  (interactive "DChange default directory: ")
  (setq dir (expand-file-name dir))
  (if (not (eq system-type 'vax-vms))
      (setq dir (file-name-as-directory dir)))
  (if (not (file-directory-p dir))
      (error "%s is not a directory" dir)
    (setq default-directory dir))
  (pwd))

(defun load-file (file)
  "Load the file FILE of Lisp code."
  (interactive "fLoad file: ")
  (load (expand-file-name file) nil nil t))

(defun load-library (library)
  "Load the library named LIBRARY.
This is an interface to the function `load'."
  (interactive "sLoad library: ")
  (load library))

(defun switch-to-buffer-other-window (buffer)
  "Select buffer BUFFER in another window."
  (interactive "BSwitch to buffer in other window: ")
  (let ((pop-up-windows t))
    (pop-to-buffer buffer t)))

(defun find-file (filename)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists."
  (interactive "FFind file: ")
  (switch-to-buffer (find-file-noselect filename)))

(defun find-file-other-window (filename)
  "Edit file FILENAME, in another window.
May create a new window, or reuse an existing one;
see the function display-buffer."
  (interactive "FFind file in other window: ")
  (switch-to-buffer-other-window (find-file-noselect filename)))

(defun find-file-read-only (filename)
  "Edit file FILENAME but don't save without confirmation.
Like find-file but marks buffer as read-only."
  (interactive "fFind file read-only: ")
  (find-file filename)
  (setq buffer-read-only t))

(defun find-alternate-file (filename)
  "Find file FILENAME, select its buffer, kill previous buffer.
If the current buffer now contains an empty file that you just visited
\(presumably by mistake), use this command to visit the file you really want."
  (interactive "FFind alternate file: ")
  (and (buffer-modified-p)
;;;       (not buffer-read-only)
       (not (yes-or-no-p (format "Buffer %s is modified; kill anyway? "
				 (buffer-name))))
       (error "Aborted"))
  (let ((obuf (current-buffer))
	(ofile buffer-file-name)
	(oname (buffer-name)))
    (rename-buffer " **lose**")
    (setq buffer-file-name nil)
    (unwind-protect
	(progn
	  (unlock-buffer)
	  (find-file filename))
      (cond ((eq obuf (current-buffer))
	     (setq buffer-file-name ofile)
	     (lock-buffer)
	     (rename-buffer oname))))
    (kill-buffer obuf)))

(defun create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return it.
FILENAME (sans directory) is used unchanged if that name is free;
otherwise a string <2> or <3> or ... is appended to get an unused name."
  (let ((lastname (file-name-nondirectory filename)))
    (if (string= lastname "")
	(setq lastname filename))
    (generate-new-buffer lastname)))

(defun find-file-noselect (filename &optional nowarn)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one,
but verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller."
  (setq filename (expand-file-name filename))
  ;; Get rid of the prefixes added by the automounter.
  (if (string-match "^/tmp_mnt/" filename)
      (setq filename (substring filename (1- (match-end 0)))))
  (if (file-directory-p filename)
      (if find-file-run-dired
	  (dired-noselect filename)
	(error "%s is a directory." filename))
    (let ((buf (get-file-buffer filename))
	  error)
      (if buf
	  (or nowarn
	      (verify-visited-file-modtime buf)
	      (cond ((not (file-exists-p filename))
		     (error "File %s no longer exists!" filename))
		    ((yes-or-no-p
		      (if (buffer-modified-p buf)
    "File has changed since last visited or saved.  Flush your changes? "
    "File has changed since last visited or saved.  Read from disk? "))
		     (save-excursion
		       (set-buffer buf)
		       (revert-buffer t t)))))
	(save-excursion
	  (setq buf (create-file-buffer filename))
	  (set-buffer buf)
	  (erase-buffer)
	  (condition-case ()
	      (insert-file-contents filename t)
	    (file-error
	     (setq error t)
	     ;; Run find-file-not-found-hooks until one returns non-nil.
	     (let ((hooks find-file-not-found-hooks))
	       (while (and hooks
			   (not (funcall (car hooks))))
		 (setq hooks (cdr hooks))))))
	  (setq default-directory (file-name-directory filename))
	  (after-find-file error (not nowarn))))
      buf)))

(defun after-find-file (&optional error warn)
  "Called after finding a file and by the default revert function.
Sets buffer mode, parses local variables.
Optional args ERROR and WARN: ERROR non-nil means there was an
error in reading the file.  WARN non-nil means warn if there
exists an auto-save file more recent than the visited file.
Finishes by calling the functions in find-file-hooks."
  (setq buffer-read-only (not (file-writable-p buffer-file-name)))
  (if noninteractive
      nil
    (let* (not-serious
	   (msg
	    (cond ((not buffer-read-only)
		   (if (and warn
			    (file-newer-than-file-p (make-auto-save-file-name)
						    buffer-file-name))
		       "Auto save file is newer; consider M-x recover-file"
		     (setq not-serious t)
		     (if error "(New file)" nil)))
		  ((not error)
		   (setq not-serious t)
		   "File is write protected")
		  ((file-attributes buffer-file-name)
		   "File exists, but is read-protected.")
		  ((file-attributes (directory-file-name default-directory))
		   "File not found and directory write-protected")
		  (t
		   "File not found and directory doesn't exist"))))
      (if msg
	  (progn
	    (message msg)
	    (or not-serious (sit-for 1 t)))))
    (if auto-save-default
	(auto-save-mode t)))
  (normal-mode t)
  (mapcar 'funcall find-file-hooks))

(defun normal-mode (&optional find-file)
  "Choose the major mode for this buffer automatically.
Also sets up any specified local variables of the file.
Uses the visited file name, the -*- line, and the local variables spec.

This function is called automatically from `find-file'.  In that case,
if `inhibit-local-variables' is non-`nil' we require confirmation before
processing a local variables spec.  If you run `normal-mode' explicitly,
confirmation is never required."
  (interactive)
  (or find-file (funcall (or default-major-mode 'fundamental-mode)))
  (condition-case err
      (set-auto-mode)
    (error (message "File mode specification error: %s"
		    (prin1-to-string err))))
  (condition-case err
      (hack-local-variables (not find-file))
    (error (message "File local-variables error: %s"
		    (prin1-to-string err)))))

;(defvar auto-mode-alist ...) now in loaddefs.el
(defun set-auto-mode ()
  "Select major mode appropriate for current buffer.
May base decision on visited file name (See variable  auto-mode-list)
or on buffer contents (-*- line or local variables spec), but does not look
for the \"mode:\" local variable.  For that, use  hack-local-variables."
  ;; Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
  (let (beg end mode)
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (if (and (search-forward "-*-" (save-excursion (end-of-line) (point)) t)
	       (progn
		 (skip-chars-forward " \t")
		 (setq beg (point))
		 (search-forward "-*-" (save-excursion (end-of-line) (point)) t))
	       (progn
		 (forward-char -3)
		 (skip-chars-backward " \t")
		 (setq end (point))
		 (goto-char beg)
		 (if (search-forward ":" end t)
		     (progn
		       (goto-char beg)
		       (if (let ((case-fold-search t))
			     (search-forward "mode:" end t))
			   (progn
			     (skip-chars-forward " \t")
			     (setq beg (point))
			     (if (search-forward ";" end t)
				 (forward-char -1)
			       (goto-char end))
			     (skip-chars-backward " \t")
			     (setq mode (buffer-substring beg (point))))))
		   (setq mode (buffer-substring beg end)))))
	  (funcall (intern (concat (downcase mode) "-mode")))
	(let ((alist auto-mode-alist)
	      (name buffer-file-name))
	  (let ((case-fold-search (eq system-type 'vax-vms)))
	    ;; Remove backup-suffixes from file name.
	    (setq name (file-name-sans-versions name))
	    ;; Find first matching alist entry.
	    (while (and (not mode) alist)
	      (if (string-match (car (car alist)) name)
		  (setq mode (cdr (car alist))))
	      (setq alist (cdr alist))))
	  (if mode (funcall mode)))))))

(defun hack-local-variables (&optional force)
  "Parse, and bind or evaluate as appropriate, any local variables
for current buffer."
  ;; Look for "Local variables:" line in last page.
  (save-excursion
    (goto-char (point-max))
    (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
    (if (let ((case-fold-search t))
	  (and (search-forward "Local Variables:" nil t)
	       (or (not inhibit-local-variables)
		   force
		   (save-window-excursion
		     (switch-to-buffer (current-buffer))
		     (save-excursion
		       (beginning-of-line)
		       (set-window-start (selected-window) (point)))
		     (y-or-n-p (format "Set local variables as specified at end of %s? "
				       (file-name-nondirectory buffer-file-name)))))))
	(let ((continue t)
	      prefix prefixlen suffix beg)
	  ;; The prefix is what comes before "local variables:" in its line.
	  ;; The suffix is what comes after "local variables:" in its line.
	  (skip-chars-forward " \t")
	  (or (eolp)
	      (setq suffix (buffer-substring (point)
					     (progn (end-of-line) (point)))))
	  (goto-char (match-beginning 0))
	  (or (bolp)
	      (setq prefix
		    (buffer-substring (point)
				      (progn (beginning-of-line) (point)))))
	  (if prefix (setq prefixlen (length prefix)
			   prefix (regexp-quote prefix)))
	  (if suffix (setq suffix (concat (regexp-quote suffix) "$")))
	  (while continue
	    ;; Look at next local variable spec.
	    (if selective-display (re-search-forward "[\n\C-m]")
	      (forward-line 1))
	    ;; Skip the prefix, if any.
	    (if prefix
		(if (looking-at prefix)
		    (forward-char prefixlen)
		  (error "Local variables entry is missing the prefix")))
	    ;; Find the variable name; strip whitespace.
	    (skip-chars-forward " \t")
	    (setq beg (point))
	    (skip-chars-forward "^:\n")
	    (if (eolp) (error "Missing colon in local variables entry"))
	    (skip-chars-backward " \t")
	    (let* ((str (buffer-substring beg (point)))
		   (var (read str))
		  val)
	      ;; Setting variable named "end" means end of list.
	      (if (string-equal (downcase str) "end")
		  (setq continue nil)
		;; Otherwise read the variable value.
		(skip-chars-forward "^:")
		(forward-char 1)
		(setq val (read (current-buffer)))
		(skip-chars-backward "\n")
		(skip-chars-forward " \t")
		(or (if suffix (looking-at suffix) (eolp))
		    (error "Local variables entry is terminated incorrectly"))
		;; Set the variable.  "Variables" mode and eval are funny.
		(cond ((eq var 'mode)
		       (funcall (intern (concat (downcase (symbol-name val))
						"-mode"))))
		      ((eq var 'eval)
		       (if (string= (user-login-name) "root")
			   (message "Ignoring `eval:' in file's local variables")
			 (eval val)))
		      (t (make-local-variable var)
			 (set var val))))))))))

(defun set-visited-file-name (filename)
  "Change name of file visited in current buffer to FILENAME.
The next time the buffer is saved it will go in the newly specified file.
nil or empty string as argument means make buffer not be visiting any file.
Remember to delete the initial contents of the minibuffer
if you wish to pass an empty string as the argument."
  (interactive "FSet visited file name: ")
  (if filename
      (setq filename
	    (if (string-equal filename "")
		nil
	      (expand-file-name filename))))
  (or (equal filename buffer-file-name)
      (null filename)
      (progn
	(lock-buffer filename)
	(unlock-buffer)))
  (setq buffer-file-name filename)
  (if filename
      (let ((new-name (file-name-nondirectory buffer-file-name)))
	(if (string= new-name "")
	    (error "Empty file name"))
	(if (eq system-type 'vax-vms)
	    (setq new-name (downcase new-name)))
	(setq default-directory (file-name-directory buffer-file-name))
	(or (get-buffer new-name) (rename-buffer new-name))))
  (setq buffer-backed-up nil)
  (clear-visited-file-modtime)
  (kill-local-variable 'write-file-hooks)
  (kill-local-variable 'revert-buffer-function)
  ;; Rename the auto-save file to go with the new visited name.
  ;; If auto-save was not already on, turn it on if appropriate.
  (if buffer-auto-save-file-name
      (rename-auto-save-file)
    (auto-save-mode (and buffer-file-name auto-save-default)))
  (if buffer-file-name
      (set-buffer-modified-p t)))

(defun write-file (filename)
  "Write current buffer into file FILENAME.
Makes buffer visit that file, and marks it not modified."
  (interactive "FWrite file: ")
  (or (null filename) (string-equal filename "")
      (set-visited-file-name filename))
  (set-buffer-modified-p t)
  (save-buffer))

(defun backup-buffer ()
  "Make a backup of the disk file visited by the current buffer, if appropriate.
This is normally done before saving the buffer the first time.
If the value is non-nil, it is the result of `file-modes' on the original file;
this means that the caller, after saving the buffer, should change the modes
of the new file to agree with the old modes."
  (and make-backup-files
       (not buffer-backed-up)
       (file-exists-p buffer-file-name)
       (memq (aref (elt (file-attributes buffer-file-name) 8) 0)
	     '(?- ?l))
       (or (< (length buffer-file-name) 5)
	   (not (string-equal "/tmp/" (substring buffer-file-name 0 5))))
    (condition-case ()
	(let* ((backup-info (find-backup-file-name buffer-file-name))
	       (backupname (car backup-info))
	       (targets (cdr backup-info))
	       setmodes)
;	  (if (file-directory-p buffer-file-name)
;	      (error "Cannot save buffer in directory %s" buffer-file-name))
	  (condition-case ()
	      (if (or file-precious-flag
		      (file-symlink-p buffer-file-name)
		      backup-by-copying
		      (and backup-by-copying-when-linked
			   (> (file-nlinks buffer-file-name) 1))
		      (and backup-by-copying-when-mismatch
			   (let ((attr (file-attributes buffer-file-name)))
			     (or (nth 9 attr)
				 (/= (nth 2 attr) (user-uid))))))
		  (copy-file buffer-file-name backupname t t)
		(condition-case ()
		    (delete-file backupname)
		  (file-error nil))
		(rename-file buffer-file-name backupname t)
		(setq setmodes (file-modes backupname)))
	    (file-error
	     ;; If trouble writing the backup, write it in ~.
	     (setq backupname (expand-file-name "~/%backup%~"))
	     (message "Cannot write backup file; backing up in ~/%%backup%%~")
	     (sleep-for 1)
	     (copy-file buffer-file-name backupname t t)))
	  (setq buffer-backed-up t)
	  (if (and targets
		   (or trim-versions-without-asking
		       (y-or-n-p (format "Delete excess backup versions of %s? "
					 buffer-file-name))))
	      (while targets
		(condition-case ()
		    (delete-file (car targets))
		  (file-error nil))
		(setq targets (cdr targets))))
	  setmodes)
      (file-error nil))))

(defun file-name-sans-versions (name)
  "Return FILENAME sans backup versions or strings.
This is a separate procedure so your site-init or startup file can
redefine it."
  (substring name 0
	     (if (eq system-type 'vax-vms)
		 (or (string-match ";[0-9]+\\'" name)
		     (string-match ".[0-9]+\\'" name)
		     (length name))
	       (or (string-match "\\.~[0-9]+~\\'" name)
		   (string-match "~\\'" name)
		   (length name)))))

(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE.
This is a separate function so you can redefine it for customization."
  (concat file "~"))

(defun backup-file-name-p (file)
  "Return non-nil if FILE is a backup file name (numeric or not).
This is a separate function so you can redefine it for customization.
You may need to redefine file-name-sans-versions as well."
  (string-match "~$" file))

;; I believe there is no need to alter this behavior for VMS;
;; since backup files are not made on VMS, it should not get called.
(defun find-backup-file-name (fn)
  "Find a file name for a backup file, and suggestions for deletions.
Value is a list whose car is the name for the backup file
 and whose cdr is a list of old versions to consider deleting now."
  (if (eq version-control 'never)
      (list (make-backup-file-name fn))
    (let* ((base-versions (concat (file-name-nondirectory fn) ".~"))
	   (bv-length (length base-versions))
	   (possibilities (file-name-all-completions
			   base-versions
			   (file-name-directory fn)))
	   (versions (sort (mapcar 'backup-extract-version possibilities)
			   '<))
	   (high-water-mark (apply 'max (cons 0 versions)))
	   (deserve-versions-p
	    (or version-control
		(> high-water-mark 0)))
	   (number-to-delete (- (length versions)
				kept-old-versions kept-new-versions -1)))
      (if (not deserve-versions-p)
	  (list (make-backup-file-name fn))
	(cons (concat fn ".~" (int-to-string (1+ high-water-mark)) "~")
	      (if (> number-to-delete 0)
		  (mapcar (function (lambda (n)
				      (concat fn ".~" (int-to-string n) "~")))
			  (let ((v (nthcdr kept-old-versions versions)))
			    (rplacd (nthcdr (1- number-to-delete) v) ())
			    v))))))))

(defun backup-extract-version (fn)
  (if (and (string-match "[0-9]+~$" fn bv-length)
	   (= (match-beginning 0) bv-length))
      (string-to-int (substring fn bv-length -1))
      0))

(defun file-nlinks (filename)
  "Return number of names file FILENAME has." 
  (car (cdr (file-attributes filename))))

(defun save-buffer (&optional args)
  "Save current buffer in visited file if modified.  Versions described below.

By default, makes the previous version into a backup file
 if previously requested or if this is the first save.
With 1 or 3 \\[universal-argument]'s, marks this version
 to become a backup when the next save is done.
With 2 or 3 \\[universal-argument]'s,
 unconditionally makes the previous version into a backup file.
With argument of 0, never makes the previous version into a backup file.

If a file's name is FOO, the names of its numbered backup versions are
 FOO.~i~ for various integers i.  A non-numbered backup file is called FOO~.
Numeric backups (rather than FOO~) will be made if value of
 `version-control' is not the atom `never' and either there are already
 numeric versions of the file being backed up, or `version-control' is
 non-nil.
We don't want excessive versions piling up, so there are variables
 `kept-old-versions', which tells Emacs how many oldest versions to keep,
 and `kept-new-versions', which tells how many newest versions to keep.
 Defaults are 2 old versions and 2 new.
`dired-kept-versions' controls dired's clean-directory (.) command.
If `trim-versions-without-asking' is nil, system will query user
 before trimming versions.  Otherwise it does it silently."
  (interactive "p")
  (let ((modp (buffer-modified-p))
	(large (> (buffer-size) 50000))
	(make-backup-files (and make-backup-files (not (eq args 0)))))
    (and modp (memq args '(16 64)) (setq buffer-backed-up nil))
    (if (and modp large) (message "Saving file %s..." (buffer-file-name)))
    (basic-save-buffer)
    (and modp (memq args '(4 64)) (setq buffer-backed-up nil))))

(defun delete-auto-save-file-if-necessary ()
  "Delete the auto-save filename for the current buffer (if it has one)
if variable  delete-auto-save-files  is non-nil."
  (and buffer-auto-save-file-name delete-auto-save-files
       (not (string= buffer-file-name buffer-auto-save-file-name))
       (progn
	 (condition-case ()
	     (delete-file buffer-auto-save-file-name)
	   (file-error nil))
	 (set-buffer-auto-saved))))

(defun basic-save-buffer ()
  "Save the current buffer in its visited file, if it has been modified."  
  (interactive)
  (if (buffer-modified-p)
      (let (setmodes tempsetmodes)
	(or buffer-file-name
	    (progn
	      (setq buffer-file-name
		    (expand-file-name (read-file-name "File to save in: ") nil)
		    default-directory (file-name-directory buffer-file-name))
	      (auto-save-mode auto-save-default)))
	(if (not (file-writable-p buffer-file-name))
	    (if (yes-or-no-p
		 (format "File %s is write-protected; try to save anyway? "
			 (file-name-nondirectory buffer-file-name)))
		(setq tempsetmodes t)
	      (error
   "Attempt to save to a file which you aren't allowed to write")))
	(or (verify-visited-file-modtime (current-buffer))
	    (not (file-exists-p buffer-file-name))
	    (yes-or-no-p
	      "Disk file has changed since visited or saved.  Save anyway? ")
	    (error "Save not confirmed"))
	(or buffer-backed-up
	    (setq setmodes (backup-buffer)))
	(save-restriction
	  (widen)
	  (and (> (point-max) 1)
	       (/= (char-after (1- (point-max))) ?\n)
	       (or (eq require-final-newline t)
		   (and require-final-newline
			(yes-or-no-p
			 (format "Buffer %s does not end in newline.  Add one? "
				 (buffer-name)))))
	       (save-excursion
		 (goto-char (point-max))
		 (insert ?\n)))
	  (let ((hooks write-file-hooks)
		(done nil))
	    (while (and hooks
			(not (setq done (funcall (car hooks)))))
	      (setq hooks (cdr hooks)))
	    ;; If a hook returned t, file is already "written".
	    (cond ((not done)
		   (if file-precious-flag
		       ;; If file is precious, rename it away before
		       ;; overwriting it.
		       (let ((rename t) nodelete
			     (file (concat buffer-file-name "#")))
			 (condition-case ()
			     (progn (rename-file buffer-file-name file t)
				    (setq setmodes (file-modes file)))
			   (file-error (setq rename nil nodelete t)))
			 (unwind-protect
			     (progn (clear-visited-file-modtime)
				    (write-region (point-min) (point-max)
						  buffer-file-name nil t)
				    (setq rename nil))
			   ;; If rename is still t, writing failed.
			   ;; So rename the old file back to original name,
			   (if rename
			       (progn
				 (rename-file file buffer-file-name t)
				 (clear-visited-file-modtime))
			     ;; Otherwise we don't need the original file,
			     ;; so flush it.  Unless we already lost it.
			     (or nodelete
				 (condition-case ()
				     (delete-file file)
				   (error nil))))))
		     ;; If file not writable, see if we can make it writable
		     ;; temporarily while we write it.
		     ;; But no need to do so if we have just backed it up
		     ;; (setmodes is set) because that says we're superseding.
		     (cond ((and tempsetmodes (not setmodes))
			    ;; Change the mode back, after writing.
			    (setq setmodes (file-modes buffer-file-name))
			    (set-file-modes buffer-file-name 511)))
		     (write-region (point-min) (point-max) 
				   buffer-file-name nil t)))))
	  (if setmodes
	      (condition-case ()
		   (set-file-modes buffer-file-name setmodes)
		(error nil))))
	(delete-auto-save-file-if-necessary))
    (message "(No changes need to be saved)")))

(defun save-some-buffers (&optional arg exiting)
  "Save some modified file-visiting buffers.  Asks user about each one.
With argument, saves all with no questions."
  (interactive "P")
  (let (considered (list (buffer-list)))
    (while list
      (let ((buffer (car list)))
	(and (buffer-modified-p buffer)
	     (save-excursion
	       (set-buffer buffer)
	       (and
		(or buffer-file-name
		    (and exiting buffer-offer-save (> (buffer-size) 0)))
		(setq considered t)
		(or arg
		    (y-or-n-p (if buffer-file-name
				  (format "Save file %s? "
					  buffer-file-name)
				(format "Save buffer %s? " (buffer-name)))))
		(condition-case ()
		    (save-buffer)
		  (error nil))))))
      (setq list (cdr list)))
    (and save-abbrevs abbrevs-changed
	 (progn
	   (setq considered t)
	   (if (or arg
		   (y-or-n-p (format "Save abbrevs in %s? " abbrev-file-name)))
	       (write-abbrev-file nil))
	   ;; Don't keep bothering user if he says no.
	   (setq abbrevs-changed nil)))
    (if considered
	(message "")
	(message "(No files need saving)"))))

(defun not-modified ()
  "Mark current buffer as unmodified, not needing to be saved."
  (interactive)
  (message "Modification-flag cleared")
  (set-buffer-modified-p nil))

(defun toggle-read-only ()
  "Change whether this buffer is visiting its file read-only."
  (interactive)
  (setq buffer-read-only (not buffer-read-only))
  ;; Force mode-line redisplay
  (set-buffer-modified-p (buffer-modified-p)))

(defun insert-file (filename)
  "Insert contents of file FILENAME into buffer after point.
Set mark after the inserted text."
  (interactive "fInsert file: ")
  (let ((tem (insert-file-contents filename)))
    (push-mark (+ (point) (car (cdr tem))))))

(defun append-to-file (start end filename)
  "Append the contents of the region to the end of file FILENAME.
When called from a function, expects three arguments,
START, END and FILENAME.  START and END are buffer positions
saying what text to write."
  (interactive "r\nFAppend to file: ")
  (write-region start end filename t))

(defvar revert-buffer-function nil
  "Function to use to revert this buffer, or nil to do the default.")

(defun revert-buffer (&optional arg noconfirm)
  "Replace the buffer text with the text of the visited file on disk.
This undoes all changes since the file was visited or saved.
If latest auto-save file is more recent than the visited file,
asks user whether to use that instead.
First argument (optional) non-nil means don't offer to use auto-save file.
 This is the prefix arg when called interactively.

Second argument (optional) non-nil means don't ask for confirmation at all.

If revert-buffer-function's value is non-nil, it is called to do the work."
  (interactive "P")
  (if revert-buffer-function
      (funcall revert-buffer-function arg noconfirm)
    (let* ((opoint (point))
	   (auto-save-p (and (null arg) (recent-auto-save-p)
			     buffer-auto-save-file-name
			     (file-readable-p buffer-auto-save-file-name)
			     (y-or-n-p
   "Buffer has been auto-saved recently.  Revert from auto-save file? ")))
	   (file-name (if auto-save-p
			  buffer-auto-save-file-name
			buffer-file-name)))
      (cond ((null file-name)
	     (error "Buffer does not seem to be associated with any file"))
	    ((not (file-exists-p file-name))
	     (error "File %s no longer exists!" file-name))
	    ((or noconfirm
		 (yes-or-no-p (format "Revert buffer from file %s? "
				      file-name)))
	     ;; If file was backed up but has changed since,
	     ;; we shd make another backup.
	     (and (not auto-save-p)
		  (not (verify-visited-file-modtime (current-buffer)))
		  (setq buffer-backed-up nil))
	     ;; Discard all the undo information.
	     (or (eq buffer-undo-list t)
		 (setq buffer-undo-list nil))
	     (let ((buffer-read-only nil)
		   ;; Don't record undo info for the revert itself.
		   ;; Doing so chews up too much storage.
		   (buffer-undo-list t))
	       ;; Bind buffer-file-name to nil
	       ;; so that we don't try to lock the file.
	       (let ((buffer-file-name nil))
		 (or auto-save-p
		     (unlock-buffer))
		 (erase-buffer))
	       (insert-file-contents file-name (not auto-save-p)))
	     (goto-char (min opoint (point-max)))
	     (after-find-file nil)
	     t)))))

(defun recover-file (file)
  "Visit file FILE, but get contents from its last auto-save file."
  (interactive "FRecover file: ")
  (setq file (expand-file-name file))
  (if (auto-save-file-name-p file) (error "%s is an auto-save file" file))
  (let ((file-name (let ((buffer-file-name file))
		     (make-auto-save-file-name))))
    (cond ((not (file-newer-than-file-p file-name file))
	   (error "Auto-save file %s not current" file-name))
	  ((save-window-excursion
	     (if (not (eq system-type 'vax-vms))
		 (with-output-to-temp-buffer "*Directory*"
		   (buffer-flush-undo standard-output)
		   (call-process "ls" nil standard-output nil
				 "-l" file file-name)))
	     (yes-or-no-p (format "Recover auto save file %s? " file-name)))
	   (switch-to-buffer (find-file-noselect file t))
	   (let ((buffer-read-only nil))
	     (erase-buffer)
	     (insert-file-contents file-name nil))
	   (after-find-file nil))
	  (t (error "Recover-file cancelled."))))
  (setq buffer-auto-save-file-name nil)
  (message "Auto-save off in this buffer till you do M-x auto-save-mode."))

(defun kill-some-buffers ()
  "For each buffer, ask whether to kill it."
  (interactive)
  (let ((list (buffer-list)))
    (while list
      (let* ((buffer (car list))
	     (name (buffer-name buffer)))
	(and (not (string-equal name ""))
	     (/= (aref name 0) ? )
	     (yes-or-no-p
	      (format "Buffer %s %s.  Kill? "
		      name
		      (if (buffer-modified-p buffer)
			  "HAS BEEN EDITED" "is unmodified")))
	     (kill-buffer buffer)))
      (setq list (cdr list)))))

(defun auto-save-mode (arg)
  "Toggle auto-saving of contents of current buffer.
With arg, turn auto-saving on if arg is positive, else off."
  (interactive "P")
  (setq buffer-auto-save-file-name
        (and (if (null arg)
		 (not buffer-auto-save-file-name)
	       (or (eq arg t) (listp arg) (and (integerp arg) (> arg 0))))
	     (if (and buffer-file-name auto-save-visited-file-name
		      (not buffer-read-only))
		 buffer-file-name
	       (make-auto-save-file-name))))
  (if (interactive-p)
      (message "Auto-save %s (in this buffer)"
	       (if buffer-auto-save-file-name "on" "off")))
  buffer-auto-save-file-name)

(defun rename-auto-save-file ()
  "Adjust current buffer's auto save file name for current conditions.
Also rename any existing auto save file."
  (let ((osave buffer-auto-save-file-name))
    (setq buffer-auto-save-file-name
	  (make-auto-save-file-name))
    (if (and osave buffer-auto-save-file-name
	     (not (string= buffer-auto-save-file-name buffer-file-name))
	     (not (string= buffer-auto-save-file-name osave))
	     (file-exists-p osave))
	(rename-file osave buffer-auto-save-file-name t))))

(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.
Does not consider auto-save-visited-file-name; that is checked
before calling this function.
You can redefine this for customization.
See also auto-save-file-name-p."
  (if buffer-file-name
      (concat (file-name-directory buffer-file-name)
	      "#"
	      (file-name-nondirectory buffer-file-name)
	      "#")
    (expand-file-name (concat "#%" (buffer-name) "#"))))

(defun auto-save-file-name-p (filename)
  "Return non-nil if FILENAME can be yielded by make-auto-save-file-name.
FILENAME should lack slashes.
You can redefine this for customization."
  (string-match "^#.*#$" filename))

(defconst list-directory-brief-switches "-CF"
  "*Switches for list-directory to pass to `ls' for brief listing,")
(defconst list-directory-verbose-switches "-l"
  "*Switches for list-directory to pass to `ls' for verbose listing,")

(defun list-directory (dirname &optional verbose)
  "Display a list of files in or matching DIRNAME, a la `ls'.
DIRNAME is globbed by the shell if necessary.
Prefix arg (second arg if noninteractive) means supply -l switch to `ls'.
Actions controlled by variables list-directory-brief-switches
 and list-directory-verbose-switches."
  (interactive (let ((pfx current-prefix-arg))
		 (list (read-file-name (if pfx "List directory (verbose): "
					 "List directory (brief): ")
				       nil default-directory nil)
		       pfx)))
  (let ((switches (if verbose list-directory-verbose-switches
		    list-directory-brief-switches))
	full-dir-p)
    (or dirname (setq dirname default-directory))
    (if (file-directory-p dirname)
	(progn
	 (setq full-dir-p t)
	 (or (string-match "/$" dirname)
	     (setq dirname (concat dirname "/")))))
    (setq dirname (expand-file-name dirname))
    (with-output-to-temp-buffer "*Directory*"
      (buffer-flush-undo standard-output)
      (princ "Directory ")
      (princ dirname)
      (terpri)
      (if full-dir-p
	  (call-process "ls" nil standard-output nil
			switches dirname)
	(let ((default-directory (file-name-directory dirname)))
	  (call-process shell-file-name nil standard-output nil
			"-c" (concat "exec ls "
				     switches " "
				     (file-name-nondirectory dirname))))))))

(defun save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer, then kill this Emacs fork.
With prefix arg, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (memq t (mapcar (function
				  (lambda (buf) (and (buffer-file-name buf)
						     (buffer-modified-p buf))))
				(buffer-list))))
	   (yes-or-no-p "Modified buffers exist; exit anyway? "))
       (or (not (fboundp 'process-list))
	   ;; process-list is not defined on VMS.
	   (let ((processes (process-list))
		 active)
	     (while processes
	       (and (memq (process-status (car processes)) '(run stop))
		    (let ((val (process-kill-without-query (car processes))))
		      (process-kill-without-query (car processes) val)
		      val)
		    (setq active t))
	       (setq processes (cdr processes)))
	     (or (not active)
		 (yes-or-no-p "Active processes exist; kill them and exit anyway? "))))
       (kill-emacs)))

(define-key ctl-x-map "\C-f" 'find-file)
(define-key ctl-x-map "\C-q" 'toggle-read-only)
(define-key ctl-x-map "\C-r" 'find-file-read-only)
(define-key ctl-x-map "\C-v" 'find-alternate-file)
(define-key ctl-x-map "\C-s" 'save-buffer)
(define-key ctl-x-map "s" 'save-some-buffers)
(define-key ctl-x-map "\C-w" 'write-file)
(define-key ctl-x-map "i" 'insert-file)
(define-key esc-map "~" 'not-modified)
(define-key ctl-x-map "\C-d" 'list-directory)
(define-key ctl-x-map "\C-c" 'save-buffers-kill-emacs)

(defvar ctl-x-4-map (make-keymap)
  "Keymap for subcommands of C-x 4")
(fset 'ctl-x-4-prefix ctl-x-4-map)
(define-key ctl-x-map "4" 'ctl-x-4-prefix)
(define-key ctl-x-4-map "f" 'find-file-other-window)
(define-key ctl-x-4-map "\C-f" 'find-file-other-window)
(define-key ctl-x-4-map "b" 'switch-to-buffer-other-window)
