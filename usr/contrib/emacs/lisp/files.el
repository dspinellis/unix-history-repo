;; File input and output commands for Emacs
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


(defconst delete-auto-save-files t
  "*Non-nil means delete a buffer's auto-save file
when the buffer is saved for real.")

(defconst make-backup-files t
  "*Create a backup of each file when it is saved for the first time.
This can be done by renaming the file or by copying, according to the
values of  backup-by-copying  and   backup-by-copying-when-linked.")

(defconst backup-by-copying nil
 "*Non-nil means create backups of files by copying rather than by renaming.")

(defconst backup-by-copying-when-linked nil
 "*Non-nil means create backups of multi-named files by copying
rather than by renaming.
This causes the alternate names to refer to the latest version as edited.")

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

(defconst save-abbrevs t
  "*Non-nil means save word abbrevs too when files are saved.")

(defconst find-file-run-dired t
  "*Non-nil says run dired if find-file is given the name of a directory.")

(defvar find-file-not-found-hook nil
  "*If non-nil specifies a function to be called for find-file on nonexistent file.
This function is called as soon as the error is detected.
buffer-file-name is already set up.")

(defvar find-file-hook nil
  "*If non-nil specifies a function to be called after a buffer
is found or reverted from a file.
The buffer's local variables (if any) will have been processed before the
function is called.")

(defvar write-file-hook nil
  "*If non-nil specifies a function to be called before writing out a buffer
to a file.")

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
  ;; (interactive "D") really doesn't do the right thing at all
  (or (string-match "/$" dir)
      (setq dir (concat dir "/")))
  (if (not (file-directory-p dir))
      (error "%s is not a directory" dir)
    (setq default-directory dir))
  (pwd))

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
  (if (null buffer-file-name)
      (error "Non-file buffer"))
  (and (buffer-modified-p)
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

(defvar ask-about-buffer-names nil
  "*Non-nil means ask user for buffer name when there is a conflict.
The default is to generate a unique name with no interaction.")

(defun create-file-buffer (filename)
  "Creates a suitably named buffer for visiting FILENAME, and returns it."
  (let ((base (file-name-nondirectory filename)))
    (if (and (get-buffer base)
	     ask-about-buffer-names)
	(get-buffer-create
	  (let ((tem (read-string (format
 "Buffer name \"%s\" is in use; type a new name, or Return to clobber: "
				    base))))
	    (if (equal tem "") base tem)))
      (generate-new-buffer base))))

(defun find-file-noselect (filename)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one,
but verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller."
  (if (file-directory-p filename)
      (if find-file-run-dired
	  (dired-noselect filename)
	(error "%s is a directory." filename))
    (let ((buf (get-file-buffer filename))
	  error)
      (if buf
	  (or (verify-visited-file-modtime buf)
	      (if (not (file-exists-p filename))
		  (progn (message "Note: file %s no longer exists" filename)
			 t))
	      (not (yes-or-no-p
		     (if (buffer-modified-p buf)
   "File has changed since last visited or saved.  Flush your changes? "
   "File has changed since last visited or saved.  Read from disk? ")))
	      (save-excursion
	        (set-buffer buf)
		(revert-buffer t)))
	(save-excursion
	  (setq buf (create-file-buffer filename)
		filename (expand-file-name filename))
	  (set-buffer buf)
	  (erase-buffer)
	  (condition-case ()
	      (insert-file-contents filename t)
	    (file-error
	     (setq error t
		   buffer-file-name filename)
	     (if find-file-not-found-hook
		 (funcall find-file-not-found-hook))))
	  (setq default-directory (file-name-directory filename))
	  (after-find-file error)))
      buf)))


(defun after-find-file (error)
  "Called after finding a file and by the default revert function.
Sets buffer mode, parses local variables, calls  find-file-hook."
  (setq buffer-read-only (not (file-writable-p buffer-file-name)))
  (if noninteractive
      nil
    (message (cond ((not buffer-read-only)
		    (if error "(New file)" ""))
		   ((not error)
		    "File is write protected")
		   ((file-attributes buffer-file-name)
		    ;; file-exists-p is not the right thing above, as that
		    ;;  returns t iff the file is READABLE by you
		    "File exists, but is read-protected.")
		   ((file-attributes default-directory)
		    "File not found and directory write-protected")
		   (t
		    "File not found and directory doesn't exist")))
    (if auto-save-default
	(auto-save-mode t)))
  (normal-mode))

(defun normal-mode ()
  "Choose the major mode for this buffer automatically.
Also sets up any specified local variables of the file.
Uses the visited file name, the -*- line, and the local variables spec.
Finishes by calling value of find-file-hook if that's not nil."
  (interactive)
  (condition-case err
		  (set-auto-mode)
    (error (message "Error processing file's mode specifications: %s"
		    (prin1-to-string err))))
  (condition-case err
		  (hack-local-variables)
    (error (message "Error processing file's local variables: %s"
		    (prin1-to-string err))))
  (and find-file-hook
       (funcall find-file-hook)))

;(defvar auto-mode-alist ...) now in loaddefs.el
(defun set-auto-mode ()
  "Select major mode appropriate for current buffer.
May base decision on visited file name (See variable  auto-mode-list)
or on buffer contents (-*- line or local variables spec), but does not look
for the \"mode:\" local variable. For that, use  hack-local-variables."
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
	  (let (case-fold-search)
	    ;; Remove backup-suffixes from file name.
	    (setq name (substring name 0
				  (or (string-match "\\.~[0-9]+~\\'" name)
				      (string-match "~\\'" name)
				      (length name))))
	    ;; Find first matching alist entry.
	    (while (and (not mode) alist)
	      (if (string-match (car (car alist)) name)
		  (setq mode (cdr (car alist))))
	      (setq alist (cdr alist))))
	  (if mode (funcall mode)))))))

(defun hack-local-variables ()
  "Parse, and bind or evaluate as appropriate, any local variables
for current buffer."
  ;; Look for "Local variables:" line in last page.
  (save-excursion
    (goto-char (point-max))
    (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
    (if (let ((case-fold-search t))
	  (search-forward "Local Variables:" nil t))
	(let ((continue t)
	      prefix prefixlen suffix beg)
	  ;; The prefix is what comes before "local variables:" in its line.
	  ;; The suffix is what comes after "local variables:" in its line.
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
	  (if suffix (setq suffix (regexp-quote suffix)))
	  (while continue
	    ;; Look at next local variable spec.
	    (forward-line 1)
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
		       (eval val))
		      (t (make-local-variable var)
			 (set var val))))))))))

(defun set-visited-file-name (filename)
  "Change name of file visited in current buffer to FILENAME.
The next time the buffer is saved it will go in the newly specified file.
nil or empty string as argument means make buffer not be visiting any file."
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
      (progn
       (setq default-directory (file-name-directory buffer-file-name))
       (or (get-buffer (file-name-nondirectory buffer-file-name))
	   (rename-buffer (file-name-nondirectory buffer-file-name)))))
  (setq buffer-backed-up nil)
  (clear-visited-file-modtime)
  (auto-save-mode (and buffer-file-name auto-save-default))
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
  "Make a backup of the disk file visited by the current buffer.
This is done before saving the buffer the first time."
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
	  (if (or (file-symlink-p buffer-file-name)
		  backup-by-copying
		  (and backup-by-copying-when-linked
		       (> (file-nlinks buffer-file-name) 1)))
	      (copy-file buffer-file-name backupname t)
	    (condition-case ()
	        (delete-file backupname)
	      (file-error nil))
	    (rename-file buffer-file-name backupname t)
	    (setq setmodes (file-modes backupname)))
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

(defun find-backup-file-name (fn)
  "Find a file name for a backup file, and suggestions for deletions.
Value is a list whose car is the name for the backup file
 and whose cdr is a list of old versions to consider deleting now."
  (if (eq version-control 'never)
      (list (concat fn "~"))
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
	  (list (concat fn "~"))
	(cons (concat fn ".~" (int-to-string (1+ high-water-mark)) "~")
	      (if (> number-to-delete 0)
		  (mapcar (function (lambda (n)
				      (concat fn ".~" (int-to-string n) "~")))
			  (let ((v (nthcdr kept-old-versions versions)))
			    (rplacd (nthcdr (1- number-to-delete) v) ())
			    v))))))))

(defun backup-extract-version (fn)
  (string-to-int (substring fn bv-length -1)))

(defun file-nlinks (filename)
  "Return number of names file FILENAME has." 
  (car (cdr (file-attributes filename))))

(defun save-buffer (&optional args)
  "Save current buffer in visited file if modified.  Versions described below.

With no arg, only backs up if first save or previously motivated.
With 1 or 3 \\[universal-argument]'s, marks this version to be backed up.
With 2 or 3 \\[universal-argument]'s, unconditionally backs up previous \
version.

If a file's name is FOO, the names of numbered versions are
 FOO.~i~ for various integers i.
Numeric backups (rather than FOO~) will be made if value of
 version-control  is not the atom never and either there are already
 numeric versions of the file being backed up, or  version-control  is
 non-nil.
dired-kept-versions  controls dired's clean-directory (.) command.
We don't want excessive versions piling up, so variables
 kept-old-versions , which tells system how many oldest versions to
 keep, and  kept-new-versions , which tells how many new versions to
 keep, are provided.  Defaults are 2 old versions and 2 new.
If  trim-versions-without-asking  is nil, system will query user
 before trimming versions.  Otherwise it does it silently."
  (interactive "p")
  (let ((modp (buffer-modified-p)))
    (and modp (memq args '(4 64)) (setq buffer-backed-up nil))
    (basic-save-buffer)
    (and modp (memq args '(16 64)) (setq buffer-backed-up nil))))

(defun delete-auto-save-file-if-necessary ()
  "Delete the auto-save filename for the current buffer (if it has one)
if variable  delete-auto-save-files  is non-nil."
  (and buffer-auto-save-file-name delete-auto-save-files
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
	    (setq buffer-file-name
		  (expand-file-name (read-file-name "File to save in: ") nil)
		  default-directory (file-name-directory buffer-file-name)))
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
	      "File has changed on disk since last visited or saved.  Save anyway? ")
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
	  (and write-file-hook
	       (funcall write-file-hook))
	  ;; If file not writable, see if we can make it writable
	  ;; temporarily while we write it.
	  ;; But no need to do so if we have just backed up the file
	  ;; (if setmodes is set) because in that case we are superseding.
	  (cond ((and tempsetmodes (not setmodes))
		 ;; Change the mode back, after writing.
		 (setq setmodes (file-modes buffer-file-name))
		 (set-file-modes buffer-file-name 511)))
	  (write-region (point-min) (point-max) buffer-file-name nil t)
	  (if setmodes
	      (condition-case ()
		   (set-file-modes buffer-file-name setmodes)
		(error nil))))
	(delete-auto-save-file-if-necessary))
    (message "(No changes need to be saved)")))

(defun save-some-buffers (&optional arg)
  "Save some modified file-visiting buffers.  Asks user about each one.
With argument, saves all with no questions."
  (interactive "P")
  (let (considered (list (buffer-list)))
    (while list
      (let ((buffer (car list)))
	(condition-case ()
	    (and (buffer-modified-p buffer)
		 (buffer-file-name buffer)
		 (setq considered t)
		 (or arg
		     (y-or-n-p (format "Save file %s? " (buffer-file-name buffer))))
		 (save-excursion
		   (set-buffer buffer)
		   (save-buffer)))
	  (error nil)))
      (setq list (cdr list)))
    (and save-abbrevs abbrevs-changed
	 (setq considered t)
	 (or arg
	     (y-or-n-p (format "Save abbrevs in %s? " abbrev-file-name)))
	 (progn
	  (write-abbrev-file nil)
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

(defun revert-buffer (&optional arg)
  "Replace the buffer text with the text of the visited file on disk.
This undoes all changes since the file was visited or saved.
If latest auto-save file is more recent than the visited file,
asks user whether to use that instead, unless a non-nil argument is given.

If revert-buffer-function's value is non-nil, it is called to do the work."
  (interactive "P")
  (if revert-buffer-function
      (funcall revert-buffer-function arg)
    (let* ((opoint (point))
	   (auto-save-p (and (null arg) (recent-auto-save-p)
			     (y-or-n-p
   "Buffer has been auto-saved recently.  Revert from auto-save file? ")))
	   (file-name (if auto-save-p
			  buffer-auto-save-file-name
			buffer-file-name)))
      (cond ((null file-name)
	     (error "Buffer does not seem to be associated with any file"))
	    ((not (file-exists-p file-name))
	     (error "File %s no longer exists!" file-name))
	    ((yes-or-no-p (format "Revert buffer from file %s? " file-name))
	     (let ((buffer-read-only nil))
	       ;; Bind buffer-file-name to nil
	       ;; so that we don't try to lock the file.
	       (let ((buffer-file-name nil))
		 (or auto-save-p
		     (unlock-buffer))
		 (erase-buffer))
	       (insert-file-contents file-name (not auto-save-p)))
	     (after-find-file nil)
	     (or find-file-hook		; the hook may have set point itself
		 (goto-char (min opoint (point-max)))))))))

(defun recover-file (file)
  "Visit file FILE, then get contents from its last auto-save file."
  (interactive "FRecover file: ")
  (find-file file)
  (let ((file-name (make-auto-save-file-name)))
    (cond ((not (file-exists-p file-name))
	   (error "Auto-save file %s does not exist" file-name))
	  ((yes-or-no-p (format "Recover buffer from file %s? " file-name))
	   (let ((buffer-read-only nil))
	     (erase-buffer)
	     (insert-file-contents file-name nil))
	   (after-find-file nil))))
  (setq buffer-auto-save-file-name nil)
  (message "Auto-save turned off, for now, in this buffer"))

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
	       (make-auto-save-file-name)))))

(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.
Does not consider auto-save-visited-file-name; that is checked
before calling this function.
This is a separate function so your .emacs file or site-init.el can redefine it.
See also auto-save-file-name-p."
  (if buffer-file-name
      (concat (file-name-directory buffer-file-name)
	      "#"
	      (file-name-nondirectory buffer-file-name))
    (expand-file-name (concat "#%" (buffer-name)))))

(defun auto-save-file-name-p (filename)
  "Return t if FILENAME can be yielded by make-auto-save-file-name.
FILENAME should lack slashes.
This is a separate function so your .emacs file or site-init.el can redefine it."
  (string-match "^#" filename))

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
	  (call-process "/bin/ls" nil standard-output nil
			switches dirname)
	(let ((default-directory (file-name-directory dirname)))
	  (call-process shell-file-name nil standard-output nil
			"-c" (concat "exec /bin/ls "
				     switches " "
				     (file-name-nondirectory dirname))))))))

(defun save-buffers-kill-emacs ()
  "Offer to save each buffer, then kill this Emacs fork."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

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
