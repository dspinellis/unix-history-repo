;; File input and output over Internet using FTP
;; Copyright (C) 1987 Free Software Foundation, Inc.
;; Author mly@prep.ai.mit.edu.

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


;; you can turn this off by doing
;;  (setq ftp-password-alist 'compulsory-urinalysis)
(defvar ftp-password-alist () "Security sucks")

(defun read-ftp-user-password (host user new)
  (let (tem)
    (if (and (not new)
	     (listp ftp-password-alist)
	     (setq tem (cdr (assoc host ftp-password-alist)))
	     (or (null user)
		 (string= user (car tem))))
	tem
      (or user
	  (progn
	    (setq tem (or (and (listp ftp-password-alist)
			       (car (cdr (assoc host ftp-password-alist))))
			  (user-login-name)))
	    (setq user (read-string (format
				      "User-name for %s (default \"%s\"): "
				      host tem)))
	    (if (equal user "") (setq user tem))))
      (setq tem (cons user
		      ;; If you want to use some non-echoing string-reader,
		      ;; feel free to write it yourself.  I don't care enough.
		      (read-string (format "Password for %s@%s: " user host)
			(if (not (listp ftp-password-alist))
			    ""
			  (or (cdr (cdr (assoc host ftp-password-alist)))
			      (let ((l ftp-password-alist))
				(catch 'foo
				  (while l
				    (if (string= (car (cdr (car l))) user)
					(throw 'foo (cdr (cdr (car l))))
				      (setq l (cdr l))))
				  nil))
			      "")))))
      (message "")
      (if (and (listp ftp-password-alist)
	       (not (string= (cdr tem) "")))
	  (setq ftp-password-alist (cons (cons host tem)
					 ftp-password-alist)))
      tem)))

(defun ftp-read-file-name (prompt)
  (let ((s ""))
    (while (not (string-match "\\`[ \t]*\\([^ \t:]+\\)[ \t]*:\\(.+\\)\\'" s))
      (setq s (read-string prompt s)))
    (list (substring s (match-beginning 1) (match-end 1))
	  (substring s (match-beginning 2) (match-end 2)))))


(defun ftp-find-file (host file &optional user password)
  "FTP to HOST to get FILE, logging in as USER with password PASSWORD.
Interactively, HOST and FILE are specified by reading a string with
 a colon character separating the host from the filename.
USER and PASSWORD are defaulted from the values used when
 last ftping from HOST (unless password-remembering is disabled).
 Supply a password of the symbol `t' to override this default
 (interactively, this is done by giving a prefix arg)"
  (interactive
	(append (ftp-read-file-name "FTP get host:file: ")
		(list nil (not (null current-prefix-arg)))))
  (ftp-find-file-or-directory host file t user password))

(defun ftp-list-directory (host file &optional user password)
  "FTP to HOST to list DIRECTORY, logging in as USER with password PASSWORD.
Interactively, HOST and FILE are specified by reading a string with
 a colon character separating the host from the filename.
USER and PASSWORD are defaulted from the values used when
 last ftping from HOST (unless password-remembering is disabled).
 Supply a password of the symbol `t' to override this default
 (interactively, this is done by giving a prefix arg)"
  (interactive
	(append (ftp-read-file-name "FTP get host:directory: ")
		(list nil (not (null current-prefix-arg)))))
  (ftp-find-file-or-directory host file nil user password))

(defun ftp-find-file-or-directory (host file filep &optional user password)
  "FTP to HOST to get FILE.  Third arg is t for file, nil for directory.
Log in as USER with PASSWORD.  If USER is nil or PASSWORD is nil or t,
we prompt for the user name and password."
  (or (and user password (not (eq password t)))
      (progn (setq user (read-ftp-user-password host user (eq password t))
		   password (cdr user)
		   user (car user))))
  (let ((buffer (get-buffer-create (format "*ftp%s %s:%s*"
					   (if filep "" "-directory")
					   host file))))
    (set-buffer buffer)
    (let ((process (ftp-setup-buffer host file))
	  (case-fold-search nil))
      (let ((win nil))
	(unwind-protect
	    (if (setq win (ftp-login process host user password))
		(message "Logged in")
	      (error "Ftp login lost"))
	  (or win (delete-process process))))
      (message "Opening %s %s:%s..." (if filep "file" "directory")
	       host file)
      (if (ftp-command process
		       (format "%s \"%s\" -\nquit\n" (if filep "get" "dir")
			       file)
		       "\\(150\\|125\\).*\n"
		       "200.*\n")
	  (progn (forward-line 1)
		 (let ((buffer-read-only nil))
		   (delete-region (point-min) (point)))
		 (message "Retrieving %s:%s in background.  Bye!" host file)
		 (set-process-sentinel process
				       'ftp-asynchronous-input-sentinel)
		 process)
	(switch-to-buffer buffer)
	(let ((buffer-read-only nil))
	  (insert-before-markers "<<<Ftp lost>>>"))
	(delete-process process)
	(error "Ftp %s:%s lost" host file)))))


(defun ftp-write-file (host file &optional user password)
  "FTP to HOST to write FILE, logging in as USER with password PASSWORD.
Interactively, HOST and FILE are specified by reading a string with colon
separating the host from the filename.
USER and PASSWORD are defaulted from the values used when
 last ftping from HOST (unless password-remembering is disabled).
 Supply a password of the symbol `t' to override this default
 (interactively, this is done by giving a prefix arg)"
  (interactive
    (append (ftp-read-file-name "FTP write host:file: ")
	    (list nil (not (null current-prefix-arg)))))
  (or (and user password (not (eq password t)))
      (progn (setq user (read-ftp-user-password host user (eq password t))
		   password (cdr user)
		   user (car user))))
  (let ((buffer (get-buffer-create (format "*ftp %s:%s*" host file)))
	(tmp (make-temp-name "/tmp/emacsftp")))
    (write-region (point-min) (point-max) tmp)
    (set-buffer buffer)
    (make-local-variable 'ftp-temp-file-name)
    (setq ftp-temp-file-name tmp)
    (let ((process (ftp-setup-buffer host file))
	  (case-fold-search nil))
      (let ((win nil))
	(unwind-protect
	    (if (setq win (ftp-login process host user password))
		(message "Logged in")
	        (error "Ftp login lost"))
	  (or win (delete-process process))))
      (message "Opening file %s:%s..." host file)
      (if (ftp-command process
		       (format "send \"%s\" \"%s\"\nquit\n" tmp file)
		       "\\(150\\|125\\).*\n"
		       "200.*\n")
	  (progn (forward-line 1)
		 (let ((buffer-read-only nil))
		   (delete-region (point-min) (point)))
		 (message "Saving %s:%s in background.  Bye!" host file)
		 (set-process-sentinel process
				       'ftp-asynchronous-output-sentinel)
		 process)
	(switch-to-buffer buffer)
	(let ((buffer-read-only nil))
	  (insert-before-markers "<<<Ftp lost>>>"))
	(delete-process process)
	(error "Ftp write %s:%s lost" host file)))))


(defun ftp-setup-buffer (host file)
  (fundamental-mode)
  (and (get-buffer-process (current-buffer))
       (progn (discard-input)
	      (if (y-or-n-p (format "Kill process \"%s\" in %s? "
				    (process-name (get-buffer-process
						    (current-buffer)))
				    (buffer-name (current-buffer))))
		  (while (get-buffer-process (current-buffer))
		    (kill-process (get-buffer-process (current-buffer))))
		(error "Foo"))))
  ;(buffer-flush-undo (current-buffer))
  (setq buffer-read-only nil)
  (erase-buffer)
  (make-local-variable 'ftp-host)
  (setq ftp-host host)
  (make-local-variable 'ftp-file)
  (setq ftp-file file)
  (setq buffer-read-only t)
  (start-process "ftp" (current-buffer) "ftp" "-i" "-n" "-g"))


(defun ftp-login (process host user password)
  (message "FTP logging in as %s@%s..." user host)
  (if (ftp-command process
		   (format "open %s\nuser %s %s\n" host user password)
		   "230.*\n"
		   "\\(Connected to \\|220\\|331\\).*\n")
      t
    (switch-to-buffer (process-buffer process))
    (delete-process process)
    (if (listp ftp-password-alist)
	(setq ftp-password-alist (delq (assoc host ftp-password-alist)
				       ftp-password-alist)))
    nil))

(defun ftp-command (process command win ignore)
  (process-send-string process command)
  (let ((p 1)
	(case-fold-search t))
    (while (numberp p)
      (cond ;((not (bolp)))
	    ((looking-at win)
	     (goto-char (point-max))
	     (setq p t))
	    ((looking-at "^ftp> \\|^\n")
	     (goto-char (match-end 0)))
	    ((looking-at ignore)
	     ;; Ignore status messages whose codes indicate no problem.
	     (forward-line 1))
	    ((not (search-forward "\n" nil t))
	     ;; the way asynchronous process-output fucks with (point)
	     ;;  is really really disgusting.
	     (setq p (point))
	     (condition-case ()
		 (accept-process-output process)
	       (error nil))
	     (goto-char p))
	    ((looking-at "^[a-z]")
	     ;; Ignore any lines that don't have error codes.
	     (forward-line 1))
	    (t
	     (setq p nil))))
    p))


(defun ftp-asynchronous-input-sentinel (process msg)
  (ftp-sentinel process msg t t))
(defun ftp-synchronous-input-sentinel (process msg)
  (ftp-sentinel process msg nil t))
(defun ftp-asynchronous-output-sentinel (process msg)
  (ftp-sentinel process msg t nil))
(defun ftp-synchronous-output-sentinel (process msg)
  (ftp-sentinel process msg nil nil))

(defun ftp-sentinel (process msg asynchronous input)
  (cond ((null (buffer-name (process-buffer process)))
	 ;; deleted buffer
	 (set-process-buffer process nil))
	((and (eq (process-status process) 'exit)
	      (= (process-exit-status process) 0))
	 (save-excursion
	   (set-buffer (process-buffer process))
	   (let (msg
		 (r (if input "[0-9]+ bytes received in [0-9]+\\.[0-9]+ seconds.*$" "[0-9]+ bytes sent in [0-9]+\\.[0-9]+ seconds.*$")))
	     (goto-char (point-max))
	     (search-backward "226 ")
	     (if (looking-at r)
		 (search-backward "226 "))
	     (let ((p (point)))
	       (setq msg (concat (format "ftp %s %s:%s done"
					 (if input "read" "write")
					 ftp-host ftp-file)
				 (if (re-search-forward r nil t)
				     (concat ": " (buffer-substring
						    (match-beginning 0)
						    (match-end 0)))
				     "")))
	       (let ((buffer-read-only nil))
		 (delete-region p (point-max)))
	       (save-excursion
		 (set-buffer (get-buffer-create "*ftp log*"))
		 (let ((buffer-read-only nil))
		   (insert msg ?\n)))
	       (set-buffer-modified-p nil))
	     (if (not input)
		 (progn
		   (condition-case ()
		       (and (boundp 'ftp-temp-file-name)
			    ftp-temp-file-name
			    (delete-file ftp-temp-file-name))
		     (error nil))
		   (kill-buffer (current-buffer)))
	       ;; You don't want to look at this.
	       (let ((kludge (generate-new-buffer (format "%s:%s (ftp)"
							  ftp-host ftp-file))))
		 (setq kludge (prog1 (buffer-name kludge) (kill-buffer kludge)))
		 (rename-buffer kludge)
		 ;; ok, you can look again now.
		 (ftp-setup-write-file-hooks)))
	     (if (and asynchronous
		      ;(waiting-for-user-input-p)
		      )
		 (progn (message "%s" msg)
			(sleep-for 2))))))
	((memq (process-status process) '(exit signal))
	 (save-excursion
	   (set-buffer (process-buffer process))
	   (setq msg (format "Ftp died (buffer %s): %s"
			     (buffer-name (current-buffer))
			     msg))
	   (let ((buffer-read-only nil))
	     (goto-char (point-max))
	     (insert ?\n ?\n msg))
	   (delete-process proc)
	   (set-buffer (get-buffer-create "*ftp log*"))
	   (let ((buffer-read-only nil))
	     (goto-char (point-max))
	     (insert msg))
	   (if (waiting-for-user-input-p)
	       (error "%s" msg))))))

(defun ftp-setup-write-file-hooks ()
  (let ((hooks write-file-hooks))
    (make-local-variable 'write-file-hooks)
    (setq write-file-hooks (append write-file-hooks
				   '(ftp-write-file-hook))))
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'ftp-revert-buffer)
  (setq default-directory "/tmp/")
  (setq buffer-file-name (concat default-directory
				 (make-temp-name
				  (buffer-name (current-buffer)))))
  (setq buffer-read-only nil))

(defun ftp-write-file-hook ()
  (let ((process (ftp-write-file ftp-host ftp-file)))
    (set-process-sentinel process 'ftp-synchronous-output-sentinel)
    (message "FTP writing %s:%s..." ftp-host ftp-file)
    (while (eq (process-status process) 'run)
      (condition-case ()
	  (accept-process-output process)
	(error nil)))
    (and (eq (process-status process) 'exit)
	 (= (process-exit-status process) 0)
	 (set-buffer-modified-p nil)))
  (message "Written")
  t)

(defun ftp-revert-buffer (&rest ignore)
  (let ((process (ftp-find-file ftp-host ftp-file)))
    (set-process-sentinel process 'ftp-synchronous-input-sentinel)
    (message "FTP reverting %s:%s" ftp-host ftp-file)
    (while (eq (process-status process) 'run)
      (condition-case ()
	  (accept-process-output process)
	(error nil)))
    (and (eq (process-status process) 'exit)
	 (= (process-exit-status process) 0)
	 (set-buffer-modified-p nil))
    (message "Reverted")))
