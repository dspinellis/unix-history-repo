;; Process Emacs shell arguments
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


; These are processed only at the beginning of the argument list.
; -batch		execute noninteractively (messages go to stdout,
;			 variable noninteractive set to t)
;			 This option must be the first in the arglist.
;			 Processed by `main' in emacs.c -- never seen by lisp
; -t file		Specify to use file rather than stdin/stdout
;			 as the terminal.
;			 This option must be the first in the arglist.
;			 Processed by `main' in emacs.c -- never seen by lisp
; -nw			Inhibit the use of any window-system-specific display
;			 code; use the current virtual terminal.
;			 This option must be the first in the arglist.
;			 Processed by `main' in emacs.c -- never seen by lisp
; -q			load no init file
; -no-init-file		same
; -u user		load user's init file
; -user user		same

; These are processed in the order encountered.
; -f function		execute function
; -funcall function	same
; -l file		load file
; -load file		same
; -i file		insert file into buffer
; -insert file		same
; file			visit file
; -kill			kill (exit) emacs

(setq top-level '(normal-top-level))

(defvar command-line-processed nil "t once command line has been processed")

(defconst inhibit-startup-message nil
  "*Non-nil inhibits the initial startup messages.
This is for use in your personal init file, once you are familiar
with the contents of the startup message.")

(defconst inhibit-default-init nil
  "*Non-nil inhibits loading the `default' library.")

(defconst command-switch-alist nil
  "Alist of command-line switches.
Elements look like (SWITCH-STRING . HANDLER-FUNCTION).
HANDLER-FUNCTION receives switch name as sole arg;
remaining command-line args are in the variable `command-line-args-left'.")

(defvar term-setup-hook nil
  "Function to be called after loading terminal-specific lisp code.
It is called with no arguments.  You can use this to override the
definitions made by the terminal-specific file.")

(defvar window-setup-hook nil)

(defconst initial-major-mode 'lisp-interaction-mode
  "Major mode command symbol to use for the initial *scratch* buffer.")

(defun normal-top-level ()
  (if command-line-processed
      (message "Back to top level.")
    (setq command-line-processed t)
    ;; In presence of symlinks, switch to cleaner form of default directory.
    (if (getenv "PWD")
	(setq default-directory (file-name-as-directory (getenv "PWD"))))
    (unwind-protect
	(command-line)
      (and term-setup-hook
	   (funcall term-setup-hook))
      (and window-setup-hook
	   (funcall window-setup-hook)))))

(defun command-line ()
  (let ((args (cdr command-line-args))
	(init (if noninteractive nil (user-login-name)))
	(done nil))
    ;; If user has not done su, use current $HOME to find .emacs.
    (and init (string= init (user-real-login-name))
	 (setq init ""))
    (while (and (not done) args)
      (let ((argi (car args)))
	(if (or (string-equal argi "-q")
		(string-equal argi "-no-init-file"))
	    (setq init nil
		  args (cdr args))
	  (if (or (string-equal argi "-u")
		  (string-equal argi "-user"))
	      (setq args (cdr args)
		    init (car args)
		    args (cdr args))
	    (setq done t)))))
    ;; Load user's init file, or load default one.
    (condition-case error
	(if init
	    (progn (load (if (eq system-type 'vax-vms)
			     "sys$login:.emacs"
			     (concat "~" init "/.emacs"))
			 t t t)
		   (or inhibit-default-init
		       (let ((inhibit-startup-message nil))
			 ;; Users are supposed to be told their rights.
			 ;; (Plus how to get help and how to undo.)
			 ;; Don't you dare turn this off for anyone
			 ;; except yourself.
			 (load "default" t t)))))
      (error (message "Error in init file")))
    (if (get-buffer "*scratch*")
	(save-excursion
	  (set-buffer "*scratch*")
	  (funcall initial-major-mode)))
    ;; Load library for our terminal type.
    ;; User init file can set term-file-prefix to nil to prevent this.
    (and term-file-prefix (not noninteractive)
	 (if window-system
	     (load (concat term-file-prefix
			   (symbol-name window-system)
			   "-win")
		   t t)
	   (let ((term (getenv "TERM"))
		 hyphend)
	     (while (and term
			 (not (load (concat term-file-prefix term) t t)))
	       ;; Strip off last hyphen and what follows, then try again
	       (if (setq hyphend (string-match "[-_][^-_]+$" term))
		   (setq term (substring term 0 hyphend))
		 (setq term nil))))))
    (command-line-1 args)
    (if noninteractive (kill-emacs t))))

(defun command-line-1 (command-line-args-left)
  (if (null command-line-args-left)
      (cond ((and (not inhibit-startup-message) (not noninteractive)
		  ;; Don't clobber a non-scratch buffer if init file
		  ;; has selected it.
		  (string= (buffer-name) "*scratch*")
		  (not (input-pending-p)))
	     ;; If there are no switches to procss, we might as well
	     ;; run this hook now, and there may be some need to do it
	     ;; before doing any output.
	     (and term-setup-hook
		  (funcall term-setup-hook))
	     ;; Don't let the hook be run twice.
	     (setq term-setup-hook nil)
	     (and window-setup-hook
		  (funcall window-setup-hook))
	     (setq window-setup-hook nil)
	     (unwind-protect
		 (progn
		   (insert (emacs-version)
			   "
Copyright (C) 1988 Free Software Foundation, Inc.\n")
		   ;; If keys have their default meanings,
		   ;; use precomputed string to save lots of time.
		   (if (and (eq (key-binding "\C-h") 'help-command)
			    (eq (key-binding "\C-xu") 'advertised-undo)
			    (eq (key-binding "\C-h\C-c") 'describe-copying)
			    (eq (key-binding "\C-h\C-d") 'describe-distribution)
			    (eq (key-binding "\C-h\C-w") 'describe-no-warranty)
			    (eq (key-binding "\C-ht") 'help-with-tutorial))
		       (insert 
       "Type C-h for help; C-x u to undo changes.  (`C-' means use CTRL key.)

GNU Emacs comes with ABSOLUTELY NO WARRANTY; type C-h C-w for full details.
You may give out copies of Emacs; type C-h C-c to see the conditions.
Type C-h C-d for information on getting the latest version.
Type C-h t for a tutorial on using Emacs.")
		     (insert (substitute-command-keys
       "Type \\[help-command] for help; \\[advertised-undo] to undo changes.  (`C-' means use CTRL key.)

GNU Emacs comes with ABSOLUTELY NO WARRANTY; type \\[describe-no-warranty] for full details.
You may give out copies of Emacs; type \\[describe-copying] to see the conditions.
Type \\[describe-distribution] for information on getting the latest version.
Type \\[help-with-tutorial] for a tutorial on using Emacs.")))
		   (set-buffer-modified-p nil)
		   (sit-for 120))
	     (erase-buffer)
	     (set-buffer-modified-p nil))))
    (let ((dir default-directory)
	  (line 0))
      (while command-line-args-left
	(let ((argi (car command-line-args-left))
	      tem)
	  (setq command-line-args-left (cdr command-line-args-left))
	  (cond ((setq tem (assoc argi command-switch-alist))
		 (funcall (cdr tem) argi))
		((or (string-equal argi "-f")  ;what the manual claims
		     (string-equal argi "-funcall")
		     (string-equal argi "-e")) ; what the source used to say
		 (setq tem (intern (car command-line-args-left)))
		 (setq command-line-args-left (cdr command-line-args-left))
		 (funcall tem))
		((or (string-equal argi "-l")
		     (string-equal argi "-load"))
		 (let ((load-path (cons default-directory load-path)))
		   (load (car command-line-args-left) nil t))
		 (setq command-line-args-left (cdr command-line-args-left)))
		((or (string-equal argi "-i")
		     (string-equal argi "-insert"))
		 (insert-file-contents (car command-line-args-left))
		 (setq command-line-args-left (cdr command-line-args-left)))
		((string-equal argi "-kill")
		 (kill-emacs t))
		((string-match "^\\+[0-9]+\\'" argi)
		 (setq line (string-to-int argi)))
		(t
		 (find-file (expand-file-name argi dir))
		 (or (zerop line)
		     (goto-line line))
		 (setq line 0))))))))
