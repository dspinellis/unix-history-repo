;; Run subshell under Emacs
;; Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation, Inc.

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


(provide 'shell)

(defvar last-input-start nil
  "In a shell-mode buffer, marker for start of last unit of input.")
(defvar last-input-end nil
  "In a shell-mode buffer, marker for end of last unit of input.")

(defvar shell-mode-map nil)

(defvar shell-directory-stack nil
  "List of directories saved by pushd in this buffer's shell.")

(defvar shell-popd-regexp "popd"
  "*Regexp to match subshell commands equivalent to popd.")

(defvar shell-pushd-regexp "pushd"
  "*Regexp to match subshell commands equivalent to pushd.")

(defvar shell-cd-regexp "cd"
  "*Regexp to match subshell commands equivalent to cd.")

(defvar explicit-shell-file-name nil
  "*If non-nil, is file name to use for explicitly requested inferior shell.")

;In loaddefs.el now.
;(defconst shell-prompt-pattern
;  "^[^#$%>]*[#$%>] *"
;  "*Regexp used by Newline command to match subshell prompts.
;Anything from beginning of line up to the end of what this pattern matches
;is deemed to be prompt, and is not reexecuted.")

(defun shell-mode ()
  "Major mode for interacting with an inferior shell.
Shell name is same as buffer name, sans the asterisks.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{shell-mode-map}

Entry to this mode calls the value of shell-mode-hook with no args,
if that value is non-nil.

cd, pushd and popd commands given to the shell are watched
by Emacs to keep this buffer's default directory
the same as the shell's working directory.
Variables shell-cd-regexp, shell-pushd-regexp and shell-popd-regexp
are used to match these command names.

You can send text to the shell (or its subjobs) from other buffers
using the commands process-send-region, process-send-string
and lisp-send-defun."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'shell-mode)
  (setq mode-name "Shell")
  (setq mode-line-process '(": %s"))
  (use-local-map shell-mode-map)
  (make-local-variable 'shell-directory-stack)
  (setq shell-directory-stack nil)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (run-hooks 'shell-mode-hook))

(if shell-mode-map
    nil
  (setq shell-mode-map (make-sparse-keymap))
  (define-key shell-mode-map "\C-m" 'shell-send-input)
  (define-key shell-mode-map "\C-c\C-d" 'shell-send-eof)
  (define-key shell-mode-map "\C-c\C-u" 'kill-shell-input)
  (define-key shell-mode-map "\C-c\C-w" 'backward-kill-word)
  (define-key shell-mode-map "\C-c\C-c" 'interrupt-shell-subjob)
  (define-key shell-mode-map "\C-c\C-z" 'stop-shell-subjob)
  (define-key shell-mode-map "\C-c\C-\\" 'quit-shell-subjob)
  (define-key shell-mode-map "\C-c\C-o" 'kill-output-from-shell)
  (define-key shell-mode-map "\C-c\C-r" 'show-output-from-shell)
  (define-key shell-mode-map "\C-c\C-y" 'copy-last-shell-input))

(defvar explicit-csh-args
  (if (eq system-type 'hpux)
      ;; -T persuades HP's csh not to think it is smarter
      ;; than us about what terminal modes to use.
      '("-i" "-T")
    '("-i"))
  "Args passed to inferior shell by M-x shell, if the shell is csh.
Value is a list of strings, which may be nil.")

(defun shell ()
  "Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
Program used comes from variable explicit-shell-file-name,
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file ~/.emacs_SHELLNAME exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in shell-mode, giving commands for sending input
and controlling the subjobs of the shell.  See shell-mode.
See also variable shell-prompt-pattern.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-arguments'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

Note that many people's .cshrc files unconditionally clear the prompt.
If yours does, you will probably want to change it."
  (interactive)
  (let* ((prog (or explicit-shell-file-name
		   (getenv "ESHELL")
		   (getenv "SHELL")
		   "/bin/sh"))		     
	 (name (file-name-nondirectory prog)))
    (switch-to-buffer
     (apply 'make-shell "shell" prog
	    (if (file-exists-p (concat "~/.emacs_" name))
		(concat "~/.emacs_" name))
	    (let ((symbol (intern-soft (concat "explicit-" name "-args"))))
	      (if (and symbol (boundp symbol))
		  (symbol-value symbol)
		'("-i")))))))

(defun make-shell (name program &optional startfile &rest switches)
  (let ((buffer (get-buffer-create (concat "*" name "*")))
	proc status size)
    (setq proc (get-buffer-process buffer))
    (if proc (setq status (process-status proc)))
    (save-excursion
      (set-buffer buffer)
      ;;    (setq size (buffer-size))
      (if (memq status '(run stop))
	  nil
	(if proc (delete-process proc))
	(setq proc (apply 'start-process name buffer
			  (concat exec-directory "env")
			  (format "TERMCAP=emacs:co#%d:tc=unknown:"
				  (screen-width))
			  "TERM=emacs"
			  "EMACS=t"
			  "-"
			  (or program explicit-shell-file-name
			      (getenv "ESHELL")
			      (getenv "SHELL")
			      "/bin/sh")
			  switches))
	(cond (startfile
	       ;;This is guaranteed to wait long enough
	       ;;but has bad results if the shell does not prompt at all
	       ;;	     (while (= size (buffer-size))
	       ;;	       (sleep-for 1))
	       ;;I hope 1 second is enough!
	       (sleep-for 1)
	       (goto-char (point-max))
	       (insert-file-contents startfile)
	       (setq startfile (buffer-substring (point) (point-max)))
	       (delete-region (point) (point-max))
	       (process-send-string proc startfile)))
	(setq name (process-name proc)))
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (shell-mode))
    buffer))

(defvar shell-set-directory-error-hook 'ignore
  "Function called with no arguments when shell-send-input
recognizes a change-directory command but gets an error
trying to change Emacs's default directory.")

(defun shell-send-input ()
  "Send input to subshell.
At end of buffer, sends all text after last output
 as input to the subshell, including a newline inserted at the end.
When not at end, copies current line to the end of the buffer and sends it,
after first attempting to discard any prompt at the beginning of the line
by matching the regexp that is the value of shell-prompt-pattern if possible.
This regexp should start with \"^\"."
  (interactive)
  (or (get-buffer-process (current-buffer))
      (error "Current buffer has no process"))
  (end-of-line)
  (if (eobp)
      (progn
	(move-marker last-input-start
		     (process-mark (get-buffer-process (current-buffer))))
	(insert ?\n)
	(move-marker last-input-end (point)))
    (beginning-of-line)
    ;; Exclude the shell prompt, if any.
    (re-search-forward shell-prompt-pattern
		       (save-excursion (end-of-line) (point))
		       t)
    (let ((copy (buffer-substring (point)
				  (progn (forward-line 1) (point)))))
      (goto-char (point-max))
      (move-marker last-input-start (point))
      (insert copy)
      (move-marker last-input-end (point))))
  ;; Even if we get an error trying to hack the working directory,
  ;; still send the input to the subshell.
  (condition-case ()
      (save-excursion
	(goto-char last-input-start)
	(shell-set-directory))
    (error (funcall shell-set-directory-error-hook)))
  (let ((process (get-buffer-process (current-buffer))))
    (process-send-region process last-input-start last-input-end)
    (set-marker (process-mark process) (point))))

;;;  If this code changes (shell-send-input and shell-set-directory),
;;;  the customization tutorial in
;;;  info/customizing-tutorial must also change, since it explains this
;;;  code.  Please let marick@gswd-vms.arpa know of any changes you
;;;  make. 

(defun shell-set-directory ()
  (cond ((and (looking-at shell-popd-regexp)
	      (memq (char-after (match-end 0)) '(?\; ?\n)))
	 (if shell-directory-stack
	     (progn
	       (cd (car shell-directory-stack))
	       (setq shell-directory-stack (cdr shell-directory-stack)))))
	((looking-at shell-pushd-regexp)
	 (cond ((memq (char-after (match-end 0)) '(?\; ?\n))
		(if shell-directory-stack
		    (let ((old default-directory))
		      (cd (car shell-directory-stack))
		      (setq shell-directory-stack
			    (cons old (cdr shell-directory-stack))))))
	       ((memq (char-after (match-end 0)) '(?\  ?\t))
		(let (dir)
		  (skip-chars-forward "^ ")
		  (skip-chars-forward " \t")
		  (if (file-directory-p
			(setq dir
			      (expand-file-name
				(substitute-in-file-name
				  (buffer-substring
				    (point)
				    (progn
				      (skip-chars-forward "^\n \t;")
				      (point)))))))
		      (progn
			(setq shell-directory-stack
			      (cons default-directory shell-directory-stack))
			(cd dir)))))))
	((looking-at shell-cd-regexp)
	 (cond ((memq (char-after (match-end 0)) '(?\; ?\n))
		(cd (getenv "HOME")))
	       ((memq (char-after (match-end 0)) '(?\  ?\t))
		(let (dir)
		  (forward-char 3)
		  (skip-chars-forward " \t")
		  (if (file-directory-p
			(setq dir
			      (expand-file-name
				(substitute-in-file-name
				  (buffer-substring
				    (point)
				    (progn
				      (skip-chars-forward "^\n \t;")
				      (point)))))))
		      (cd dir))))))))
  
(defun shell-send-eof ()
  "Send eof to subshell (or to the program running under it)."
  (interactive)
  (process-send-eof))

(defun kill-output-from-shell ()
  "Kill all output from shell since last input."
  (interactive)
  (goto-char (point-max))
  (beginning-of-line)
  (kill-region last-input-end (point))
  (insert "*** output flushed ***\n")
  (goto-char (point-max)))

(defun show-output-from-shell ()
  "Display start of this batch of shell output at top of window.
Also put cursor there."
  (interactive)
  (set-window-start (selected-window) last-input-end)
  (goto-char last-input-end))

(defun copy-last-shell-input ()
  "Copy previous shell input, sans newline, and insert before point."
  (interactive)
  (insert (buffer-substring last-input-end last-input-start))
  (delete-char -1))

(defun interrupt-shell-subjob ()
  "Interrupt this shell's current subjob."
  (interactive)
  (interrupt-process nil t))

(defun kill-shell-subjob ()
  "Send kill signal to this shell's current subjob."
  (interactive)
  (kill-process nil t))

(defun quit-shell-subjob ()
  "Send quit signal to this shell's current subjob."
  (interactive)
  (quit-process nil t))

(defun stop-shell-subjob ()
  "Stop this shell's current subjob."
  (interactive)
  (stop-process nil t))

(defun kill-shell-input ()
  "Kill all text since last stuff output by the shell or its subjobs."
  (interactive)
  (kill-region (process-mark (get-buffer-process (current-buffer)))
	       (point)))

(defvar inferior-lisp-mode-map nil)
(if inferior-lisp-mode-map
    nil
  (setq inferior-lisp-mode-map (copy-alist shell-mode-map))
  (lisp-mode-commands inferior-lisp-mode-map)
  (define-key inferior-lisp-mode-map "\e\C-x" 'lisp-send-defun))

(defvar inferior-lisp-program "lisp"
  "*Program name for invoking an inferior Lisp with `run-lisp'.")

(defvar inferior-lisp-load-command "(load \"%s\")\n"
  "*Format-string for building a Lisp expression to load a file.
This format string should use %s to substitute a file name
and should result in a Lisp expression that will command the inferior Lisp
to load that file.  The default works acceptably on most Lisps.
The string \"(progn (load \\\"%s\\\" :verbose nil :print t) (values))\\\n\"
produces cosmetically superior output for this application,
but it works only in Common Lisp.")

(defvar inferior-lisp-prompt "^.*>:? *$"
  "*Regexp to recognize prompts from the inferior Lisp.
Default is right for Franz Lisp and kcl.")

(defun inferior-lisp-mode ()
  "Major mode for interacting with an inferior Lisp process.
Runs a Lisp interpreter as a subprocess of Emacs, with Lisp I/O
through an Emacs buffer.  Variable inferior-lisp-program controls
which Lisp interpreter is run.  Variables inferior-lisp-prompt
and inferior-lisp-load-command can customize this mode for different
Lisp interpreters.

Commands:
DELETE converts tabs to spaces as it moves back.
TAB indents for Lisp; with argument, shifts rest
 of expression rigidly with the current line.
Meta-Control-Q does TAB on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{shell-mode-map}

Entry to this mode calls the value of lisp-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
lisp-mode-hook is called after shell-mode-hook.

You can send text to the inferior Lisp from other buffers
using the commands process-send-region, process-send-string
and \\[lisp-send-defun]."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'inferior-lisp-mode)
  (setq mode-name "Inferior Lisp")
  (setq mode-line-process '(": %s"))
  (lisp-mode-variables t)
  (use-local-map inferior-lisp-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (run-hooks 'shell-mode-hook 'lisp-mode-hook))

(defun run-lisp ()
  "Run an inferior Lisp process, input and output via buffer *lisp*."
  (interactive)
  (switch-to-buffer (make-shell "lisp" inferior-lisp-program))
  (inferior-lisp-mode))

(defun lisp-send-defun (display-flag)
  "Send the current defun to the Lisp process made by M-x run-lisp.
With argument, force redisplay and scrolling of the *lisp* buffer.
Variable `inferior-lisp-load-command' controls formatting of
the `load' form that is set to the Lisp process."
  (interactive "P")
  (or (get-process "lisp")
      (error "No current lisp process"))
  (save-excursion
   (end-of-defun)
   (let ((end (point))
	 (filename (format "/tmp/emlisp%d" (process-id (get-process "lisp")))))
     (beginning-of-defun)
     (write-region (point) end filename nil 'nomessage)
     (process-send-string "lisp" (format inferior-lisp-load-command filename)))
   (if display-flag
       (let* ((process (get-process "lisp"))
	      (buffer (process-buffer process))
	      (w (or (get-buffer-window buffer) (display-buffer buffer)))
	      (height (window-height w))
	      (end))
	 (save-excursion
	   (set-buffer buffer)
	   (setq end (point-max))
	   (while (progn
		    (accept-process-output process)
		    (goto-char (point-max))
		    (beginning-of-line)
		    (or (= (point-max) end)
			(not (looking-at inferior-lisp-prompt)))))
	   (setq end (point-max))
	   (vertical-motion (- 4 height))
	   (set-window-start w (point)))
	 (set-window-point w end)))))

(defun lisp-send-defun-and-go ()
  "Send the current defun to the inferior Lisp, and switch to *lisp* buffer."
  (interactive)
  (lisp-send-defun nil)
  (switch-to-buffer "*lisp*"))
