;; Run subshell under Emacs
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


(provide 'shell)

(defvar last-input-start nil
  "In a shell-mode buffer, marker for start of last unit of input.")
(defvar last-input-end nil
  "In a shell-mode buffer, marker for start of last unit of input.")

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
using the commands send-region, send-string and lisp-send-defun."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'shell-mode)
  (setq mode-name "Shell")
  (setq mode-line-format 
	"--%1*%1*-Emacs: %17b   %M   %[(%m: %s)%]----%3p--%-")
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

Note that many people's .cshrc files unconditionally clear the prompt.
If yours does, you will probably want to change it."
  (interactive)
  (let* ((prog (or explicit-shell-file-name
		   (getenv "ESHELL")
		   (if (eq system-type 'hpux) "sh"
		     ;; On hpux people normally use csh,
		     ;; but the csh in hpux has stty sanity checking
		     ;; so it does not work under emacs.
		     (getenv "SHELL"))
		   "/bin/sh"))		     
	 (name (file-name-nondirectory prog)))
    (switch-to-buffer
     (make-shell "shell" prog
		 (if (file-exists-p (concat "~/.emacs_" name))
		     (concat "~/.emacs_" name))
		 "-i"))))

(defun make-shell (name program &optional startfile &rest switches)
  (let ((buffer (get-buffer-create (concat "*" name "*")))
	proc status size)
    (setq proc (get-buffer-process buffer))
    (if proc
	(setq status (process-status proc)))
    (save-excursion
      (set-buffer buffer)
      ;;    (setq size (buffer-size))
      (if (memq status '(run stop))
	  nil
	(if proc (delete-process proc))
	(setq proc (apply 'start-process (append (list name buffer program) switches)))
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
	       (send-string proc startfile)))
	(setq name (process-name proc)))
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (shell-mode))
    buffer))

(defun shell-send-input ()
  "Send input to subshell.
At end of buffer, sends all text after last output
 as input to the subshell, including a newline inserted at the end.
Not at end, copies current line to the end of the buffer and sends it,
after first attempting to discard any prompt at the beginning of the line
by matching the regexp that is the value of shell-prompt-pattern if possible.
This regexp should start with \"^\"."
  (interactive)
  (end-of-line)
    (if (eobp)
	(progn
	  (move-marker last-input-start
		       (process-mark (get-buffer-process (current-buffer))))
	  (insert ?\n)
	  (move-marker last-input-end (point)))
    (beginning-of-line)
    (re-search-forward shell-prompt-pattern nil t)
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
      (error nil))
  (let ((process (get-buffer-process (current-buffer))))
    (send-region process last-input-start last-input-end)
    (set-marker (process-mark process) (point))))

(defun shell-send-eof ()
  "Send eof to subshell (or to the program running under it)."
  (interactive)
  (process-send-eof))

(defun kill-output-from-shell ()
  "Kill all output from shell since last input."
  (interactive)
  (goto-char (point-max))
  (kill-region last-input-end (point))
  (insert "> output flushed ***\n"))

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

(defun inferior-lisp-mode ()
  "Major mode for interacting with an inferior Lisp process.

The following commands are available:
\\{inferior-lisp-mode-map}

Entry to this mode calls the value of lisp-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
lisp-mode-hook is called after shell-mode-hook.

You can send text to the inferior Lisp from other buffers
using the commands send-region, send-string and \\[lisp-send-defun].

Commands:
Delete converts tabs to spaces as it moves back.
Tab indents for Lisp; with argument, shifts rest
 of expression rigidly with the current line.
Meta-Control-Q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
C-d at end of buffer sends end-of-file as input.
C-d not at end or with arg deletes or kills characters.
C-u and C-w are kill commands, imitating normal Unix input editing.
C-c interrupts the shell or its current subjob if any.
C-z stops, likewise.  C-\\ sends quit signal, likewise.

C-x C-k deletes last batch of output from shell.
C-x C-v puts top of last batch of output at top of window."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'inferior-lisp-mode)
  (setq mode-name "Inferior Lisp")
  (setq mode-line-format 
	"--%1*%1*-Emacs: %17b   %M   %[(%m: %s)%]----%3p--%-")
  (lisp-mode-variables)
  (use-local-map inferior-lisp-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (run-hooks 'shell-mode-hook 'lisp-mode-hook))

(defun run-lisp ()
  "Run an inferior Lisp process, input and output via buffer *lisp*."
  (interactive)
  (switch-to-buffer (make-shell "lisp" "lisp"))
  (inferior-lisp-mode))

(defun lisp-send-defun ()
  "Send the current defun to the Lisp process made by M-x run-lisp."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (send-region "lisp" (point) end)
     (send-string "lisp" "\n"))))

(defun lisp-send-defun-and-go ()
  "Send the current defun to the inferior Lisp, and switch to *lisp* buffer."
  (interactive)
  (lisp-send-defun)
  (switch-to-buffer "*lisp*"))
