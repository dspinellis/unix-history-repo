;; Run Scheme under Emacs
;; Copyright (C) 1986, 1987, 1989 Free Software Foundation, Inc.

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

;;; Requires C-Scheme release 5 or later
;;; Changes to Control-G handler require runtime version 13.85 or later

;;; $Header: xscheme.el,v 1.23 89/04/28 22:59:40 GMT cph Rel $

(require 'scheme)

(defvar scheme-program-name "scheme"
  "*Program invoked by the `run-scheme' command.")

(defvar scheme-band-name nil
  "*Band loaded by the `run-scheme' command.")

(defvar scheme-program-arguments nil
  "*Arguments passed to the Scheme program by the `run-scheme' command.")

(defvar xscheme-allow-pipelined-evaluation t
  "If non-nil, an expression may be transmitted while another is evaluating.
Otherwise, attempting to evaluate an expression before the previous expression
has finished evaluating will signal an error.")

(defvar xscheme-startup-message
  "This is the Scheme process buffer.
Type \\[advertised-xscheme-send-previous-expression] to evaluate the expression before point.
Type \\[xscheme-send-control-g-interrupt] to abort evaluation.
Type \\[describe-mode] for more information.

"
  "String to insert into Scheme process buffer first time it is started.
Is processed with `substitute-command-keys' first.")

(defvar xscheme-signal-death-message nil
  "If non-nil, causes a message to be generated when the Scheme process dies.")

(defun xscheme-evaluation-commands (keymap)
  (define-key keymap "\e\C-x" 'xscheme-send-definition)
  (define-key keymap "\C-x\C-e" 'advertised-xscheme-send-previous-expression)
  (define-key keymap "\eo" 'xscheme-send-buffer)
  (define-key keymap "\ez" 'xscheme-send-definition)
  (define-key keymap "\e\C-m" 'xscheme-send-previous-expression)
  (define-key keymap "\e\C-z" 'xscheme-send-region))

(defun xscheme-interrupt-commands (keymap)
  (define-key keymap "\C-c\C-s" 'xscheme-select-process-buffer)
  (define-key keymap "\C-c\C-b" 'xscheme-send-breakpoint-interrupt)
  (define-key keymap "\C-c\C-c" 'xscheme-send-control-g-interrupt)
  (define-key keymap "\C-c\C-u" 'xscheme-send-control-u-interrupt)
  (define-key keymap "\C-c\C-x" 'xscheme-send-control-x-interrupt))

(xscheme-evaluation-commands scheme-mode-map)
(xscheme-interrupt-commands scheme-mode-map)

(defun run-scheme (command-line)
  "Run an inferior Scheme process.
Output goes to the buffer `*scheme*'.
With argument, asks for a command line."
  (interactive
   (list (let ((default
		 (or xscheme-process-command-line
		     (xscheme-default-command-line))))
	   (if current-prefix-arg
	       (read-string "Run Scheme: " default)
	       default))))
  (setq xscheme-process-command-line command-line)
  (switch-to-buffer (xscheme-start-process command-line)))

(defun reset-scheme ()
  "Reset the Scheme process."
  (interactive)
  (let ((process (get-process "scheme")))
    (cond ((or (not process)
	       (not (eq (process-status process) 'run))
	       (yes-or-no-p
"The Scheme process is running, are you SURE you want to reset it? "))
	   (message "Resetting Scheme process...")
	   (if process (kill-process process t))
	   (xscheme-start-process xscheme-process-command-line)
	   (message "Resetting Scheme process...done")))))

(defun xscheme-default-command-line ()
  (concat scheme-program-name " -emacs"
	  (if scheme-program-arguments
	      (concat " " scheme-program-arguments)
	      "")
	  (if scheme-band-name
	      (concat " -band " scheme-band-name)
	      "")))

;;;; Interaction Mode

(defun scheme-interaction-mode ()
  "Major mode for interacting with the inferior Scheme process.
Like  scheme-mode  except that:

\\[advertised-xscheme-send-previous-expression] sends the expression before point to the Scheme process as input
\\[xscheme-yank-previous-send] yanks the expression most recently sent to Scheme

All output from the Scheme process is written in the Scheme process
buffer, which is initially named \"*scheme*\".  The result of
evaluating a Scheme expression is also printed in the process buffer,
preceded by the string \";Value: \" to highlight it.  If the process
buffer is not visible at that time, the value will also be displayed
in the minibuffer.  If an error occurs, the process buffer will
automatically pop up to show you the error message.

While the Scheme process is running, the modelines of all buffers in
scheme-mode are modified to show the state of the process.  The
possible states and their meanings are:

input		waiting for input
run		evaluating
gc		garbage collecting

The process buffer's modeline contains additional information where
the buffer's name is normally displayed: the command interpreter level
and type.

Scheme maintains a stack of command interpreters.  Every time an error
or breakpoint occurs, the current command interpreter is pushed on the
command interpreter stack, and a new command interpreter is started.
One example of why this is done is so that an error that occurs while
you are debugging another error will not destroy the state of the
initial error, allowing you to return to it after the second error has
been fixed.

The command interpreter level indicates how many interpreters are in
the command interpreter stack.  It is initially set to one, and it is
incremented every time that stack is pushed, and decremented every
time it is popped.  The following commands are useful for manipulating
the command interpreter stack:

\\[xscheme-send-breakpoint-interrupt]	pushes the stack once
\\[xscheme-send-control-u-interrupt]	pops the stack once
\\[xscheme-send-control-g-interrupt]	pops everything off
\\[xscheme-send-control-x-interrupt]	aborts evaluation, doesn't affect stack

Some possible command interpreter types and their meanings are:

[Evaluator]	read-eval-print loop for evaluating expressions
[Debugger]	single character commands for debugging errors
[Where]		single character commands for examining environments

Starting with release 6.2 of Scheme, the latter two types of command
interpreters will change the major mode of the Scheme process buffer
to scheme-debugger-mode , in which the evaluation commands are
disabled, and the keys which normally self insert instead send
themselves to the Scheme process.  The command character ? will list
the available commands.

For older releases of Scheme, the major mode will be be
scheme-interaction-mode , and the command characters must be sent as
if they were expressions.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-interaction-mode-map}

Entry to this mode calls the value of scheme-interaction-mode-hook
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (scheme-interaction-mode-initialize)
  (scheme-mode-variables)
  (make-local-variable 'xscheme-previous-send)
  (run-hooks 'scheme-interaction-mode-hook))

(defun scheme-interaction-mode-initialize ()
  (use-local-map scheme-interaction-mode-map)
  (setq major-mode 'scheme-interaction-mode)
  (setq mode-name "Scheme Interaction"))

(defun scheme-interaction-mode-commands (keymap)
  (define-key keymap "\C-c\C-m" 'xscheme-send-current-line)
  (define-key keymap "\C-c\C-p" 'xscheme-send-proceed)
  (define-key keymap "\C-c\C-y" 'xscheme-yank-previous-send))

(defvar scheme-interaction-mode-map nil)
(if (not scheme-interaction-mode-map)
    (progn
      (setq scheme-interaction-mode-map (make-keymap))
      (scheme-mode-commands scheme-interaction-mode-map)
      (xscheme-interrupt-commands scheme-interaction-mode-map)
      (xscheme-evaluation-commands scheme-interaction-mode-map)
      (scheme-interaction-mode-commands scheme-interaction-mode-map)))

(defun xscheme-enter-interaction-mode ()
  (save-excursion
    (set-buffer (xscheme-process-buffer))
    (if (not (eq major-mode 'scheme-interaction-mode))
	(if (eq major-mode 'scheme-debugger-mode)
	    (scheme-interaction-mode-initialize)
	    (scheme-interaction-mode)))))

(fset 'advertised-xscheme-send-previous-expression
      'xscheme-send-previous-expression)

;;;; Debugger Mode

(defun scheme-debugger-mode ()
  "Major mode for executing the Scheme debugger.
Like  scheme-mode  except that the evaluation commands
are disabled, and characters that would normally be self inserting are
sent to the Scheme process instead.  Typing ?  will show you which
characters perform useful functions.

Commands:
\\{scheme-debugger-mode-map}"
  (error "Illegal entry to scheme-debugger-mode"))

(defun scheme-debugger-mode-initialize ()
  (use-local-map scheme-debugger-mode-map)
  (setq major-mode 'scheme-debugger-mode)
  (setq mode-name "Scheme Debugger"))

(defun scheme-debugger-mode-commands (keymap)
  (let ((char ? ))
    (while (< char 127)
      (define-key keymap (char-to-string char) 'scheme-debugger-self-insert)
      (setq char (1+ char)))))

(defvar scheme-debugger-mode-map nil)
(if (not scheme-debugger-mode-map)
    (progn
      (setq scheme-debugger-mode-map (make-keymap))
      (scheme-mode-commands scheme-debugger-mode-map)
      (xscheme-interrupt-commands scheme-debugger-mode-map)
      (scheme-debugger-mode-commands scheme-debugger-mode-map)))

(defun scheme-debugger-self-insert ()
  "Transmit this character to the Scheme process."
  (interactive)
  (xscheme-send-char last-command-char))

(defun xscheme-enter-debugger-mode (prompt-string)
  (save-excursion
    (set-buffer (xscheme-process-buffer))
    (if (not (eq major-mode 'scheme-debugger-mode))
	(progn
	  (if (not (eq major-mode 'scheme-interaction-mode))
	      (scheme-interaction-mode))
	  (scheme-debugger-mode-initialize)))))

(defun xscheme-debugger-mode-p ()
  (let ((buffer (xscheme-process-buffer)))
    (and buffer
	 (save-excursion
	   (set-buffer buffer)
	   (eq major-mode 'scheme-debugger-mode)))))

;;;; Evaluation Commands

(defun xscheme-send-string (&rest strings)
  "Send the string arguments to the Scheme process.
The strings are concatenated and terminated by a newline."
  (cond ((not (xscheme-process-running-p))
	 (if (yes-or-no-p "The Scheme process has died.  Reset it? ")
	     (progn
	       (reset-scheme)
	       (xscheme-wait-for-process)
	       (goto-char (point-max))
	       (apply 'insert-before-markers strings)
	       (xscheme-send-string-1 strings))))
	((xscheme-debugger-mode-p) (error "No sends allowed in debugger mode"))
	((and (not xscheme-allow-pipelined-evaluation)
	      xscheme-running-p)
	 (error "No sends allowed while Scheme running"))
	(t (xscheme-send-string-1 strings))))

(defun xscheme-send-string-1 (strings)
  (let ((string (apply 'concat strings)))
    (xscheme-send-string-2 string)
    (if (eq major-mode 'scheme-interaction-mode)
	(setq xscheme-previous-send string))))

(defun xscheme-send-string-2 (string)
  (let ((process (get-process "scheme")))
    (send-string process (concat string "\n"))
    (if (xscheme-process-buffer-current-p)
	(set-marker (process-mark process) (point)))))

(defun xscheme-yank-previous-send ()
  "Insert the most recent expression at point."
  (interactive)
  (push-mark)
  (insert xscheme-previous-send))

(defun xscheme-select-process-buffer ()
  "Select the Scheme process buffer and move to its output point."
  (interactive)
  (let ((process (or (get-process "scheme") (error "No scheme process"))))
    (let ((buffer (or (process-buffer process) (error "No process buffer"))))
      (let ((window (get-buffer-window buffer)))
	(if window
	    (select-window window)
	    (switch-to-buffer buffer))
	(goto-char (process-mark process))))))

(defun xscheme-send-region (start end)
  "Send the current region to the Scheme process.
The region is sent terminated by a newline."
  (interactive "r")
  (if (xscheme-process-buffer-current-p)
      (progn (goto-char end)
	     (set-marker (process-mark (get-process "scheme")) end)))
  (xscheme-send-string (buffer-substring start end)))

(defun xscheme-send-definition ()
  "Send the current definition to the Scheme process.
If the current line begins with a non-whitespace character,
parse an expression from the beginning of the line and send that instead."
  (interactive)
  (let ((start nil) (end nil))
    (save-excursion
      (end-of-defun)
      (setq end (point))
      (if (re-search-backward "^\\s(" nil t)
	  (setq start (point))
	  (error "Can't find definition")))
    (xscheme-send-region start end)))

(defun xscheme-send-next-expression ()
  "Send the expression to the right of `point' to the Scheme process."
  (interactive)
  (let ((start (point)))
    (xscheme-send-region start (save-excursion (forward-sexp) (point)))))

(defun xscheme-send-previous-expression ()
  "Send the expression to the left of `point' to the Scheme process."
  (interactive)
  (let ((end (point)))
    (xscheme-send-region (save-excursion (backward-sexp) (point)) end)))

(defun xscheme-send-current-line ()
  "Send the current line to the Scheme process.
Useful for working with debugging Scheme under adb."
  (interactive)
  (let ((line
	 (save-excursion
	   (beginning-of-line)
	   (let ((start (point)))
	     (end-of-line)
	     (buffer-substring start (point))))))
    (end-of-line)
    (insert ?\n)
    (xscheme-send-string-2 line)))

(defun xscheme-send-buffer ()
  "Send the current buffer to the Scheme process."
  (interactive)
  (if (xscheme-process-buffer-current-p)
      (error "Not allowed to send this buffer's contents to Scheme"))
  (xscheme-send-region (point-min) (point-max)))

(defun xscheme-send-char (char)
  "Prompt for a character and send it to the Scheme process."
  (interactive "cCharacter to send: ")
  (send-string "scheme" (char-to-string char)))

;;;; Interrupts

(defun xscheme-send-breakpoint-interrupt ()
  "Cause the Scheme process to enter a breakpoint."
  (interactive)
  (xscheme-send-interrupt ?b nil))

(defun xscheme-send-proceed ()
  "Cause the Scheme process to proceed from a breakpoint."
  (interactive)
  (send-string "scheme" "(proceed)\n"))

(defun xscheme-send-control-g-interrupt ()
  "Cause the Scheme processor to halt and flush input.
Control returns to the top level rep loop."
  (interactive)
  (let ((inhibit-quit t))
    (cond ((not xscheme-control-g-synchronization-p)
	   (interrupt-process "scheme"))
	  (xscheme-control-g-disabled-p
	   (message "Relax..."))
	  (t
	   (setq xscheme-control-g-disabled-p t)
	   (message "Sending C-G interrupt to Scheme...")
	   (interrupt-process "scheme")
	   (send-string "scheme" (char-to-string 0))))))

(defun xscheme-send-control-u-interrupt ()
  "Cause the Scheme process to halt, returning to previous rep loop."
  (interactive)
  (xscheme-send-interrupt ?u t))

(defun xscheme-send-control-x-interrupt ()
  "Cause the Scheme process to halt, returning to current rep loop."
  (interactive)
  (xscheme-send-interrupt ?x t))

;;; This doesn't really work right -- Scheme just gobbles the first
;;; character in the input.  There is no way for us to guarantee that
;;; the argument to this procedure is the first char unless we put
;;; some kind of marker in the input stream.

(defun xscheme-send-interrupt (char mark-p)
  "Send a ^A type interrupt to the Scheme process."
  (interactive "cInterrupt character to send: ")
  (quit-process "scheme")
  (send-string "scheme" (char-to-string char))
  (if (and mark-p xscheme-control-g-synchronization-p)
      (send-string "scheme" (char-to-string 0))))

;;;; Internal Variables

(defvar xscheme-process-command-line nil
  "Command used to start the most recent Scheme process.")

(defvar xscheme-previous-send ""
  "Most recent expression transmitted to the Scheme process.")

(defvar xscheme-process-filter-state 'idle
  "State of scheme process escape reader state machine:
idle                   waiting for an escape sequence
reading-type           received an altmode but nothing else
reading-string         reading prompt string")

(defvar xscheme-running-p nil
  "This variable, if nil, indicates that the scheme process is
waiting for input.  Otherwise, it is busy evaluating something.")

(defconst xscheme-control-g-synchronization-p t
  "If non-nil, insert markers in the scheme input stream to indicate when
control-g interrupts were signalled.  Do not allow more control-g's to be
signalled until the scheme process acknowledges receipt.")

(defvar xscheme-control-g-disabled-p nil
  "This variable, if non-nil, indicates that a control-g is being processed
by the scheme process, so additional control-g's are to be ignored.")

(defvar xscheme-allow-output-p t
  "This variable, if nil, prevents output from the scheme process
from being inserted into the process-buffer.")

(defvar xscheme-prompt ""
  "The current scheme prompt string.")

(defvar xscheme-string-accumulator ""
  "Accumulator for the string being received from the scheme process.")

(defvar xscheme-string-receiver nil
  "Procedure to send the string argument from the scheme process.")

(defvar xscheme-start-hook nil
  "If non-nil, a procedure to call when the Scheme process is started.
When called, the current buffer will be the Scheme process-buffer.")

(defvar xscheme-runlight-string nil)
(defvar xscheme-mode-string nil)
(defvar xscheme-filter-input nil)

;;;; Basic Process Control

(defun xscheme-start-process (command-line)
  (let ((buffer (get-buffer-create "*scheme*")))
    (let ((process (get-buffer-process buffer)))
      (save-excursion
	(set-buffer buffer)
	(if (and process (memq (process-status process) '(run stop)))
	    (set-marker (process-mark process) (point-max))
	    (progn (if process (delete-process process))
		   (goto-char (point-max))
		   (scheme-interaction-mode)
		   (if (bobp)
		       (insert-before-markers
			(substitute-command-keys xscheme-startup-message)))
		   (setq process
			 (let ((process-connection-type nil))
			   (apply 'start-process
				  (cons "scheme"
					(cons buffer
					      (xscheme-parse-command-line
					       command-line))))))
		   (set-marker (process-mark process) (point-max))
		   (xscheme-process-filter-initialize t)
		   (xscheme-modeline-initialize)
		   (set-process-sentinel process 'xscheme-process-sentinel)
		   (set-process-filter process 'xscheme-process-filter)
		   (run-hooks 'xscheme-start-hook)))))
    buffer))

(defun xscheme-parse-command-line (string)
  (setq string (substitute-in-file-name string))
  (let ((start 0)
	(result '()))
    (while start
      (let ((index (string-match "[ \t]" string start)))
	(setq start
	      (cond ((not index)
		     (setq result
			   (cons (substring string start)
				 result))
		     nil)
		    ((= index start)
		     (string-match "[^ \t]" string start))
		    (t
		     (setq result
			   (cons (substring string start index)
				 result))
		     (1+ index))))))
    (nreverse result)))

(defun xscheme-wait-for-process ()
  (sleep-for 2)
  (while xscheme-running-p
    (sleep-for 1)))

(defun xscheme-process-running-p ()
  "True iff there is a Scheme process whose status is `run'."
  (let ((process (get-process "scheme")))
    (and process
	 (eq (process-status process) 'run))))

(defun xscheme-process-buffer ()
  (let ((process (get-process "scheme")))
    (and process (process-buffer process))))

(defun xscheme-process-buffer-window ()
  (let ((buffer (xscheme-process-buffer)))
    (and buffer (get-buffer-window buffer))))

(defun xscheme-process-buffer-current-p ()
  "True iff the current buffer is the Scheme process buffer."
  (eq (xscheme-process-buffer) (current-buffer)))

;;;; Process Filter

(defun xscheme-process-sentinel (proc reason)
  (xscheme-process-filter-initialize (eq reason 'run))
  (if (eq reason 'run)
      (xscheme-modeline-initialize)
      (progn
       (setq scheme-mode-line-process "")
       (setq xscheme-mode-string "no process")))
  (if (and (not (memq reason '(run stop)))
	   xscheme-signal-death-message)
      (progn (beep)
	     (message
"The Scheme process has died!  Do M-x reset-scheme to restart it"))))

(defun xscheme-process-filter-initialize (running-p)
  (setq xscheme-process-filter-state 'idle)
  (setq xscheme-running-p running-p)
  (setq xscheme-control-g-disabled-p nil)
  (setq xscheme-allow-output-p t)
  (setq xscheme-prompt "")
  (setq scheme-mode-line-process '(": " xscheme-runlight-string)))

(defun xscheme-process-filter (proc string)
  (let ((xscheme-filter-input string))
    (while xscheme-filter-input
      (cond ((eq xscheme-process-filter-state 'idle)
	     (let ((start (string-match "\e" xscheme-filter-input)))
	       (if start
		   (progn
		     (xscheme-process-filter-output
		      (substring xscheme-filter-input 0 start))
		     (setq xscheme-filter-input
			   (substring xscheme-filter-input (1+ start)))
		     (setq xscheme-process-filter-state 'reading-type))
		   (let ((string xscheme-filter-input))
		     (setq xscheme-filter-input nil)
		     (xscheme-process-filter-output string)))))
	    ((eq xscheme-process-filter-state 'reading-type)
	     (if (zerop (length xscheme-filter-input))
		 (setq xscheme-filter-input nil)
		 (let ((char (aref xscheme-filter-input 0)))
		   (setq xscheme-filter-input
			 (substring xscheme-filter-input 1))
		   (let ((entry (assoc char xscheme-process-filter-alist)))
		     (if entry
			 (funcall (nth 2 entry) (nth 1 entry))
			 (progn
			   (xscheme-process-filter-output ?\e char)
			   (setq xscheme-process-filter-state 'idle)))))))
	    ((eq xscheme-process-filter-state 'reading-string)
	     (let ((start (string-match "\e" xscheme-filter-input)))
	       (if start
		   (let ((string
			  (concat xscheme-string-accumulator
				  (substring xscheme-filter-input 0 start))))
		     (setq xscheme-filter-input
			   (substring xscheme-filter-input (1+ start)))
		     (setq xscheme-process-filter-state 'idle)
		     (funcall xscheme-string-receiver string))
		   (progn
		     (setq xscheme-string-accumulator
			   (concat xscheme-string-accumulator
				   xscheme-filter-input))
		     (setq xscheme-filter-input nil)))))
	    (t
	     (error "Scheme process filter -- bad state"))))))

;;;; Process Filter Output

(defun xscheme-process-filter-output (&rest args)
  (if xscheme-allow-output-p
      (let ((string (apply 'concat args)))
	(save-excursion
	  (xscheme-goto-output-point)
	  (while (string-match "\\(\007\\|\f\\)" string)
	    (let ((start (match-beginning 0))
		  (end (match-end 0)))
	      (insert-before-markers (substring string 0 start))
	      (if (= ?\f (aref string start))
		  (progn
		    (if (not (bolp))
			(insert-before-markers ?\n))
		    (insert-before-markers ?\f))
		  (beep))
	      (setq string (substring string (1+ start)))))
	  (insert-before-markers string)))))

(defun xscheme-guarantee-newlines (n)
  (if xscheme-allow-output-p
      (save-excursion
	(xscheme-goto-output-point)
	(let ((stop nil))
	  (while (and (not stop)
		      (bolp))
	    (setq n (1- n))
	    (if (bobp)
		(setq stop t)
		(backward-char))))
	(xscheme-goto-output-point)
	(while (> n 0)
	  (insert-before-markers ?\n)
	  (setq n (1- n))))))

(defun xscheme-goto-output-point ()
  (let ((process (get-process "scheme")))
    (set-buffer (process-buffer process))
    (goto-char (process-mark process))))

(defun xscheme-modeline-initialize ()
  (setq xscheme-runlight-string "")
  (setq xscheme-mode-string "")
  (setq mode-line-buffer-identification '("Scheme: " xscheme-mode-string)))

(defun xscheme-set-runlight (runlight)
  (setq xscheme-runlight-string runlight)
  (xscheme-modeline-redisplay))

(defun xscheme-modeline-redisplay ()
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

;;;; Process Filter Operations

(defvar xscheme-process-filter-alist
  '((?D xscheme-enter-debugger-mode
	xscheme-process-filter:string-action)
    (?P xscheme-set-prompt-variable
	xscheme-process-filter:string-action)
    (?R xscheme-enter-interaction-mode
	xscheme-process-filter:simple-action)
    (?b xscheme-start-gc
	xscheme-process-filter:simple-action)
    (?e xscheme-finish-gc
	xscheme-process-filter:simple-action)
    (?f xscheme-exit-input-wait
	xscheme-process-filter:simple-action)
    (?g xscheme-enable-control-g
	xscheme-process-filter:simple-action)
    (?i xscheme-prompt-for-expression
	xscheme-process-filter:string-action)
    (?m xscheme-message
	xscheme-process-filter:string-action)
    (?n xscheme-prompt-for-confirmation
	xscheme-process-filter:string-action)
    (?o xscheme-output-goto
	xscheme-process-filter:simple-action)
    (?p xscheme-set-prompt
	xscheme-process-filter:string-action)
    (?s xscheme-enter-input-wait
	xscheme-process-filter:simple-action)
    (?v xscheme-write-value
	xscheme-process-filter:string-action)
    (?w xscheme-cd
	xscheme-process-filter:string-action)
    (?z xscheme-display-process-buffer
	xscheme-process-filter:simple-action)
    (?c xscheme-unsolicited-read-char
	xscheme-process-filter:simple-action))
  "Table used to decide how to handle process filter commands.
Value is a list of entries, each entry is a list of three items.

The first item is the character that the process filter dispatches on.
The second item is the action to be taken, a function.
The third item is the handler for the entry, a function.

When the process filter sees a command whose character matches a
particular entry, it calls the handler with two arguments: the action
and the string containing the rest of the process filter's input
stream.  It is the responsibility of the handler to invoke the action
with the appropriate arguments, and to reenter the process filter with
the remaining input.")

(defun xscheme-process-filter:simple-action (action)
  (setq xscheme-process-filter-state 'idle)
  (funcall action))

(defun xscheme-process-filter:string-action (action)
  (setq xscheme-string-receiver action)
  (setq xscheme-string-accumulator "")
  (setq xscheme-process-filter-state 'reading-string))

(defconst xscheme-runlight:running "run"
  "The character displayed when the Scheme process is running.")

(defconst xscheme-runlight:input "input"
  "The character displayed when the Scheme process is waiting for input.")

(defconst xscheme-runlight:gc "gc"
  "The character displayed when the Scheme process is garbage collecting.")

(defun xscheme-start-gc ()
  (xscheme-set-runlight xscheme-runlight:gc))

(defun xscheme-finish-gc ()
  (xscheme-set-runlight
   (if xscheme-running-p xscheme-runlight:running xscheme-runlight:input)))

(defun xscheme-enter-input-wait ()
  (xscheme-set-runlight xscheme-runlight:input)
  (setq xscheme-running-p nil))

(defun xscheme-exit-input-wait ()
  (xscheme-set-runlight xscheme-runlight:running)
  (setq xscheme-running-p t))

(defun xscheme-enable-control-g ()
  (setq xscheme-control-g-disabled-p nil))

(defun xscheme-display-process-buffer ()
  (let ((window (or (xscheme-process-buffer-window)
		    (display-buffer (xscheme-process-buffer)))))
    (save-window-excursion
      (select-window window)
      (xscheme-goto-output-point)
      (if (xscheme-debugger-mode-p)
	  (xscheme-enter-interaction-mode)))))

(defun xscheme-unsolicited-read-char ()
  nil)

(defun xscheme-message (string)
  (if (not (zerop (length string)))
      (xscheme-write-message-1 string (format ";%s" string))))

(defun xscheme-write-value (string)
  (if (zerop (length string))
      (xscheme-write-message-1 "(no value)" ";No value")
      (xscheme-write-message-1 string (format ";Value: %s" string))))

(defun xscheme-write-message-1 (message-string output-string)
  (let* ((process (get-process "scheme"))
	 (window (get-buffer-window (process-buffer process))))
    (if (or (not window)
	    (not (pos-visible-in-window-p (process-mark process)
					  window)))
	(message "%s" message-string)))
  (xscheme-guarantee-newlines 1)
  (xscheme-process-filter-output output-string))

(defun xscheme-set-prompt-variable (string)
  (setq xscheme-prompt string))

(defun xscheme-set-prompt (string)
  (setq xscheme-prompt string)
  (xscheme-guarantee-newlines 2)
  (setq xscheme-mode-string (xscheme-coerce-prompt string))
  (xscheme-modeline-redisplay))

(defun xscheme-output-goto ()
  (xscheme-goto-output-point)
  (xscheme-guarantee-newlines 2))

(defun xscheme-coerce-prompt (string)
  (if (string-match "^[0-9]+ " string)
      (let ((end (match-end 0)))
	(concat (substring string 0 end)
		(let ((prompt (substring string end)))
		  (let ((entry (assoc prompt xscheme-prompt-alist)))
		    (if entry
			(cdr entry)
			prompt)))))
      string))

(defvar xscheme-prompt-alist
  '(("[Normal REPL]" . "[Evaluator]")
    ("[Error REPL]" . "[Evaluator]")
    ("[Breakpoint REPL]" . "[Evaluator]")
    ("[Debugger REPL]" . "[Evaluator]")
    ("[Visiting environment]" . "[Evaluator]")
    ("[Environment Inspector]" . "[Where]"))
  "An alist which maps the Scheme command interpreter type to a print string.")

(defun xscheme-cd (directory-string)
  (save-excursion
    (set-buffer (xscheme-process-buffer))
    (cd directory-string)))

(defun xscheme-prompt-for-confirmation (prompt-string)
  (xscheme-send-char (if (y-or-n-p prompt-string) ?y ?n)))

(defun xscheme-prompt-for-expression (prompt-string)
  (xscheme-send-string-2
   (read-from-minibuffer prompt-string nil xscheme-prompt-for-expression-map)))

(defvar xscheme-prompt-for-expression-map nil)
(if (not xscheme-prompt-for-expression-map)
    (progn
      (setq xscheme-prompt-for-expression-map
	    (copy-keymap minibuffer-local-map))
      (substitute-key-definition 'exit-minibuffer
				 'xscheme-prompt-for-expression-exit
				 xscheme-prompt-for-expression-map)))

(defun xscheme-prompt-for-expression-exit ()
  (interactive)
  (if (eq (xscheme-region-expression-p (point-min) (point-max)) 'one)
      (exit-minibuffer)
      (error "input must be a single, complete expression")))

(defun xscheme-region-expression-p (start end)
  (save-excursion
    (let ((old-syntax-table (syntax-table)))
      (unwind-protect
	  (progn
	    (set-syntax-table scheme-mode-syntax-table)
	    (let ((state (parse-partial-sexp start end)))
	      (and (zerop (car state))	;depth = 0
		   (nth 2 state)	;last-sexp exists, i.e. >= 1 sexps
		   (let ((state (parse-partial-sexp start (nth 2 state))))
		     (if (nth 2 state) 'many 'one)))))
	(set-syntax-table old-syntax-table)))))
