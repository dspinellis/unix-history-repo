;; Run compiler as inferior of Emacs, and parse its error messages.
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

(provide 'compile)

(defvar compilation-process nil
  "Process created by compile command, or nil if none exists now.
Note that the process may have been \"deleted\" and still
be the value of this variable.")

(defvar compilation-error-list nil
  "List of error message descriptors for visiting erring functions.
Each error descriptor is a list of length two.
Its car is a marker pointing to an error message.
Its cadr is a marker pointing to the text of the line the message is about,
  or nil if that is not interesting.
The value may be t instead of a list;
this means that the buffer of error messages should be reparsed
the next time the list of errors is wanted.")

(defvar compilation-parsing-end nil
  "Position of end of buffer when last error messages parsed.")

(defvar compilation-error-message nil
  "Message to print when no more matches for compilation-error-regexp are found")

;; The filename excludes colons to avoid confusion when error message
;; starts with digits.
(defvar compilation-error-regexp
  "\\([^ :\n]+\\(: *\\|, line \\|(\\)[0-9]+\\)\\|\\([0-9]+ *of *[^ \n]+\\)"
  "Regular expression for filename/linenumber in error in compilation log.")

(defun compile (command)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it."
  (interactive (list (read-string "Compile command: " compile-command)))
  (setq compile-command command)
  (compile1 compile-command "No more errors"))

(defun grep (command)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to."
  (interactive "sRun grep (with args): ")
  (compile1 (concat "grep -n " command " /dev/null")
	    "No more grep hits" "grep"))

(defun compile1 (command error-message &optional name-of-mode)
  (save-some-buffers)
  (if compilation-process
      (if (or (not (eq (process-status compilation-process) 'run))
	      (yes-or-no-p "A compilation process is running; kill it? "))
	  (condition-case ()
	      (let ((comp-proc compilation-process))
		(interrupt-process comp-proc)
		(sit-for 1)
		(delete-process comp-proc))
	    (error nil))
	(error "Cannot have two compilation processes")))
  (setq compilation-process nil)
  (compilation-forget-errors)
  (setq compilation-error-list t)
  (setq compilation-error-message error-message)
  (setq compilation-process
	(start-process "compilation" "*compilation*"
		       shell-file-name
		       "-c" (concat "exec " command)))
  (with-output-to-temp-buffer "*compilation*"
    (princ "cd ")
    (princ default-directory)
    (terpri)
    (princ command)
    (terpri))
  (let ((regexp compilation-error-regexp))
    (save-excursion
      (set-buffer "*compilation*")
      (make-local-variable 'compilation-error-regexp)
      (setq compilation-error-regexp regexp)))
  (set-process-sentinel compilation-process 'compilation-sentinel)
  (let* ((thisdir default-directory)
	 (outbuf (process-buffer compilation-process))
	 (outwin (get-buffer-window outbuf)))
    (if (eq outbuf (current-buffer))
	(goto-char (point-max)))
    (save-excursion
      (set-buffer outbuf)
      (buffer-flush-undo outbuf)
      (let ((start (save-excursion (set-buffer outbuf) (point-min))))
	(set-window-start outwin start)
	(or (eq outwin (selected-window))
	    (set-window-point outwin start)))
      (setq default-directory thisdir)
      (fundamental-mode)
      (setq mode-name (or name-of-mode "Compilation"))
      ;; Make log buffer's mode line show process state
      (setq mode-line-process '(": %s")))))

;; Called when compilation process changes state.

(defun compilation-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 (let* ((obuf (current-buffer))
		omax opoint)
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 (setq omax (point-max) opoint (point))
		 (goto-char (point-max))
		 (insert ?\n mode-name " " msg)
		 (forward-char -1)
		 (insert " at "
			 (substring (current-time-string) 0 -5))
		 (forward-char 1)
		 (setq mode-line-process
		       (concat ": "
			       (symbol-name (process-status proc))))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     (setq compilation-process nil)
	     ;; Force mode line redisplay soon
	     (set-buffer-modified-p (buffer-modified-p)))
	   (if (and opoint (< opoint omax))
	       (goto-char opoint))
	   (set-buffer obuf)))))

(defun kill-compilation ()
  "Kill the process made by the \\[compile] command."
  (interactive)
  (if compilation-process
      (interrupt-process compilation-process)))

(defun kill-grep ()
  "Kill the process made by the \\[grep] command."
  (interactive)
  (if compilation-process
      (interrupt-process compilation-process)))

(defun next-error (&optional argp)
  "Visit next compilation error message and corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.
A non-nil argument (prefix arg, if interactive)
means reparse the error message buffer and start at the first error."
  (interactive "P")
  (if (or (eq compilation-error-list t)
	  argp)
      (progn (compilation-forget-errors)
	     (setq compilation-parsing-end 1)))
  (if compilation-error-list
      nil
    (save-excursion
      (switch-to-buffer "*compilation*")
      (set-buffer-modified-p nil)
      (compilation-parse-errors)))
  (let ((next-error (car compilation-error-list)))
    (if (null next-error)
	(error (concat compilation-error-message
		       (if (and compilation-process
				(eq (process-status compilation-process)
				    'run))
			   " yet" ""))))
    (setq compilation-error-list (cdr compilation-error-list))
    (if (null (car (cdr next-error)))
	nil
      (switch-to-buffer (marker-buffer (car (cdr next-error))))
      (goto-char (car (cdr next-error)))
      (set-marker (car (cdr next-error)) nil))
    (let* ((pop-up-windows t)
	   (w (display-buffer (marker-buffer (car next-error)))))
      (set-window-point w (car next-error))
      (set-window-start w (car next-error)))
    (set-marker (car next-error) nil)))

;; Set compilation-error-list to nil, and
;; unchain the markers that point to the error messages and their text,
;; so that they no longer slow down gap motion.
;; This would happen anyway at the next garbage collection,
;; but it is better to do it right away.
(defun compilation-forget-errors ()
  (if (eq compilation-error-list t)
      (setq compilation-error-list nil))
  (while compilation-error-list
    (let ((next-error (car compilation-error-list)))
      (set-marker (car next-error) nil)
      (if (car (cdr next-error))
	  (set-marker (car (cdr next-error)) nil)))
    (setq compilation-error-list (cdr compilation-error-list))))

(defun compilation-parse-errors ()
  "Parse the current buffer as error messages.
This makes a list of error descriptors, compilation-error-list.
For each source-file, line-number pair in the buffer,
the source file is read in, and the text location is saved in compilation-error-list.
The function next-error, assigned to \\[next-error], takes the next error off the list
and visits its location."
  (setq compilation-error-list nil)
  (message "Parsing error messages...")
  (let (text-buffer
	last-filename last-linenum)
    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(forward-line 2))
    (while (re-search-forward compilation-error-regexp nil t)
      (let (linenum filename
	    error-marker text-marker)
	;; Extract file name and line number from error message.
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (goto-char (point-max))
	  (skip-chars-backward "[0-9]")
	  ;; If it's a lint message, use the last file(linenum) on the line.
	  ;; Normally we use the first on the line.
	  (if (= (preceding-char) ?\()
	      (progn
		(narrow-to-region (point-min) (1+ (buffer-size)))
		(end-of-line)
		(re-search-backward compilation-error-regexp)
		(skip-chars-backward "^ \t\n")
		(narrow-to-region (point) (match-end 0))
		(goto-char (point-max))
		(skip-chars-backward "[0-9]")))
	  ;; Are we looking at a "filename-first" or "line-number-first" form?
	  (if (looking-at "[0-9]")
	      (progn
		(setq linenum (read (current-buffer)))
		(goto-char (point-min)))
	    ;; Line number at start, file name at end.
	    (progn
	      (goto-char (point-min))
	      (setq linenum (read (current-buffer)))
	      (goto-char (point-max))
	      (skip-chars-backward "^ \t\n")))
	  (setq filename (compilation-grab-filename)))
	;; Locate the erring file and line.
	(if (and (equal filename last-filename)
		 (= linenum last-linenum))
	    nil
	  (beginning-of-line 1)
	  (setq error-marker (point-marker))
	  ;; text-buffer gets the buffer containing this error's file.
	  (if (not (equal filename last-filename))
	      (setq text-buffer
		    (and (file-exists-p (setq last-filename filename))
			 (find-file-noselect filename))
		    last-linenum 0))
	  (if text-buffer
	      ;; Go to that buffer and find the erring line.
	      (save-excursion
		(set-buffer text-buffer)
		(if (zerop last-linenum)
		    (progn
		      (goto-char 1)
		      (setq last-linenum 1)))
		(forward-line (- linenum last-linenum))
		(setq last-linenum linenum)
		(setq text-marker (point-marker))
		(setq compilation-error-list
		      (cons (list error-marker text-marker)
			    compilation-error-list)))))
	(forward-line 1)))
    (setq compilation-parsing-end (point-max)))
  (message "Parsing error messages...done")
  (setq compilation-error-list (nreverse compilation-error-list)))

(defun compilation-grab-filename ()
  "Return a string which is a filename, starting at point.
Ignore quotes and parentheses around it, as well as trailing colons."
  (if (eq (following-char) ?\")
      (save-restriction
	(narrow-to-region (point)
			  (progn (forward-sexp 1) (point)))
	(goto-char (point-min))
	(read (current-buffer)))
    (buffer-substring (point)
		      (progn
			(skip-chars-forward "^ :,\n\t(")
			(point)))))

(define-key ctl-x-map "`" 'next-error)
