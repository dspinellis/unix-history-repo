;; Run dbx under Emacs
;; Copyright (C) 1988 Free Software Foundation, Inc.
;; Main author Masanobu UMEDA (umerin@flab.fujitsu.junet)

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

(require 'shell)

(defvar dbx-trace-flag nil
  "Dbx trace switch.")

(defvar dbx-process nil
  "The process in which dbx is running.")

(defvar dbx-break-point
  "stopped in .* at line \\([0-9]*\\) in file \"\\([^\"]*\\)\""
  "Regexp of pattern that dbx writes at break point.")

(defvar inferior-dbx-mode-map nil)
(if inferior-dbx-mode-map
    nil
  (setq inferior-dbx-mode-map (copy-keymap shell-mode-map))
  (define-key inferior-dbx-mode-map "\C-cw" 'dbx-where)
  (define-key inferior-dbx-mode-map "\C-c\C-t" 'dbx-trace-mode)
  (define-key ctl-x-map "\C-@" 'dbx-stop-at))

(defun inferior-dbx-mode ()
  "Major mode for interacting with an inferior Dbx process.

The following commands are available:
\\{inferior-dbx-mode-map}

Entry to this mode calls the value of dbx-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
dbx-mode-hook is called after shell-mode-hook.

You can display the debugging program in other window and point out
where you are looking at using the command \\[dbx-where].

\\[dbx-trace-mode] toggles dbx-trace mode. In dbx-trace mode,
debugging program is automatically traced using output from dbx.

The command \\[dbx-stop-at] sets break point at current line of the
program in the buffer. Major mode name of the buffer must be in
dbx-language-mode-list.

Commands:

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[shell-send-eof] sends end-of-file as input.
\\[kill-shell-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[interrupt-shell-subjob] interrupts the shell or its current subjob if any.
\\[stop-shell-subjob] stops, likewise. \\[quit-shell-subjob] sends quit signal, likewise.
\\[dbx-where] displays debugging program in other window and
 points out where you are looking at.
\\[dbx-trace-mode] toggles dbx-trace mode.
\\[dbx-stop-at] sets break point at current line."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'inferior-dbx-mode)
  (setq mode-name "Inferior Dbx")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-dbx-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'dbx-trace-flag)
  (setq dbx-trace-flag nil)
  (make-variable-buffer-local 'shell-prompt-pattern)
  (setq shell-prompt-pattern "^[^)]*dbx) *") ;Set dbx prompt pattern
  (or (assq 'dbx-trace-flag minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(dbx-trace-flag " Trace") minor-mode-alist)))
  (run-hooks 'shell-mode-hook 'dbx-mode-hook))

(defun run-dbx (path)
  "Run an inferior Dbx process, input and output via buffer *dbx*."
  (interactive "fProgram to debug: ")
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*dbx-" file "*"))
    (setq default-directory (file-name-directory path))
    (switch-to-buffer (make-shell (concat "dbx-" file) "dbx" nil file)))
  (setq dbx-process (get-buffer-process (current-buffer)))
  (set-process-filter dbx-process 'dbx-filter)
  (inferior-dbx-mode))

(defun dbx-trace-mode (arg)
  "Toggle dbx-trace mode.
With arg, turn dbx-trace mode on iff arg is positive.
In dbx-trace mode, user program is automatically traced."
  (interactive "P")
  (if (not (eql major-mode 'inferior-dbx-mode))
      (error "Dbx-trace mode is effective in inferior-dbx mode only."))
  (setq dbx-trace-flag
	(if (null arg)
	    (not dbx-trace-flag)
	  (> (prefix-numeric-value arg) 0)))
  ;; Force mode line redisplay
  (set-buffer-modified-p (buffer-modified-p)))

(defun dbx-filter (process string)
  "Trace debugging program automatically if dbx-trace-flag is not nil."
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (let ((beg (point)))
      (insert string)
      (if dbx-trace-flag		;Trace mode is on?
	  (dbx-where beg t)))
    (if (process-mark process)
	(set-marker (process-mark process) (point-max))))
  (if (eq (process-buffer process)
	  (current-buffer))
      (goto-char (point-max)))
  )

(defun dbx-where (&optional begin quiet)
  "Display dbx'ed program in other window and point out where you are looking at.
BEGIN bounds the search. If QUIET, just return nil (no error) if fail."
  (interactive)
  (let (file line)
    (save-excursion
      (if (re-search-backward dbx-break-point begin quiet)
	  (progn
	    (setq line (buffer-substring (match-beginning 1) (match-end 1)))
	    (setq file (buffer-substring (match-beginning 2) (match-end 2)))
	    )))
    (if (and file line)			;Find break point?
	(progn
	  (find-file-other-window (expand-file-name file nil))
	  (goto-line (string-to-int line)) ;Jump to the line
	  (beginning-of-line)
	  (setq overlay-arrow-string "=>")
	  (or overlay-arrow-position 
	      (setq overlay-arrow-position (make-marker)))
	  (set-marker overlay-arrow-position (point) (current-buffer))
	  (other-window 1))		;Return to dbx
      )))

(defun dbx-stop-at ()
  "Set break point at current line."
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name))
	(line (save-restriction
		(widen)
		(1+ (count-lines 1 (point))))))
    (send-string dbx-process
		 (concat "stop at \"" file-name "\":" line "\n"))))
