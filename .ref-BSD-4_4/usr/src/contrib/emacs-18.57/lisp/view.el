;; View: Peruse file or buffer without editing.
;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Principal author K. Shane Hartman

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


(provide 'view)

(defvar view-mode-map nil)
(if view-mode-map
    nil
  (setq view-mode-map (make-keymap))
  (fillarray view-mode-map 'View-undefined)
  (define-key view-mode-map "\C-c" 'exit-recursive-edit)
  (define-key view-mode-map "\C-z" 'suspend-emacs)
  (define-key view-mode-map "q" 'exit-recursive-edit)
  (define-key view-mode-map "-" 'negative-argument)
  (define-key view-mode-map "0" 'digit-argument)
  (define-key view-mode-map "1" 'digit-argument)
  (define-key view-mode-map "2" 'digit-argument)
  (define-key view-mode-map "3" 'digit-argument)
  (define-key view-mode-map "4" 'digit-argument)
  (define-key view-mode-map "5" 'digit-argument)
  (define-key view-mode-map "6" 'digit-argument)
  (define-key view-mode-map "7" 'digit-argument)
  (define-key view-mode-map "8" 'digit-argument)
  (define-key view-mode-map "9" 'digit-argument)
  (define-key view-mode-map "\C-u" 'universal-argument)
  (define-key view-mode-map "\e" nil)
  (define-key view-mode-map "\C-x" 'Control-X-prefix)
  (define-key view-mode-map "\e-" 'negative-argument)
  (define-key view-mode-map "\e0" 'digit-argument)
  (define-key view-mode-map "\e1" 'digit-argument)
  (define-key view-mode-map "\e2" 'digit-argument)
  (define-key view-mode-map "\e3" 'digit-argument)
  (define-key view-mode-map "\e4" 'digit-argument)
  (define-key view-mode-map "\e5" 'digit-argument)
  (define-key view-mode-map "\e6" 'digit-argument)
  (define-key view-mode-map "\e7" 'digit-argument)
  (define-key view-mode-map "\e8" 'digit-argument)
  (define-key view-mode-map "\e9" 'digit-argument)
  (define-key view-mode-map "<" 'beginning-of-buffer)
  (define-key view-mode-map ">" 'end-of-buffer)
  (define-key view-mode-map "\ev" 'View-scroll-lines-backward)
  (define-key view-mode-map "\C-v" 'View-scroll-lines-forward)
  (define-key view-mode-map " " 'View-scroll-lines-forward)
  (define-key view-mode-map "\177" 'View-scroll-lines-backward)
  (define-key view-mode-map "\n" 'View-scroll-one-more-line)
  (define-key view-mode-map "\r" 'View-scroll-one-more-line)
  (define-key view-mode-map "\C-l" 'recenter)
  (define-key view-mode-map "z" 'View-scroll-lines-forward-set-scroll-size)
  (define-key view-mode-map "g" 'View-goto-line)
  (define-key view-mode-map "=" 'what-line)
  (define-key view-mode-map "." 'set-mark-command)
  (define-key view-mode-map "\C-@" 'set-mark-command)
  (define-key view-mode-map "'" 'View-back-to-mark)
  (define-key view-mode-map "@" 'View-back-to-mark)  
  (define-key view-mode-map "x" 'exchange-point-and-mark)
  (define-key view-mode-map "h" 'Helper-describe-bindings)
  (define-key view-mode-map "?" 'Helper-describe-bindings)
  (define-key view-mode-map "\C-h" 'Helper-help)
  (define-key view-mode-map "\C-n" 'next-line)
  (define-key view-mode-map "\C-p" 'previous-line)
  (define-key view-mode-map "\C-s" 'isearch-forward)
  (define-key view-mode-map "\C-r" 'isearch-backward)
  (define-key view-mode-map "s" 'isearch-forward)
  (define-key view-mode-map "r" 'isearch-backward)
  (define-key view-mode-map "/" 'View-search-regexp-forward)
  (define-key view-mode-map "\\" 'View-search-regexp-backward)
  ;; This conflicts with the standard binding of isearch-regexp-forward
  (define-key view-mode-map "\e\C-s" 'View-search-regexp-forward)
  (define-key view-mode-map "\e\C-r" 'View-search-regexp-backward)  
  (define-key view-mode-map "n" 'View-search-last-regexp-forward)
  (define-key view-mode-map "p" 'View-search-last-regexp-backward)
  )


(defun view-file (file-name)
  "View FILE in View mode, returning to previous buffer when done.
The usual Emacs commands are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type ? or h while viewing.

Calls the value of  view-hook  if that is non-nil."
  (interactive "fView file: ")
  (let ((had-a-buf (get-file-buffer file-name))
	(buf-to-view nil))
    (unwind-protect
	(view-mode (prog1 (current-buffer)
		     (switch-to-buffer
		      (setq buf-to-view (find-file-noselect file-name)) t)))
      (and (not had-a-buf) buf-to-view (not (buffer-modified-p buf-to-view))
	   (kill-buffer buf-to-view)))))

(defun view-buffer (buffer-name)
  "View BUFFER in View mode, returning to previous buffer when done.
The usual Emacs commands are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type ? or h while viewing.

Calls the value of  view-hook  if that is non-nil."
  (interactive "bView buffer: ")
  (view-mode (prog1 (current-buffer) (switch-to-buffer buffer-name))))

(defun view-mode (&optional view-return-to-buffer)
  "Major mode for viewing text but not editing it.
Letters do not insert themselves.  Instead these commands are provided.
Most commands take prefix arguments.  Commands dealing with lines
default to \"scroll size\" lines (initially size of window).
Search commands default to a repeat count of one.
M-< or <	move to beginning of buffer.
M-> or >	move to end of buffer.
C-v or Space	scroll forward lines.
M-v or DEL	scroll backward lines.
CR or LF	scroll forward one line (backward with prefix argument).
z		like Space except set number of lines for further
		   scrolling commands to scroll by.
C-u and Digits	provide prefix arguments.  `-' denotes negative argument.
=		prints the current line number.
g		goes to line given by prefix argument.
/ or M-C-s	searches forward for regular expression
\\ or M-C-r	searches backward for regular expression.
n		searches forward for last regular expression.
p		searches backward for last regular expression.
C-@ or .	set the mark.
x		exchanges point and mark.
C-s or s	do forward incremental search.
C-r or r	do reverse incremental search.
@ or '		return to mark and pops mark ring.
		  Mark ring is pushed at start of every
		  successful search and when jump to line to occurs.
		  The mark is set on jump to buffer start or end.
? or h		provide help message (list of commands).
C-h		provides help (list of commands or description of a command).
C-n		moves down lines vertically.
C-p		moves upward lines vertically.
C-l		recenters the screen.
q or C-c	exit view-mode and return to previous buffer.

Entry to this mode calls the value of  view-hook  if non-nil.
\\{view-mode-map}"
;  Not interactive because dangerous things happen
;  if you call it without passing a buffer as argument
;  and they are not easy to fix.
;  (interactive)
  (let* ((view-buffer-window (selected-window))
	 (view-scroll-size nil))
    (unwind-protect
	(let ((buffer-read-only t)
	      (mode-line-buffer-identification
	       (list
		(if (buffer-file-name)
		    "Viewing %f"
		  "Viewing %b")))
	      (mode-name "View"))
	  (beginning-of-line)
	  (catch 'view-mode-exit (view-mode-command-loop)))
      (if view-return-to-buffer
	  (switch-to-buffer view-return-to-buffer)))))

(defun view-helpful-message ()
  (message
   (if (and (eq (key-binding "\C-h") 'Helper-help)
	    (eq (key-binding "?") 'Helper-describe-bindings)
	    (eq (key-binding "\C-c") 'exit-recursive-edit))
       "Type C-h for help, ? for commands, C-c to quit"
     (substitute-command-keys
      "Type \\[Helper-help] for help, \\[Helper-describe-bindings] for commands, \\[exit-recursive-edit] to quit."))))

(defun View-undefined ()
  (interactive)
  (ding)
  (view-helpful-message))

(defun view-window-size () (1- (window-height view-buffer-window)))

(defun view-scroll-size ()
  (min (view-window-size) (or view-scroll-size (view-window-size))))

(defvar view-hook nil
  "If non-nil, its value is called when viewing buffer or file.")

(defun view-mode-command-loop ()
  (push-mark)
  (let ((old-local-map (current-local-map))
	(mark-ring)
;	(view-last-command)
;	(view-last-command-entry)
;	(view-last-command-argument)
	(view-last-regexp)
	(Helper-return-blurb
	 (format "continue viewing %s"
		 (if (buffer-file-name)
		     (file-name-nondirectory (buffer-file-name))
		     (buffer-name))))
	(goal-column 0)
	(view-buffer (buffer-name)))
    (unwind-protect
	(progn
	  (use-local-map view-mode-map)
	  (run-hooks 'view-hook)
	  (view-helpful-message)
	  (recursive-edit))
      (save-excursion
	(set-buffer view-buffer)
	(use-local-map old-local-map))))
  (pop-mark))

;(defun view-last-command (&optional who what)
;  (setq view-last-command-entry this-command)
;  (setq view-last-command who)
;  (setq view-last-command-argument what))

;(defun View-repeat-last-command ()
;  "Repeat last command issued in View mode."
;  (interactive)
;  (if (and view-last-command
;	   (eq view-last-command-entry last-command))
;      (funcall view-last-command view-last-command-argument))
;  (setq this-command view-last-command-entry))

(defun View-goto-line (&optional line)
  "Move to LINE in View mode.
Display is centered at LINE.  Sets mark at starting position and pushes
mark ring."
  (interactive "p")
  (push-mark)
  (goto-line (or line 1))
  (recenter (/ (view-window-size) 2)))

(defun View-scroll-lines-forward (&optional lines)
  "Scroll forward in View mode, or exit if end of text is visible.
No arg means whole window full, or number of lines set by \\[View-scroll-lines-forward-set-scroll-size].
Arg is number of lines to scroll."
  (interactive "P")
  (if (pos-visible-in-window-p (point-max))
      (exit-recursive-edit))
  (setq lines
	(if lines (prefix-numeric-value lines)
	  (view-scroll-size)))
; (view-last-command 'View-scroll-lines-forward lines)
  (if (>= lines (view-window-size))
      (scroll-up nil)
    (if (>= (- lines) (view-window-size))
	(scroll-down nil)
      (scroll-up lines)))
  (cond ((pos-visible-in-window-p (point-max))
	 (goto-char (point-max))
	 (recenter -1)
	 (message (substitute-command-keys
		"End.  Type \\[exit-recursive-edit] to quit viewing."))))
  (move-to-window-line -1)
  (beginning-of-line))

(defun View-scroll-lines-forward-set-scroll-size (&optional lines)
  "Scroll forward LINES lines in View mode, setting the \"scroll size\".
This is the number of lines which \\[View-scroll-lines-forward] and \\[View-scroll-lines-backward] scroll by default.
The absolute value of LINES is used, so this command can be used to scroll
backwards (but \"scroll size\" is always positive).  If LINES is greater than
window height or omitted, then window height is assumed.  If LINES is less
than window height then scrolling context is provided from previous screen."
  (interactive "P")
  (if (not lines)
      (setq view-scroll-size (view-window-size))
    (setq lines (prefix-numeric-value lines))
    (setq view-scroll-size
	  (min (if (> lines 0) lines (- lines)) (view-window-size))))
  (View-scroll-lines-forward lines))

(defun View-scroll-one-more-line (&optional arg)
  "Scroll one more line up in View mode.
With ARG scroll one line down."
  (interactive "P")
  (View-scroll-lines-forward (if (not arg) 1 -1)))

(defun View-scroll-lines-backward (&optional lines)
  "Scroll backward in View mode.
No arg means whole window full, or number of lines set by \\[View-scroll-lines-forward-set-scroll-size].
Arg is number of lines to scroll."
  (interactive "P")
  (View-scroll-lines-forward (if lines
				 (- (prefix-numeric-value lines))
			       (- (view-scroll-size)))))
  
(defun View-search-regexp-forward (times regexp)
  "Search forward for NTH occurrence of REGEXP in View mode.
Displays line found at center of window.  REGEXP is remembered for
searching with \\[View-search-last-regexp-forward] and \\[View-search-last-regexp-backward].  Sets mark at starting position and pushes mark ring."
  (interactive "p\nsSearch forward (regexp): ")
  (if (> (length regexp) 0)
      (progn
       ;(view-last-command 'View-search-last-regexp-forward times)
	(view-search times regexp))))

(defun View-search-regexp-backward (times regexp)
  "Search backward from window start for NTH instance of REGEXP in View mode.
Displays line found at center of window.  REGEXP is remembered for
searching with \\[View-search-last-regexp-forward] and \\[View-search-last-regexp-backward].  Sets mark at starting position and pushes mark ring."
  (interactive "p\nsSearch backward (regexp): ")
  (View-search-regexp-forward (- times) regexp))

(defun View-search-last-regexp-forward (times)
  "Search forward from window end for NTH instance of last regexp in View mode.
Displays line found at center of window.  Sets mark at starting position
and pushes mark ring."
  (interactive "p")
  (View-search-regexp-forward times view-last-regexp))

(defun View-search-last-regexp-backward (times)
  "Search backward from window start for NTH instance of last regexp in View mode.
Displays line found at center of window.  Sets mark at starting position and
pushes mark ring."
  (interactive "p")
  (View-search-regexp-backward times view-last-regexp))

(defun View-back-to-mark (&optional ignore)
  "Return to last mark set in View mode, else beginning of file.
Displays line at center of window.  Pops mark ring so successive
invocations return to earlier marks."
  (interactive)
  (goto-char (or (mark) (point-min)))
  (pop-mark)
  (recenter (/ (view-window-size) 2)))
	     
(defun view-search (times regexp)
  (setq view-last-regexp regexp)
  (let (where)
    (save-excursion
      (move-to-window-line (if (< times 0) 0 -1))
      (if (re-search-forward regexp nil t times)
	  (setq where (point))))
    (if where
	(progn
	  (push-mark)
	  (goto-char where)
	  (beginning-of-line)
	  (recenter (/ (view-window-size) 2)))
      (message "Can't find occurrence %d of %s" times regexp)
      (sit-for 4))))

