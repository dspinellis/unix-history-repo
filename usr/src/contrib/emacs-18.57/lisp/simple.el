;; Basic editing commands for Emacs
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

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


(defun open-line (arg)
  "Insert a newline and leave point before it.
With arg, inserts that many newlines."
  (interactive "*p")
  (let ((flag (and (bolp) (not (bobp)))))
    (if flag (forward-char -1))
    (while (> arg 0)
      (insert ?\n)
      (goto-char (1- (point)))
      (setq arg (1- arg)))
    (if flag (forward-char 1))))

(defun split-line ()
  "Split current line, moving portion beyond point vertically down."
  (interactive "*")
  (skip-chars-forward " \t")
  (let ((col (current-column))
	(pos (point)))
    (insert ?\n)
    (indent-to col 0)
    (goto-char pos)))

(defun quoted-insert (arg)
  "Read next input character and insert it.
Useful for inserting control characters.
You may also type up to 3 octal digits, to insert a character with that code"
  (interactive "*p")
  (let ((char (read-quoted-char)))
    (while (> arg 0)
      (insert char)
      (setq arg (1- arg)))))

(defun delete-indentation (&optional arg)
  "Join this line to previous and fix up whitespace at join.
With argument, join this line to following line."
  (interactive "*P")
  (beginning-of-line)
  (if arg (forward-line 1))
  (if (eq (preceding-char) ?\n)
      (progn
	(delete-region (point) (1- (point)))
	(fixup-whitespace))))

(defun fixup-whitespace ()
  "Fixup white space between objects around point.
Leave one space or none, according to the context."
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (if (or (looking-at "^\\|\\s)")
	    (save-excursion (forward-char -1)
			    (looking-at "$\\|\\s(\\|\\s'")))
	nil
      (insert ?\ ))))

(defun delete-horizontal-space ()
  "Delete all spaces and tabs around point."
  (interactive "*")
  (skip-chars-backward " \t")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun just-one-space ()
  "Delete all spaces and tabs around point, leaving one space."
  (interactive "*")
  (skip-chars-backward " \t")
  (if (= (following-char) ? )
      (forward-char 1)
    (insert ? ))
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun delete-blank-lines ()
  "On blank line, delete all surrounding blank lines, leaving just one.
On isolated blank line, delete that one.
On nonblank line, delete all blank lines that follow it."
  (interactive "*")
  (let (thisblank singleblank)
    (save-excursion
      (beginning-of-line)
      (setq thisblank (looking-at "[ \t]*$"))
      (setq singleblank
	    (and thisblank
		 (not (looking-at "[ \t]*\n[ \t]*$"))
		 (or (bobp)
		     (progn (forward-line -1)
			    (not (looking-at "[ \t]*$")))))))
    (if thisblank
	(progn
	  (beginning-of-line)
	  (if singleblank (forward-line 1))
	  (delete-region (point)
			 (if (re-search-backward "[^ \t\n]" nil t)
			     (progn (forward-line 1) (point))
			   (point-min)))))
    (if (not (and thisblank singleblank))
	(save-excursion
	  (end-of-line)
	  (forward-line 1)
	  (delete-region (point)
			 (if (re-search-forward "[^ \t\n]" nil t)
			     (progn (beginning-of-line) (point))
			   (point-max)))))))

(defun back-to-indentation ()
  "Move point to the first non-whitespace character on this line."
  (interactive)
  (beginning-of-line 1)
  (skip-chars-forward " \t"))

(defun newline-and-indent ()
  "Insert a newline, then indent according to major mode.
Indentation is done using the current indent-line-function.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this indents to the
specified left-margin column."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
  (insert ?\n)
  (indent-according-to-mode))

(defun reindent-then-newline-and-indent ()
  "Reindent current line, insert newline, then indent the new line.
Indentation of both lines is done according to the current major mode,
which means that the current value of indent-line-function is called.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this indents to the
specified left-margin column."
  (interactive "*")
  (save-excursion
    (delete-region (point) (progn (skip-chars-backward " \t") (point)))
    (indent-according-to-mode))
  (insert ?\n)
  (indent-according-to-mode))

(defun kill-forward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (+ (point) arg)))

(defun kill-backward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (- (point) arg)))

(defun backward-delete-char-untabify (arg &optional killp)
  "Delete characters backward, changing tabs into spaces.
Delete ARG chars, and kill (save in kill ring) if KILLP is non-nil.
Interactively, ARG is the prefix arg (default 1)
and KILLP is t if prefix arg is was specified."
  (interactive "*p\nP")
  (let ((count arg))
    (save-excursion
      (while (and (> count 0) (not (bobp)))
	(if (= (preceding-char) ?\t)
	    (let ((col (current-column)))
	      (forward-char -1)
	      (setq col (- col (current-column)))
	      (insert-char ?\ col)
	      (delete-char 1)))
	(forward-char -1)
	(setq count (1- count)))))
  (delete-backward-char arg killp))

(defun zap-to-char (arg char)
  "Kill up to (but not including) ARG'th occurrence of CHAR.
Goes backward if ARG is negative; goes to end of buffer if CHAR not found."
  (interactive "*p\ncZap to char: ")
  (kill-region (point) (if (search-forward (char-to-string char) nil t arg)
			 (progn (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
				(point))
		       (if (> arg 0) (point-max) (point-min)))))

(defun beginning-of-buffer (&optional arg)
  "Move point to the beginning of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the true beginning.
Don't use this in Lisp programs!
\(goto-char (point-min)) is faster and does not set the mark."
  (interactive "P")
  (push-mark)
  (goto-char (if arg
		 (if (> (buffer-size) 10000)
		     ;; Avoid overflow for large buffer sizes!
		     (* (prefix-numeric-value arg)
			(/ (buffer-size) 10))
		   (/ (+ 10 (* (buffer-size) (prefix-numeric-value arg))) 10))
	       (point-min)))
  (if arg (forward-line 1)))

(defun end-of-buffer (&optional arg)
  "Move point to the end of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the true end.
Don't use this in Lisp programs!
\(goto-char (point-max)) is faster and does not set the mark."
  (interactive "P")
  (push-mark)
  (goto-char (if arg
		 (- (1+ (buffer-size))
		    (if (> (buffer-size) 10000)
			;; Avoid overflow for large buffer sizes!
			(* (prefix-numeric-value arg)
			   (/ (buffer-size) 10))
		      (/ (* (buffer-size) (prefix-numeric-value arg)) 10)))
	       (point-max)))
  (if arg (forward-line 1)))

(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer."
  (interactive)
  (push-mark (point))
  (push-mark (point-max))
  (goto-char (point-min)))

(defun count-lines-region (start end)
  "Print number of lines in the region."
  (interactive "r")
  (message "Region has %d lines" (count-lines start end)))

(defun what-line ()
  "Print the current line number (in the buffer) of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (message "Line %d"
	       (1+ (count-lines 1 (point)))))))

(defun count-lines (start end)
  "Return number of newlines between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (- (buffer-size) (forward-line (buffer-size))))))

(defun what-cursor-position ()
  "Print info on cursor position (on screen and within buffer)."
  (interactive)
  (let* ((char (following-char))
	 (beg (point-min))
	 (end (point-max))
         (pos (point))
	 (total (buffer-size))
	 (percent (if (> total 50000)
		      ;; Avoid overflow from multiplying by 100!
		      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
		    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
	 (hscroll (if (= (window-hscroll) 0)
		      ""
		    (format " Hscroll=%d" (window-hscroll))))
	 (col (current-column)))
    (if (= pos end)
	(if (or (/= beg 1) (/= end (1+ total)))
	    (message "point=%d of %d(%d%%) <%d - %d>  column %d %s"
		     pos total percent beg end col hscroll)
	  (message "point=%d of %d(%d%%)  column %d %s"
		   pos total percent col hscroll))
      (if (or (/= beg 1) (/= end (1+ total)))
	  (message "Char: %s (0%o)  point=%d of %d(%d%%) <%d - %d>  column %d %s"
		   (single-key-description char) char pos total percent beg end col hscroll)
	(message "Char: %s (0%o)  point=%d of %d(%d%%)  column %d %s"
		 (single-key-description char) char pos total percent col hscroll)))))

(defun fundamental-mode ()
  "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one."
  (interactive)
  (kill-all-local-variables))

(put 'eval-expression 'disabled t)

;; We define this, rather than making  eval  interactive,
;; for the sake of completion of names like eval-region, eval-current-buffer.
(defun eval-expression (expression)
  "Evaluate EXPRESSION and print value in minibuffer.
Value is also consed on to front of variable  values  's value."
  (interactive "xEval: ")
  (setq values (cons (eval expression) values))
  (prin1 (car values) t))

(defun edit-and-eval-command (prompt command)
  "Prompting with PROMPT, let user edit COMMAND and eval result.
COMMAND is a Lisp expression.  Let user edit that expression in
the minibuffer, then read and evaluate the result."
  (eval (read-minibuffer prompt
			 (prin1-to-string command))))

(defvar repeat-complex-command-map (copy-alist minibuffer-local-map))
(define-key repeat-complex-command-map "\ep" 'previous-complex-command)
(define-key repeat-complex-command-map "\en" 'next-complex-command)
(defun repeat-complex-command (repeat-complex-command-arg)
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Lisp form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous command
it is added to the front of the command history.
Whilst editing the command, the following commands are available:
\\{repeat-complex-command-map}"
  (interactive "p")
  (let ((elt (nth (1- repeat-complex-command-arg) command-history))
	newcmd)
    (if elt
	(progn
	  (setq newcmd (read-from-minibuffer "Redo: "
					     (prin1-to-string elt)
					     repeat-complex-command-map
					     t))
	  ;; If command to be redone does not match front of history,
	  ;; add it to the history.
	  (or (equal newcmd (car command-history))
	      (setq command-history (cons newcmd command-history)))
	  (eval newcmd))
      (ding))))

(defun next-complex-command (n)
  "Inserts the next element of `command-history' into the minibuffer."
  (interactive "p")
  (let ((narg (min (max 1 (- repeat-complex-command-arg n))
		   (length command-history))))
    (if (= repeat-complex-command-arg narg)
	(error (if (= repeat-complex-command-arg 1)
		   "No following item in command history"
		 "No preceeding item command history"))
      (erase-buffer)
      (setq repeat-complex-command-arg narg)
      (insert (prin1-to-string (nth (1- repeat-complex-command-arg)
				    command-history)))
      (goto-char (point-min)))))

(defun previous-complex-command (n)
  "Inserts the previous element of `command-history' into the minibuffer."
  (interactive "p")
  (next-complex-command (- n)))

(defun goto-line (arg)
  "Goto line ARG, counting from line 1 at beginning of buffer."
  (interactive "NGoto line: ")
  (save-restriction
    (widen)
    (goto-char 1)
    (forward-line (1- arg))))

;Put this on C-x u, so we can force that rather than C-_ into startup msg
(fset 'advertised-undo 'undo)

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "*p")
  (let ((modified (buffer-modified-p)))
    (message "Undo!")
    (or (eq last-command 'undo)
	(progn (undo-start)
	       (undo-more 1)))
    (setq this-command 'undo)
    (undo-more (or arg 1))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary))))

(defun shell-command (command &optional flag)
  "Execute string COMMAND in inferior shell; display output, if any.
Optional second arg non-nil (prefix arg, if interactive)
means insert output in current buffer after point (leave mark after it)."
  (interactive "sShell command: \nP")
  (if flag
      (progn (barf-if-buffer-read-only)
	     (push-mark)
	     (call-process shell-file-name nil t nil
			   "-c" command)
	     (exchange-point-and-mark))
    (shell-command-on-region (point) (point) command nil)))

(defun shell-command-on-region (start end command &optional flag interactive)
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer;
Prefix arg means replace the region with it.
Noninteractive args are START, END, COMMAND, FLAG.
Noninteractively FLAG means insert output in place of text from START to END,
and put point at the end, but don't alter the mark."
  (interactive "r\nsShell command on region: \nP\np")
  (if flag
      ;; Replace specified region with output from command.
      (let ((swap (and interactive (< (point) (mark)))))
	;; Don't muck with mark
	;; unless called interactively.
	(and interactive (push-mark))
	(call-process-region start end shell-file-name t t nil
			     "-c" command)
	(and interactive swap (exchange-point-and-mark)))
    (let ((buffer (get-buffer-create "*Shell Command Output*")))
      (save-excursion
	(set-buffer buffer)
	(erase-buffer))
      (if (eq buffer (current-buffer))
	  (setq start 1 end 1))
      (call-process-region start end shell-file-name
			   nil buffer nil
			   "-c" command)
      (if (save-excursion
	    (set-buffer buffer)
	    (> (buffer-size) 0))
	  (set-window-start (display-buffer buffer) 1)
	(message "(Shell command completed with no output)")))))

(defun universal-argument ()
  "Begin a numeric argument for the following command.
Digits or minus sign following this command make up the numeric argument.
If no digits or minus sign follow, this command by itself provides 4 as argument.
Used more than once, this command multiplies the argument by 4 each time."
  (interactive nil)
  (let ((c-u 4) (argstartchar last-command-char)
	char)
;   (describe-arg (list c-u) 1)
    (setq char (read-char))
    (while (= char argstartchar)
      (setq c-u (* 4 c-u))
;     (describe-arg (list c-u) 1)
      (setq char (read-char)))
    (prefix-arg-internal char c-u nil)))

(defun prefix-arg-internal (char c-u value)
  (let ((sign 1))
    (if (and (numberp value) (< value 0))
	(setq sign -1 value (- value)))
    (if (eq value '-)
	(setq sign -1 value nil))
;   (describe-arg value sign)
    (while (= ?- char)
      (setq sign (- sign) c-u nil)
;     (describe-arg value sign)
      (setq char (read-char)))
    (while (and (>= char ?0) (<= char ?9))
      (setq value (+ (* (if (numberp value) value 0) 10) (- char ?0)) c-u nil)
;     (describe-arg value sign)
      (setq char (read-char)))
    (setq prefix-arg
	  (cond (c-u (list c-u))
		((numberp value) (* value sign))
		((= sign -1) '-)))
    (setq unread-command-char char)))

;(defun describe-arg (value sign)
; (cond ((numberp value)
;	 (message "Arg: %d" (* value sign)))
;	((consp value)
;	 (message "Arg: C-u factor %d" (car value)))
;	((< sign 0)
;	 (message "Arg: -"))))

(defun digit-argument (arg)
  "Part of the numeric argument for the next command."
  (interactive "P")
  (prefix-arg-internal last-command-char nil arg))

(defun negative-argument (arg)
  "Begin a negative numeric argument for the next command."
  (interactive "P")
  (prefix-arg-internal ?- nil arg))

(defun forward-to-indentation (arg)
  "Move forward ARG lines and position at first nonblank character."
  (interactive "p")
  (forward-line arg)
  (skip-chars-forward " \t"))

(defun backward-to-indentation (arg)
  "Move backward ARG lines and position at first nonblank character."
  (interactive "p")
  (forward-line (- arg))
  (skip-chars-forward " \t"))

(defun kill-line (&optional arg)
  "Kill the rest of the current line; if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg."
  (interactive "*P")
  (kill-region (point)
	       (progn
		 (if arg
		     (forward-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (if (looking-at "[ \t]*$")
		       (forward-line 1)
		     (end-of-line)))
		 (point))))

;;;; The kill ring

(defvar kill-ring nil
  "List of killed text sequences.")

(defconst kill-ring-max 30
  "*Maximum length of kill ring before oldest elements are thrown away.")

(defvar kill-ring-yank-pointer nil
  "The tail of the kill ring whose car is the last thing yanked.")

(defun kill-append (string before-p)
  (setcar kill-ring
	  (if before-p
	      (concat string (car kill-ring))
	      (concat (car kill-ring) string))))

(defun kill-region (beg end)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command \\[yank] can retrieve it from there.
\(If you want to kill and then yank immediately, use \\[copy-region-as-kill].)

This is the primitive for programs to kill text (as opposed to deleting it).
Supply two arguments, character numbers indicating the stretch of text
 to be killed.
Any command that calls this function is a \"kill command\".
If the previous command was also a kill command,
the text killed this time appends to the text killed last time
to make one entry in the kill ring."
  (interactive "*r")
  (if (and (not (eq buffer-undo-list t))
	   (not (eq last-command 'kill-region)))
      ;; Don't let the undo list be truncated before we can even access it.
      (let ((undo-high-threshold (+ (- (max beg end) (min beg end)) 100)))
	(delete-region beg end)
	;; Take the same string recorded for undo
	;; and put it in the kill-ring.
	(setq kill-ring (cons (car (car buffer-undo-list)) kill-ring))
	(if (> (length kill-ring) kill-ring-max)
	    (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
	(setq this-command 'kill-region)
	(setq kill-ring-yank-pointer kill-ring))
    (copy-region-as-kill beg end)
    (delete-region beg end)))

(fset 'kill-ring-save 'copy-region-as-kill)

(defun copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (buffer-substring beg end) (< end beg))
    (setq kill-ring (cons (buffer-substring beg end) kill-ring))
    (if (> (length kill-ring) kill-ring-max)
	(setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
  (setq this-command 'kill-region)
  (setq kill-ring-yank-pointer kill-ring))

(defun append-next-kill ()
  "Cause following command, if kill, to append to previous kill."
  (interactive)
  (if (interactive-p)
      (setq this-command 'kill-region)
      (setq last-command 'kill-region)))

(defun rotate-yank-pointer (arg)
  "Rotate the yanking point in the kill ring."
  (interactive "p")
  (let ((length (length kill-ring)))
    (if (zerop length)
	(error "Kill ring is empty")
      (setq kill-ring-yank-pointer
	    (nthcdr (% (+ arg (- length (length kill-ring-yank-pointer)))
		       length)
		    kill-ring)))))

(defun yank-pop (arg)
  "Replace just-yanked stretch of killed-text with a different stretch.
This command is allowed only immediately after a  yank  or a  yank-pop.
At such a time, the region contains a stretch of reinserted
previously-killed text.  yank-pop  deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument n, the n'th previous kill is inserted.
If n is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  (interactive "*p")
  (if (not (eq last-command 'yank))
      (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (let ((before (< (point) (mark))))
    (delete-region (point) (mark))
    (rotate-yank-pointer arg)
    (set-mark (point))
    (insert (car kill-ring-yank-pointer))
    (if before (exchange-point-and-mark))))

(defun yank (&optional arg)
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.
With just C-U as argument, same but put point in front (and mark at end).
With argument n, reinsert the nth most recently killed stretch of killed
text.
See also the command \\[yank-pop]."
  (interactive "*P")
  (rotate-yank-pointer (if (listp arg) 0
			 (if (eq arg '-) -1
			   (1- arg))))
  (push-mark (point))
  (insert (car kill-ring-yank-pointer))
  (if (consp arg)
      (exchange-point-and-mark)))

(defun insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name."
  (interactive "*bInsert buffer: ")
  (or (bufferp buffer)
      (setq buffer (get-buffer buffer)))
  (let (start end newmark)
    (save-excursion
      (save-excursion
	(set-buffer buffer)
	(setq start (point-min) end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point)))
    (push-mark newmark)))

(defun append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.

When calling from a program, give three arguments:
a buffer or the name of one, and two character numbers
specifying the portion of the current buffer to be copied."
  (interactive "BAppend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (insert-buffer-substring oldbuf start end))))

(defun prepend-to-buffer (buffer start end)
  "Prepend to specified buffer the text of the region.
It is inserted into that buffer after its point.

When calling from a program, give three arguments:
a buffer or the name of one, and two character numbers
specifying the portion of the current buffer to be copied."
  (interactive "BPrepend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

(defun copy-to-buffer (buffer start end)
  "Copy to specified buffer the text of the region.
It is inserted into that buffer, replacing existing text there.

When calling from a program, give three arguments:
a buffer or the name of one, and two character numbers
specifying the portion of the current buffer to be copied."
  (interactive "BCopy to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (erase-buffer)
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

(defun mark ()
  "Return this buffer's mark value as integer, or nil if no mark.
If you are using this in an editing command, you are most likely making
a mistake; see the documentation of `set-mark'."
  (marker-position (mark-marker)))

(defun set-mark (pos)
  "Set this buffer's mark to POS.  Don't use this function!
That is to say, don't use this function unless you want
the user to see that the mark has moved, and you want the previous
mark position to be lost.

Normally, when a new mark is set, the old one should go on the stack.
This is why most applications should use push-mark, not set-mark.

Novice emacs-lisp programmers often try to use the mark for the wrong
purposes.  The mark saves a location for the user's convenience.
Most editing commands should not alter the mark.
To remember a location for internal use in the Lisp program,
store it in a Lisp variable.  Example:

   (let ((beg (point))) (forward-line 1) (delete-region beg (point)))."

  (set-marker (mark-marker) pos (current-buffer)))

(defvar mark-ring nil
  "The list of saved former marks of the current buffer,
most recent first.")
(make-variable-buffer-local 'mark-ring)

(defconst mark-ring-max 16
  "*Maximum size of mark ring.  Start discarding off end if gets this big.")

(defun set-mark-command (arg)
  "Set mark at where point is, or jump to mark.
With no prefix argument, set mark, and push previous mark on mark ring.
With argument, jump to mark, and pop into mark off the mark ring.

Novice emacs-lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (interactive "P")
  (if (null arg)
      (push-mark)
    (if (null (mark))
	(error "No mark set in this buffer")
      (goto-char (mark))
      (pop-mark))))

(defun push-mark (&optional location nomsg)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
Displays \"Mark set\" unless the optional second arg NOMSG is non-nil.

Novice emacs-lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (if (null (mark))
      nil
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (if (> (length mark-ring) mark-ring-max)
	(progn
	  (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
	  (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil))))
  (set-mark (or location (point)))
  (or nomsg executing-macro (> (minibuffer-depth) 0)
      (message "Mark set")))

(defun pop-mark ()
  "Pop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (if mark-ring
      (progn
	(setq mark-ring (nconc mark-ring (list (copy-marker (mark-marker)))))
	(set-mark (+ 0 (car mark-ring)))
	(move-marker (car mark-ring) nil)
	(if (null (mark)) (ding))
	(setq mark-ring (cdr mark-ring)))))

(fset 'exchange-dot-and-mark 'exchange-point-and-mark)
(defun exchange-point-and-mark ()
  "Put the mark where point is now, and point where the mark is now."
  (interactive nil)
  (let ((omark (mark)))
    (if (null omark)
	(error "No mark set in this buffer"))
    (set-mark (point))
    (goto-char omark)
    nil))

(defun next-line (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one,
a newline character is inserted to create a line
and the cursor moves to that line.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (if (= arg 1)
      (let ((opoint (point)))
	(forward-line 1)
	(if (or (= opoint (point))
		(not (eq (preceding-char) ?\n)))
	    (insert ?\n)
	  (goto-char opoint)
	  (next-line-internal arg)))
    (next-line-internal arg))
  nil)

(defun previous-line (arg)
  "Move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with negative argument instead..  It is usually easier
to use and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (next-line-internal (- arg))
  nil)

(defconst track-eol nil
  "*Non-nil means vertical motion starting at the end of a line should keep to ends of lines.
This means moving to the end of each line moved onto.")

(defvar goal-column nil
  "*Semipermanent goal column for vertical motion, as set by \\[set-goal-column], or nil.")

(defvar temporary-goal-column 0
  "Current goal column for vertical motion.
It is the column where point was at the start of current run of vertical motion commands.")

(defun next-line-internal (arg)
  (if (not (or (eq last-command 'next-line)
	       (eq last-command 'previous-line)))
      (setq temporary-goal-column
	    (if (and track-eol (eolp))
		t
	      (current-column))))
  (if (not (integerp selective-display))
      (forward-line arg)
    ;; Move by arg lines, but ignore invisible ones.
    (while (> arg 0)
      (vertical-motion 1)
      (forward-char -1)
      (forward-line 1)
      (setq arg (1- arg)))
    (while (< arg 0)
      (vertical-motion -1)
      (beginning-of-line)
      (setq arg (1+ arg))))
  (if (eq (or goal-column temporary-goal-column) t)
      (end-of-line)
    (move-to-column (or goal-column temporary-goal-column)))
  nil)


(defun set-goal-column (arg)
  "Set the current horizontal position as a goal for \\[next-line] and \\[previous-line].
Those commands will move to this position in the line moved to
rather than trying to keep the same horizontal position.
With a non-nil argument, clears out the goal column
so that \\[next-line] and \\[previous-line] resume vertical motion."
  (interactive "P")
  (if arg
      (progn
        (setq goal-column nil)
        (message "No goal column"))
    (setq goal-column (current-column))
    (message (substitute-command-keys
	      "Goal column %d (use \\[set-goal-column] with an arg to unset it)")
	     goal-column))
  nil)

(defun transpose-chars (arg)
  "Interchange characters around point, moving forward one character.
With prefix arg ARG, effect is to take character before point
and drag it forward past ARG other characters (backward if ARG negative).
If no argument and at end of line, the previous two chars are exchanged."
  (interactive "*P")
  (and (null arg) (eolp) (forward-char -1))
  (transpose-subr 'forward-char (prefix-numeric-value arg)))

(defun transpose-words (arg)
  "Interchange words around point, leaving point at end of them.
With prefix arg ARG, effect is to take word before or around point
and drag it forward past ARG other words (backward if ARG negative).
If ARG is zero, the words around or after point and around or after mark
are interchanged."
  (interactive "*p")
  (transpose-subr 'forward-word arg))

(defun transpose-sexps (arg)
  "Like \\[transpose-words] but applies to sexps.
Does not work on a sexp that point is in the middle of
if it is a list or string."
  (interactive "*p")
  (transpose-subr 'forward-sexp arg))

(defun transpose-lines (arg)
  "Exchange current line and previous line, leaving point after both.
With argument ARG, takes previous line and moves it past ARG lines.
With argument 0, interchanges line point is in with line mark is in."
  (interactive "*p")
  (transpose-subr (function
		   (lambda (arg)
		     (if (= arg 1)
			 (progn
			   ;; Move forward over a line,
			   ;; but create a newline if none exists yet.
			   (end-of-line)
			   (if (eobp)
			       (newline)
			     (forward-char 1)))
		       (forward-line arg))))
		  arg))

(defun transpose-subr (mover arg)
  (let (start1 end1 start2 end2)
    (if (= arg 0)
	(progn
	  (save-excursion
	    (funcall mover 1)
	    (setq end2 (point))
	    (funcall mover -1)
	    (setq start2 (point))
	    (goto-char (mark))
	    (funcall mover 1)
	    (setq end1 (point))
	    (funcall mover -1)
	    (setq start1 (point))
	    (transpose-subr-1))
	  (exchange-point-and-mark)))
    (while (> arg 0)
      (funcall mover -1)
      (setq start1 (point))
      (funcall mover 1)
      (setq end1 (point))
      (funcall mover 1)
      (setq end2 (point))
      (funcall mover -1)
      (setq start2 (point))
      (transpose-subr-1)
      (goto-char end2)
      (setq arg (1- arg)))
    (while (< arg 0)
      (funcall mover -1)
      (setq start2 (point))
      (funcall mover -1)
      (setq start1 (point))
      (funcall mover 1)
      (setq end1 (point))
      (funcall mover 1)
      (setq end2 (point))
      (transpose-subr-1)
      (setq arg (1+ arg)))))

(defun transpose-subr-1 ()
  (if (> (min end1 end2) (max start1 start2))
      (error "Don't have two things to transpose"))
  (let ((word1 (buffer-substring start1 end1))
	(word2 (buffer-substring start2 end2)))
    (delete-region start2 end2)
    (goto-char start2)
    (insert word1)
    (goto-char (if (< start1 start2) start1
		 (+ start1 (- (length word1) (length word2)))))
    (delete-char (length word1))
    (insert word2)))

(defconst comment-column 32
  "*Column to indent right-margin comments to.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'comment-column)

(defconst comment-start nil
  "*String to insert to start a new comment, or nil if no comment syntax defined.")

(defconst comment-start-skip nil
  "*Regexp to match the start of a comment plus everything up to its body.
If there are any \\(...\\) pairs, the comment delimiter text is held to begin
at the place matched by the close of the first pair.")

(defconst comment-end ""
  "*String to insert to end a new comment.
Should be an empty string if comments are terminated by end-of-line.")

(defconst comment-indent-hook
  '(lambda () comment-column)
  "Function to compute desired indentation for a comment
given the character number it starts at.")

(defun indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment."
  (interactive "*")
  (beginning-of-line 1)
  (if (null comment-start)
      (error "No comment syntax defined")
    (let* ((eolpos (save-excursion (end-of-line) (point)))
	   cpos indent begpos)
      (if (re-search-forward comment-start-skip eolpos 'move)
	  (progn (setq cpos (point-marker))
		 ;; Find the start of the comment delimiter.
		 ;; If there were paren-pairs in comment-start-skip,
		 ;; position at the end of the first pair.
		 (if (match-end 1)
		     (goto-char (match-end 1))
		   ;; If comment-start-skip matched a string with internal
		   ;; whitespace (not final whitespace) then the delimiter
		   ;; start at the end of that whitespace.
		   ;; Otherwise, it starts at the beginning of what was matched.
		   (skip-chars-backward " \t" (match-beginning 0))
		   (skip-chars-backward "^ \t" (match-beginning 0)))))
      (setq begpos (point))
      ;; Compute desired indent.
      (if (= (current-column)
	     (setq indent (funcall comment-indent-hook)))
	  (goto-char begpos)
	;; If that's different from current, change it.
	(skip-chars-backward " \t")
	(delete-region (point) begpos)
	(indent-to indent))
      ;; An existing comment?
      (if cpos 
	  (progn (goto-char cpos)
		 (set-marker cpos nil))
	;; No, insert one.
	(insert comment-start)
	(save-excursion
	  (insert comment-end))))))

(defun set-comment-column (arg)
  "Set the comment column based on point.
With no arg, set the comment column to the current column.
With just minus as arg, kill any comment on this line.
With any other arg, set comment column to indentation of the previous comment
 and then align or create a comment on this line at that column."
  (interactive "P")
  (if (eq arg '-)
      (kill-comment nil)
    (if arg
	(progn
	  (save-excursion
	    (beginning-of-line)
	    (re-search-backward comment-start-skip)
	    (beginning-of-line)
	    (re-search-forward comment-start-skip)
	    (goto-char (match-beginning 0))
	    (setq comment-column (current-column))
	    (message "Comment column set to %d" comment-column))
	  (indent-for-comment))
      (setq comment-column (current-column))
      (message "Comment column set to %d" comment-column))))

(defun kill-comment (arg)
  "Kill the comment on this line, if any.
With argument, kill comments on that many lines starting with this one."
  (interactive "P")
  (barf-if-buffer-read-only)
  (let ((count (prefix-numeric-value arg)))
    (beginning-of-line)
    (while (> count 0)
      (let ((eolpos (save-excursion (end-of-line) (point))))
	(if (re-search-forward comment-start-skip eolpos t)
	    (progn
	      (goto-char (match-beginning 0))
	      (skip-chars-backward " \t")
	      (kill-region (point) eolpos))))
      (if arg
	  (forward-line 1))
      (setq count (1- count)))))

(defun backward-word (arg)
  "Move backward until encountering the end of a word.
With argument, do this that many times.
In programs, it is faster to call forward-word with negative arg."
  (interactive "p")
  (forward-word (- arg)))

(defun mark-word (arg)
  "Set mark arg words away from point."
  (interactive "p")
  (push-mark
    (save-excursion
      (forward-word arg)
      (point))))

(defun kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p")
  (kill-region (point) (progn (forward-word arg) (point))))

(defun backward-kill-word (arg)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p")
  (kill-word (- arg)))

(defconst fill-prefix nil
  "*String for filling to insert at front of new line, or nil for none.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'fill-prefix)

(defun do-auto-fill ()
  (let ((fill-point
	 (let ((opoint (point)))
	   (save-excursion
	     (move-to-column (1+ fill-column))
	     (skip-chars-backward "^ \t\n")
	     (if (bolp)
		 (re-search-forward "[ \t]" opoint t))
	     (skip-chars-backward " \t")
	     (point)))))
    ;; If there is a space on the line before fill-point,
    ;; and nonspaces precede it, break the line there.
    (if (save-excursion
	  (goto-char fill-point)
	  (not (bolp)))
	;; If point is at the fill-point, do not `save-excursion'.
	;; Otherwise, if a comment prefix or fill-prefix is inserted,
	;; point will end up before it rather than after it.
	(if (save-excursion
	      (skip-chars-backward " \t")
	      (= (point) fill-point))
	    (indent-new-comment-line)
	    (save-excursion
	      (goto-char fill-point)
	      (indent-new-comment-line))))))

(defconst comment-multi-line nil
  "*Non-nil means \\[indent-new-comment-line] should continue same comment
on new line, with no new terminator or starter.")

(defun indent-new-comment-line ()
  "Break line at point and indent, continuing comment if presently within one.
The body of the continued comment is indented under the previous comment line."
  (interactive "*")
  (let (comcol comstart)
    (skip-chars-backward " \t")
    (delete-region (point)
		   (progn (skip-chars-forward " \t")
			  (point)))
    (insert ?\n)
    (save-excursion
      (if (and comment-start-skip
	       (let ((opoint (point)))
		 (forward-line -1)
		 (re-search-forward comment-start-skip opoint t)))
	  ;; The old line is a comment.
	  ;; Set WIN to the pos of the comment-start.
	  ;; But if the comment is empty, look at preceding lines
	  ;; to find one that has a nonempty comment.
	  (let ((win (match-beginning 0)))
	    (while (and (eolp) (not (bobp))
			(let (opoint)
			  (beginning-of-line)
			  (setq opoint (point))
			  (forward-line -1)
			  (re-search-forward comment-start-skip opoint t)))
	      (setq win (match-beginning 0)))
	    ;; Indent this line like what we found.
	    (goto-char win)
	    (setq comcol (current-column))
	    (setq comstart (buffer-substring (point) (match-end 0))))))
    (if comcol
	(let ((comment-column comcol)
	      (comment-start comstart)
	      (comment-end comment-end))
	  (and comment-end (not (equal comment-end ""))
	       (if (not comment-multi-line)
		   (progn
		     (forward-char -1)
		     (insert comment-end)
		     (forward-char 1))
		 (setq comment-column (+ comment-column (length comment-start))
		       comment-start "")))
	  (if (not (eolp))
	      (setq comment-end ""))
	  (insert ?\n)
	  (forward-char -1)
	  (indent-for-comment)
	  (delete-char 1))
      (if fill-prefix
	  (insert fill-prefix)
	(indent-according-to-mode)))))

(defun auto-fill-mode (arg)
  "Toggle auto-fill mode.
With arg, turn auto-fill mode on iff arg is positive.
In auto-fill mode, inserting a space at a column beyond  fill-column
automatically breaks the line at a previous space."
  (interactive "P")
  (prog1 (setq auto-fill-hook
	       (if (if (null arg)
		       (not auto-fill-hook)
		       (> (prefix-numeric-value arg) 0))
		   'do-auto-fill
		   nil))
    ;; update mode-line
    (set-buffer-modified-p (buffer-modified-p))))

(defun turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode."
  (auto-fill-mode 1))

(defun set-fill-column (arg)
  "Set fill-column to current column, or to argument if given.
fill-column's value is separate for each buffer."
  (interactive "P")
  (setq fill-column (if (integerp arg) arg (current-column)))
  (message "fill-column set to %d" fill-column))

(defun set-selective-display (arg)
  "Set selective-display to ARG; clear it if no arg.
When selective-display is a number > 0,
lines whose indentation is >= selective-display are not displayed.
selective-display's value is separate for each buffer."
  (interactive "P")
  (if (eq selective-display t)
      (error "selective-display already in use for marked lines"))
  (setq selective-display
	(and arg (prefix-numeric-value arg)))
  (set-window-start (selected-window) (window-start (selected-window)))
  (princ "selective-display set to " t)
  (prin1 selective-display t)
  (princ "." t))

(defun overwrite-mode (arg)
  "Toggle overwrite mode.
With arg, turn overwrite mode on iff arg is positive.
In overwrite mode, printing characters typed in replace existing text
on a one-for-one basis, rather than pushing it to the right."
  (interactive "P")
  (setq overwrite-mode
	(if (null arg) (not overwrite-mode)
	  (> (prefix-numeric-value arg) 0)))
  (set-buffer-modified-p (buffer-modified-p))) ;No-op, but updates mode line.

(defconst blink-matching-paren t
  "*Non-nil means show matching open-paren when close-paren is inserted.")

(defconst blink-matching-paren-distance 4000
  "*If non-nil, is maximum distance to search for matching open-paren
when close-paren is inserted.")

(defun blink-matching-open ()
  "Move cursor momentarily to the beginning of the sexp before point."
  (and (> (point) (1+ (point-min)))
       (/= (char-syntax (char-after (- (point) 2))) ?\\ )
       blink-matching-paren
       (let* ((oldpos (point))
	      (blinkpos)
	      (mismatch))
	 (save-excursion
	   (save-restriction
	     (if blink-matching-paren-distance
		 (narrow-to-region (max (point-min)
					(- (point) blink-matching-paren-distance))
				   oldpos))
	     (condition-case ()
		 (setq blinkpos (scan-sexps oldpos -1))
	       (error nil)))
	   (and blinkpos (/= (char-syntax (char-after blinkpos))
			     ?\$)
		(setq mismatch
		      (/= (char-after (1- oldpos))
			  (logand (lsh (aref (syntax-table)
					     (char-after blinkpos))
				       -8)
				  255))))
	   (if mismatch (setq blinkpos nil))
	   (if blinkpos
	       (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (goto-char blinkpos)
		  (message
		   "Matches %s"
		   (if (save-excursion
			 (skip-chars-backward " \t")
			 (not (bolp)))
		       (buffer-substring (progn (beginning-of-line) (point))
					 (1+ blinkpos))
		     (buffer-substring blinkpos
				       (progn
					(forward-char 1)
					(skip-chars-forward "\n \t")
					(end-of-line)
					(point)))))))
	     (cond (mismatch
		    (message "Mismatched parentheses"))
		   ((not blink-matching-paren-distance)
		    (message "Unmatched parenthesis"))))))))

;Turned off because it makes dbx bomb out.
(setq blink-paren-hook 'blink-matching-open)

; this is just something for the luser to see in a keymap -- this is not
;  how quitting works normally!
(defun keyboard-quit ()
  "Signal a  quit  condition."
  (interactive)
  (signal 'quit nil))

(define-key global-map "\C-g" 'keyboard-quit)

(defun set-variable (var val)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
When using this interactively, supply a Lisp expression for VALUE.
If you want VALUE to be a string, you must surround it with doublequotes."
  (interactive
   (let* ((var (read-variable "Set variable: "))
	  (minibuffer-help-form
	   '(funcall myhelp))
	  (myhelp
	   (function
	    (lambda ()
	      (with-output-to-temp-buffer "*Help*"
		(prin1 var)
		(princ "\nDocumentation:\n")
		(princ (substring (documentation-property var 'variable-documentation)
				  1))
		(if (boundp var)
		    (let ((print-length 20))
		      (princ "\n\nCurrent value: ")
		      (prin1 (symbol-value var))))
		nil)))))
     (list var
	   (eval-minibuffer (format "Set %s to value: " var)))))
  (set var val))

;These commands are defined in editfns.c
;but they are not assigned to keys there.
(put 'narrow-to-region 'disabled t)
(define-key ctl-x-map "n" 'narrow-to-region)
(define-key ctl-x-map "w" 'widen)

(define-key global-map "\C-j" 'newline-and-indent)
(define-key global-map "\C-m" 'newline)
(define-key global-map "\C-o" 'open-line)
(define-key esc-map "\C-o" 'split-line)
(define-key global-map "\C-q" 'quoted-insert)
(define-key esc-map "^" 'delete-indentation)
(define-key esc-map "\\" 'delete-horizontal-space)
(define-key esc-map "m" 'back-to-indentation)
(define-key ctl-x-map "\C-o" 'delete-blank-lines)
(define-key esc-map " " 'just-one-space)
(define-key esc-map "z" 'zap-to-char)
(define-key esc-map "=" 'count-lines-region)
(define-key ctl-x-map "=" 'what-cursor-position)
(define-key esc-map "\e" 'eval-expression)
(define-key ctl-x-map "\e" 'repeat-complex-command)
(define-key ctl-x-map "u" 'advertised-undo)
(define-key global-map "\C-_" 'undo)
(define-key esc-map "!" 'shell-command)
(define-key esc-map "|" 'shell-command-on-region)

(define-key global-map "\C-u" 'universal-argument)
(let ((i ?0))
  (while (<= i ?9)
    (define-key esc-map (char-to-string i) 'digit-argument)
    (setq i (1+ i))))
(define-key esc-map "-" 'negative-argument)

(define-key global-map "\C-k" 'kill-line)
(define-key global-map "\C-w" 'kill-region)
(define-key esc-map "w" 'copy-region-as-kill)
(define-key esc-map "\C-w" 'append-next-kill)
(define-key global-map "\C-y" 'yank)
(define-key esc-map "y" 'yank-pop)

(define-key ctl-x-map "a" 'append-to-buffer)

(define-key global-map "\C-@" 'set-mark-command)
(define-key ctl-x-map "\C-x" 'exchange-point-and-mark)

(define-key global-map "\C-n" 'next-line)
(define-key global-map "\C-p" 'previous-line)
(define-key ctl-x-map "\C-n" 'set-goal-column)

(define-key global-map "\C-t" 'transpose-chars)
(define-key esc-map "t" 'transpose-words)
(define-key esc-map "\C-t" 'transpose-sexps)
(define-key ctl-x-map "\C-t" 'transpose-lines)

(define-key esc-map ";" 'indent-for-comment)
(define-key esc-map "j" 'indent-new-comment-line)
(define-key esc-map "\C-j" 'indent-new-comment-line)
(define-key ctl-x-map ";" 'set-comment-column)
(define-key ctl-x-map "f" 'set-fill-column)
(define-key ctl-x-map "$" 'set-selective-display)

(define-key esc-map "@" 'mark-word)
(define-key esc-map "f" 'forward-word)
(define-key esc-map "b" 'backward-word)
(define-key esc-map "d" 'kill-word)
(define-key esc-map "\177" 'backward-kill-word)

(define-key esc-map "<" 'beginning-of-buffer)
(define-key esc-map ">" 'end-of-buffer)
(define-key ctl-x-map "h" 'mark-whole-buffer)
(define-key esc-map "\\" 'delete-horizontal-space)

(fset 'mode-specific-command-prefix (make-sparse-keymap))
(defconst mode-specific-map (symbol-function 'mode-specific-command-prefix)
  "Keymap for characters following C-c.")
(define-key global-map "\C-c" 'mode-specific-command-prefix)
