;; Note: use
;;  (autoload 'icon-mode "icon" nil t)
;;  (setq auto-mode-alist (cons '("\\.icn$" . icon-mode) auto-mode-alist))
;; if not permanently installed in your emacs

;; Icon code editing commands for Emacs
;; from c-mode.el  13-Apr-88  Chris Smith;  bugs to convex!csmith
;; Copyright (C) 1988 Free Software Foundation, Inc.

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


(defvar icon-mode-abbrev-table nil
  "Abbrev table in use in Icon-mode buffers.")
(define-abbrev-table 'icon-mode-abbrev-table ())

(defvar icon-mode-map ()
  "Keymap used in Icon mode.")
(if icon-mode-map
    ()
  (setq icon-mode-map (make-sparse-keymap))
  (define-key icon-mode-map "{" 'electric-icon-brace)
  (define-key icon-mode-map "}" 'electric-icon-brace)
  (define-key icon-mode-map "\e\C-h" 'mark-icon-function)
  (define-key icon-mode-map "\e\C-a" 'beginning-of-icon-defun)
  (define-key icon-mode-map "\e\C-e" 'end-of-icon-defun)
  (define-key icon-mode-map "\e\C-q" 'indent-icon-exp)
  (define-key icon-mode-map "\177" 'backward-delete-char-untabify)
  (define-key icon-mode-map "\t" 'icon-indent-command))

(defvar icon-mode-syntax-table nil
  "Syntax table in use in Icon-mode buffers.")

(if icon-mode-syntax-table
    ()
  (setq icon-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" icon-mode-syntax-table)
  (modify-syntax-entry ?# "<" icon-mode-syntax-table)
  (modify-syntax-entry ?\n ">" icon-mode-syntax-table)
  (modify-syntax-entry ?$ "." icon-mode-syntax-table)
  (modify-syntax-entry ?/ "." icon-mode-syntax-table)
  (modify-syntax-entry ?* "." icon-mode-syntax-table)
  (modify-syntax-entry ?+ "." icon-mode-syntax-table)
  (modify-syntax-entry ?- "." icon-mode-syntax-table)
  (modify-syntax-entry ?= "." icon-mode-syntax-table)
  (modify-syntax-entry ?% "." icon-mode-syntax-table)
  (modify-syntax-entry ?< "." icon-mode-syntax-table)
  (modify-syntax-entry ?> "." icon-mode-syntax-table)
  (modify-syntax-entry ?& "." icon-mode-syntax-table)
  (modify-syntax-entry ?| "." icon-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" icon-mode-syntax-table))

(defconst icon-indent-level 4
  "*Indentation of Icon statements with respect to containing block.")
(defconst icon-brace-imaginary-offset 0
  "*Imagined indentation of a Icon open brace that actually follows a statement.")
(defconst icon-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context.")
(defconst icon-continued-statement-offset 4
  "*Extra indent for lines not starting new statements.")
(defconst icon-continued-brace-offset 0
  "*Extra indent for substatements that start with open-braces.
This is in addition to icon-continued-statement-offset.")

(defconst icon-auto-newline nil
  "*Non-nil means automatically newline before and after braces,
and after colons and semicolons, inserted in C code.")

(defconst icon-tab-always-indent t
  "*Non-nil means TAB in Icon mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defun icon-mode ()
  "Major mode for editing Icon code.
Expression and list commands understand all Icon brackets.
Tab indents for Icon code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{icon-mode-map}
Variables controlling indentation style:
 icon-tab-always-indent
    Non-nil means TAB in Icon mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 icon-auto-newline
    Non-nil means automatically newline before and after braces
    inserted in Icon code.
 icon-indent-level
    Indentation of Icon statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 icon-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 icon-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to icon-continued-statement-offset.
 icon-brace-offset
    Extra indentation for line if it starts with an open brace.
 icon-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.

Turning on Icon mode calls the value of the variable icon-mode-hook with no args,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map icon-mode-map)
  (setq major-mode 'icon-mode)
  (setq mode-name "Icon")
  (setq local-abbrev-table icon-mode-abbrev-table)
  (set-syntax-table icon-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'icon-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "# *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'icon-comment-indent)
  (run-hooks 'icon-mode-hook))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in Icon code
;; based on its context.
(defun icon-comment-indent ()
  (if (looking-at "^#")
      0				;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	;Else indent at comment column
	   comment-column))))	; except leave at least one space.

(defun electric-icon-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if icon-auto-newline
		     (progn (icon-indent-line) (newline) t)
		   nil)))
	(progn
	  (insert last-command-char)
	  (icon-indent-line)
	  (if icon-auto-newline
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(icon-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun icon-indent-command (&optional whole-exp)
  (interactive "P")
  "Indent current line as Icon code, or in some cases insert a tab character.
If icon-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (if whole-exp
      ;; If arg, always indent this line as Icon
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (icon-indent-line))
	    beg end)
	(save-excursion
	  (if icon-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "#")))
    (if (and (not icon-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (icon-indent-line))))

(defun icon-indent-line ()
  "Indent current line as Icon code.
Return the amount the indentation changed by."
  (let ((indent (calculate-icon-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  ((eq indent t)
	   (setq indent (calculate-icon-indent-within-comment)))
	  ((looking-at "[ \t]*#")
	   (setq indent 0))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((and (looking-at "else\\b")
		       (not (looking-at "else\\s_")))
		  (setq indent (save-excursion
				 (icon-backward-to-start-of-if)
				 (current-indentation))))
		 ((or (= (following-char) ?})
		      (looking-at "end\\b"))
		  (setq indent (- indent icon-indent-level)))
		 ((= (following-char) ?{)
		  (setq indent (+ indent icon-brace-offset))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun calculate-icon-indent (&optional parse-start)
  "Return appropriate indentation for current line as Icon code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp
	  toplevel)
      (if parse-start
	  (goto-char parse-start)
	(setq toplevel (beginning-of-icon-defun)))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((and containing-sexp
		  (/= (char-after containing-sexp) ?{))
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (current-column))
	    (t
	      ;; Statement level.  Is it a continuation or a new statement?
	      ;; Find previous non-comment character.
	      (if toplevel
		  (progn (icon-backward-to-noncomment (point-min))
			 (if (icon-is-continuation-line)
			     icon-continued-statement-offset 0))
		(if (null containing-sexp)
		    (progn (beginning-of-icon-defun)
			   (setq containing-sexp (point))))
		(goto-char indent-point)
		(icon-backward-to-noncomment containing-sexp)
		;; Now we get the answer.
		(if (icon-is-continuation-line)
		    ;; This line is continuation of preceding line's statement;
		    ;; indent  icon-continued-statement-offset  more than the
		    ;; first line of the statement.
		    (progn
		      (icon-backward-to-start-of-continued-exp containing-sexp)
		      (+ icon-continued-statement-offset (current-column)
			 (if (save-excursion (goto-char indent-point)
					     (skip-chars-forward " \t")
					     (eq (following-char) ?{))
			     icon-continued-brace-offset 0)))
		  ;; This line starts a new statement.
		  ;; Position following last unclosed open.
		  (goto-char containing-sexp)
		  ;; Is line first statement after an open-brace?
		  (or
		    ;; If no, find that first statement and indent like it.
		    (save-excursion
		      (if (looking-at "procedure\\s ")
			  (forward-sexp 3)
			(forward-char 1))
		      (while (progn (skip-chars-forward " \t\n")
				    (looking-at "#"))
			;; Skip over comments following openbrace.
			(forward-line 1))
		      ;; The first following code counts
		      ;; if it is before the line we want to indent.
		      (and (< (point) indent-point)
			   (current-column)))
		    ;; If no previous statement,
		    ;; indent it relative to line brace is on.
		    ;; For open brace in column zero, don't let statement
		    ;; start there too.  If icon-indent-level is zero,
		    ;; use icon-brace-offset + icon-continued-statement-offset instead.
		    ;; For open-braces not the first thing in a line,
		    ;; add in icon-brace-imaginary-offset.
		    (+ (if (and (bolp) (zerop icon-indent-level))
			   (+ icon-brace-offset icon-continued-statement-offset)
			 icon-indent-level)
		       ;; Move back over whitespace before the openbrace.
		       ;; If openbrace is not first nonwhite thing on the line,
		       ;; add the icon-brace-imaginary-offset.
		       (progn (skip-chars-backward " \t")
			      (if (bolp) 0 icon-brace-imaginary-offset))
		       ;; here we are
		       (current-indentation))))))))))

(defun icon-is-continuation-line ()
  (let* ((ch (preceding-char))
	 (ch-syntax (char-syntax ch)))
    (if (eq ch-syntax ?w)
	(assoc (buffer-substring
		 (progn (forward-word -1) (point))
		 (progn (forward-word 1) (point)))
	       '(("do") ("dynamic") ("else") ("initial") ("link")
		 ("local") ("of") ("static") ("then")))
      (not (memq ch '(0 ?\; ?\} ?\{ ?\) ?\] ?\" ?\' ?\n))))))

(defun icon-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (beginning-of-line)
      (if (and (search-forward "#" opoint 'move)
	       (< lim (point)))
	  (forward-char -1)
	(setq stop t)))))

(defun icon-backward-to-start-of-continued-exp (lim)
  (if (memq (preceding-char) '(?\) ?\]))
      (forward-sexp -1))
  (while (icon-is-continued-line)
    (end-of-line 0))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun icon-is-continued-line ()
  (save-excursion
    (end-of-line 0)
    (icon-is-continuation-line)))

(defun icon-backward-to-start-of-if (&optional limit)
  "Move to the start of the last ``unbalanced'' if."
  (or limit (setq limit (save-excursion (beginning-of-icon-defun) (point))))
  (let ((if-level 1)
	(case-fold-search nil))
    (while (not (zerop if-level))
      (backward-sexp 1)
      (cond ((looking-at "else\\b")
	     (setq if-level (1+ if-level)))
	    ((looking-at "if\\b")
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))

(defun mark-icon-function ()
  "Put mark at end of Icon function, point at beginning."
  (interactive)
  (push-mark (point))
  (end-of-icon-defun)
  (push-mark (point))
  (beginning-of-line 0)
  (beginning-of-icon-defun))

(defun beginning-of-icon-defun ()
  "Go to the start of the enclosing procedure; return t if at top level."
  (interactive)
  (if (re-search-backward "^procedure\\s \\|^end[ \t\n]" (point-min) 'move)
      (looking-at "e")
    t))

(defun end-of-icon-defun ()
  (interactive)
  (if (not (bobp)) (forward-char -1))
  (re-search-forward "\\(\\s \\|^\\)end\\(\\s \\|$\\)" (point-max) 'move)
  (forward-word -1)
  (forward-line 1))

(defun indent-icon-exp ()
  "Indent each line of the Icon grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	restart outer-loop-done inner-loop-done state ostate
	this-indent last-sexp
	at-else at-brace at-do
	(opoint (point))
	(next-depth 0))
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq innerloop-done nil)
	(while (and (not innerloop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 ostate))
	      (icon-indent-line))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq innerloop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack (or (car (cdr state))
					(save-excursion (forward-sexp -1)
							(point)))))
	  (forward-line 1)
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		;; Lines inside parens are handled specially.
		(if (/= (char-after (car contain-stack)) ?{)
		    (setq this-indent (car indent-stack))
		  ;; Line is at statement level.
		  ;; Is it a new statement?  Is it an else?
		  ;; Find last non-comment character before this line
		  (save-excursion
		    (setq at-else (looking-at "else\\W"))
		    (setq at-brace (= (following-char) ?{))
		    (icon-backward-to-noncomment opoint)
		    (if (icon-is-continuation-line)
			;; Preceding line did not end in comma or semi;
			;; indent this line  icon-continued-statement-offset
			;; more than previous.
			(progn
			  (icon-backward-to-start-of-continued-exp (car contain-stack))
			  (setq this-indent
				(+ icon-continued-statement-offset (current-column)
				   (if at-brace icon-continued-brace-offset 0))))
		      ;; Preceding line ended in comma or semi;
		      ;; use the standard indent for this level.
		      (if at-else
			  (progn (icon-backward-to-start-of-if opoint)
				 (setq this-indent (current-indentation)))
			(setq this-indent (car indent-stack))))))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (calculate-icon-indent
			   (if (car indent-stack)
			       (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))))
	    ;; Adjust line indentation according to its contents
	    (if (or (= (following-char) ?})
		    (looking-at "end\\b"))
		(setq this-indent (- this-indent icon-indent-level)))
	    (if (= (following-char) ?{)
		(setq this-indent (+ this-indent icon-brace-offset)))
	    ;; Put chosen indentation into effect.
	    (or (= (current-column) this-indent)
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to this-indent)))
	    ;; Indent any comment following the text.
	    (or (looking-at comment-start-skip)
		(if (re-search-forward comment-start-skip (save-excursion (end-of-line) (point)) t)
		    (progn (indent-for-comment) (beginning-of-line))))))))))
