;; C++ code editing commands for Emacs
;; 1987 Dave Detlefs  (dld@cs.cmu.edu) 
;; and  Stewart Clamen (clamen@cs.cmu.edu).
;; Done by fairly faithful modification of:
;; c-mode.el, Copyright (C) 1985 Richard M. Stallman.
;;
;; Feb, 1990 (Dave Detlefs, dld@cs.cmu.edu)
;;   Fixed electric-c++-terminator to handle double colons, at the
;;   request of John Hagerman.
;; 
;; Jan, 1990 (Doug Lea, dl@oswego.edu)
;;   Replaced c++-comment-region and c++-uncomment-region with
;;     versions from Igor Metz that avoid potential infinite loops.
;;
;; Oct, 1989 (Dave Detlefs, dld@cs.cmu.edu)
;;   Added contribution from Igor Metz <metz@iam.unibe.ch>:
;;     functions c++-comment-region and c++-uncomment-region and 
;;     corresponding key-binding.
;;   Also fixed bug in indentation of second line after an empty
;;   arglist with empty-arglist non-null.
;;   
;; Sept, 1989 (Glen Ditchfield, gjditchfield@violet.uwaterloo.ca):
;;   Textual changes to more closely imitate Emacs 18.55's c-mode.
;;   Fixed handling of "default:", where ":" was the last character in the
;;   buffer.  Fixed indentation of comments starting in column 0, and when
;;   previous line contained more than one comment start string.  Fixed
;;   handling of "friend class".
;;
;; Aug 7, 1989; John Hagerman (hagerman@ece.cmu.edu):
;;   Changed calculate-c++-indent to handle member initializations
;;   more flexibly. Two new variables are used to control behavior:
;;   c++-member-init-indent and c++-continued-member-init-offset.
;;   Note the assumption that member initializations and argument
;;   declarations are not mixed in one function definition.
;;
;; June 1989 (Dave Detlefs, dld@cs.cmu.edu)
;;   Fixed calculate-c++-indent to handle continued lines ending in
;;   {'s.  (I wasn't following C-mode closely enough, or C-mode
;;   changed.)  Made ' a quote character, at the behest of someone
;;   whose mail I apparently deleted (if they send me mail I'll credit
;;   them here in a future revision.)
;;   Dan Weinreb (dlw@odi.com) pointed out that 'c++-mode successively
;;   bound c++-indent-exp and c++-indent-defun to ESC-^q.  ESC-^q is
;;   now bound to c++-indent-exp, while, c++-indent-defun is invoked
;;   with ESC-^x.

;; February 1989 (Dave Detlefs, dld@cs.cmu.edu)
;;   Fixed some errors in c++-indent-defun, as pointed out by Sam
;;   Haradhvala (odi!sam@talcott.harvard.edu).
;; October 1988 (Dave Detlefs, dld@cs.cmu.edu)
;;   It turns out I had only *thought* I had made
;;   beginning(end)-of-defun work.  It should work better now -- you
;;   can either attempt to match defun headers "strongly," using a
;;   very complicated regexp, or "weakly," using a simple one.  This
;;   is settable by a variable; the default is the cheaper weak
;;   method.  (Stewart Clamen was intimately involved in this, too.)
;;
;;   I made "'" *not* be a string delimiter, because that was causing
;;   comments containing contractions to ("// don't") to mess up paren
;;   balancing.
;;
;;   I also incorporated another slight indentation fix from Glen
;;   Ditchfield.
;;
;;   We hope this is will make into version 19 of gnu-emacs.
;;
;; September 1988: incorporated changes from Fred Calm at Schlumberger.
;;   Also, made beginning(end)-of-defun, indent-defun work.
;;
;; August 1987: incorporated changes done by Glen Ditchfield of Waterloo.

(defvar c++-mode-abbrev-table nil
  "Abbrev table in use in C++-mode buffers.")
(define-abbrev-table 'c++-mode-abbrev-table ())

(defvar c++-mode-map ()
  "Keymap used in C++ mode.")
(if c++-mode-map
    ()
  (setq c++-mode-map (make-sparse-keymap))
  (define-key c++-mode-map "\C-j" 'reindent-then-newline-and-indent)
  (define-key c++-mode-map "{" 'electric-c++-brace)
  (define-key c++-mode-map "}" 'electric-c++-brace)
  (define-key c++-mode-map ";" 'electric-c++-semi)
  (define-key c++-mode-map "\e\C-h" 'mark-c-function)
  (define-key c++-mode-map "\e\C-q" 'indent-c++-exp)
  (define-key c++-mode-map "\177" 'backward-delete-char-untabify)
  (define-key c++-mode-map "\t" 'c++-indent-command)
  (define-key c++-mode-map "\C-c\C-i" 'c++-insert-header)
  (define-key c++-mode-map "\C-c\C-\\" 'c++-macroize-region)
  (define-key c++-mode-map "\C-c\C-c" 'c++-comment-region)
  (define-key c++-mode-map "\C-c\C-u" 'c++-uncomment-region)
  (define-key c++-mode-map "\e\C-a" 'c++-beginning-of-defun)
  (define-key c++-mode-map "\e\C-e" 'c++-end-of-defun)
  (define-key c++-mode-map "\e\C-x" 'c++-indent-defun))

(defvar c++-mode-syntax-table nil
  "Syntax table in use in C++-mode buffers.")

(if c++-mode-syntax-table
    ()
  (setq c++-mode-syntax-table (copy-syntax-table c-mode-syntax-table))
  (modify-syntax-entry ?/ ". 12" c++-mode-syntax-table)
  (modify-syntax-entry ?\n ">" c++-mode-syntax-table)
  (modify-syntax-entry ?\' "." c++-mode-syntax-table))

(defvar c++-continued-member-init-offset nil
  "*Extra indent for continuation lines of member inits; NIL means to align
with previous initializations rather than with the colon on the first line.")
(defvar c++-member-init-indent 0
  "*Indentation level of member initializations in function declarations.")
(defvar c++-friend-offset -4
  "*Offset of C++ friend class declarations relative to member declarations.")
(defvar c++-electric-colon t
  "*If t, colon is an electric terminator.")
(defvar c++-empty-arglist-indent nil
  "*Indicates how far to indent an line following an empty argument
list.  Nil indicates to just after the paren.")


(defun c++-mode ()
  "Major mode for editing C++ code.  Very much like editing C code.
Expression and list commands understand all C++ brackets.
Tab at left margin indents for C++ code
Comments are delimited with /* ... */ {or with // ... <newline>}
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{c++-mode-map}
Variables controlling indentation style:
 c-tab-always-indent
    Non-nil means TAB in C mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
    Default is t.
 c-auto-newline
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in C code.
 c-indent-level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 c-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 c-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to c-continued-statement-offset.
 c-brace-offset
    Extra indentation for line if it starts with an open brace.
 c-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 c-argdecl-indent
    Indentation level of declarations of C function arguments.
 c-label-offset
    Extra indentation for line that is a label, or case or ``default:'', or
    ``public:'' or ``private:'', or ``protected:''.
 c++-electric-colon
    If non-nil at invocation of c++-mode (t is the default) colon electricly
    indents.
 c++-empty-arglist-indent
    If non-nil, a function declaration or invocation which ends a line with a
    left paren is indented this many extra spaces, instead of flush with the
    left paren.
 c++-friend-offset
    Offset of C++ friend class declarations relative to member declarations.
 c++-member-init-indent
    Indentation level of member initializations in function declarations,
    if they are on a separate line beginning with a colon.
 c++-continued-member-init-offset
    Extra indentation for continuation lines of member initializations; NIL
    means to align with previous initializations rather than with the colon.

Settings for K&R, BSD, and Stroustrup indentation styles are
  c-indent-level                5    8    4
  c-continued-statement-offset  5    8    4
  c-continued-brace-offset                0
  c-brace-offset               -5   -8    0
  c-brace-imaginary-offset                0
  c-argdecl-indent              0    8    4
  c-label-offset               -5   -8   -4
  c++-empty-arglist-indent                4
  c++-friend-offset                       0

Turning on C++ mode calls the value of the variable c++-mode-hook with
no args,if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map c++-mode-map)
  (setq major-mode 'c++-mode)
  (setq mode-name "C++")
  (setq local-abbrev-table c++-mode-abbrev-table)
  (set-syntax-table c++-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'c++-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "// ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *\\|// *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c++-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (run-hooks 'c++-mode-hook)
  (if c++-electric-colon
      (define-key c++-mode-map ":" 'electric-c++-terminator)))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in C++ code
;; based on its context.
(defun c++-comment-indent ()
  (if (looking-at "^\\(/\\*\\|//\\)")
      0					;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max
       ;; leave at least one space on non-empty lines.
       (if (zerop (current-column)) 0 (1+ (current-column)))
       (let ((cur-pt (point)))
	 (beginning-of-line 0)
	 ;; If previous line had a comment, use it's indent
	 (if (re-search-forward comment-start-skip cur-pt t)
	     (progn
	       (goto-char (match-beginning 0))
	       (current-column))
	   comment-column))))))		;otherwise indent at comment column.

(defun electric-c++-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if c-auto-newline (progn (c++-indent-line) (newline) t) nil)))
	(progn
	  (insert last-command-char)
	  (c++-indent-line)
	  (if c-auto-newline
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(c++-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun electric-c++-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if c-auto-newline
      (electric-c++-terminator arg)
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-c++-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos (end (point)))
    (if (and (not arg) (eolp)
	     (not (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    (or (= (following-char) ?#)
			;; Colon is special only after a label, or
			;; case, or another colon.
			;; So quickly rule out most other uses of colon
			;; and do no indentation for them.
			(and (eq last-command-char ?:)
			     (not (looking-at "case[ \t]"))
			     (save-excursion
			       (forward-word 1)
			       (skip-chars-forward " \t")
			       (< (point) end))
			     ;; Do re-indent double colons
			     (save-excursion
			       (end-of-line 1)
			       (looking-at ":")))
			(progn
			  (beginning-of-defun)
			  (let ((pps (parse-partial-sexp (point) end)))
			    (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))))
	(progn
	  (insert last-command-char)
	  (c++-indent-line)
	  (and c-auto-newline
	       (not (c-inside-parens-p))
	       (progn
		 ;; the new marker object, used to be just an integer
		 (setq insertpos (make-marker))
		 ;; changed setq to set-marker
		 (set-marker insertpos (1- (point)))
		 ;; do this before the newline, since in auto fill can break
		 (newline)
		 (c-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun c++-indent-command (&optional whole-exp)
  "Indent current line as C++ code, or in some cases insert a tab character.
If c-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as C
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (c++-indent-line))
	    beg end)
	(save-excursion
	  (if c-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "#")))
    (if (and (not c-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (c++-indent-line))))

(defun c++-indent-line ()
  "Indent current line as C++ code.
Return the amount the indentation changed by."
  (let ((indent (calculate-c++-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  ((eq indent t)
	   (setq indent (calculate-c-indent-within-comment)))
	  ((looking-at "[ \t]*#")
	   (setq indent 0))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((looking-at "\\(default\\|public\\|private\\|protected\\):")
		  (setq indent (+ indent c-label-offset)))
		 ((or (looking-at "case\\b")
		      (and (looking-at "[A-Za-z]")
			   (save-excursion
			     (forward-sexp 1)
			     (looking-at ":[^:]"))))
		  (setq indent (max 1 (+ indent c-label-offset))))
		 ((and (looking-at "else\\b")
		       (not (looking-at "else\\s_")))
		  (setq indent (save-excursion
				 (c-backward-to-start-of-if)
				 (current-indentation))))
		 ((looking-at "friend\[ \t]class[ \t]")
		  (setq indent (+ indent c++-friend-offset)))
		 ((= (following-char) ?})
		  (setq indent (- indent c-indent-level)))
		 ((= (following-char) ?{)
		  (setq indent (+ indent c-brace-offset))))))
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

(defun calculate-c++-indent (&optional parse-start)
  "Return appropriate indentation for current line as C++ code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.  May be data or function definition, or
	     ;; may be function argument declaration or member initialization.
	     ;; Indent like the previous top level line unless
	     ;; (1) the previous line ends in a closeparen without semicolon,
	     ;; in which case this line is the first argument declaration or
	     ;; member initialization, or
	     ;; (2) the previous line begins with a colon,
	     ;; in which case this is the second line of member inits.
	     ;; It is assumed that arg decls and member inits are not mixed.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (if (= (following-char) ?{)
		 0   ; Unless it starts a function body
	       (c++-backward-to-noncomment (or parse-start (point-min)))
	       (if (= (preceding-char) ?\))
		   (progn		; first arg decl or member init
		     (goto-char indent-point)
		     (skip-chars-forward " \t")
		     (if (= (following-char) ?:)
			 c++-member-init-indent
		       c-argdecl-indent))
		 (if (= (preceding-char) ?\;)
		     (backward-char 1))
		 (if (= (preceding-char) ?})
		     0
		   (beginning-of-line)	; continued arg decls or member inits
		   (skip-chars-forward " \t")
		   (if (= (following-char) ?:)
		       (if c++-continued-member-init-offset
			   (+ (current-indentation)
			      c++-continued-member-init-offset)
			 (progn
			   (forward-char 1)
			   (skip-chars-forward " \t")
			   (current-column)))
		     (current-indentation)))
		 )))
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open -- unless
	     ;; empty arg list, in which case we do what
	     ;; c++-empty-arglist-indent says to do.
	     (if (and c++-empty-arglist-indent
		      (or (null (nth 2 state))	;; indicates empty arg
						;; list.
			  ;; Use a heuristic: if the first
			  ;; non-whitespace following left paren on
			  ;; same line is not a comment,
			  ;; is not an empty arglist.
			  (save-excursion
			    (goto-char (1+ containing-sexp))
			    (not
			     (looking-at "\\( \\|\t\\)*[^/\n]")))))
		 (progn
		   (goto-char containing-sexp)
		   (beginning-of-line)
		   (skip-chars-forward " \t")
		   (goto-char (min (+ (point) c++-empty-arglist-indent)
				   (1+ containing-sexp)))
		   (current-column))
	       ;; In C-mode, we would always indent to one after the
	       ;; left paren.  Here, though, we may have an
	       ;; empty-arglist, so we'll indent to the min of that
	       ;; and the beginning of the first argument.
	       (goto-char (1+ containing-sexp))
	       (current-column)))
	    (t
	     ;; Statement.  Find previous non-comment character.
	     (goto-char indent-point)
	     (c++-backward-to-noncomment containing-sexp)
	     (if (not (memq (preceding-char) '(nil ?\, ?\; ?} ?: ?\{)))
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  c-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (c-backward-to-start-of-continued-exp containing-sexp)
		   (+ c-continued-statement-offset (current-column)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   (while (progn (skip-chars-forward " \t\n")
				 (looking-at
				  (concat
				   "#\\|/\\*\\|//"
				   "\\|case[ \t]"
				   "\\|[a-zA-Z0-9_$]*:[^:]"
				   "\\|friend[ \t]class[ \t]")))
		     ;; Skip over comments and labels following openbrace.
		     (cond ((= (following-char) ?\#)
			    (forward-line 1))
			   ((looking-at "/\\*")
			    (search-forward "*/" nil 'move))
			   ((looking-at "//\\|friend[ \t]class[ \t]")
			    (forward-line 1))
			   (t
			    (re-search-forward ":[^:]" nil 'move))))
		      ;; The first following code counts
		      ;; if it is before the line we want to indent.
		      (and (< (point) indent-point)
			   (current-column)))
		 ;; If no previous statement,
		 ;; indent it relative to line brace is on.
		 ;; For open brace in column zero, don't let statement
		 ;; start there too.  If c-indent-offset is zero,
		 ;; use c-brace-offset + c-continued-statement-offset instead.
		 ;; For open-braces not the first thing in a line,
		 ;; add in c-brace-imaginary-offset.
		 (+ (if (and (bolp) (zerop c-indent-level))
			(+ c-brace-offset c-continued-statement-offset)
		      c-indent-level)
		    ;; Move back over whitespace before the openbrace.
		    ;; If openbrace is not first nonwhite thing on the line,
		    ;; add the c-brace-imaginary-offset.
		    (progn (skip-chars-backward " \t")
			   (if (bolp) 0 c-brace-imaginary-offset))
		    ;; If the openbrace is preceded by a parenthesized exp,
		    ;; move to the beginning of that;
		    ;; possibly a different line
		    (progn
		      (if (eq (preceding-char) ?\))
			  (forward-sexp -1))
		      ;; Get initial indentation of the line we are on.
		      (current-indentation))))))))))

(defun c++-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\r\f" lim)
      (setq opoint (point))
      (cond ((and (>= (point) (+ 2 lim))
		  (save-excursion
		    (forward-char -2)
		    (looking-at "\\*/")))
	     (search-backward "/*" lim 'move))
	    ((and
	      (search-backward "//" (max (point-bol) lim) 'move)
	      (not (within-string-p (point) opoint))))
	  (t (beginning-of-line)
	     (skip-chars-forward " \t")
	     (if (looking-at "#")
		 (setq stop (<= (point) lim))
	       (setq stop t)
	       (goto-char opoint)))))))

(defun indent-c++-exp ()
  "Indent each line of the C++ grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	restart outer-loop-done inner-loop-done state ostate
	this-indent last-sexp
	at-else at-brace
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
	      (c++-indent-line))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq innerloop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  ;; Add levels for any parens that were started in this line.
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
		    (c++-backward-to-noncomment opoint)
		    (if (not (memq (preceding-char) '(nil ?\, ?\; ?} ?: ?{)))
			;; Preceding line did not end in comma or semi;
			;; indent this line  c-continued-statement-offset
			;; more than previous.
			(progn
			  (c-backward-to-start-of-continued-exp (car contain-stack))
			  (setq this-indent
				(+ c-continued-statement-offset (current-column)
				   (if at-brace c-continued-brace-offset 0))))
		      ;; Preceding line ended in comma or semi;
		      ;; use the standard indent for this level.
		      (if at-else
			  (progn (c-backward-to-start-of-if opoint)
				 (setq this-indent (current-indentation)))
			(setq this-indent (car indent-stack))))))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (calculate-c++-indent
			  (if (car indent-stack)
			      (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))))
	    ;; Adjust line indentation according to its contents
	    (if (looking-at "\\(public\\|private\\|protected\\):")
		(setq this-indent (- this-indent c-indent-level)))
	    (if (or (looking-at "case[ \t]")
		    (and (looking-at "[A-Za-z]")
			 (save-excursion
			   (forward-sexp 1)
			   (looking-at ":[^:]"))))
		(setq this-indent (max 1 (+ this-indent c-label-offset))))
	    (if (looking-at "friend[ \t]class[ \t]")
		(setq this-indent (+ this-indent c++-friend-offset)))
	    (if (= (following-char) ?})
		(setq this-indent (- this-indent c-indent-level)))
	    (if (= (following-char) ?{)
		(setq this-indent (+ this-indent c-brace-offset)))
	    ;; Put chosen indentation into effect.
	    (or (= (current-column) this-indent)
		(= (following-char) ?\#)
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to this-indent)))
	    ;; Indent any comment following the text.
	    (or (looking-at comment-start-skip)
		(if (re-search-forward comment-start-skip (save-excursion (end-of-line) (point)) t)
		    (progn (indent-for-comment) (beginning-of-line)))))))))
  )

(defun fill-C-comment ()
  (interactive)
  (save-excursion
    (let ((save fill-prefix))
      (beginning-of-line 1)
      (save-excursion
	(re-search-forward comment-start-skip
			   (save-excursion (end-of-line) (point))
			   t)
	(goto-char (match-end 0))
	(set-fill-prefix))
      (while (looking-at fill-prefix)
	(previous-line 1))
      (next-line 1)
      (insert-string "\n")
      (fill-paragraph nil)
      (delete-char -1)
      (setq fill-prefix save))))

(defun point-bol ()
  "Returns the value of the point at the beginning of the current
line."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun c++-insert-header ()
  "Insert header denoting C++ code at top of buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "// "
	    "This may look like C code, but it is really "
	    "-*- C++ -*-"
	    "\n\n")))


(defun within-string-p (point1 point2)
  "Returns true if number of double quotes between two points is odd."
  (let ((s (buffer-substring point1 point2)))
    (not (zerop (mod (count-char-in-string ?\" s) 2)))))

(defun count-char-in-string (c s)
  (let ((count 0)
	(pos 0))
    (while (< pos (length s))
      (setq count (+ count (if (\= (aref s pos) c) 1 0)))
      (setq pos (1+ pos)))
    count))



;;; This page covers "macroization;" making C++ parameterized types
;;; via macros.

(defvar c++-default-macroize-column 78
  "Place to insert backslashes.")

(defun c++-macroize-region (from to arg)
  "Insert backslashes at end of every line in region.  Useful for defining cpp
macros.  If called with negative argument, will remove trailing backslashes,
so that indentation will work right."
  (interactive "r\np")
  (save-excursion
    (goto-char from)
    (beginning-of-line 1)
    (let ((line (count-lines (point-min) (point)))
	  (to-line (save-excursion (goto-char to)
				   (count-lines (point-min) (point)))))
      (while (< line to-line)
	(backslashify-current-line (> arg 0))
	(next-line 1) (setq line (1+ line))))))

(defun backslashify-current-line (doit)
  (end-of-line 1)
  (cond
   (doit
    ;; Note that "\\\\" is needed to get one backslash.
    (if (not (save-excursion (forward-char -1) (looking-at "\\\\")))
	(progn
	  (if (>= (current-column) c++-default-macroize-column)
	      (insert " \\")
	    (while (<= (current-column) c++-default-macroize-column)
	      (insert "\t") (end-of-line))
	    (delete-char -1)
	    (while (< (current-column) c++-default-macroize-column)
	      (insert " ") (end-of-line))
	    (insert "\\")))))
   (t
    (forward-char -1)
    (if (looking-at "\\\\")
	(progn (skip-chars-backward " \t")
	       (kill-line))))))


;;; This page covers commenting out multiple lines.

(defun c++-comment-region ()
  "Comment out all lines in a region between mark and current point by
inserting \"// \" (comment-start)in front of each line."
  (interactive)
  (let* ((m      (if (eq (mark) nil) (error "Mark is not set!") (mark)))
	 (start  (if (< (point) m) (point) m))
	 (end    (if (> (point) m) (point) m))
	 (mymark (copy-marker end)))
    (save-excursion
	(goto-char start)
	(while (< (point) (marker-position mymark))
	    (beginning-of-line)
	    (insert comment-start)
	    (beginning-of-line)
	    (next-line 1)
	    ))))


(defun c++-uncomment-region ()
  "Uncomment all lines in region between mark and current point by deleting
the leading \"// \" from each line, if any."
  (interactive)
  (let* ((m      (if (eq (mark) nil) (error "Mark is not set!") (mark)))
	 (start  (if (< (point) m) (point) m))
	 (end    (if (> (point) m) (point) m))
	 (mymark (copy-marker end))
	 (len    (length comment-start))
	 (char   (string-to-char comment-start))
	 )
    (save-excursion
	(goto-char start)
	(while (< (point) (marker-position mymark))
	    (beginning-of-line)
	    (if (looking-at (concat " *" comment-start))
		(progn
		  (zap-to-char 1 char)
		  (delete-char len)))
	    (beginning-of-line)
	    (next-line 1)
	    ))))

;;; Below are two regular expressions that attempt to match defuns
;;; "strongly" and "weakly."  The strong one almost reconstructs the
;;; grammar of C++; the weak one just figures anything id or curly on
;;; the left begins a defun.  The constant "c++-match-header-strongly"
;;; determines which to use; the default is the weak one.

(defvar c++-match-header-strongly nil
  "*If NIL, use c++-defun-header-weak to identify beginning of definitions,\
if nonNIL, use c++-defun-header-strong")

(defvar c++-defun-header-strong-struct-equivs "\\(class\\|struct\\|enum\\)"
  "Regexp to match names of structure declaration blocks in C++")

(defconst c++-defun-header-strong
  (let*
      (; valid identifiers
       ;; There's a real wierdness here -- if I switch the below
       (id "\\(\\w\\|_\\)+")
       ;; to be
       ;; (id "\\(_\\|\\w\\)+")
       ;; things no longer work right.  Try it and see!

       ; overloadable operators
       (op-sym1
	 "[---+*/%^&|~!=<>]\\|[---+*/%^&|<>=!]=\\|<<=?\\|>>=?")
       (op-sym2
	 "&&\\|||\\|\\+\\+\\|--\\|()\\|\\[\\]")	 
       (op-sym (concat "\\(" op-sym1 "\\|" op-sym2 "\\)"))
       ; whitespace
       (middle "[^\\*]*\\(\\*+[^/\\*][^\\*]*\\)*")
       (c-comment (concat "/\\*" middle "\\*+/"))
       (wh (concat "\\(\\s \\|\n\\|//.*$\\|" c-comment "\\)"))
       (wh-opt (concat wh "*"))
       (wh-nec (concat wh "+"))
       (oper (concat "\\(" "operator" "\\("
		     wh-opt op-sym "\\|" wh-nec id "\\)" "\\)"))
       (dcl-list "([^():]*)")
       (func-name (concat "\\(" oper "\\|" id "::" id "\\|" id "\\)"))
       (inits
	 (concat "\\(:"
		 "\\(" wh-opt id "(.*\\()" wh-opt "," "\\)\\)*"
		 wh-opt id "(.*)" wh-opt "{"
		 "\\|" wh-opt "{\\)"))
       (type-name (concat
		    "\\(" c++-defun-header-strong-struct-equivs wh-nec "\\)?"
		    id))
       (type (concat "\\(const" wh-nec "\\)?"
		     "\\(" type-name "\\|" type-name wh-opt "\\*+" "\\|"
		     type-name wh-opt "&" "\\)"))
       (modifier "\\(inline\\|virtual\\|overload\\|auto\\|static\\)")
       (modifiers (concat "\\(" modifier wh-nec "\\)*"))
       (func-header
	 ;;     type               arg-dcl
	 (concat modifiers type wh-nec func-name wh-opt dcl-list wh-opt inits))
       (inherit (concat "\\(:" wh-opt "\\(public\\|private\\)?"
			wh-nec id "\\)"))
       (cs-header (concat
		    c++-defun-header-strong-struct-equivs
		    wh-nec id wh-opt inherit "?" wh-opt "{"))
       )
    (concat "^\\(" func-header "\\|" cs-header "\\)"))
  "Strongly-defined regexp to match beginning of structure \
or function definition.  ")


;; This part has to do with recognizing defuns.

;; The weak convention we will use is that a defun begins any time
;; there is a left curly brace, or some identifier on the left margin,
;; followed by a left curly somewhere on the line.  (This will also
;; incorrectly match some continued strings, but this is after all
;; just a weak heuristic.)  Suggestions for improvement (short of the
;; strong scheme shown above) are welcomed.

(defconst c++-defun-header-weak "^{\\|^[_a-zA-Z].*{"
  "Weakly-defined regexp to match beginning of structure \
or function definition.  ")


(defun c++-beginning-of-defun (arg)
  (interactive "p")
  (let ((c++-defun-header (if c++-match-header-strongly
			      c++-defun-header-strong
			    c++-defun-header-weak)))
    (cond ((or (= arg 0) (and (> arg 0) (bobp))) nil)
	  ((and (not (looking-at c++-defun-header))
		(let ((curr-pos (point))
		      (open-pos (if (search-forward "{" nil 'move)
				    (point)))
		      (beg-pos
			(if (re-search-backward c++-defun-header nil 'move)
			    (match-beginning 0))))
		  (if (and open-pos beg-pos
			   (< beg-pos curr-pos)
			   (> open-pos curr-pos))
		      (progn
			(goto-char beg-pos)
			(if (= arg 1) t nil));; Are we done?
		    (goto-char curr-pos)
		    nil))))
	  (t
	    (if (and (looking-at c++-defun-header) (not (bobp)))
		(forward-char (if (< arg 0) 1 -1)))
	    (and (re-search-backward c++-defun-header nil 'move (or arg 1))
		 (goto-char (match-beginning 0)))))))


(defun c++-end-of-defun (arg)
  (interactive "p")
  (let ((c++-defun-header (if c++-match-header-strongly
			      c++-defun-header-strong
			    c++-defun-header-weak)))
    (if (and (eobp) (> arg 0))
	nil
      (if (and (> arg 0) (looking-at c++-defun-header)) (forward-char 1))
      (let ((pos (point)))
	(c++-beginning-of-defun 
	  (if (< arg 0)
	      (- (- arg (if (eobp) 0 1)))
	    arg))
	(if (and (< arg 0) (bobp))
	    t
	  (if (re-search-forward c++-defun-header nil 'move)
	      (progn (forward-char -1)
		     (forward-sexp)
		     (beginning-of-line 2)))
	  (if (and (= pos (point)) 
		   (re-search-forward c++-defun-header nil 'move))
	      (c++-end-of-defun 1))))
      t
      ))
  )

(defun c++-indent-defun ()
  "Indents the current function def, struct or class decl."
  (interactive)
  (let ((restore (point)))
    (c++-end-of-defun 1)
    (beginning-of-line 1)
    (let ((end (point)))
      (c++-beginning-of-defun 1)
      (while (<= (point) end)
	(c++-indent-line)
	(next-line 1)
	(beginning-of-line 1)))
    (goto-char restore)))
