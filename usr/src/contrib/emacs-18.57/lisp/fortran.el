;;; Fortran mode for GNU Emacs  (beta test version 1.21, Oct. 1, 1985)
;;; Copyright (c) 1986 Free Software Foundation, Inc.
;;; Written by Michael D. Prange (prange@erl.mit.edu)

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

;;; Author acknowledges help from Stephen Gildea <gildea@erl.mit.edu>

;;; Bugs to bug-fortran-mode@erl.mit.edu.

(defvar fortran-do-indent 3
  "*Extra indentation applied to `do' blocks.")

(defvar fortran-if-indent 3
  "*Extra indentation applied to `if' blocks.")

(defvar fortran-continuation-indent 5
  "*Extra indentation applied to `continuation' lines.")

(defvar fortran-comment-indent-style 'fixed
  "*nil forces comment lines not to be touched,
'fixed produces fixed comment indentation to comment-column,
and 'relative indents to current fortran indentation plus comment-column.")

(defvar fortran-comment-line-column 6
  "*Indentation for text in comment lines.")

(defvar comment-line-start nil
  "*Delimiter inserted to start new full-line comment.")

(defvar comment-line-start-skip nil
  "*Regexp to match the start of a full-line comment.")

(defvar fortran-minimum-statement-indent 6
  "*Minimum indentation for fortran statements.")

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defvar fortran-comment-indent-char ? 
  "*Character to be inserted for Fortran comment indentation.
Normally a space.")

(defvar fortran-line-number-indent 1
  "*Maximum indentation for Fortran line numbers.
5 means right-justify them within their five-column field.")

(defvar fortran-check-all-num-for-matching-do nil
  "*Non-nil causes all numbered lines to be treated as possible do-loop ends.")

(defvar fortran-continuation-char ?$
  "*Character which is inserted in column 5 by \\[fortran-split-line]
to begin a continuation line.  Normally $.")

(defvar fortran-comment-region "c$$$"
  "*String inserted by \\[fortran-comment-region] at start of each line in region.")

(defvar fortran-electric-line-number t
  "*Non-nil causes line number digits to be moved to the correct column as typed.")

(defvar fortran-startup-message t
  "*Non-nil displays a startup message when fortran-mode is first called.")

(defvar fortran-column-ruler
  (concat "0   4 6  10        20        30        40        50        60        70\n"
	  "[   ]|{   |    |    |    |    |    |    |    |    |    |    |    |    |}\n")
  "*String displayed above current line by \\[fortran-column-ruler].")

(defconst fortran-mode-version "1.21")

(defvar fortran-mode-syntax-table nil
  "Syntax table in use in fortran-mode buffers.")

(if fortran-mode-syntax-table
    ()
  (setq fortran-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\; "w" fortran-mode-syntax-table)
  (modify-syntax-entry ?+ "." fortran-mode-syntax-table)
  (modify-syntax-entry ?- "." fortran-mode-syntax-table)
  (modify-syntax-entry ?* "." fortran-mode-syntax-table)
  (modify-syntax-entry ?/ "." fortran-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" fortran-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" fortran-mode-syntax-table)
  (modify-syntax-entry ?\\ "/" fortran-mode-syntax-table)
  (modify-syntax-entry ?. "w" fortran-mode-syntax-table)
  (modify-syntax-entry ?\n ">" fortran-mode-syntax-table))

(defvar fortran-mode-map () 
  "Keymap used in fortran mode.")

(if fortran-mode-map
    ()
  (setq fortran-mode-map (make-sparse-keymap))
  (define-key fortran-mode-map ";" 'fortran-abbrev-start)
  (define-key fortran-mode-map "\C-c;" 'fortran-comment-region)
  (define-key fortran-mode-map "\e\C-a" 'beginning-of-fortran-subprogram)
  (define-key fortran-mode-map "\e\C-e" 'end-of-fortran-subprogram)
  (define-key fortran-mode-map "\e;" 'fortran-indent-comment)
  (define-key fortran-mode-map "\e\C-h" 'mark-fortran-subprogram)
  (define-key fortran-mode-map "\e\n" 'fortran-split-line)
  (define-key fortran-mode-map "\e\C-q" 'fortran-indent-subprogram)
  (define-key fortran-mode-map "\C-c\C-w" 'fortran-window-create)
  (define-key fortran-mode-map "\C-c\C-r" 'fortran-column-ruler)
  (define-key fortran-mode-map "\C-c\C-p" 'fortran-previous-statement)
  (define-key fortran-mode-map "\C-c\C-n" 'fortran-next-statement)
  (define-key fortran-mode-map "\t" 'fortran-indent-line)
  (define-key fortran-mode-map "0" 'fortran-electric-line-number)
  (define-key fortran-mode-map "1" 'fortran-electric-line-number)
  (define-key fortran-mode-map "2" 'fortran-electric-line-number)
  (define-key fortran-mode-map "3" 'fortran-electric-line-number)
  (define-key fortran-mode-map "4" 'fortran-electric-line-number)
  (define-key fortran-mode-map "5" 'fortran-electric-line-number)
  (define-key fortran-mode-map "6" 'fortran-electric-line-number)
  (define-key fortran-mode-map "7" 'fortran-electric-line-number)
  (define-key fortran-mode-map "8" 'fortran-electric-line-number)
  (define-key fortran-mode-map "9" 'fortran-electric-line-number))

(defvar fortran-mode-abbrev-table nil)
(if fortran-mode-abbrev-table
    ()
  (define-abbrev-table 'fortran-mode-abbrev-table ())
  (let ((abbrevs-changed nil))
    (define-abbrev fortran-mode-abbrev-table  ";b"   "byte" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ch"  "character" nil)
    (define-abbrev fortran-mode-abbrev-table  ";cl"  "close" nil)
    (define-abbrev fortran-mode-abbrev-table  ";c"   "continue" nil)
    (define-abbrev fortran-mode-abbrev-table  ";cm"  "common" nil)
    (define-abbrev fortran-mode-abbrev-table  ";cx"  "complex" nil)
    (define-abbrev fortran-mode-abbrev-table  ";di"  "dimension" nil)
    (define-abbrev fortran-mode-abbrev-table  ";do"  "double" nil)
    (define-abbrev fortran-mode-abbrev-table  ";dc"  "double complex" nil)
    (define-abbrev fortran-mode-abbrev-table  ";dp"  "double precision" nil)
    (define-abbrev fortran-mode-abbrev-table  ";dw"  "do while" nil)
    (define-abbrev fortran-mode-abbrev-table  ";e"   "else" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ed"  "enddo" nil)
    (define-abbrev fortran-mode-abbrev-table  ";el"  "elseif" nil)
    (define-abbrev fortran-mode-abbrev-table  ";en"  "endif" nil)
    (define-abbrev fortran-mode-abbrev-table  ";eq"  "equivalence" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ex"  "external" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ey"  "entry" nil)
    (define-abbrev fortran-mode-abbrev-table  ";f"   "format" nil)
    (define-abbrev fortran-mode-abbrev-table  ";fu"  "function" nil)
    (define-abbrev fortran-mode-abbrev-table  ";g"   "goto" nil)
    (define-abbrev fortran-mode-abbrev-table  ";im"  "implicit" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ib"  "implicit byte" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ic"  "implicit complex" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ich" "implicit character" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ii"  "implicit integer" nil)
    (define-abbrev fortran-mode-abbrev-table  ";il"  "implicit logical" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ir"  "implicit real" nil)
    (define-abbrev fortran-mode-abbrev-table  ";inc" "include" nil)
    (define-abbrev fortran-mode-abbrev-table  ";in"  "integer" nil)
    (define-abbrev fortran-mode-abbrev-table  ";intr" "intrinsic" nil)
    (define-abbrev fortran-mode-abbrev-table  ";l"   "logical" nil)
    (define-abbrev fortran-mode-abbrev-table  ";op"  "open" nil)
    (define-abbrev fortran-mode-abbrev-table  ";pa"  "parameter" nil)
    (define-abbrev fortran-mode-abbrev-table  ";pr"  "program" nil)
    (define-abbrev fortran-mode-abbrev-table  ";p"   "print" nil)
    (define-abbrev fortran-mode-abbrev-table  ";re"  "real" nil)
    (define-abbrev fortran-mode-abbrev-table  ";r"   "read" nil)
    (define-abbrev fortran-mode-abbrev-table  ";rt"  "return" nil)
    (define-abbrev fortran-mode-abbrev-table  ";rw"  "rewind" nil)
    (define-abbrev fortran-mode-abbrev-table  ";s"   "stop" nil)
    (define-abbrev fortran-mode-abbrev-table  ";su"  "subroutine" nil)
    (define-abbrev fortran-mode-abbrev-table  ";ty"  "type" nil)
    (define-abbrev fortran-mode-abbrev-table  ";w"   "write" nil)))

(defun fortran-mode ()
  "Major mode for editing fortran code.
Tab indents the current fortran line correctly. 
`do' statements must not share a common `continue'.

Type `;?' or `;\\[help-command]' to display a list of built-in abbrevs for Fortran keywords.

Variables controlling indentation style and extra features:

 comment-start
    Normally nil in Fortran mode.  If you want to use comments
    starting with `!', set this to the string \"!\".
 fortran-do-indent
    Extra indentation within do blocks.  (default 3)
 fortran-if-indent
    Extra indentation within if blocks.  (default 3)
 fortran-continuation-indent
    Extra indentation appled to continuation statements.  (default 5)
 fortran-comment-line-column
    Amount of indentation for text within full-line comments. (default 6)
 fortran-comment-indent-style
    nil    means don't change indentation of text in full-line comments,
    fixed  means indent that text at column fortran-comment-line-column
    relative  means indent at fortran-comment-line-column beyond the
 	      indentation for a line of code.
    Default value is fixed.
 fortran-comment-indent-char
    Character to be inserted instead of space for full-line comment
    indentation.  (default is a space)
 fortran-minimum-statement-indent
    Minimum indentation for fortran statements. (default 6)
 fortran-line-number-indent
    Maximum indentation for line numbers.  A line number will get
    less than this much indentation if necessary to avoid reaching
    column 5.  (default 1)
 fortran-check-all-num-for-matching-do
    Non-nil causes all numbered lines to be treated as possible 'continue'
    statements.  (default nil)
 fortran-continuation-char
    character to be inserted in column 5 of a continuation line.
    (default $)
 fortran-comment-region
    String inserted by \\[fortran-comment-region] at start of each line in 
    region.  (default \"c$$$\")
 fortran-electric-line-number
    Non-nil causes line number digits to be moved to the correct column 
    as typed.  (default t)
 fortran-startup-message
    Set to nil to inhibit message first time fortran-mode is used.

Turning on Fortran mode calls the value of the variable fortran-mode-hook 
with no args, if that value is non-nil.
\\{fortran-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (if fortran-startup-message
      (message "Emacs Fortran mode version %s.  Bugs to bug-fortran-mode@erl.mit.edu" fortran-mode-version))
  (setq fortran-startup-message nil)
  (setq local-abbrev-table fortran-mode-abbrev-table)
  (set-syntax-table fortran-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'fortran-indent-line)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'fortran-comment-hook)
  (make-local-variable 'comment-line-start-skip)
  (setq comment-line-start-skip "^[Cc*][^ \t\n]*[ \t]*") ;[^ \t\n]* handles comment strings such as c$$$
  (make-local-variable 'comment-line-start)
  (setq comment-line-start "c")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "![ \t]*")
  (make-local-variable 'comment-start)
  (setq comment-start nil)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'abbrev-all-caps)
  (setq abbrev-all-caps t)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (use-local-map fortran-mode-map)
  (setq mode-name "Fortran")
  (setq major-mode 'fortran-mode)
  (run-hooks 'fortran-mode-hook))

(defun fortran-comment-hook ()
  (save-excursion
    (skip-chars-backward " \t")
    (max (+ 1 (current-column))
	 comment-column)))

(defun fortran-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned
if the value of  comment-start  is not nil.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive)
  (beginning-of-line)
  ;; Recognize existing comments of either kind.
  (cond ((looking-at comment-line-start-skip)
	 (fortran-indent-line))
	((re-search-forward comment-start-skip
			    (save-excursion (end-of-line) (point)) t)
	 (indent-for-comment))
	;; No existing comment.
	;; If side-by-side comments are defined, insert one,
	;; unless line is now blank.
	((and comment-start (not (looking-at "^[ \t]*$")))
	 (end-of-line)
	 (delete-horizontal-space)
	 (indent-to (fortran-comment-hook))
	 (insert comment-start))
	;; Else insert separate-line comment, making a new line if nec.
	(t
	 (if (looking-at "^[ \t]*$")
	     (delete-horizontal-space)
	   (beginning-of-line)
	   (insert "\n")
	   (forward-char -1))
	 (insert comment-line-start)
	 (insert-char (if (stringp fortran-comment-indent-char)
			  (aref fortran-comment-indent-char 0)
			  fortran-comment-indent-char)
		      (- (calculate-fortran-indent) (current-column))))))

(defun fortran-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts fortran-comment-region at the beginning of every line in the region. 
BEG-REGION and END-REGION are args which specify the region boundaries. 
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert fortran-comment-region)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert fortran-comment-region)))
      (let ((com (regexp-quote fortran-comment-region))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

(defun fortran-abbrev-start ()
  "Typing \";\\[help-command]\" or \";?\" lists all the fortran abbrevs. 
Any other key combination is executed normally." ;\\[help-command] is just a way to print the value of the variable help-char.
  (interactive)
  (let (c)
    (insert last-command-char)
    (if (or (= (setq c (read-char)) ??)	;insert char if not equal to `?'
	    (= c help-char))
	(fortran-abbrev-help)
      (setq unread-command-char c))))

(defun fortran-abbrev-help ()
  "List the currently defined abbrevs in Fortran mode."
  (interactive)
  (message "Listing abbrev table...")
  (require 'abbrevlist)
  (list-one-abbrev-table fortran-mode-abbrev-table "*Help*")
  (message "Listing abbrev table...done"))

(defun fortran-column-ruler ()
  "Inserts a column ruler momentarily above current line, till next keystroke.
The ruler is defined by the value of fortran-column-ruler.
The key typed is executed unless it is SPC."
  (interactive)
  (momentary-string-display 
   fortran-column-ruler (save-excursion (beginning-of-line) (point))
   nil "Type SPC or any command to erase ruler."))

(defun fortran-window-create ()
  "Makes the window 72 columns wide."
  (interactive)
  (let ((window-min-width 2))
    (split-window-horizontally 73))
  (other-window 1)
  (switch-to-buffer " fortran-window-extra" t)
  (select-window (previous-window)))

(defun fortran-split-line ()
  "Break line at point and insert continuation marker and alignment."
  (interactive)
  (delete-horizontal-space)
  (if (save-excursion (beginning-of-line) (looking-at comment-line-start-skip))
      (insert "\n" comment-line-start " ")
    (insert "\n " fortran-continuation-char))
  (fortran-indent-line))

(defun delete-horizontal-regexp (chars)
  "Delete all characters in CHARS around point.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \."
  (interactive "*s")
  (skip-chars-backward chars)
  (delete-region (point) (progn (skip-chars-forward chars) (point))))

(defun fortran-electric-line-number (arg)
  "Self insert, but if part of a Fortran line number indent it automatically.
Auto-indent does not happen if a numeric arg is used."
  (interactive "P")
  (if (or arg (not fortran-electric-line-number))
      (self-insert-command arg)
    (if (or (save-excursion (re-search-backward "[^ \t0-9]"
						(save-excursion
						  (beginning-of-line)
						  (point))
						t)) ;not a line number
	    (looking-at "[0-9]"))		;within a line number
	(insert last-command-char)
      (skip-chars-backward " \t")
      (insert last-command-char)
      (fortran-indent-line))))

(defun beginning-of-fortran-subprogram ()
  "Moves point to the beginning of the current fortran subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line -1)
    (re-search-backward "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]" nil 'move)
    (if (looking-at "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]")
	(forward-line 1))))

(defun end-of-fortran-subprogram ()
  "Moves point to the end of the current fortran subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line 2)
    (re-search-forward "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]" nil 'move)
    (goto-char (match-beginning 0))
    (forward-line 1)))

(defun mark-fortran-subprogram ()
  "Put mark at end of fortran subprogram, point at beginning. 
The marks are pushed."
  (interactive)
  (end-of-fortran-subprogram)
  (push-mark (point))
  (beginning-of-fortran-subprogram))
  
(defun fortran-previous-statement ()
  "Moves point to beginning of the previous fortran statement.
Returns 'first-statement if that statement is the first
non-comment Fortran statement in the file, and nil otherwise."
  (interactive)
  (let (not-first-statement continue-test)
    (beginning-of-line)
    (setq continue-test
	  (or (looking-at
	        (concat "[ \t]*" (regexp-quote (char-to-string
						 fortran-continuation-char))))
	      (looking-at "     [^ 0\n]")))
    (while (and (setq not-first-statement (= (forward-line -1) 0))
		(or (looking-at comment-line-start-skip)
		    (looking-at "[ \t]*$")
		    (looking-at "     [^ 0\n]")
		    (looking-at (concat "[ \t]*"  comment-start-skip)))))
    (cond ((and continue-test
		(not not-first-statement))
	   (message "Incomplete continuation statement."))
	  (continue-test	
	   (fortran-previous-statement))
	  ((not not-first-statement)
	   'first-statement))))

(defun fortran-next-statement ()
  "Moves point to beginning of the next fortran statement.
 Returns 'last-statement if that statement is the last
 non-comment Fortran statement in the file, and nil otherwise."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement (= (forward-line 1) 0))
 		(or (looking-at comment-line-start-skip)
 		    (looking-at "[ \t]*$")
 		    (looking-at "     [^ 0\n]")
 		    (looking-at (concat "[ \t]*"  comment-start-skip)))))
    (if (not not-last-statement)
 	'last-statement)))

(defun fortran-indent-line ()
  "Indents current fortran line based on its contents and on previous lines."
  (interactive)
  (let ((cfi (calculate-fortran-indent)))
    (save-excursion
      (beginning-of-line)
      (if (or (not (= cfi (fortran-current-line-indentation)))
	      (and (re-search-forward "^[ \t]*[0-9]+" (+ (point) 4) t)
		   (not (fortran-line-number-indented-correctly-p))))
	  (fortran-indent-to-column cfi)
	(beginning-of-line)
	(if (re-search-forward comment-start-skip
			       (save-excursion (end-of-line) (point)) 'move)
	    (fortran-indent-comment))))
    ;; Never leave point in left margin.
    (if (< (current-column) cfi)
	(move-to-column cfi))))

(defun fortran-indent-subprogram ()
  "Properly indents the Fortran subprogram which contains point."
  (interactive)
  (save-excursion
    (mark-fortran-subprogram)
    (message "Indenting subprogram...")
    (indent-region (point) (mark) nil))
  (message "Indenting subprogram...done."))

(defun calculate-fortran-indent ()
  "Calculates the fortran indent column based on previous lines."
  (let (icol first-statement (case-fold-search t))
    (save-excursion
      (setq first-statement (fortran-previous-statement))
      (if first-statement
	  (setq icol fortran-minimum-statement-indent)
	(progn
	  (if (= (point) (point-min))
	      (setq icol fortran-minimum-statement-indent)
	    (setq icol (fortran-current-line-indentation)))
	  (skip-chars-forward " \t0-9")
	  (cond ((looking-at "if[ \t]*(")
		 (if (or (looking-at ".*)[ \t]*then\\b[ \t]*[^ \t(=a-z0-9]")
			 (let (then-test)	;multi-line if-then
			   (while (and (= (forward-line 1) 0) ;search forward for then
				       (looking-at "     [^ 0]")
				       (not (setq then-test (looking-at ".*then\\b[ \t]*[^ \t(=a-z0-9]")))))
			   then-test))
		     (setq icol (+ icol fortran-if-indent))))
		((looking-at "\\(else\\|elseif\\)\\b")
		 (setq icol (+ icol fortran-if-indent)))
		((looking-at "do\\b")
		 (setq icol (+ icol fortran-do-indent)))))))
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at "[ \t]*$"))
	    ((looking-at comment-line-start-skip)
	     (cond ((eq fortran-comment-indent-style 'relative)
		    (setq icol (+ icol fortran-comment-line-column)))
		   ((eq fortran-comment-indent-style 'fixed)
		    (setq icol fortran-comment-line-column))))
	    ((or (looking-at (concat "[ \t]*"
				     (regexp-quote (char-to-string fortran-continuation-char))))
		 (looking-at "     [^ 0\n]"))
	     (setq icol (+ icol fortran-continuation-indent)))
	    (first-statement)
	    ((and fortran-check-all-num-for-matching-do
		  (looking-at "[ \t]*[0-9]+")
		  (fortran-check-for-matching-do))
	     (setq icol (- icol fortran-do-indent)))
	    (t
	     (skip-chars-forward " \t0-9")
	     (cond ((looking-at "end[ \t]*if\\b")
		    (setq icol (- icol fortran-if-indent)))
		   ((looking-at "\\(else\\|elseif\\)\\b")
		    (setq icol (- icol fortran-if-indent)))
		   ((and (looking-at "continue\\b")
			 (fortran-check-for-matching-do))
		    (setq icol (- icol fortran-do-indent)))
		   ((looking-at "end[ \t]*do\\b")
		    (setq icol (- icol fortran-do-indent)))
		   ((and (looking-at "end\\b[ \t]*[^ \t=(a-z]")
			 (not (= icol fortran-minimum-statement-indent)))
 		    (message "Warning: `end' not in column %d.  Probably an unclosed block." fortran-minimum-statement-indent))))))
    (max fortran-minimum-statement-indent icol)))

(defun fortran-current-line-indentation ()
  "Indentation of current line, ignoring Fortran line number or continuation.
This is the column position of the first non-whitespace character
aside from the line number and/or column 5 line-continuation character.
For comment lines, returns indentation of the first
non-indentation text within the comment."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at comment-line-start-skip)
	   (goto-char (match-end 0))
	   (skip-chars-forward
	     (if (stringp fortran-comment-indent-char)
		 fortran-comment-indent-char
	         (char-to-string fortran-comment-indent-char))))
	  ((looking-at "     [^ 0\n]")
	   (goto-char (match-end 0)))
	  (t
	   ;; Move past line number.
	   (move-to-column 5)))
    ;; Move past whitespace.
    (skip-chars-forward " \t")
    (current-column)))

(defun fortran-indent-to-column (col)
  "Indents current line with spaces to column COL.
notes: 1) A non-zero/non-blank character in column 5 indicates a continuation
          line, and this continuation character is retained on indentation;
       2) If fortran-continuation-char is the first non-whitespace character,
          this is a continuation line;
       3) A non-continuation line which has a number as the first
          non-whitespace character is a numbered line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at comment-line-start-skip)
	(if fortran-comment-indent-style
	    (let ((char (if (stringp fortran-comment-indent-char)
			    (aref fortran-comment-indent-char 0)
			    fortran-comment-indent-char)))
	      (goto-char (match-end 0))
	      (delete-horizontal-regexp (concat " \t" (char-to-string char)))
	      (insert-char char (- col (current-column)))))
      (if (looking-at "     [^ 0\n]")
	  (forward-char 6)
	(delete-horizontal-space)
	;; Put line number in columns 0-4
	;; or put continuation character in column 5.
	(cond ((eobp))
	      ((= (following-char) fortran-continuation-char)
	       (indent-to 5)
	       (forward-char 1))
	      ((looking-at "[0-9]+")
	       (let ((extra-space (- 5 (- (match-end 0) (point)))))
		 (if (< extra-space 0)
		     (message "Warning: line number exceeds 5-digit limit.")
		   (indent-to (min fortran-line-number-indent extra-space))))
	       (skip-chars-forward "0-9"))))
      ;; Point is now after any continuation character or line number.
      ;; Put body of statement where specified.
      (delete-horizontal-space)
      (indent-to col)
      ;; Indent any comment following code on the same line.
      (if (re-search-forward comment-start-skip
			     (save-excursion (end-of-line) (point)) t)
	  (progn (goto-char (match-beginning 0))
		 (if (not (= (current-column) (fortran-comment-hook)))
		     (progn (delete-horizontal-space)
			    (indent-to (fortran-comment-hook)))))))))

(defun fortran-line-number-indented-correctly-p ()
  "Return t if current line's line number is correctly indente.
Do not call if there is no line number."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (and (<= (current-column) fortran-line-number-indent)
	 (or (= (current-column) fortran-line-number-indent)
	     (progn (skip-chars-forward "0-9")
		    (= (current-column) 5))))))

(defun fortran-check-for-matching-do ()
  "When called from a numbered statement, returns t
 if matching 'do' is found, and nil otherwise."
  (let (charnum
	(case-fold-search t))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "[ \t]*[0-9]+")
	  (progn
	    (skip-chars-forward " \t")
	    (skip-chars-forward "0") ;skip past leading zeros
	    (setq charnum (buffer-substring (point)
					    (progn (skip-chars-forward "0-9")
						   (point))))
	    (beginning-of-line)
	    (and (re-search-backward
		  (concat "\\(^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]\\)\\|\\(^[ \t0-9]*do[ \t]*0*"
			  charnum "\\b\\)\\|\\(^[ \t]*0*" charnum "\\b\\)")
		  nil t)
		 (looking-at (concat "^[ \t0-9]*do[ \t]*0*" charnum))))))))


