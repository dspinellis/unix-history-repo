;; Scheme mode, and its idiosyncratic commands.
;; Copyright (C) 1986, 1987, 1988 Free Software Foundation, Inc.
;; Adapted from Lisp mode by Bill Rozas, jinx@prep.

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


;; Initially a query replace of Lisp mode, except for the indentation 
;; of special forms.  Probably the code should be merged at some point 
;; so that there is sharing between both libraries.

;;; $Header: scheme.el,v 1.7 88/07/15 20:20:00 GMT cph Exp $

(provide 'scheme)

(defvar scheme-mode-syntax-table nil "")
(if (not scheme-mode-syntax-table)
    (let ((i 0))
      (setq scheme-mode-syntax-table (make-syntax-table))
      (set-syntax-table scheme-mode-syntax-table)

      ;; Default is atom-constituent.
      (while (< i 256)
	(modify-syntax-entry i "_   ")
	(setq i (1+ i)))

      ;; Word components.
      (setq i ?0)
      (while (<= i ?9)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (setq i ?A)
      (while (<= i ?Z)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (setq i ?a)
      (while (<= i ?z)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))

      ;; Whitespace
      (modify-syntax-entry ?\t "    ")
      (modify-syntax-entry ?\n ">   ")
      (modify-syntax-entry ?\f "    ")
      (modify-syntax-entry ?\r "    ")
      (modify-syntax-entry ?  "    ")

      ;; These characters are delimiters but otherwise undefined.
      ;; Brackets and braces balance for editing convenience.
      (modify-syntax-entry ?[ "(]  ")
      (modify-syntax-entry ?] ")[  ")
      (modify-syntax-entry ?{ "(}  ")
      (modify-syntax-entry ?} "){  ")
      (modify-syntax-entry ?\| "  23")

      ;; Other atom delimiters
      (modify-syntax-entry ?\( "()  ")
      (modify-syntax-entry ?\) ")(  ")
      (modify-syntax-entry ?\; "<   ")
      (modify-syntax-entry ?\" "\"    ")
      (modify-syntax-entry ?' "'   ")
      (modify-syntax-entry ?` "'   ")

      ;; Special characters
      (modify-syntax-entry ?, "'   ")
      (modify-syntax-entry ?@ "'   ")
      (modify-syntax-entry ?# "' 14")
      (modify-syntax-entry ?\\ "\\   ")))

(defvar scheme-mode-abbrev-table nil "")
(define-abbrev-table 'scheme-mode-abbrev-table ())

(defun scheme-mode-variables ()
  (set-syntax-table scheme-mode-syntax-table)
  (setq local-abbrev-table scheme-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'scheme-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'scheme-comment-indent)
  (setq mode-line-process '("" scheme-mode-line-process)))

(defvar scheme-mode-line-process "")

(defun scheme-mode-commands (map)
  (define-key map "\t" 'scheme-indent-line)
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\e\C-q" 'scheme-indent-sexp))

(defvar scheme-mode-map nil)
(if (not scheme-mode-map)
    (progn
      (setq scheme-mode-map (make-sparse-keymap))
      (scheme-mode-commands scheme-mode-map)))

(defun scheme-mode ()
  "Major mode for editing Scheme code.
Editing commands are similar to those of lisp-mode.

In addition, if an inferior Scheme process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
modeline of all Scheme buffers.  The names of commands that interact
with the Scheme process start with \"xscheme-\".  For more information
see the documentation for xscheme-interaction-mode.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-mode-map}
Entry to this mode calls the value of scheme-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (scheme-mode-initialize)
  (scheme-mode-variables)
  (run-hooks 'scheme-mode-hook))

(defun scheme-mode-initialize ()
  (use-local-map scheme-mode-map)
  (setq major-mode 'scheme-mode)
  (setq mode-name "Scheme"))

(autoload 'run-scheme "xscheme"
  "Run an inferior Scheme process.
Output goes to the buffer `*scheme*'.
With argument, asks for a command line."
  t)

(defvar scheme-mit-dialect t
  "If non-nil, scheme mode is specialized for MIT Scheme.
Set this to nil if you normally use another dialect.")

(defun scheme-comment-indent (&optional pos)
  (save-excursion
    (if pos (goto-char pos))
    (cond ((looking-at ";;;") (current-column))
	  ((looking-at ";;")
	   (let ((tem (calculate-scheme-indent)))
	     (if (listp tem) (car tem) tem)))
	  (t
	   (skip-chars-backward " \t")
	   (max (if (bolp) 0 (1+ (current-column)))
		comment-column)))))

(defvar scheme-indent-offset nil "")
(defvar scheme-indent-hook 'scheme-indent-hook "")

(defun scheme-indent-line (&optional whole-exp)
  "Indent current line as Scheme code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (calculate-scheme-indent)) shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at "[ \t]*;;;")
	;; Don't alter indentation of a ;;; comment line.
	nil
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
	  nil
	(delete-region beg (point))
	(indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp (not (zerop shift-amt))
	   (save-excursion
	     (goto-char beg)
	     (forward-sexp 1)
	     (setq end (point))
	     (goto-char beg)
	     (forward-line 1)
	     (setq beg (point))
	     (> end beg))
	   (indent-code-rigidly beg end shift-amt)))))

(defun calculate-scheme-indent (&optional parse-start)
  "Return appropriate indentation for current line as scheme code.
In usual case returns an integer: the column to indent to.
Can instead return a list, whose car is the column to indent to.
This means that following lines at the same level of indentation
should not necessarily be indented the same way.
The second element of the list is the buffer position
of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point)) state paren-depth desired-indent (retry t)
	  last-sexp containing-sexp first-sexp-list-p)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
	(setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry (setq paren-depth (car state)) (> paren-depth 0))
	(setq retry nil)
	(setq last-sexp (nth 2 state))
	(setq containing-sexp (car (cdr state)))
	;; Position following last unclosed open.
	(goto-char (1+ containing-sexp))
	;; Is there a complete sexp since then?
	(if (and last-sexp (> last-sexp (point)))
	    ;; Yes, but is there a containing sexp after that?
	    (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
	      (if (setq retry (car (cdr peek))) (setq state peek))))
	(if (not retry)
	    ;; Innermost containing sexp found
	    (progn
	      (goto-char (1+ containing-sexp))
	      (if (not last-sexp)
		  ;; indent-point immediately follows open paren.
		  ;; Don't call hook.
		  (setq desired-indent (current-column))
		;; Move to first sexp after containing open paren
		(parse-partial-sexp (point) last-sexp 0 t)
		(setq first-sexp-list-p (looking-at "\\s("))
		(cond
		 ((> (save-excursion (forward-line 1) (point))
		     last-sexp)
		  ;; Last sexp is on same line as containing sexp.
		  ;; It's almost certainly a function call.
		  (parse-partial-sexp (point) last-sexp 0 t)
		  (if (/= (point) last-sexp)
		      ;; Indent beneath first argument or, if only one sexp
		      ;; on line, indent beneath that.
		      (progn (forward-sexp 1)
			     (parse-partial-sexp (point) last-sexp 0 t)))
		  (backward-prefix-chars))
		 (t
		  ;; Indent beneath first sexp on same line as last-sexp.
		  ;; Again, it's almost certainly a function call.
		  (goto-char last-sexp)
		  (beginning-of-line)
		  (parse-partial-sexp (point) last-sexp 0 t)
		  (backward-prefix-chars)))))))
      ;; If looking at a list, don't call hook.
      (if first-sexp-list-p
	  (setq desired-indent (current-column)))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overriden by scheme-indent-offset
      ;; or if the desired indentation has already been computed.
      (cond ((car (nthcdr 3 state))
	     ;; Inside a string, don't change indentation.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (setq desired-indent (current-column)))
	    ((and (integerp scheme-indent-offset) containing-sexp)
	     ;; Indent by constant offset
	     (goto-char containing-sexp)
	     (setq desired-indent (+ scheme-indent-offset (current-column))))
	    ((not (or desired-indent
		      (and (boundp 'scheme-indent-hook)
			   scheme-indent-hook
			   (not retry)
			   (setq desired-indent
				 (funcall scheme-indent-hook
					  indent-point state)))))
	     ;; Use default indentation if not computed yet
	     (setq desired-indent (current-column))))
      desired-indent)))

(defun scheme-indent-hook (indent-point state)
  (let ((normal-indent (current-column)))
    (save-excursion
      (goto-char (1+ (car (cdr state))))
      (re-search-forward "\\sw\\|\\s_")
      (if (/= (point) (car (cdr state)))
	  (let ((function (buffer-substring (progn (forward-char -1) (point))
					    (progn (forward-sexp 1) (point))))
		method)
	    ;; Who cares about this, really?
	    ;(if (not (string-match "\\\\\\||" function)))
	    (setq function (downcase function))
	    (setq method (get (intern-soft function) 'scheme-indent-hook))
	    (cond ((integerp method)
		   (scheme-indent-specform method state indent-point))
		  (method
		   (funcall method state indent-point))
		  ((and (> (length function) 3)
			(string-equal (substring function 0 3) "def"))
		   (scheme-indent-defform state indent-point))))))))

(defvar scheme-body-indent 2 "")

(defun scheme-indent-specform (count state indent-point)
  (let ((containing-form-start (car (cdr state))) (i count)
	body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  scheme-indent-hook guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ scheme-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
		(condition-case nil
		    (progn
		      (setq count (1- count))
		      (forward-sexp 1)
		      (parse-partial-sexp (point) indent-point 1 t))
		  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (cond ((> count 0)
	   ;; A distinguished form.  Use double scheme-body-indent.
	   (list (+ containing-form-column (* 2 scheme-body-indent))
		 containing-form-start))
	  ;; A non-distinguished form. Use body-indent if there are no
	  ;; distinguished forms and this is the first undistinguished
	  ;; form, or if this is the first undistinguished form and
	  ;; the preceding distinguished form has indentation at least
	  ;; as great as body-indent.
	  ((and (= count 0)
		(or (= i 0)
		    (<= body-indent normal-indent)))
	   body-indent)
	  (t
	   normal-indent))))

(defun scheme-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
	(goto-char (car (cdr state)))
	(+ scheme-body-indent (current-column)))))

;;; Let is different in Scheme

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

;; This is correct but too slow.
;; The one below works almost always.
;;(defun scheme-let-indent (state indent-point)
;;  (if (would-be-symbol (next-sexp-as-string))
;;      (scheme-indent-specform 2 state indent-point)
;;      (scheme-indent-specform 1 state indent-point)))

(defun scheme-let-indent (state indent-point)
  (skip-chars-forward " \t")
  (if (looking-at "[a-zA-Z0-9+-*/?!@$%^&_:~]")
      (scheme-indent-specform 2 state indent-point)
      (scheme-indent-specform 1 state indent-point)))

;; (put 'begin 'scheme-indent-hook 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin 'scheme-indent-hook 0)
(put 'case 'scheme-indent-hook 1)
(put 'delay 'scheme-indent-hook 0)
(put 'do 'scheme-indent-hook 2)
(put 'lambda 'scheme-indent-hook 1)
(put 'let 'scheme-indent-hook 'scheme-let-indent)
(put 'let* 'scheme-indent-hook 1)
(put 'letrec 'scheme-indent-hook 1)
(put 'sequence 'scheme-indent-hook 0)

(put 'call-with-input-file 'scheme-indent-hook 1)
(put 'with-input-from-file 'scheme-indent-hook 1)
(put 'with-input-from-port 'scheme-indent-hook 1)
(put 'call-with-output-file 'scheme-indent-hook 1)
(put 'with-output-to-file 'scheme-indent-hook 1)
(put 'with-output-to-port 'scheme-indent-hook 1)

;;;; MIT Scheme specific indentation.

(if scheme-mit-dialect
    (progn
      (put 'fluid-let 'scheme-indent-hook 1)
      (put 'in-package 'scheme-indent-hook 1)
      (put 'let-syntax 'scheme-indent-hook 1)
      (put 'local-declare 'scheme-indent-hook 1)
      (put 'macro 'scheme-indent-hook 1)
      (put 'make-environment 'scheme-indent-hook 0)
      (put 'named-lambda 'scheme-indent-hook 1)
      (put 'using-syntax 'scheme-indent-hook 1)

      (put 'with-input-from-string 'scheme-indent-hook 1)
      (put 'with-output-to-string 'scheme-indent-hook 0)
      (put 'with-values 'scheme-indent-hook 1)

      (put 'syntax-table-define 'scheme-indent-hook 2)
      (put 'list-transform-positive 'scheme-indent-hook 1)
      (put 'list-transform-negative 'scheme-indent-hook 1)
      (put 'list-search-positive 'scheme-indent-hook 1)
      (put 'list-search-negative 'scheme-indent-hook 1)

      (put 'access-components 'scheme-indent-hook 1)
      (put 'assignment-components 'scheme-indent-hook 1)
      (put 'combination-components 'scheme-indent-hook 1)
      (put 'comment-components 'scheme-indent-hook 1)
      (put 'conditional-components 'scheme-indent-hook 1)
      (put 'disjunction-components 'scheme-indent-hook 1)
      (put 'declaration-components 'scheme-indent-hook 1)
      (put 'definition-components 'scheme-indent-hook 1)
      (put 'delay-components 'scheme-indent-hook 1)
      (put 'in-package-components 'scheme-indent-hook 1)
      (put 'lambda-components 'scheme-indent-hook 1)
      (put 'lambda-components* 'scheme-indent-hook 1)
      (put 'lambda-components** 'scheme-indent-hook 1)
      (put 'open-block-components 'scheme-indent-hook 1)
      (put 'pathname-components 'scheme-indent-hook 1)
      (put 'procedure-components 'scheme-indent-hook 1)
      (put 'sequence-components 'scheme-indent-hook 1)
      (put 'unassigned\?-components 'scheme-indent-hook 1)
      (put 'unbound\?-components 'scheme-indent-hook 1)
      (put 'variable-components 'scheme-indent-hook 1)))

(defun scheme-indent-sexp ()
  "Indent each line of the list starting just after point."
  (interactive)
  (let ((indent-stack (list nil)) (next-depth 0) bol
	outer-loop-done inner-loop-done state this-indent)
    (save-excursion (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (not outer-loop-done)
	(setq last-depth next-depth
	      innerloop-done nil)
	(while (and (not innerloop-done)
		    (not (setq outer-loop-done (eobp))))
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (car (nthcdr 4 state))
	      (progn (indent-for-comment)
		     (end-of-line)
		     (setcar (nthcdr 4 state) nil)))
	  (if (car (nthcdr 3 state))
	      (progn
		(forward-line 1)
		(setcar (nthcdr 5 state) nil))
	    (setq innerloop-done t)))
	(if (setq outer-loop-done (<= next-depth 0))
	    nil
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  last-depth (1+ last-depth)))
	  (forward-line 1)
	  (setq bol (point))
	  (skip-chars-forward " \t")
	  (if (or (eobp) (looking-at "[;\n]"))
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		(setq this-indent (car indent-stack))
	      (let ((val (calculate-scheme-indent
			  (if (car indent-stack) (- (car indent-stack))))))
		(if (integerp val)
		    (setcar indent-stack
			    (setq this-indent val))
		  (setcar indent-stack (- (car (cdr val))))
		  (setq this-indent (car val)))))
	    (if (/= (current-column) this-indent)
		(progn (delete-region bol (point))
		       (indent-to this-indent)))))))))
