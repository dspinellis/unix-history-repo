;; Lisp editing commands for Emacs
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


(defun forward-sexp (&optional arg)
  "Move forward across one balanced expression.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (goto-char (or (scan-sexps (point) arg) (buffer-end arg)))
  (if (< arg 0) (backward-prefix-chars)))

(defun backward-sexp (&optional arg)
  "Move backward across one balanced expression.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-sexp (- arg)))

(defun mark-sexp (arg)
  "Set mark ARG sexps from point."
  (interactive "p")
  (push-mark
    (save-excursion
      (forward-sexp arg)
      (point))))

(defun forward-list (&optional arg)
  "Move forward across one balanced group of parentheses.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (goto-char (or (scan-lists (point) arg 0) (buffer-end arg))))

(defun backward-list (&optional arg)
  "Move backward across one balanced group of parentheses.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-list (- arg)))

(defun down-list (arg)
  "Move forward down one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still go down a level."
  (interactive "p")
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (scan-lists (point) inc -1) (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun backward-up-list (arg)
  "Move backward out of one level of parentheses.
With argument, do this that many times.
A negative argument means move forward but still to a less deep spot."
  (interactive "p")
  (up-list (- arg)))

(defun up-list (arg) 
  "Move forward out of one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still to a less deep spot."
  (interactive "p")
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (scan-lists (point) inc 1) (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun kill-sexp (arg)
  "Kill the syntactic expression following the cursor.
With argument, kill that many expressions after (or before) the cursor."
  (interactive "p")
  (let ((opoint (point)))
    (forward-sexp arg)
    (kill-region opoint (point))))

(defun backward-kill-sexp (arg)
  "Kill the syntactic expression preceding the cursor.
With argument, kill that many expressions before (or after) the cursor."
  (interactive "p")
  (kill-sexp (- arg)))

(defun beginning-of-defun (&optional arg)
  "Move backward to next beginning-of-defun.
With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (and arg (< arg 0) (forward-char 1))
  (and (re-search-backward "^\\s(" nil 'move (or arg 1))
       (progn (beginning-of-line) t)))

(defun buffer-end (arg)
  (if (> arg 0) (point-max) (point-min)))

(defun end-of-defun (&optional arg)
  "Move forward to next end of defun.
An end of a defun is found by moving forward from the beginning of one."
  (interactive "p")
  (if (or (null arg) (= arg 0)) (setq arg 1))
  (let ((first t))
    (while (and (> arg 0) (< (point) (point-max)))
      (let ((pos (point)) npos)
	(while (progn
		(if (and first
			 (progn
			  (forward-char 1)
			  (beginning-of-defun 1)))
		    nil
		  (or (bobp) (forward-char -1))
		  (beginning-of-defun -1))
		(setq first nil)
		(forward-list 1)
		(skip-chars-forward " \t")
		(if (looking-at "[;\n]")
		    (forward-line 1))
		(<= (point) pos))))
      (setq arg (1- arg)))
    (while (< arg 0)
      (let ((pos (point)))
	(beginning-of-defun 1)
	(forward-sexp 1)
	(forward-line 1)
	(if (>= (point) pos)
	    (if (beginning-of-defun 2)
		(progn
		  (forward-list 1)
		  (skip-chars-forward " \t")
		  (if (looking-at "[;\n]")
		      (forward-line 1)))
	      (goto-char (point-min)))))
      (setq arg (1+ arg)))))

(defun mark-defun ()
  "Put mark at end of defun, point at beginning."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  (push-mark (point))
  (beginning-of-defun)
  (re-search-backward "^\n" (- (point) 1) t))

(defun insert-parentheses (arg)
  "Put parentheses around next ARG sexps.  Leave point after open-paren.
No argument is equivalent to zero: just insert () and leave point between."
  (interactive "P")
;Install these commented-out lines for version 19.
;  (if arg (skip-chars-forward " \t")
;    (or (memq (char-syntax (preceding-char)) '(?\  ?> ?\( ))
;	(insert " ")))
  (insert ?\()
  (save-excursion
    (if arg
	(forward-sexp (prefix-numeric-value arg)))
    (insert ?\))
;    (or (memq (char-syntax (following-char)) '(?\  ?> ?\( ))
;	(insert " "))
    ))

(defun move-past-close-and-reindent ()
  "Move past next ), delete indentation before it, then indent after it."
  (interactive)
  (up-list 1)
  (forward-char -1)
  (while (save-excursion		; this is my contribution
	   (let ((before-paren (point)))
	     (back-to-indentation)
	     (= (point) before-paren)))
    (delete-indentation))
  (forward-char 1)
  (newline-and-indent))

(defun lisp-complete-symbol ()
  "Perform completion on Lisp symbol preceding point.
That symbol is compared against the symbols that exist
and any additional characters determined by what is there
are inserted.
If the symbol starts just after an open-parenthesis,
only symbols with function definitions are considered.
Otherwise, all symbols with function definitions, values
or properties are considered."
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion
		(backward-sexp 1)
		(while (= (char-syntax (following-char)) ?\')
		  (forward-char 1))
		(point)))
	 (pattern (buffer-substring beg end))
	 (predicate
	  (if (eq (char-after (1- beg)) ?\()
	      'fboundp
	    (function (lambda (sym)
			(or (boundp sym) (fboundp sym)
			    (symbol-plist sym))))))
	 (completion (try-completion pattern obarray predicate)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern obarray predicate)))
	     (or (eq predicate 'fboundp)
		 (let (new)
		   (while list
		     (setq new (cons (if (fboundp (intern (car list)))
					 (list (car list) " <f>")
				       (car list))
				     new))
		     (setq list (cdr list)))
		   (setq list (nreverse new))))
	     (with-output-to-temp-buffer "*Help*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))
