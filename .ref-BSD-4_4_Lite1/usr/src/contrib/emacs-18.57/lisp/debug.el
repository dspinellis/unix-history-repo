;; Debuggers and related commands for Emacs
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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


(setq debugger 'debug)

(defun debug (&rest debugger-args)
  "Enter debugger.  Returns if user says \"continue\".
Arguments are mainly for use when this is called
 from the internals of the evaluator.
You may call with no args, or you may
 pass nil as the first arg and any other args you like.
 In that case, the list of args after the first will 
 be printed into the backtrace buffer."
  (message "Entering debugger...")
  (let (debugger-value
	(debugger-match-data (match-data))
	(debug-on-error nil)
	(debug-on-quit nil)
	(debugger-buffer (let ((default-major-mode 'fundamental-mode))
			   (generate-new-buffer "*Backtrace*")))
	(debugger-old-buffer (current-buffer))
	(debugger-step-after-exit nil)
	;; Don't keep reading from an executing kbd macro!
	(executing-macro nil)
	(cursor-in-echo-area nil))
    (unwind-protect
	(save-excursion
	  (save-window-excursion
	    (pop-to-buffer debugger-buffer)
	    (erase-buffer)
	    (let ((standard-output (current-buffer))
		  (print-escape-newlines t)
		  (print-length 50))
	      (backtrace))
	    (goto-char (point-min))
	    (debugger-mode)
	    (delete-region (point)
			   (progn
			     (forward-sexp 8)
			     (forward-line 1)
			     (point)))
	    (cond ((memq (car debugger-args) '(lambda debug))
		   (insert "Entering:\n")
		   (if (eq (car debugger-args) 'debug)
		       (progn
			 (backtrace-debug 4 t)
			 (delete-char 1)
			 (insert ?*)
			 (beginning-of-line))))
		  ((eq (car debugger-args) 'exit)
		   (insert "Return value: ")
		   (setq debugger-value (nth 1 debugger-args))
		   (prin1 debugger-value (current-buffer))
		   (insert ?\n)
		   (delete-char 1)
		   (insert ? )
		   (beginning-of-line))
		  ((eq (car debugger-args) 'error)
		   (insert "Signalling: ")
		   (prin1 (nth 1 debugger-args) (current-buffer))
		   (insert ?\n))
		  ((eq (car debugger-args) t)
		   (insert "Beginning evaluation of function call form:\n"))
		  (t
		   (prin1 (if (eq (car debugger-args) 'nil)
			      (cdr debugger-args) debugger-args)
			  (current-buffer))
		   (insert ?\n)))
	    (message "")
	    (let ((inhibit-trace t)
		  (standard-output nil)
		  (buffer-read-only t))
	      (message "")
	      (recursive-edit))))
      ;; So that users do not try to execute debugger commands
      ;;  in an invalid context
      (kill-buffer debugger-buffer)
      (store-match-data debugger-match-data))
    (setq debug-on-next-call debugger-step-after-exit)
    debugger-value))

(defun debugger-step-through ()
  "Proceed, stepping through subexpressions of this expression.
Enter another debugger on next entry to eval, apply or funcall."
  (interactive)
  (setq debugger-step-after-exit t)
  (message "Proceding, will debug on next eval or call.")
  (exit-recursive-edit))

(defun debugger-continue ()
  "Continue, evaluating this expression without stopping."
  (interactive)
  (message "Continuing.")
  (exit-recursive-edit))

(defun debugger-return-value (val)
  "Continue, specifying value to return.
This is only useful when the value returned from the debugger
will be used, such as in a debug on exit from a frame."
  (interactive "XReturn value (evaluated): ")
  (setq debugger-value val)
  (princ "Returning " t)
  (prin1 debugger-value)
  (exit-recursive-edit))

(defun debugger-frame-number ()
  "Return number of frames in backtrace before the one point points at."
  (save-excursion
    (beginning-of-line)
    (let ((opoint (point))
	  (count 0))
      (goto-char (point-min))
      (if (or (equal (buffer-substring (point) (+ (point) 6))
		     "Signal")
	      (equal (buffer-substring (point) (+ (point) 6))
		     "Return"))
	  (progn
	    (search-forward ":")
	    (forward-sexp 1)))
      (forward-line 1)
      (while (progn
	       (forward-char 2)
	       (if (= (following-char) ?\()
		   (forward-sexp 1)
		 (forward-sexp 2))
	       (forward-line 1)
	       (<= (point) opoint))
	(setq count (1+ count)))
      count)))

;; Chosen empirically to account for all the frames
;; that will exist when debugger-frame is called
;; within the first one that appears in the backtrace buffer.
;; Assumes debugger-frame is called from a key;
;; will be wrong if it is called with Meta-x.
(defconst debugger-frame-offset 8 "")

(defun debugger-frame ()
  "Request entry to debugger when this frame exits.
Applies to the frame whose line point is on in the backtrace."
  (interactive)
  (beginning-of-line)
  (let ((level (debugger-frame-number)))
    (backtrace-debug (+ level debugger-frame-offset) t))
  (if (= (following-char) ? )
      (let ((buffer-read-only nil))
	(delete-char 1)
	(insert ?*)))
  (beginning-of-line))

(defun debugger-frame-clear ()
  "Do not enter to debugger when this frame exits.
Applies to the frame whose line point is on in the backtrace."
  (interactive)
  (beginning-of-line)
  (let ((level (debugger-frame-number)))
    (backtrace-debug (+ level debugger-frame-offset) nil))
  (if (= (following-char) ?*)
      (let ((buffer-read-only nil))
	(delete-char 1)
	(insert ? )))
  (beginning-of-line))

(defun debugger-eval-expression (exp)
  (interactive "xEval: ")
  (save-excursion
    (if (null (buffer-name debugger-old-buffer))
	;; old buffer deleted
	(setq debugger-old-buffer (current-buffer)))
    (set-buffer debugger-old-buffer)
    (eval-expression exp)))

(defvar debugger-mode-map nil)
(if debugger-mode-map
    nil
  (let ((loop ? ))
    (setq debugger-mode-map (make-keymap))
    (suppress-keymap debugger-mode-map)
    (define-key debugger-mode-map "-" 'negative-argument)
    (define-key debugger-mode-map "b" 'debugger-frame)
    (define-key debugger-mode-map "c" 'debugger-continue)
    (define-key debugger-mode-map "r" 'debugger-return-value)
    (define-key debugger-mode-map "u" 'debugger-frame-clear)
    (define-key debugger-mode-map "d" 'debugger-step-through)
    (define-key debugger-mode-map "h" 'describe-mode)
    (define-key debugger-mode-map "q" 'top-level)
    (define-key debugger-mode-map "e" 'debugger-eval-expression)
    (define-key debugger-mode-map " " 'next-line)))

(put 'debugger-mode 'mode-class 'special)

(defun debugger-mode ()
  "Mode for backtrace buffers, selected in debugger.
\\{debugger-mode-map}
For the r command, when in debugger due to frame being exited,
    the value specified here will be used as the value of that frame.

Note lines starting with * are frames that will
 enter debugger when exited."
  (kill-all-local-variables)    
  (setq major-mode 'debugger-mode)
  (setq mode-name "Debugger")
  (setq truncate-lines t)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map debugger-mode-map))

(defun debug-on-entry (function)
  "Request FUNCTION to invoke debugger each time it is called.
If the user continues, FUNCTION's execution proceeds.
Works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined.
Use `cancel-debug-on-entry' to cancel the effect of this command.
Redefining FUNCTION also does that."
  (interactive "aDebug on entry (to function): ")
  (let ((defn (symbol-function function)))
    (if (eq (car defn) 'macro)
	(fset function (cons 'macro (debug-on-entry-1 function (cdr defn) t)))
      (fset function (debug-on-entry-1 function defn t))))
  function)

(defun cancel-debug-on-entry (function)
  "Undoes effect of debug-on-entry on FUNCTION."
  (interactive "aCancel debug on entry (to function): ")
  (let ((defn (symbol-function function)))
    (if (eq (car defn) 'macro)
	(fset function
	      (cons 'macro (debug-on-entry-1 function (cdr defn) nil)))
      (fset function (debug-on-entry-1 function defn nil))))
  function)

(defun debug-on-entry-1 (function defn flag)
  (or (eq (car defn) 'lambda)
      (error "%s not user-defined Lisp function." function))
  (let (tail prec)
    (if (stringp (car (nthcdr 2 defn)))
	(setq tail (nthcdr 3 defn)
	      prec (list (car defn) (car (cdr defn)) (car (cdr (cdr defn)))))
      (setq tail (nthcdr 2 defn)
	    prec (list (car defn) (car (cdr defn)))))
    (if (eq flag (equal (car tail) '(debug 'debug)))
	nil
      (if flag
	  (nconc prec (cons '(debug 'debug) tail))
	(nconc prec (cdr tail))))))
