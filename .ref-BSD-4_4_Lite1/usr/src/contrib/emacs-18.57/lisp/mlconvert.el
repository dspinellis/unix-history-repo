;; Convert buffer of Mocklisp code to real lisp.
;; Copyright (C) 1985 Free Software Foundation, Inc.

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

(defun convert-mocklisp-buffer ()
  "Convert buffer of Mocklisp code to real Lisp that GNU Emacs can run."
  (interactive)
  (emacs-lisp-mode)
  (set-syntax-table (copy-sequence (syntax-table)))
  (modify-syntax-entry ?\| "w")
  (message "Converting mocklisp (ugh!)...")
  (goto-char (point-min))
  (fix-mlisp-syntax)

  ;; Emulation of mocklisp is accurate only within a mocklisp-function
  ;; so turn any non-function into a defun and then call it.
  (goto-char (point-min))
  (condition-case ignore
      (while t
	(let ((opt (point))
	      (form (read (current-buffer))))
	  (and (listp form)
	       (not (eq (car form) 'defun))
	       (progn (insert "))\n\n(ml-foo)\n\n")
		      (save-excursion
			(goto-char opt)
			(skip-chars-forward "\n")
			(insert "(defun (ml-foo \n "))))))
    (end-of-file nil))

  (goto-char (point-min))
  (insert ";;; GNU Emacs code converted from Mocklisp\n")
  (insert "(require 'mlsupport)\n\n")
  (fix-mlisp-symbols)

  (goto-char (point-min))
  (message "Converting mocklisp...done"))

(defun fix-mlisp-syntax ()
  (while (re-search-forward "['\"]" nil t)
    (if (= (preceding-char) ?\")
	(progn (forward-char -1)
	       (forward-sexp 1))
      (delete-char -1)
      (insert "?")
    (if (or (= (following-char) ?\\) (= (following-char) ?^))
	  (forward-char 1)
	(if (looking-at "[^a-zA-Z]")
	    (insert ?\\)))
      (forward-char 1)
      (delete-char 1))))

(defun fix-mlisp-symbols ()
  (while (progn
	   (skip-chars-forward " \t\n()")
	   (not (eobp)))
    (cond ((or (= (following-char) ?\?)
	       (= (following-char) ?\"))
	   (forward-sexp 1))
	  ((= (following-char) ?\;)
	   (forward-line 1))
	  (t
	   (let ((start (point)) prop)
	     (forward-sexp 1)
	     (setq prop (get (intern-soft (buffer-substring start (point)))
			     'mocklisp))
	     (cond ((null prop))
		   ((stringp prop)
		    (delete-region start (point))
		    (insert prop))
		   (t
		    (save-excursion
		      (goto-char start)
		      (funcall prop)))))))))

(defun ml-expansion (ml-name lisp-string)
  (put ml-name 'mocklisp lisp-string))

(ml-expansion 'defun "ml-defun")
(ml-expansion 'if "ml-if")
(ml-expansion 'setq '(lambda ()
		       (if (looking-at "setq[ \t\n]+buffer-modified-p")
			   (replace-match "set-buffer-modified-p"))))

(ml-expansion 'while '(lambda ()
			 (let ((end (progn (forward-sexp 2) (point-marker)))
			       (start (progn (forward-sexp -1) (point))))
			   (let ((cond (buffer-substring start end)))
			     (cond ((equal cond "1")
				    (delete-region (point) end)
				    (insert "t"))
				   (t
				    (insert "(not (zerop ")
				    (goto-char end)
				    (insert "))")))
			     (set-marker end nil)
			     (goto-char start)))))

(ml-expansion 'arg "ml-arg")
(ml-expansion 'nargs "ml-nargs")
(ml-expansion 'interactive "ml-interactive")
(ml-expansion 'message "ml-message")
(ml-expansion 'print "ml-print")
(ml-expansion 'set "ml-set")
(ml-expansion 'set-default "ml-set-default")
(ml-expansion 'provide-prefix-argument "ml-provide-prefix-argument")
(ml-expansion 'prefix-argument-loop "ml-prefix-argument-loop")
(ml-expansion 'prefix-argument "ml-prefix-arg")
(ml-expansion 'use-local-map "ml-use-local-map")
(ml-expansion 'use-global-map "ml-use-global-map")
(ml-expansion 'modify-syntax-entry "ml-modify-syntax-entry")
(ml-expansion 'error-message "error")

(ml-expansion 'dot "point-marker")
(ml-expansion 'mark "mark-marker")
(ml-expansion 'beginning-of-file "beginning-of-buffer")
(ml-expansion 'end-of-file "end-of-buffer")
(ml-expansion 'exchange-dot-and-mark "exchange-point-and-mark")
(ml-expansion 'set-mark "set-mark-command")
(ml-expansion 'argument-prefix "universal-arg")

(ml-expansion 'previous-page "ml-previous-page")
(ml-expansion 'next-page "ml-next-page")
(ml-expansion 'next-window "ml-next-window")
(ml-expansion 'previous-window "ml-previous-window")

(ml-expansion 'newline "ml-newline")
(ml-expansion 'next-line "ml-next-line")
(ml-expansion 'previous-line "ml-previous-line")
(ml-expansion 'self-insert "self-insert-command")
(ml-expansion 'meta-digit "digit-argument")
(ml-expansion 'meta-minus "negative-argument")

(ml-expansion 'newline-and-indent "ml-newline-and-indent")
(ml-expansion 'yank-from-killbuffer "yank")
(ml-expansion 'yank-buffer "insert-buffer")
(ml-expansion 'copy-region "copy-region-as-kill")
(ml-expansion 'delete-white-space "delete-horizontal-space")
(ml-expansion 'widen-region "widen")

(ml-expansion 'forward-word '(lambda ()
			       (if (looking-at "forward-word[ \t\n]*)")
				   (replace-match "forward-word 1)"))))
(ml-expansion 'backward-word '(lambda ()
			       (if (looking-at "backward-word[ \t\n]*)")
				   (replace-match "backward-word 1)"))))

(ml-expansion 'forward-paren "forward-list")
(ml-expansion 'backward-paren "backward-list")
(ml-expansion 'search-reverse "ml-search-backward")
(ml-expansion 're-search-reverse "ml-re-search-backward")
(ml-expansion 'search-forward "ml-search-forward")
(ml-expansion 're-search-forward "ml-re-search-forward")
(ml-expansion 'quote "regexp-quote")
(ml-expansion 're-query-replace "query-replace-regexp")
(ml-expansion 're-replace-string "replace-regexp")

; forward-paren-bl, backward-paren-bl

(ml-expansion 'get-tty-character "read-char")
(ml-expansion 'get-tty-input "read-input")
(ml-expansion 'get-tty-string "read-string")
(ml-expansion 'get-tty-buffer "read-buffer")
(ml-expansion 'get-tty-command "read-command")
(ml-expansion 'get-tty-variable "read-variable")
(ml-expansion 'get-tty-no-blanks-input "read-no-blanks-input")
(ml-expansion 'get-tty-key "read-key")

(ml-expansion 'c= "char-equal")
(ml-expansion 'goto-character "goto-char")
(ml-expansion 'substr "ml-substr")
(ml-expansion 'variable-apropos "apropos")
(ml-expansion 'execute-mlisp-buffer "eval-current-buffer")
(ml-expansion 'execute-mlisp-file "load")
(ml-expansion 'visit-file "find-file")
(ml-expansion 'read-file "find-file")
(ml-expansion 'write-modified-files "save-some-buffers")
(ml-expansion 'backup-before-writing "make-backup-files")
(ml-expansion 'write-file-exit "save-buffers-kill-emacs")
(ml-expansion 'write-named-file "write-file")
(ml-expansion 'change-file-name "set-visited-file-name")
(ml-expansion 'change-buffer-name "rename-buffer")
(ml-expansion 'buffer-exists "get-buffer")
(ml-expansion 'delete-buffer "kill-buffer")
(ml-expansion 'unlink-file "delete-file")
(ml-expansion 'unlink-checkpoint-files "delete-auto-save-files")
(ml-expansion 'file-exists "file-exists-p")
(ml-expansion 'write-current-file "save-buffer")
(ml-expansion 'change-directory "cd")
(ml-expansion 'temp-use-buffer "set-buffer")
(ml-expansion 'fast-filter-region "filter-region")

(ml-expansion 'pending-input "input-pending-p")
(ml-expansion 'execute-keyboard-macro "call-last-kbd-macro")
(ml-expansion 'start-remembering "start-kbd-macro")
(ml-expansion 'end-remembering "end-kbd-macro")
(ml-expansion 'define-keyboard-macro "name-last-kbd-macro")
(ml-expansion 'define-string-macro "ml-define-string-macro")

(ml-expansion 'current-column "ml-current-column")
(ml-expansion 'current-indent "ml-current-indent")
(ml-expansion 'insert-character "insert")

(ml-expansion 'users-login-name "user-login-name")
(ml-expansion 'users-full-name "user-full-name")
(ml-expansion 'current-time "current-time-string")
(ml-expansion 'current-numeric-time "current-numeric-time-you-lose")
(ml-expansion 'current-buffer-name "buffer-name")
(ml-expansion 'current-file-name "buffer-file-name")

(ml-expansion 'local-binding-of "local-key-binding")
(ml-expansion 'global-binding-of "global-key-binding")

;defproc (ProcedureType, "procedure-type");

(ml-expansion 'remove-key-binding "global-unset-key")
(ml-expansion 'remove-binding "global-unset-key")
(ml-expansion 'remove-local-binding "local-unset-key")
(ml-expansion 'remove-all-local-bindings "use-local-map nil")
(ml-expansion 'autoload "ml-autoload")

(ml-expansion 'checkpoint-frequency "auto-save-interval")

(ml-expansion 'mode-string "mode-name")
(ml-expansion 'right-margin "fill-column")
(ml-expansion 'tab-size "tab-width")
(ml-expansion 'default-right-margin "default-fill-column")
(ml-expansion 'default-tab-size "default-tab-width")
(ml-expansion 'buffer-is-modified "(buffer-modified-p)")

(ml-expansion 'file-modified-time "you-lose-on-file-modified-time")
(ml-expansion 'needs-checkpointing "you-lose-on-needs-checkpointing")

(ml-expansion 'lines-on-screen "set-screen-height")
(ml-expansion 'columns-on-screen "set-screen-width")

(ml-expansion 'dumped-emacs "t")

(ml-expansion 'buffer-size "ml-buffer-size")
(ml-expansion 'dot-is-visible "pos-visible-in-window-p")

(ml-expansion 'track-eol-on-^N-^P "track-eol")
(ml-expansion 'ctlchar-with-^ "ctl-arrow")
(ml-expansion 'help-on-command-completion-error "completion-auto-help")
(ml-expansion 'dump-stack-trace "backtrace")
(ml-expansion 'pause-emacs "suspend-emacs")
(ml-expansion 'compile-it "compile")

(ml-expansion '!= "/=")
(ml-expansion '& "logand")
(ml-expansion '| "logior")
(ml-expansion '^ "logxor")
(ml-expansion '! "ml-not")
(ml-expansion '<< "lsh")

;Variable pause-writes-files

