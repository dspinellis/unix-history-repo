;;; -*-Emacs-Lisp-*- Scheme under emacs stuff.
;; Copyright (C) 1985 Bill Rozas & Richard M. Stallman

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

;; Adapted from shell.el to scheme.  
;; Note: It only  works with Cscheme release 4 or later.

(require 'scheme)
(require 'shell)

(defvar inferior-scheme-mode-map nil)
(if inferior-scheme-mode-map
    nil
  (setq inferior-scheme-mode-map (copy-alist shell-mode-map))
  (define-key inferior-scheme-mode-map "\C-c\C-a" 'quit-shell-subjob)
  (define-key inferior-scheme-mode-map "\C-c\C-g" 'interrupt-shell-subjob)
  (define-key inferior-scheme-mode-map "\e\C-g" 'interrupt-shell-subjob)
  (scheme-mode-commands inferior-scheme-mode-map))

(defun inferior-scheme-mode ()
  "Major mode for interacting with an inferior Scheme process.

The following commands are available:
\\{inferior-scheme-mode-map}

Entry to this mode calls the value of scheme-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
scheme-mode-hook is called after shell-mode-hook.

You can send text to the inferior Scheme from other buffers
using the commands send-region, send-string and \\[scheme-send-definition].

Commands:
Delete converts tabs to spaces as it moves back.
Tab indents for Scheme; with argument, shifts rest
 of expression rigidly with the current line.
Meta-Control-Q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
C-d at end of buffer sends end-of-file as input.
C-d not at end or with arg deletes or kills characters.
C-u and C-w are kill commands, imitating normal Unix input editing.
C-c interrupts the shell or its current subjob if any.
C-z stops, likewise.  C-\\ sends quit signal, likewise.

C-x C-k deletes last batch of output from shell.
C-x C-v puts top of last batch of output at top of window."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'inferior-scheme-mode)
  (setq mode-name "Inferior Scheme")
  (setq mode-line-format 
	"--%1*%1*-Emacs: %17b   %M   %[(%m: %s)%]----%3p--%-")
  (scheme-mode-variables)
  (use-local-map inferior-scheme-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (run-hooks 'shell-mode-hook 'scheme-mode-hook))

(defun args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (args-to-list (substring string (+ 1 where)
					  (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (args-to-list (substring string pos (length string)))))))))

(defconst scheme-program-name "scheme"
  "Program invoked by the scheme and run-scheme commands")

(defun scheme (arg)
  "Run an inferior Scheme process reading a command line from the terminal."
  (interactive "sExtra arguments to scheme: ")
  (switch-to-buffer
   (apply 'make-shell (append (list "scheme" scheme-program-name nil)
			      (args-to-list arg)
			      '("-emacs"))))
  (inferior-scheme-mode))

(defun run-scheme (arg)
  "Run an inferior Scheme process.
Input and output via buffer *scheme*.
With argument it asks for a command line."
  (interactive "P")
  (if arg (call-interactively 'scheme)
    (switch-to-buffer (make-shell "scheme" scheme-program-name nil "-emacs"))
    (inferior-scheme-mode)))

(defun scheme-send-definition ()
  "Send the current definition to the Scheme process made by M-x run-scheme."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (send-region "scheme" (point) end)
     (send-string "scheme" "\n"))))

(defun scheme-send-definition-and-go ()
  "Send the current definition to the inferior Scheme, and switch to *scheme* buffer."
  (interactive)
  (scheme-send-definition)
  (switch-to-buffer "*scheme*"))

