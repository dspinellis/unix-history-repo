;; Copyright (C) 1985 Free Software Foundation

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


;; Author William F. Schelter

;;to do fix software types for lispm:
;;to eval current expression.  Also to try to send escape keys correctly.
;;essentially we'll want the rubout-handler off.

(defvar telnet-new-line "\r")
(defvar telnet-mode-map nil)
(defvar telnet-prompt-pattern "^[^#$%>]*[#$%>] *")
(defvar telnet-interrupt-string "\^c" "String sent by C-c.")
(defvar telnet-count 0)
(defvar telnet-replace-c-g nil)
(defvar telnet-remote-echoes nil)

(defun telnet-interrupt-subjob ()
  (interactive)
  "Interrupt the program running through telnet on the remote host."
  (send-string nil telnet-interrupt-string))

(defun telnet-c-z ()
  (interactive)
  (send-string nil ""))

(defun send-process-next-char ()
  (interactive)
  (send-string nil
	       (char-to-string
		(let ((inhibit-quit t))
		  (prog1 (read-char)
		    (setq quit-flag nil))))))

(setq telnet-mode-map (make-sparse-keymap))

(progn
  (define-key telnet-mode-map "\C-m" 'telnet-send-input)
  (define-key telnet-mode-map "\C-j" 'telnet-send-input)
  (define-key telnet-mode-map "\C-c\C-d" 'shell-send-eof)
  (define-key telnet-mode-map "\C-c\C-q" 'send-process-next-char)
  (define-key telnet-mode-map "\C-c\C-c" 'telnet-interrupt-subjob) 
  (define-key telnet-mode-map "\C-c\C-z" 'telnet-c-z)
  (define-key telnet-mode-map "\C-c\C-u" 'kill-shell-input)
  (define-key telnet-mode-map "\C-c\C-w" 'backward-kill-word)
  (define-key telnet-mode-map "\C-c\C-o" 'kill-output-from-shell)
  (define-key telnet-mode-map "\C-c\C-r" 'show-output-from-shell))

;;maybe should have a flag for when have found type
(defun telnet-check-software-type-initialize (string)
  "Tries to put correct initializations in.  Needs work."
  (cond ((string-match "unix" string)
	 (setq telnet-prompt-pattern shell-prompt-pattern)
	 (setq telnet-new-line "\n"))
	((string-match "tops-20" string) ;;maybe add telnet-replace-c-g
	 (setq telnet-prompt-pattern  "[@>]*"))
	((string-match "its" string)
	 (setq telnet-prompt-pattern  "^[^*>]*[*>] *"))
	((string-match "explorer" string)  ;;explorer telnet needs work
	 (setq telnet-replace-c-g ?\n))
	))

(defun telnet-initial-filter (proc string)
  ;For reading up to and including password; also will get machine type.
  (cond ((string-match "No such host" string)
	 (kill-buffer (process-buffer proc))
	 (error "No such host."))
	((string-match "passw" string)
	 (telnet-filter proc string)
	 (let ((echo-keystrokes 0))
	   (setq password (read-password)))
	 (setq telnet-count 0)
	 (send-string proc (concat password  telnet-new-line)))
	(t (telnet-check-software-type-initialize string)
	   (telnet-filter proc string)
	   (cond ((> telnet-count 4)
		  (set-process-filter proc 'telnet-filter))
		 (t (setq telnet-count (1+ telnet-count)))))))

(defun telnet-filter (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (point-max))
    (let ((now (point)))
      (insert string)
      (subst-char-in-region now (point) ?\^m ?\ )
      (and telnet-replace-c-g
	   (subst-char-in-region now (point) ?\^g telnet-replace-c-g)))
    (if (process-mark proc)
	(set-marker (process-mark proc) (point)))
    (if (and (integer-or-marker-p last-input-start)
	     (marker-position last-input-start)
	     telnet-remote-echoes)
	(delete-region last-input-start last-input-end)))
  (if (eq (process-buffer proc)
	  (current-buffer))
      (goto-char (point-max))))

(defun delete-char-or-send-eof (arg killp)
  "At end of buffer, send eof to subshell.  Otherwise delete character."
  (interactive "p\nP")
  (if (and (eobp) (not killp))
      (process-send-eof)
    (delete-char arg killp)))

(defun telnet-send-input ()
  "Send input to remote host
At end of buffer, sends all text after last output
as input to the telnet, including a telnet-new-line inserted at the end.
Not at end, copies current line to the end of the buffer and sends it,
after first attempting to discard any prompt at the beginning of the line
by matching the regexp that is the value of telnet-prompt-pattern if possible."
 (interactive)
 (let (copied)
  (end-of-line)
  (if (eobp)
      (progn
	(move-marker last-input-start
		     (process-mark (get-buffer-process (current-buffer))))
	(move-marker last-input-end (point)))
    (beginning-of-line)
    (re-search-forward telnet-prompt-pattern nil t)
    (let ((copy (buffer-substring (point)
				  (progn (forward-line 1) (point)))))
      (goto-char (point-max))
      (move-marker last-input-start (point))
      (insert copy) (setq copied t)
      (move-marker last-input-end (point))))
  (save-excursion
    (goto-char last-input-start)
    (let ((process (get-buffer-process (current-buffer))))
      (send-region process last-input-start last-input-end)
      (if (not copied) (send-string process telnet-new-line))
      (set-marker (process-mark process) (point))))))

(defun telnet (arg)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer *HOST-telnet*.
Normally input is edited in Emacs and sent a line at a time."
  (interactive "sOpen telnet connection to host: ")
  (require 'shell)
  (let ((name (concat arg "-telnet" )))
    (switch-to-buffer (make-shell name "telnet"))
    (set-process-filter (get-process name) 'telnet-initial-filter)
    (erase-buffer)
    (send-string  name (concat "open " arg "\n"))
    (telnet-mode)
    (setq telnet-count -16)))

(defun read-password ()
  (let ((answ "") tem)
    (while (not(or  (= (setq tem (read-char)) ?\^m)
		    (= tem ?\n)))
      (setq answ (concat answ (char-to-string tem))))
    answ))

(defun telnet-mode ()
  "This mode is for use during telnet from a buffer to another
host. It has most of the same commands as shell mode.
There is a variable `telnet-interrupt-string' which is the character
sent to try to stop execution of a job on the remote host.
Data is sent to the remote host when `return' is typed.
Thus if you may need to edit the data before sending you
should use c-n to move down a line.  Then you can return
to alter a previous line.  Of course you should not use this
mode of telnet if you want to run emacs like programs on the
remote host (at least not yet!).

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{telnet-mode-map}

Bugs:
--Replace  by a space, really should remove.
--For Unix interacts poorly with tcsh although csh,sh,ksh are ok."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'telnet-mode)
  (setq mode-name "Telnet")
  (setq mode-line-process '(": %s"))
  (make-local-variable 'last-input-start)
  (use-local-map telnet-mode-map)
  (let ((tem telnet-prompt-pattern))
    (make-local-variable 'telnet-prompt-pattern)
    (setq telnet-prompt-pattern tem))
  (make-local-variable 'telnet-interrupt-string)
  (setq telnet-interrupt-string "")
  (make-local-variable 'telnet-new-line)
  (setq telnet-new-line "\r")
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'telnet-remote-echoes)
  (setq telnet-remote-echoes t)
  (make-local-variable 'telnet-replace-c-g)
  (setq telnet-replace-c-g nil))



