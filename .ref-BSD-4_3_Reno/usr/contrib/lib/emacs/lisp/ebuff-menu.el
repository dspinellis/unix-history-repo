; buggestions to mly@ai.ai.mit.edu

;; who says one can't have typeout windows in gnu emacs?
;; like ^r select buffer from its emacs lunar or tmacs libraries.

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


(require 'electric)

;; this depends on the format of list-buffers (from src/buffer.c) and
;; on stuff in lisp/buff-menu.el

(defvar electric-buffer-menu-mode-map nil)
(defun electric-buffer-list (arg)
  "Vaguely like ITS lunar select buffer;
combining typeoutoid buffer listing with menuoid buffer selection.

This pops up a buffer describing the set of emacs buffers.
If the very next character typed is a space then the buffer list
 window disappears.

Otherwise, one may move around in the buffer list window, marking
 buffers to be selected, saved or deleted.

To exit and select a new buffer, type Space when the cursor is on the
 appropriate line of the buffer-list window.

Other commands are much like those of buffer-menu-mode.

Calls value of  electric-buffer-menu-mode-hook  on entry if non-nil.

\\{electric-buffer-menu-mode-map}" 
  (interactive "P")
  (let (select buffer)
    (save-window-excursion
      (save-window-excursion (list-buffers arg))
      (setq buffer (window-buffer (Electric-pop-up-window "*Buffer List*")))
      (unwind-protect
	  (progn
	    (set-buffer buffer)
	    (Electric-buffer-menu-mode)
	    (setq select
		  (catch 'electric-buffer-menu-select
		    (message "<<< Press Space to bury the buffer list >>>")
		    (if (= (setq unread-command-char (read-char)) ?\ )
			(progn (setq unread-command-char -1)
			       (throw 'electric-buffer-menu-select nil)))
		    (let ((first (progn (goto-char (point-min))
					(forward-line 2)
					(point)))
			  (last (progn (goto-char (point-max))
				       (forward-line -1)
				       (point)))
			  (goal-column 0))
		      (goto-char first)
		      (Electric-command-loop 'electric-buffer-menu-select
					     nil
					     t
					     'electric-buffer-menu-looper
					     (cons first last))))))
	(set-buffer buffer)
	(Buffer-menu-mode)
	(bury-buffer buffer)
	(message "")))
    (if select
	(progn (set-buffer buffer)
	       (let ((opoint (point-marker)))
		 (Buffer-menu-execute)
		 (goto-char (point-min))
		 (if (prog1 (search-forward "\n>" nil t)
		       (goto-char opoint) (set-marker opoint nil))
		     (Buffer-menu-select)
		     (switch-to-buffer (Buffer-menu-buffer t))))))))

(defun electric-buffer-menu-looper (state condition)
  (cond ((and condition
	      (not (memq (car condition) '(buffer-read-only
					   end-of-buffer
					   beginning-of-buffer))))
	 (signal (car condition) (cdr condition)))
	((< (point) (car state))
	 (goto-char (point-min))
	 (forward-line 2))
	((> (point) (cdr state))
	 (goto-char (point-max))
	 (forward-line -1)
	 (if (pos-visible-in-window-p (point-max))
	     (recenter -1)))))

(put 'Electric-buffer-menu-mode 'mode-class 'special)
(defun Electric-buffer-menu-mode ()
  "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
Letters do not insert themselves; instead, they are commands.
\\{electric-buffer-menu-mode-map}

C-g or C-c C-c -- exit buffer menu, returning to previous window and buffer
  configuration.  If the very first character typed is a space, it
  also has this effect.
Space -- select buffer of line point is on.
  Also show buffers marked with m in other windows,
  deletes buffers marked with \"D\", and saves those marked with \"S\".
m -- mark buffer to be displayed.
~ -- clear modified-flag on that buffer.
s -- mark that buffer to be saved.
d or C-d -- mark that buffer to be deleted.
u -- remove all kinds of marks from current line.
v -- view buffer, returning when done.
Delete -- back up a line and remove marks.


Entry to this mode via command \\[electric-buffer-list] calls the value of
electric-buffer-menu-mode-hook if it is non-nil."
  (kill-all-local-variables)
  (use-local-map electric-buffer-menu-mode-map)
  (setq mode-name "Electric Buffer Menu")
  (setq mode-line-buffer-identification "Electric Buffer List")
  (if (memq 'mode-name mode-line-format)
      (progn (setq mode-line-format (copy-sequence mode-line-format))
	     (setcar (memq 'mode-name mode-line-format) "Buffers")))
  (make-local-variable 'Helper-return-blurb)
  (setq Helper-return-blurb "return to buffer editing")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'Electric-buffer-menu-mode)
  (goto-char (point-min))
  (if (search-forward "\n." nil t) (forward-char -1))
  (run-hooks 'electric-buffer-menu-mode-hook))

;; generally the same as Buffer-menu-mode-map
;;  (except we don't indirect to global-map)
(put 'Electric-buffer-menu-undefined 'suppress-keymap t)
(if electric-buffer-menu-mode-map
    nil
  (let ((map (make-keymap)))
    (fillarray map 'Electric-buffer-menu-undefined)
    (define-key map "\e" (make-keymap))
    (fillarray (lookup-key map "\e") 'Electric-buffer-menu-undefined)
    (define-key map "\C-z" 'suspend-emacs)
    (define-key map "v" 'Electric-buffer-menu-mode-view-buffer)
    (define-key map "\C-h" 'Helper-help)
    (define-key map "?" 'Helper-describe-bindings)
    (define-key map "\C-c" nil)
    (define-key map "\C-c\C-c" 'Electric-buffer-menu-quit)
    (define-key map "\C-]" 'Electric-buffer-menu-quit)
    (define-key map "q" 'Electric-buffer-menu-quit)
    (define-key map " " 'Electric-buffer-menu-select)  
    (define-key map "\C-l" 'recenter)
    (define-key map "s" 'Buffer-menu-save)
    (define-key map "d" 'Buffer-menu-delete)
    (define-key map "k" 'Buffer-menu-delete)
    (define-key map "\C-d" 'Buffer-menu-delete-backwards)
    ;(define-key map "\C-k" 'Buffer-menu-delete)
    (define-key map "\177" 'Buffer-menu-backup-unmark)
    (define-key map "~" 'Buffer-menu-not-modified)
    (define-key map "u" 'Buffer-menu-unmark)
    (let ((i ?0))
      (while (<= i ?9)
	(define-key map (char-to-string i) 'digit-argument)
	(define-key map (concat "\e" (char-to-string i)) 'digit-argument)
	(setq i (1+ i))))
    (define-key map "-" 'negative-argument)
    (define-key map "\e-" 'negative-argument)
    (define-key map "m" 'Buffer-menu-mark)
    (define-key map "\C-u" 'universal-argument)
    (define-key map "\C-p" 'previous-line)
    (define-key map "\C-n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "\C-v" 'scroll-up)
    (define-key map "\ev" 'scroll-down)
    (define-key map "\e\C-v" 'scroll-other-window)
    (define-key map "\e>" 'end-of-buffer)
    (define-key map "\e<" 'beginning-of-buffer)
    (setq electric-buffer-menu-mode-map map)))
 
(defun Electric-buffer-menu-exit ()
  (interactive)
  (setq unread-command-char last-input-char)
  ;; for robustness
  (condition-case ()
      (throw 'electric-buffer-menu-select nil)
    (error (Buffer-menu-mode)
	   (other-buffer))))

(defun Electric-buffer-menu-select ()
  "Leave Electric Buffer Menu, selecting buffers and executing changes.
Saves buffers marked \"S\".  Deletes buffers marked \"K\".
Selects buffer at point and displays buffers marked \">\" in other
windows."
  (interactive)
  (throw 'electric-buffer-menu-select (point)))

(defun Electric-buffer-menu-quit ()
  "Leave Electric Buffer Menu, restoring previous window configuration.
Does not execute select, save, or delete commands."
  (interactive)
  (throw 'electric-buffer-menu-select nil))

(defun Electric-buffer-menu-undefined ()
  (interactive)
  (ding)
  (message (if (and (eq (key-binding "\C-c\C-c") 'Electric-buffer-menu-quit)
		    (eq (key-binding " ") 'Electric-buffer-menu-select)
		    (eq (key-binding "\C-h") 'Helper-help)
		    (eq (key-binding "?") 'Helper-describe-bindings))
	       "Type C-c C-c to exit, Space to select, C-h for help, ? for commands"
	     (substitute-command-keys "\
Type \\[Electric-buffer-menu-quit] to exit, \
\\[Electric-buffer-menu-select] to select, \
\\[Helper-help] for help, \\[Helper-describe-bindings] for commands.")))
  (sit-for 4))

(defun Electric-buffer-menu-mode-view-buffer ()
  "View buffer on current line in Electric Buffer Menu.
Returns to Electric Buffer Menu when done."
  (interactive)
  (let ((bufnam (Buffer-menu-buffer nil)))
    (if bufnam
	(view-buffer bufnam)
      (ding)
      (message "Buffer %s does not exist!" bufnam)
      (sit-for 4))))




