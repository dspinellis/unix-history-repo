;; Electric Command History Mode
;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Principal author K. Shane Hartman

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


(require 'electric)			; command loop
(require 'chistory)			; history lister

(defun Electric-command-history-redo-expression (&optional noconfirm)
  "Edit current history line in minibuffer and execute result.
With prefix argument NOCONFIRM, execute current line as is without editing."
  (interactive "P")
  (let (todo)
    (save-excursion
      (set-buffer "*Command History*")
      (beginning-of-line)
      (setq todo (read (current-buffer)))
      (if (boundp 'electric-history-in-progress)
	  (if todo (throw 'electric-history-quit (list noconfirm todo)))))))

(defvar electric-history-map ())
(if electric-history-map
    ()
  (setq electric-history-map (make-keymap))
  (fillarray electric-history-map 'Electric-history-undefined)
  (define-key electric-history-map "\e" (make-keymap))
  (fillarray (lookup-key electric-history-map "\e") 'Electric-history-undefined)
  (define-key electric-history-map "\C-u" 'universal-argument)
  (define-key electric-history-map " " 'Electric-command-history-redo-expression)
  (define-key electric-history-map "!" 'Electric-command-history-redo-expression)
  (define-key electric-history-map "\e\C-x" 'eval-sexp)
  (define-key electric-history-map "\e\C-d" 'down-list)
  (define-key electric-history-map "\e\C-u" 'backward-up-list)
  (define-key electric-history-map "\e\C-b" 'backward-sexp)
  (define-key electric-history-map "\e\C-f" 'forward-sexp)
  (define-key electric-history-map "\e\C-a" 'beginning-of-defun)
  (define-key electric-history-map "\e\C-e" 'end-of-defun)
  (define-key electric-history-map "\e\C-n" 'forward-list)
  (define-key electric-history-map "\e\C-p" 'backward-list)
  (define-key electric-history-map "q" 'Electric-history-quit)
  (define-key electric-history-map "\C-c" nil)
  (define-key electric-history-map "\C-c\C-c" 'Electric-history-quit)
  (define-key electric-history-map "\C-]" 'Electric-history-quit)
  (define-key electric-history-map "\C-z" 'suspend-emacs)
  (define-key electric-history-map "\C-h" 'Helper-help)
  (define-key electric-history-map "?" 'Helper-describe-bindings)
  (define-key electric-history-map "\e>" 'end-of-buffer)
  (define-key electric-history-map "\e<" 'beginning-of-buffer)
  (define-key electric-history-map "\n" 'next-line)
  (define-key electric-history-map "\r" 'next-line)
  (define-key electric-history-map "\177" 'previous-line)  
  (define-key electric-history-map "\C-n" 'next-line)
  (define-key electric-history-map "\C-p" 'previous-line)
  (define-key electric-history-map "\ev" 'scroll-down)
  (define-key electric-history-map "\C-v" 'scroll-up)
  (define-key electric-history-map "\C-l" 'recenter)
  (define-key electric-history-map "\e\C-v" 'scroll-other-window))

(defvar electric-command-history-hook nil
  "If non-nil, its value is called by  electric-command-history.")

(defun electric-command-history ()
  "Major mode for examining and redoing commands from  command-history.
The number of command listed is controlled by  list-command-history-max.
The command history is filtered by  list-command-history-filter  if non-nil.
Combines typeout Command History list window with menu like selection
of an expression from the history for re-evaluation in the *original* buffer.

The history displayed is filtered by  list-command-history-filter  if non-nil.

This pops up a window with the Command History listing.  If the very
next character typed is Space, the listing is killed and the previous
window configuration is restored.  Otherwise, you can browse in the
Command History with  Return  moving down and  Delete  moving up, possibly
selecting an expression to be redone with Space or quitting with `Q'.

Like Emacs-Lisp Mode except that characters do not insert themselves and
Tab and linefeed do not indent.  Instead these commands are provided:
Space or !	edit then evaluate current line in history inside
		   the ORIGINAL buffer which invoked this mode.
		   The previous window configuration is restored
		   unless the invoked command changes it.
C-c C-c, C-], Q	Quit and restore previous window configuration.
LFD, RET	Move to the next line in the history.
DEL		Move to the previous line in the history.
?		Provides a complete list of commands.

Calls the value of  electric-command-history-hook  if that is non-nil
The Command History listing is recomputed each time this mode is invoked."
  (interactive)
  (let ((electric-history-in-progress t)
	(old-buffer (current-buffer))
	(todo))
    (unwind-protect
	(setq todo
	      (catch 'electric-history-quit
		(save-window-excursion
		  (save-window-excursion
		    (list-command-history)
		    (set-buffer "*Command History*")
		    (Command-history-setup 'electric-command-history
					   "Electric History"
					   electric-history-map))
		  (Electric-pop-up-window "*Command History*")
		  (run-hooks 'electric-command-history-hook)
		  (if (eobp)
		      (progn (ding)
			     (message "No command history.")
			     (throw 'electric-history-quit nil))
		    (let ((Helper-return-blurb "return to History"))
		      (Electric-command-loop 'electric-history-quit
					     "->" t))))))
      (set-buffer "*Command History*")
      (Command-history-setup)
      (bury-buffer (current-buffer)))
    (if (consp todo)
	(progn (set-buffer old-buffer)
	       (if (car todo)
		   (apply (car (car (cdr todo))) (cdr (car (cdr todo))))
		 (edit-and-eval-command "Redo: " (car (cdr todo))))))))

(defun Electric-history-undefined ()
  (interactive)
  (ding)
  (message "Type C-h for help, ? for commands, C-c to quit, Space to execute")
  (sit-for 4))

(defun Electric-history-quit ()
  "Quit Electric Command History, restoring previous window configuration."
  (interactive)
  (if (boundp 'electric-history-in-progress)
      (progn (message "")
	     (throw 'electric-history-quit nil))))
