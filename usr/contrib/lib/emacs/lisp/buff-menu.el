;; Buffer menu main function and support functions.
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

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


; Put buffer *Buffer List* into proper mode right away
; so that from now on even list-buffers is enough to get a buffer menu.

(defvar Buffer-menu-mode-map nil "")

(if Buffer-menu-mode-map
    ()
  (setq Buffer-menu-mode-map (make-keymap))
  (suppress-keymap Buffer-menu-mode-map t)
  (define-key Buffer-menu-mode-map "q" 'Buffer-menu-select)
  (define-key Buffer-menu-mode-map "2" 'Buffer-menu-2-window)
  (define-key Buffer-menu-mode-map "1" 'Buffer-menu-1-window)
  (define-key Buffer-menu-mode-map "f" 'Buffer-menu-this-window)
  (define-key Buffer-menu-mode-map "o" 'Buffer-menu-other-window)
  (define-key Buffer-menu-mode-map "s" 'Buffer-menu-save)
  (define-key Buffer-menu-mode-map "d" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "k" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "\C-d" 'Buffer-menu-delete-backwards)
  (define-key Buffer-menu-mode-map "\C-k" 'Buffer-menu-delete)
  (define-key Buffer-menu-mode-map "x" 'Buffer-menu-execute)
  (define-key Buffer-menu-mode-map " " 'next-line)
  (define-key Buffer-menu-mode-map "\177" 'Buffer-menu-backup-unmark)
  (define-key Buffer-menu-mode-map "~" 'Buffer-menu-not-modified)
  (define-key Buffer-menu-mode-map "?" 'describe-mode)
  (define-key Buffer-menu-mode-map "u" 'Buffer-menu-unmark)
  (define-key Buffer-menu-mode-map "m" 'Buffer-menu-mark))

;; Buffer Menu mode is suitable only for specially formatted data.
(put 'Buffer-menu-mode 'mode-class 'special)

(defun Buffer-menu-mode ()
  "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
Letters do not insert themselves; instead, they are commands.
m -- mark buffer to be displayed.
q -- select buffer of line point is on.
  Also show buffers marked with m in other windows.
1 -- select that buffer in full-screen window.
2 -- select that buffer in one window,
  together with buffer selected before this one in another window.
f -- select that buffer in place of the buffer menu buffer.
o -- select that buffer in another window,
  so the buffer menu buffer remains visible in its window.
~ -- clear modified-flag on that buffer.
s -- mark that buffer to be saved, and move down.
d or k -- mark that buffer to be deleted, and move down.
C-d -- mark that buffer to be deleted, and move up.
x -- delete or save marked buffers.
u -- remove all kinds of marks from current line.
Delete -- back up a line and remove marks.

Precisely,\\{Buffer-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map Buffer-menu-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'Buffer-menu-mode)
  (setq mode-name "Buffer Menu")
  (run-hooks 'buffer-menu-mode-hook))

(defvar Buffer-menu-buffer-column nil)

(defvar Buffer-menu-size-column nil)

(defun Buffer-menu-buffer (error-if-non-existent-p)
  "Return buffer described by this line of buffer menu."
  (if (null Buffer-menu-buffer-column)
      (save-excursion
       (goto-char (point-min))
       (search-forward "Buffer")
       (backward-word 1)
       (setq Buffer-menu-buffer-column (current-column))
       (search-forward "Size")
       (backward-word 1)
       (setq Buffer-menu-size-column (current-column))))
  (save-excursion
    (beginning-of-line)
    (forward-char Buffer-menu-buffer-column)
    (let ((start (point))
	  string)
      ;; End of buffer name marked by tab or two spaces.
      (re-search-forward "\t\\|  ")
      (skip-chars-backward " \t")
      (setq string (buffer-substring start (point)))
      (or (get-buffer string)
	  (if error-if-non-existent-p
	      (error "No buffer named \"%s\"" string)
	    nil)))))

(defun buffer-menu (arg)
  "Make a menu of buffers so you can save, delete or select them.
With argument, show only buffers that are visiting files.
Type ? after invocation to get help on commands available.
Type q immediately to make the buffer menu go away."
  (interactive "P")
  (list-buffers arg)
  (pop-to-buffer "*Buffer List*")
  (forward-line 2)
  (message
   "Commands: d, s, x; 1, 2, m, u, q; delete; ~;  ? for help."))

(defun Buffer-menu-mark ()
  "Mark buffer on this line for being displayed by \\[Buffer-menu-select] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-M]")
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?>)
      (forward-line 1))))

(defun Buffer-menu-unmark ()
  "Cancel all requested operations on buffer on this line."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-M]")
      (ding)
    (let* ((buf (Buffer-menu-buffer t))
	   (mod (buffer-modified-p buf))
	   (readonly (save-excursion (set-buffer buf) buffer-read-only))
	   (buffer-read-only nil))
      (delete-char 3)
      (insert (if readonly (if mod " *%" "  %") (if mod " * " "   ")))))
  (forward-line 1))

(defun Buffer-menu-backup-unmark ()
  "Move up and cancel all requested operations on buffer on line above."
  (interactive)
  (forward-line -1)
  (Buffer-menu-unmark)
  (forward-line -1))

(defun Buffer-menu-delete ()
  "Mark buffer on this line to be deleted by \\[Buffer-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-M]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?D)
      (forward-line 1))))

(defun Buffer-menu-delete-backwards ()
  "Mark buffer on this line to be deleted by \\[Buffer-menu-execute] command
and then move up one line"
  (interactive)
  (Buffer-menu-delete)
  (forward-line -2)
  (if (looking-at " [-M]") (forward-line 1)))

(defun Buffer-menu-save ()
  "Mark buffer on this line to be saved by \\[Buffer-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (forward-char 1)
  (if (looking-at " [-M]")		;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?S)
      (forward-line 1))))

(defun Buffer-menu-not-modified ()
  "Mark buffer on this line as unmodified (no changes to save)."
  (interactive)
  (save-excursion
    (set-buffer (Buffer-menu-buffer t))
    (set-buffer-modified-p nil))
  (save-excursion
   (beginning-of-line)
   (forward-char 1)
   (if (looking-at "\\*")
       (let ((buffer-read-only nil))
	 (delete-char 1)
	 (insert ? )))))

(defun Buffer-menu-execute ()
  "Save and/or delete buffers marked with \\[Buffer-menu-save] or \\[Buffer-menu-delete] commands."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "^.S" nil t)
      (let ((modp nil))
	(save-excursion
	  (set-buffer (Buffer-menu-buffer t))
	  (save-buffer)
	  (setq modp (buffer-modified-p)))
	(let ((buffer-read-only nil))
	  (delete-char -1)
	  (insert (if modp ?* ? ))))))
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((buff-menu-buffer (current-buffer))
	  (buffer-read-only nil))
      (while (search-forward "\nD" nil t)
	(forward-char -1)
	(let ((buf (Buffer-menu-buffer nil)))
	  (or (eq buf nil)
	      (eq buf buff-menu-buffer)
	      (save-excursion (kill-buffer buf))))
	(if (Buffer-menu-buffer nil)
	    (progn (delete-char 1)
		   (insert ? ))
	  (delete-region (point) (progn (forward-line 1) (point)))
 	  (forward-char -1))))))

(defun Buffer-menu-select ()
  "Select this line's buffer; also display buffers marked with \">\".
You can mark buffers with the \\[Buffer-menu-mark] command."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
	(menu (current-buffer))	      
	(others ())
	tem)
    (goto-char (point-min))
    (while (search-forward "\n>" nil t)
      (setq tem (Buffer-menu-buffer t))
      (let ((buffer-read-only nil))
	(delete-char -1)
	(insert ?\ ))
      (or (eq tem buff) (memq tem others) (setq others (cons tem others))))
    (setq others (nreverse others)
	  tem (/ (1- (screen-height)) (1+ (length others))))
    (delete-other-windows)
    (switch-to-buffer buff)
    (or (eq menu buff)
	(bury-buffer menu))
    (while others
      (split-window nil tem)
      (other-window 1)
      (switch-to-buffer (car others))
      (setq others (cdr others)))
    (other-window 1)))			;back to the beginning!

(defun Buffer-menu-1-window ()
  "Select this line's buffer, alone, in full screen."
  (interactive)
  (switch-to-buffer (Buffer-menu-buffer t))
  (bury-buffer (other-buffer))
  (delete-other-windows))

(defun Buffer-menu-this-window ()
  "Select this line's buffer in this window."
  (interactive)
  (switch-to-buffer (Buffer-menu-buffer t)))

(defun Buffer-menu-other-window ()
  "Select this line's buffer in other window, leaving buffer menu visible."
  (interactive)
  (switch-to-buffer-other-window (Buffer-menu-buffer t)))

(defun Buffer-menu-2-window ()
  "Select this line's buffer, with previous buffer in second window."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
	(menu (current-buffer))
	(pop-up-windows t))
    (switch-to-buffer (other-buffer))
    (pop-to-buffer buff)
    (bury-buffer menu)))
