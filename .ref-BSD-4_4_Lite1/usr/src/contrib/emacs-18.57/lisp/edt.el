;; Copyright (C) 1986 Free Software Foundation, Inc.
;;  It started from public domain code by Mike Clarkson
;;  but has been greatly altered.

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

(require 'keypad)

(defvar edt-last-deleted-lines ""
  "Last text deleted by an EDT emulation line-delete command.")
(defvar edt-last-deleted-words ""
  "Last text deleted by an EDT emulation word-delete command.")
(defvar edt-last-deleted-chars ""
  "Last text deleted by an EDT emulation character-delete command.")

(defun delete-current-line (num)
  "Delete one or specified number of lines after point.
This includes the newline character at the end of each line.
They are saved for the EDT undelete-lines command."
  (interactive "p")
  (let ((beg (point)))
    (forward-line num)
    (if (not (eq (preceding-char) ?\n))
	(insert "\n"))
    (setq edt-last-deleted-lines
	  (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun delete-to-eol (num)
  "Delete text up to end of line.
With argument, delete up to to Nth line-end past point.
They are saved for the EDT undelete-lines command."
  (interactive "p")
  (let ((beg (point)))
    (forward-char 1)
    (end-of-line num)
    (setq edt-last-deleted-lines
	  (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun delete-current-word (num)
  "Delete one or specified number of words after point.
They are saved for the EDT undelete-words command."
  (interactive "p")
  (let ((beg (point)))
    (forward-word num)
    (setq edt-last-deleted-words
	  (buffer-substring beg (point)))
    (delete-region beg (point))))

(defun delete-previous-word (num)
  "Delete one or specified number of words before point.
They are saved for the EDT undelete-words command."
  (interactive "p")
  (let ((beg (point)))
    (forward-word (- num))
    (setq edt-last-deleted-words
	  (buffer-substring (point) beg))
    (delete-region beg (point))))

(defun delete-current-char (num)
  "Delete one or specified number of characters after point.
They are saved for the EDT undelete-chars command."
  (interactive "p")
  (setq edt-last-deleted-chars
	(buffer-substring (point) (min (point-max) (+ (point) num))))
  (delete-region (point) (min (point-max) (+ (point) num))))

(defun delete-previous-char (num)
  "Delete one or specified number of characters before point.
They are saved for the EDT undelete-chars command."
  (interactive "p")
  (setq edt-last-deleted-chars
	(buffer-substring (max (point-min) (- (point) num)) (point)))
  (delete-region (max (point-min) (- (point) num)) (point)))

(defun undelete-lines ()
  "Yank lines deleted by last EDT line-deletion command."
  (interactive)
  (insert edt-last-deleted-lines))

(defun undelete-words ()
  "Yank words deleted by last EDT word-deletion command."
  (interactive)
  (insert edt-last-deleted-words))

(defun undelete-chars ()
  "Yank characters deleted by last EDT character-deletion command."
  (interactive)
  (insert edt-last-deleted-chars))

(defun next-end-of-line (num)
  "Move to end of line; if at end, move to end of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (forward-char)
  (end-of-line num))

(defun previous-end-of-line (num)
  "Move EOL upward.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (end-of-line (- 1 num)))

(defun forward-to-word (num)
  "Move to next word-beginning, or to Nth following word-beginning."
  (interactive "p")
  (forward-word (1+ num))
  (forward-word -1))

(defun backward-to-word (num)
  "Move back to word-end, or to Nth word-end seen."
  (interactive "p")
  (forward-word (- (1+ num)))
  (forward-word 1))

(defun backward-line (num)
  "Move point to start of previous line.
Prefix argument serves as repeat-count."
  (interactive "p")
  (forward-line (- num)))

(defun scroll-window-down (num)
  "Scroll the display down a window-full.
Accepts a prefix argument for the number of window-fulls to scroll."
  (interactive "p")
  (scroll-down (- (* (window-height) num) 2)))

(defun scroll-window-up (num)
  "Scroll the display up a window-full.
Accepts a prefix argument for the number of window-fulls to scroll."
  (interactive "p")
  (scroll-up (- (* (window-height) num) 2)))

(defun next-paragraph (num)
  "Move to beginning of the next indented paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (while (> num 0)
    (next-line 1)
    (forward-paragraph)
    (previous-line 1)
    (if (eolp) (next-line 1))
    (setq num (1- num))))

(defun previous-paragraph (num)
  "Move to beginning of previous indented paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (while (> num 0)
    (backward-paragraph)
    (previous-line 1)
    (if (eolp) (next-line 1))
    (setq num (1- num))))

(defun move-to-beginning ()
  "Move cursor to the beginning of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-min)))

(defun move-to-end ()
  "Move cursor to the end of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-max)))

(defun goto-percent (perc)
  "Move point to ARG percentage of the buffer."
  (interactive "NGoto-percentage: ")
  (if (or (> perc 100) (< perc 0))
      (error "Percentage %d out of range 0 < percent < 100" perc)
    (goto-char (/ (* (point-max) perc) 100))))

(defun update-mode-line ()
  "Make sure mode-line in the current buffer reflects all changes."
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

(defun advance-direction ()
  "Set EDT Advance mode so keypad commands move forward."
  (interactive)
  (setq edt-direction-string " ADVANCE")
  (define-key function-keymap "\C-c" 'isearch-forward)  ; PF3
  (define-key function-keymap "8" 'scroll-window-up) ; "8"
  (define-key function-keymap "7" 'next-paragraph)   ; "7"
  (define-key function-keymap "1" 'forward-to-word)  ; "1"
  (define-key function-keymap "2" 'next-end-of-line) ; "2"
  (define-key function-keymap "3" 'forward-char)     ; "3"
  (define-key function-keymap "0" 'forward-line)     ; "0"
  (update-mode-line))

(defun backup-direction ()
  "Set EDT Backup mode so keypad commands move backward."
  (interactive)
  (setq edt-direction-string " BACKUP")
  (define-key function-keymap "\C-c" 'isearch-backward) ; PF3
  (define-key function-keymap "8" 'scroll-window-down) ; "8"
  (define-key function-keymap "7" 'previous-paragraph) ; "7"
  (define-key function-keymap "1" 'backward-to-word)    ; "1"
  (define-key function-keymap "2" 'previous-end-of-line) ; "2"
  (define-key function-keymap "3" 'backward-char)    ; "3"
  (define-key function-keymap "0" 'backward-line)    ; "0"
  (update-mode-line))

(defun beginning-of-window ()
  "Home cursor to top of window."
  (interactive)
  (move-to-window-line 0))

(defun line-to-bottom-of-window ()
  "Move the current line to the top of the window."
  (interactive)
  (recenter -1))

(defun line-to-top-of-window ()
  "Move the current line to the top of the window."
  (interactive)
  (recenter 0))

(defun case-flip-character (num)
  "Change the case of the character under the cursor.
Accepts a prefix argument of the number of characters to invert."
  (interactive "p")
  (while (> num 0)
    (funcall (if (<= ?a (following-char))
		 'upcase-region 'downcase-region)
	     (point) (1+ (point)))
    (forward-char 1)
    (setq num (1- num))))

(defun indent-or-fill-region ()
  "Fill region in text modes, indent region in programming language modes."
  (interactive)
  (if (string= paragraph-start "^$\\|^")
      (indent-region (point) (mark) nil)
    (fill-region (point) (mark))))

(defun mark-section-wisely ()
  "Mark the section in a manner consistent with the major-mode.
Uses mark-defun for emacs-lisp, lisp,
mark-c-function for C,
and mark-paragraph for other modes."
  (interactive)
  (cond  ((eq major-mode 'emacs-lisp-mode)
	  (mark-defun))
	 ((eq major-mode 'lisp-mode)
	  (mark-defun))
	 ((eq major-mode 'c-mode)
	  (mark-c-function))
	 (t (mark-paragraph))))

;;; Key Bindings
(defun edt-emulation-on ()
  "Begin emulating DEC's EDT editor.
Certain keys are rebound; including nearly all keypad keys.
Use \\[edt-emulation-off] to undo all rebindings except the keypad keys.
Note that this function does not work if called directly from the .emacs file.
Instead, the .emacs file should do (setq term-setup-hook 'edt-emulation-on)
Then this function will be called at the time when it will work."
  (interactive)
  (advance-direction)
  (edt-bind-gold-keypad)	;Must do this *after* $TERM.el is loaded
  (setq edt-mode-old-c-\\ (lookup-key global-map "\C-\\"))
  (global-set-key "\C-\\" 'quoted-insert)
  (setq edt-mode-old-delete (lookup-key global-map "\177"))
  (global-set-key "\177" 'delete-previous-char)      ;"Delete"
  (setq edt-mode-old-lisp-delete (lookup-key emacs-lisp-mode-map "\177"))
  (define-key emacs-lisp-mode-map "\177" 'delete-previous-char) ;"Delete"
  (define-key lisp-mode-map "\177" 'delete-previous-char) ;"Delete"
  (setq edt-mode-old-linefeed (lookup-key global-map "\C-j"))
  (global-set-key "\C-j" 'delete-previous-word)           ;"LineFeed"
  (define-key esc-map "?" 'apropos))                      ;"<ESC>?"

(defun edt-emulation-off ()
  "Return from EDT emulation to normal Emacs key bindings.
The keys redefined by \\[edt-emulation-on] are given their old definitions."
  (interactive)
  (setq edt-direction-string nil)
  (global-set-key "\C-\\" edt-mode-old-c-\\)
  (global-set-key "\177" edt-mode-old-delete)		;"Delete"
  (define-key emacs-lisp-mode-map "\177" edt-mode-old-lisp-delete) ;"Delete"
  (define-key lisp-mode-map "\177" edt-mode-old-lisp-delete) ;"Delete"
  (global-set-key "\C-j" edt-mode-old-linefeed))           ;"LineFeed"

(define-key function-keymap "u" 'previous-line)		;Up arrow
(define-key function-keymap "d" 'next-line)		;down arrow
(define-key function-keymap "l" 'backward-char)		;right arrow
(define-key function-keymap "r" 'forward-char)		;left arrow
(define-key function-keymap "h" 'beginning-of-window)	;home
(define-key function-keymap "\C-b" 'describe-key)	;PF2
(define-key function-keymap "\C-d" 'delete-current-line);PF4
(define-key function-keymap "9" 'append-to-buffer)	;9 keypad key, etc.
(define-key function-keymap "-" 'delete-current-word)
(define-key function-keymap "4" 'advance-direction)
(define-key function-keymap "5" 'backup-direction)
(define-key function-keymap "6" 'kill-region)
(define-key function-keymap "," 'delete-current-char)
(define-key function-keymap "." 'set-mark-command)
(define-key function-keymap "e" 'other-window)		;enter key
(define-key function-keymap "\C-a" 'GOLD-prefix)	;PF1 ("gold")

(setq GOLD-map (make-keymap))
(fset 'GOLD-prefix GOLD-map)

(defvar GOLD-map nil
   "GOLD-map maps the function keys on the VT100 keyboard preceeded
by the PF1 key.  GOLD is the ASCII the 7-bit escape sequence <ESC>OP.")

(defun define-keypad-key (keymap function-keymap-slot definition)
  (let ((function-key-sequence (function-key-sequence function-keymap-slot)))
    (if function-key-sequence
	(define-key keymap function-key-sequence definition))))

;;Bind GOLD/Keyboard keys

(define-key GOLD-map "\C-g"  'keyboard-quit)            ; just for safety
(define-key GOLD-map "\177" 'delete-window)             ;"Delete"
(define-key GOLD-map "\C-h" 'delete-other-windows)      ;"BackSpace"
(define-key GOLD-map "\C-m" 'newline-and-indent)        ;"Return"
(define-key GOLD-map " " 'undo)				;"Spacebar"
(define-key GOLD-map "%" 'goto-percent)                 ; "%"
(define-key GOLD-map "=" 'goto-line)                    ; "="
(define-key GOLD-map "`" 'what-line)                    ; "`"
(define-key GOLD-map "\C-\\" 'split-window-vertically)  ; "Control-\"

; GOLD letter combinations:
(define-key GOLD-map "b" 'buffer-menu)                  ; "b"
(define-key GOLD-map "B" 'buffer-menu)                  ; "B"
(define-key GOLD-map "d" 'delete-window)                ; "d"
(define-key GOLD-map "D" 'delete-window)                ; "D"
(define-key GOLD-map "e" 'compile)                      ; "e"
(define-key GOLD-map "E" 'compile)                      ; "E"
(define-key GOLD-map "i" 'insert-file)                  ; "i"
(define-key GOLD-map "I" 'insert-file)                  ; "I"
(define-key GOLD-map "l" 'goto-line)                    ; "l"
(define-key GOLD-map "L" 'goto-line)                    ; "L"
(define-key GOLD-map "m" 'save-some-buffers)		; "m"
(define-key GOLD-map "M" 'save-some-buffers)		; "m"
(define-key GOLD-map "n" 'next-error)                           ; "n"
(define-key GOLD-map "N" 'next-error)                           ; "N"
(define-key GOLD-map "o" 'switch-to-buffer-other-window)        ; "o"
(define-key GOLD-map "O" 'switch-to-buffer-other-window)        ; "O"
(define-key GOLD-map "r" 'revert-file)                          ; "r"
(define-key GOLD-map "r" 'revert-file)                          ; "R"
(define-key GOLD-map "s" 'save-buffer)                          ; "s"
(define-key GOLD-map "S" 'save-buffer)                          ; "S"
(define-key GOLD-map "v" 'find-file-other-window)               ; "v"
(define-key GOLD-map "V" 'find-file-other-window)               ; "V"
(define-key GOLD-map "w" 'write-file)                           ; "w"
(define-key GOLD-map "w" 'write-file)                           ; "W"
;(define-key GOLD-map "z" 'shrink-window)                 ; "z"
;(define-key GOLD-map "Z" 'shrink-window)                 ; "z"

;Bind GOLD/Keypad keys
(defun edt-bind-gold-keypad ()
  (define-keypad-key GOLD-map ?u 'line-to-top-of-window) ;"up-arrow"
  (define-keypad-key GOLD-map ?d 'line-to-bottom-of-window) ;"down-arrow"
  (define-keypad-key GOLD-map ?l 'backward-sentence) ;"left-arrow"
  (define-keypad-key GOLD-map ?r 'forward-sentence) ;"right-arrow"
  (define-keypad-key GOLD-map ?\C-a 'mark-section-wisely) ;Gold     "PF1"
  (define-keypad-key GOLD-map ?\C-b 'describe-function)	;Help     "PF2"
  (define-keypad-key GOLD-map ?\C-c 'occur) ;Find     "PF3"
  (define-keypad-key GOLD-map ?\C-d 'undelete-lines) ;Und Line "PF4"
  (define-keypad-key GOLD-map ?0 'open-line) ;Open L   "0"
  (define-keypad-key GOLD-map ?1 'case-flip-character) ;Chgcase  "1"
  (define-keypad-key GOLD-map ?2 'delete-to-eol) ;Del EOL  "2"
  (define-keypad-key GOLD-map ?3 'copy-region-as-kill) ;Copy     "3"
  (define-keypad-key GOLD-map ?4 'move-to-end) ;Bottom   "4"
  (define-keypad-key GOLD-map ?5 'move-to-beginning) ;Top      "5"
  (define-keypad-key GOLD-map ?6 'yank)	;Paste    "6"
  (define-keypad-key GOLD-map ?7 'execute-extended-command) ;Command  "7"
  (define-keypad-key GOLD-map ?8 'indent-or-fill-region) ;Fill     "8"
  (define-keypad-key GOLD-map ?9 'replace-regexp) ;Replace  "9"
  (define-keypad-key GOLD-map ?- 'undelete-words) ;UND word "-"
  (define-keypad-key GOLD-map ?, 'undelete-chars) ;UND Char ","
  (define-keypad-key GOLD-map ?. 'redraw-display) ;Reset Window "."
  (define-keypad-key GOLD-map ?e 'shell-command)) ;"ENTER"

;; Make direction of motion show in mode line
;; while EDT emulation is turned on.
;; Note that the keypad is always turned on when in Emacs.

(or (assq 'edt-direction-string minor-mode-alist)
    (setq minor-mode-alist (cons '(edt-direction-string edt-direction-string)
				 minor-mode-alist)))
