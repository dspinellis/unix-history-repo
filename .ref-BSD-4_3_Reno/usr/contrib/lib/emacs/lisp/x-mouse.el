;; Mouse support for X window system.
;; Copyright (C) 1985, 1987 Free Software Foundation, Inc.

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


(provide 'x-mouse)

(defconst x-button-right (char-to-string 0))
(defconst x-button-middle (char-to-string 1))
(defconst x-button-left (char-to-string 2))

(defconst x-button-right-up (char-to-string 4))
(defconst x-button-middle-up (char-to-string 5))
(defconst x-button-left-up (char-to-string 6))

(defconst x-button-s-right (char-to-string 16))
(defconst x-button-s-middle (char-to-string 17))
(defconst x-button-s-left (char-to-string 18))

(defconst x-button-s-right-up (char-to-string 20))
(defconst x-button-s-middle-up (char-to-string 21))
(defconst x-button-s-left-up (char-to-string 22))

(defconst x-button-m-right (char-to-string 32))
(defconst x-button-m-middle (char-to-string 33))
(defconst x-button-m-left (char-to-string 34))

(defconst x-button-m-right-up (char-to-string 36))
(defconst x-button-m-middle-up (char-to-string 37))
(defconst x-button-m-left-up (char-to-string 38))

(defconst x-button-c-right (char-to-string 64))
(defconst x-button-c-middle (char-to-string 65))
(defconst x-button-c-left (char-to-string 66))

(defconst x-button-c-right-up (char-to-string 68))
(defconst x-button-c-middle-up (char-to-string 69))
(defconst x-button-c-left-up (char-to-string 70))

(defconst x-button-m-s-right (char-to-string 48))
(defconst x-button-m-s-middle (char-to-string 49))
(defconst x-button-m-s-left (char-to-string 50))

(defconst x-button-m-s-right-up (char-to-string 52))
(defconst x-button-m-s-middle-up (char-to-string 53))
(defconst x-button-m-s-left-up (char-to-string 54))

(defconst x-button-c-s-right (char-to-string 80))
(defconst x-button-c-s-middle (char-to-string 81))
(defconst x-button-c-s-left (char-to-string 82))

(defconst x-button-c-s-right-up (char-to-string 84))
(defconst x-button-c-s-middle-up (char-to-string 85))
(defconst x-button-c-s-left-up (char-to-string 86))

(defconst x-button-c-m-right (char-to-string 96))
(defconst x-button-c-m-middle (char-to-string 97))
(defconst x-button-c-m-left (char-to-string 98))

(defconst x-button-c-m-right-up (char-to-string 100))
(defconst x-button-c-m-middle-up (char-to-string 101))
(defconst x-button-c-m-left-up (char-to-string 102))

(defconst x-button-c-m-s-right (char-to-string 112))
(defconst x-button-c-m-s-middle (char-to-string 113))
(defconst x-button-c-m-s-left (char-to-string 114))

(defconst x-button-c-m-s-right-up (char-to-string 116))
(defconst x-button-c-m-s-middle-up (char-to-string 117))
(defconst x-button-c-m-s-left-up (char-to-string 118))

(defvar x-process-mouse-hook nil
  "Hook to run after each mouse event is processed.  Should take two
arguments; the first being a list (XPOS YPOS) corresponding to character
offset from top left of screen and the second being a specifier for the
buttons/keys.

This will normally be set on a per-buffer basis.")

(defun x-flush-mouse-queue () 
  "Process all queued mouse events."
  ;; A mouse event causes a special character sequence to be given
  ;; as keyboard input.  That runs this function, which process all
  ;; queued mouse events and returns.
  (interactive)
  (while (> (x-mouse-events) 0)
    (x-proc-mouse-event)
    (and (boundp 'x-process-mouse-hook)
	 (symbol-value 'x-process-mouse-hook)
	 (funcall x-process-mouse-hook x-mouse-pos x-mouse-item))))

(define-key global-map "\C-c\C-m" 'x-flush-mouse-queue)
(define-key global-map "\C-x\C-@" 'x-flush-mouse-queue)

(defun x-mouse-select (arg)
  "Select Emacs window the mouse is on."
  (let ((start-w (selected-window))
	(done nil)
	(w (selected-window))
	(rel-coordinate nil))
    (while (and (not done)
		(null (setq rel-coordinate
			    (coordinates-in-window-p arg w))))
      (setq w (next-window w))
      (if (eq w start-w)
	  (setq done t)))
    (select-window w)
    rel-coordinate))

(defun x-mouse-keep-one-window (arg)
  "Select Emacs window mouse is on, then kill all other Emacs windows."
  (if (x-mouse-select arg)
      (delete-other-windows)))

(defun x-mouse-select-and-split (arg)
  "Select Emacs window mouse is on, then split it vertically in half."
  (if (x-mouse-select arg)
      (split-window-vertically nil)))

(defun x-mouse-set-point (arg)
  "Select Emacs window mouse is on, and move point to mouse position."
  (let* ((relative-coordinate (x-mouse-select arg))
	 (rel-x (car relative-coordinate))
	 (rel-y (car (cdr relative-coordinate))))
    (if relative-coordinate
	(progn
	  (move-to-window-line rel-y)
	  (move-to-column (+ rel-x (current-column)))))))

(defun x-mouse-set-mark (arg)
  "Select Emacs window mouse is on, and set mark at mouse position.
Display cursor at that position for a second."
  (if (x-mouse-select arg)
      (let ((point-save (point)))
	(unwind-protect
	    (progn (x-mouse-set-point arg)
		   (push-mark nil t)
		   (sit-for 1))
	  (goto-char point-save)))))

(defun x-cut-text (arg &optional kill)
  "Copy text between point and mouse position into window system cut buffer.
Save in Emacs kill ring also."
  (if (coordinates-in-window-p arg (selected-window))
      (save-excursion
	(let ((opoint (point))
	      beg end)
	  (x-mouse-set-point arg)
	  (setq beg (min opoint (point))
		end (max opoint (point)))
	  (x-store-cut-buffer (buffer-substring beg end))
	  (copy-region-as-kill beg end)
	  (if kill (delete-region beg end))))
    (message "Mouse not in selected window")))

(defun x-paste-text (arg)
  "Move point to mouse position and insert window system cut buffer contents."
  (x-mouse-set-point arg)
  (insert (x-get-cut-buffer)))

(defun x-cut-and-wipe-text (arg)
  "Kill text between point and mouse; also copy to window system cut buffer."
  (x-cut-text arg t))

(defun x-mouse-ignore (arg)
  "Don't do anything.")

(defun x-buffer-menu (arg)
  "Pop up a menu of buffers for selection with the mouse."
  (let ((menu
	 (list "Buffer Menu"
	       (cons "Select Buffer"
		     (let ((tail (buffer-list))
			   head)
		       (while tail
			 (let ((elt (car tail)))
			   (if (not (string-match "^ "
						  (buffer-name elt)))
			       (setq head (cons
					   (cons
					    (format
					     "%14s   %s"
					     (buffer-name elt)
					     (or (buffer-file-name elt) ""))
					    elt)
					   head))))
			 (setq tail (cdr tail)))
		       (reverse head))))))
    (switch-to-buffer (or (x-popup-menu arg menu) (current-buffer)))))

(defun x-help (arg)
  "Enter a menu-based help system."
  (let ((selection
	 (x-popup-menu
	  arg
	  '("Help" ("Is there a command that..."
		    ("Command apropos" . command-apropos)
		    ("Apropos" . apropos))
		   ("Key Commands <==> Functions"
		    ("List all keystroke commands" . describe-bindings)
		    ("Describe key briefly" . describe-key-briefly)
		    ("Describe key verbose" . describe-key)
		    ("Describe Lisp function" . describe-function)
		    ("Where is this command" . where-is))
		   ("Manual and tutorial"
		    ("Info system" . info)
		    ("Invoke Emacs tutorial" . help-with-tutorial))
		   ("Odds and ends"
		    ("Last 100 Keystrokes" . view-lossage)
		    ("Describe syntax table" . describe-syntax))
		   ("Modes"
		    ("Describe current major mode" . describe-mode)
		    ("List all keystroke commands" . describe-bindings))
		   ("Administrivia"
		    ("View Emacs news" . view-emacs-news)
		    ("View the GNU Emacs license" . describe-copying)
		    ("Describe distribution" . describe-distribution)
		    ("Describe (non)warranty" . describe-no-warranty))))))
    (and selection (call-interactively selection))))

; Prevent beeps on button-up.  If the button isn't bound to anything, it
; will beep on button-down.
(define-key mouse-map x-button-right-up 'x-mouse-ignore)
(define-key mouse-map x-button-middle-up 'x-mouse-ignore)
(define-key mouse-map x-button-left-up 'x-mouse-ignore)
(define-key mouse-map x-button-s-right-up 'x-mouse-ignore)
(define-key mouse-map x-button-s-middle-up 'x-mouse-ignore)
(define-key mouse-map x-button-s-left-up 'x-mouse-ignore)
(define-key mouse-map x-button-m-right-up 'x-mouse-ignore)
(define-key mouse-map x-button-m-middle-up 'x-mouse-ignore)
(define-key mouse-map x-button-m-left-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-right-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-middle-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-left-up 'x-mouse-ignore)
(define-key mouse-map x-button-m-s-right-up 'x-mouse-ignore)
(define-key mouse-map x-button-m-s-middle-up 'x-mouse-ignore)
(define-key mouse-map x-button-m-s-left-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-s-right-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-s-middle-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-s-left-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-m-right-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-m-middle-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-m-left-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-m-s-right-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-m-s-middle-up 'x-mouse-ignore)
(define-key mouse-map x-button-c-m-s-left-up 'x-mouse-ignore)

(define-key mouse-map x-button-c-s-left 'x-buffer-menu)
(define-key mouse-map x-button-c-s-middle 'x-help)
(define-key mouse-map x-button-c-s-right 'x-mouse-keep-one-window)
(define-key mouse-map x-button-s-middle 'x-cut-text)
(define-key mouse-map x-button-s-right 'x-paste-text)
(define-key mouse-map x-button-c-middle 'x-cut-and-wipe-text)
(define-key mouse-map x-button-c-right 'x-mouse-select-and-split)

(if (= window-system-version 10)
    (progn
      (define-key mouse-map x-button-right 'x-mouse-select)
      (define-key mouse-map x-button-left 'x-mouse-set-mark)
      (define-key mouse-map x-button-middle 'x-mouse-set-point))
  (define-key mouse-map x-button-right 'x-cut-text)
  (define-key mouse-map x-button-left 'x-mouse-set-point)
  (define-key mouse-map x-button-middle 'x-paste-text))
