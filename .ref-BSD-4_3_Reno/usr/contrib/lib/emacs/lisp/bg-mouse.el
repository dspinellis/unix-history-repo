;; GNU Emacs code for BBN Bitgraph mouse.
;; Copyright (C) Free Software Foundation, Inc. Oct 1985.
;; Time stamp <89/03/21 14:27:08 gildea>

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


;;;  Original version by John Robinson (jr@bbn-unix.arpa, bbncca!jr), Oct 1985
;;;  Modularized and enhanced by gildea@bbn.com Nov 1987

(provide 'bg-mouse)

;;;  User customization option:

(defvar bg-mouse-fast-select-window nil
  "*Non-nil for mouse hits to select new window, then execute; else just select.")

;;; These numbers are summed to make the index into the mouse-map.
;;; The low three bits correspond to what the mouse actually sends.
(defconst bg-button-r 1)
(defconst bg-button-m 2)
(defconst bg-button-c 2)
(defconst bg-button-l 4)
(defconst bg-in-modeline 8)
(defconst bg-in-scrollbar 16)
(defconst bg-in-minibuf 24)

;;; semicolon screws up indenting, so use this instead
(defconst semicolon ?\;)

;;;  Defuns:

(defun bg-mouse-report (prefix-arg)
  "Read, parse, and execute a BBN BitGraph mouse click.

L-- move point             | These apply for mouse click in a window.
--R set mark               | If bg-mouse-fast-select-window is nil,
L-R kill region            | these commands on a nonselected window
-C- move point and yank    | just select that window.
LC- yank-pop		   |
-CR or LCR undo		   | \"Scroll bar\" is right-hand window column.

on modeline:		    on \"scroll bar\":	in minibuffer:
L-- scroll-up		    line to top		execute-extended-command
--R scroll-down		    line to bottom	eval-expression
-C- proportional goto-char  line to middle	suspend-emacs

To reinitialize the mouse if the terminal is reset, type ESC : RET"
  (interactive "P")
  (bg-get-tty-num semicolon)
  (let*
      ((screen-mouse-x (min (1- (screen-width))	;don't hit column 86!
			    (/ (bg-get-tty-num semicolon) 9)))
       (screen-mouse-y (- (1- (screen-height)) ;assume default font size.
			  (/ (bg-get-tty-num semicolon) 16))) 
       (bg-mouse-buttons (% (bg-get-tty-num ?c) 8))
       (bg-mouse-window (bg-window-from-x-y screen-mouse-x screen-mouse-y))
       (bg-cursor-window (selected-window))
       (edges (window-edges bg-mouse-window))
       (minibuf-p (= screen-mouse-y (1- (screen-height))))
       (in-modeline-p (and (not minibuf-p)
			   (= screen-mouse-y (1- (nth 3 edges)))))
       (in-scrollbar-p (and (not minibuf-p) (not in-modeline-p)
			    (>= screen-mouse-x (1- (nth 2 edges)))))
       (same-window-p (eq bg-mouse-window bg-cursor-window))
       (in-minibuf-p (and minibuf-p
			  (not bg-mouse-window))) ;minibuf must be inactive
       (bg-mode-bits (+ (if in-minibuf-p bg-in-minibuf 0)
			(if in-modeline-p bg-in-modeline 0)
			(if in-scrollbar-p bg-in-scrollbar 0)))
       (bg-command
	 (lookup-key mouse-map
		     (char-to-string (+ bg-mode-bits bg-mouse-buttons))))
       (bg-mouse-x (- screen-mouse-x (nth 0 edges)))
       (bg-mouse-y (- screen-mouse-y (nth 1 edges))))
    (cond ((or in-modeline-p in-scrollbar-p)
	   (select-window bg-mouse-window)
	   (bg-command-execute bg-command)
	   (select-window bg-cursor-window))
	  ((or same-window-p in-minibuf-p)
	   (bg-command-execute bg-command))
	  (t				;in another window
	    (select-window bg-mouse-window)
	    (if bg-mouse-fast-select-window
		(bg-command-execute bg-command)))
	  )))


;;; Library of commands:

(defun bg-set-point ()
  "Move point to location of BitGraph mouse."
  (interactive)
  (bg-move-point-to-x-y bg-mouse-x bg-mouse-y)
  (setq this-command 'next-line)	;make subsequent line moves work
  (setq temporary-goal-column bg-mouse-x))

(defun bg-set-mark ()
  "Set mark at location of BitGraph mouse."
  (interactive)
  (push-mark)
  (bg-move-point-to-x-y bg-mouse-x bg-mouse-y)
  (exchange-point-and-mark))

(defun bg-yank ()
  "Move point to location of BitGraph mouse and yank."
  (interactive "*")
  (bg-move-point-to-x-y bg-mouse-x bg-mouse-y)
  (setq this-command 'yank)
  (yank))

(defun yank-pop-1 ()
  (interactive "*")
  (yank-pop 1))

(defun bg-yank-or-pop ()
  "Move point to location of BitGraph mouse and yank or yank-pop.
Do a yank unless last command was a yank, in which case do a yank-pop."
  (interactive "*")
  (if (eql last-command 'yank)
      (yank-pop 1)
    (bg-yank)))

;;; In 18.51, Emacs Lisp doesn't provide most-positive-fixnum
(defconst bg-most-positive-fixnum 8388607)

(defun bg-move-by-percentage ()
  "Go to location in buffer that is the same percentage of the
way through the buffer as the BitGraph mouse's X position in the window."
  (interactive)
  ;; check carefully for overflow in intermediate calculations
  (goto-char
   (cond ((zerop bg-mouse-x)
	  0)
	 ((< (buffer-size) (/ bg-most-positive-fixnum bg-mouse-x))
	  ;; no danger of overflow: compute it exactly
	  (/ (* bg-mouse-x (buffer-size))
	     (1- (window-width))))
	 (t
	  ;; overflow possible: approximate
	  (* (/ (buffer-size) (1- (window-width)))
	     bg-mouse-x))))
  (beginning-of-line)
  (what-cursor-position))

(defun bg-mouse-line-to-top ()
  "Scroll the line pointed to by the BitGraph mouse to the top of the window."
  (interactive)
  (scroll-up bg-mouse-y))

(defun bg-mouse-line-to-center ()
  "Scroll the line pointed to by the BitGraph mouse to the center 
of the window"
  (interactive)
  (scroll-up (/ (+ 2 bg-mouse-y bg-mouse-y (- (window-height))) 2)))

(defun bg-mouse-line-to-bottom ()
  "Scroll the line pointed to by the mouse to the bottom of the window."
  (interactive)
  (scroll-up (+ bg-mouse-y (- 2 (window-height)))))

(defun bg-kill-region ()
  (interactive "*")
  (kill-region (region-beginning) (region-end)))

(defun bg-insert-moused-sexp ()
  "Insert a copy of the word (actually sexp) that the mouse is pointing at.
Sexp is inserted into the buffer at point (where the text cursor is).
By gildea 7 Feb 89"
  (interactive)
  (let ((moused-text
	  (save-excursion
	    (bg-move-point-to-x-y bg-mouse-x bg-mouse-y)
	    (forward-sexp 1)
	    (buffer-substring (save-excursion (backward-sexp 1) (point))
			      (point)))))
    (select-window bg-cursor-window)
    (delete-horizontal-space)
    (cond
     ((bolp)
      (indent-according-to-mode))
     ;; In Lisp assume double-quote is closing; in Text assume opening.
     ;; Why?  Because it does the right thing most often.
     ((save-excursion (forward-char -1)
		      (and (not (looking-at "\\s\""))
			   (looking-at "[`'\"\\]\\|\\s(")))
      nil)
     (t
      (insert-string " ")))
    (insert-string moused-text)
    (or (eolp)
	(looking-at "\\s.\\|\\s)")
	(and (looking-at "'") (looking-at "\\sw")) ;hack for text mode
	(save-excursion (insert-string " ")))))

;;; Utility functions:

(defun bg-get-tty-num (term-char)
  "Read from terminal until TERM-CHAR is read, and return intervening number.
If non-numeric not matching TERM-CHAR, reprogram the mouse and signal an error."
  (let
      ((num 0)
       (char (- (read-char) 48)))
    (while (and (>= char 0)
		(<= char 9))
      (setq num (+ (* num 10) char))
      (setq char (- (read-char) 48)))
    (or (eq term-char (+ char 48))
	(progn
	  (bg-program-mouse)
	  (error
	    "Invalid data format in bg-mouse command: mouse reinitialized.")))
    num))

;;; Note that this fails in the minibuf because move-to-column doesn't
;;; allow for the width of the prompt.
(defun bg-move-point-to-x-y (x y)
  "Position cursor in window coordinates.
X and Y are 0-based character positions in the window."
  (move-to-window-line y)
  ;; if not on a wrapped line, zero-column will be 0
  (let ((zero-column (current-column))
	(scroll-offset (window-hscroll)))
    ;; scrolling takes up column 0 to display the $
    (if (> scroll-offset 0)
	(setq scroll-offset (1- scroll-offset)))
    (move-to-column (+ zero-column scroll-offset x))
    ))

;;; Returns the window that screen position (x, y) is in or nil if none,
;;; meaning we are in the echo area with a non-active minibuffer.
;;; If coordinates-in-window-p were not in an X-windows-specific file
;;; we could use that.  In Emacs 19 can even use locate-window-from-coordinates
(defun bg-window-from-x-y (x y)
  "Find window corresponding to screen coordinates.
X and Y are 0-based character positions on the screen."
  (let ((edges (window-edges))
	(window nil))
    (while (and (not (eq window (selected-window)))
		(or (<  y (nth 1 edges))
		    (>= y (nth 3 edges))
		    (<  x (nth 0 edges))
		    (>= x (nth 2 edges))))
      (setq window (next-window window))
      (setq edges (window-edges window)))
    (cond ((eq window (selected-window))
	   nil)				;we've looped: not found
	  ((not window)
	   (selected-window))		;just starting: current window
	  (t
	    window))
    ))

(defun bg-command-execute (bg-command)
  (if (commandp bg-command)
      (command-execute bg-command)
    (ding)))

(defun bg-program-mouse ()
  (send-string-to-terminal "\e:0;7;;;360;512;9;16;9;16c"))

;;; Note that the doc string for mouse-map (as defined in subr.el)
;;; says it is for the X-window mouse.  This is wrong; that keymap
;;; should be used for your mouse no matter what terminal you have.

(or (keymapp mouse-map)
    (setq mouse-map (make-keymap)))

(defun bind-bg-mouse-click (click-code function)
  "Bind bg-mouse CLICK-CODE to run FUNCTION."
  (define-key mouse-map (char-to-string click-code) function))

(bind-bg-mouse-click bg-button-l 'bg-set-point) 
(bind-bg-mouse-click bg-button-m 'bg-yank)
(bind-bg-mouse-click bg-button-r 'bg-set-mark)
(bind-bg-mouse-click (+ bg-button-l bg-button-m) 'yank-pop-1)
(bind-bg-mouse-click (+ bg-button-l bg-button-r) 'bg-kill-region)
(bind-bg-mouse-click (+ bg-button-m bg-button-r) 'undo)
(bind-bg-mouse-click (+ bg-button-l bg-button-m bg-button-r) 'undo)
(bind-bg-mouse-click (+ bg-in-modeline bg-button-l) 'scroll-up)
(bind-bg-mouse-click (+ bg-in-modeline bg-button-m) 'bg-move-by-percentage)
(bind-bg-mouse-click (+ bg-in-modeline bg-button-r) 'scroll-down)
(bind-bg-mouse-click (+ bg-in-scrollbar bg-button-l) 'bg-mouse-line-to-top)
(bind-bg-mouse-click (+ bg-in-scrollbar bg-button-m) 'bg-mouse-line-to-center)
(bind-bg-mouse-click (+ bg-in-scrollbar bg-button-r) 'bg-mouse-line-to-bottom)
(bind-bg-mouse-click (+ bg-in-minibuf bg-button-l) 'execute-extended-command)
(bind-bg-mouse-click (+ bg-in-minibuf bg-button-m) 'suspend-emacs)
(bind-bg-mouse-click (+ bg-in-minibuf bg-button-r) 'eval-expression)

