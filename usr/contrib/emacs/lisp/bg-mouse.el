;; GNU Emacs code for BBN Bitgraph mouse.
;; Copyright (C) Free Software Foundation Oct 1985.

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

;;;  User customization option:

(defvar bg-mouse-fast-select-window nil
  "*Non-nil for mouse hits to select new window, then execute; else just select.")

;;;  Defuns:

(defun bg-mouse-report ()
  "Read and parse BBN BitGraph mouse report, and do what it asks.

L-- move point          *  |---- These apply for mouse click in a window.
--R set mark            *  | If bg-mouse-fast-select-window is nil,
L-R kill region            | a starred command on a nonselected window
-C- move point and yank *  | just selects that window.
LC- yank-pop
-CR or LCR undo		   | \"Scroll bar\" is right-hand window column.

on modeline		    on \"scroll bar\"	in minibuffer
L-- scroll-up		    line to top		execute-extended-command
--R scroll-down		    line to bottom	eval-expression
-C- proportional goto-char  line to middle	suspend-emacs

To reenable the mouse if terminal is reset, type ESC : RET ."
  (interactive)
  (bg-get-tty-num ?\;)
  (let*
      ((x (min (1- (screen-width))
	       (/ (bg-get-tty-num ?\;) 9)))	; Don't hit column 86!
       (y (- (1- (screen-height))
	     (/ (bg-get-tty-num ?\;) 16)))	; Assume default font size.
       (buttons (% (bg-get-tty-num ?c) 8))
       (window (bg-pos-to-window x y))
       (edges (window-edges window))
       (old-window (selected-window))
       (in-minibuf-p (eq y (1- (screen-height))))
       (same-window-p (and (not in-minibuf-p) (eq window old-window)))
       (in-modeline-p (eq y (1- (nth 3 edges))))
       (in-scrollbar-p (>= x (1- (nth 2 edges)))))
    (setq x (- x (nth 0 edges)))
    (setq y (- y (nth 1 edges)))
    (cond (in-modeline-p
	   (select-window window)
	   (cond ((= buttons 4)
		  (scroll-up))
		 ((= buttons 1)
		  (scroll-down))
		 ((= buttons 2)
		  (goto-char (/ (* x
				   (- (point-max) (point-min)))
				(1- (window-width))))
		  (beginning-of-line)
		  (what-cursor-position)))
	   (select-window old-window))
	  (in-scrollbar-p
	   (select-window window)
	   (scroll-up
	    (cond ((= buttons 4)
		   y)
		 ((= buttons 1)
		   (+ y (- 2 (window-height))))
		 ((= buttons 2)
		   (/ (+ 2 y y (- (window-height))) 2))
		 (t
		  0)))
	   (select-window old-window))
	  (same-window-p
	   (cond ((= buttons 4)
		  (bg-move-point-to-x-y x y))
		 ((= buttons 1)
		  (push-mark)
		  (bg-move-point-to-x-y x y)
		  (exchange-point-and-mark))
		 ((= buttons 5)
		  (kill-region (mark) (point)))
		 ((= buttons 2)
		  (bg-move-point-to-x-y x y)
		  (setq this-command 'yank)
		  (yank))
		 ((= buttons 6)
		  (yank-pop 1))
		 ((or (= buttons 3) (= buttons 7))
		  (undo))
		 )
	   )
	  (in-minibuf-p
	   (cond ((= buttons 1)
		  (call-interactively 'eval-expression))
		 ((= buttons 4)
		  (call-interactively 'execute-extended-command))
		 ((= buttons 2)
		  (suspend-emacs))
		 ))
	  (t				;in another window
	   (select-window window)
	   (cond ((not bg-mouse-fast-select-window))
		 ((= buttons 4)
		  (bg-move-point-to-x-y x y))
		 ((= buttons 1)
		  (push-mark)
		  (bg-move-point-to-x-y x y)
		  (exchange-point-and-mark))
		 ((= buttons 2)
		  (bg-move-point-to-x-y x y)
		  (setq this-command 'yank)
		  (yank))
		 ))
	  )))

(defun bg-get-tty-num (term-char)
  "Read from terminal until TERM-CHAR is read, and return intervening number.
Upon non-numeric not matching TERM-CHAR, reprogram the mouse and signal an error."
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
	  (error "Invalid data format in mouse command")))
    num))

(defun bg-move-point-to-x-y (x y)
  "Position cursor in window coordinates.
X and Y are 0-based character positions in the window."
  (move-to-window-line y)
  (move-to-column x)
  )

(defun bg-pos-to-window (x y)
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
      (setq edges (window-edges window))
      )
    (or window (selected-window))
    )
  )

(defun bg-program-mouse ()
  (send-string-to-terminal "\e:0;7;;;360;512;9;16;9;16c"))

