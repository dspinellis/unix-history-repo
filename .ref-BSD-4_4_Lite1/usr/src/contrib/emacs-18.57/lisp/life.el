;; Conway's `Life' for GNU Emacs
;; Copyright (C) 1988 Free Software Foundation, Inc.
;; Contributed by Kyle Jones, talos!kjones@uunet.uu.net

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

(provide 'life)

(defconst life-patterns
  [("@@@" " @@" "@@@")
   ("@@@ @@@" "@@  @@ " "@@@ @@@")
   ("@@@ @@@" "@@   @@" "@@@ @@@")
   ("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
   ("@@@@@@@@@@")
   ("   @@@@@@@@@@       "
    "     @@@@@@@@@@     "
    "       @@@@@@@@@@   "
    "@@@@@@@@@@          "
    "@@@@@@@@@@          ")
   ("@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@")
   ("@               @" "@               @"  "@               @"
    "@               @" "@               @"  "@               @"
    "@               @" "@               @"  "@               @"
    "@               @" "@               @"  "@               @"
    "@               @" "@               @"  "@               @")
   ("@@               " " @@              " "  @@             "
    "   @@            " "    @@           " "     @@          "
    "      @@         " "       @@        " "        @@       "
    "         @@      " "          @@     " "           @@    "
    "            @@   " "             @@  " "              @@ "
    "               @@")
   ("@@@@@@@@@" "@   @   @" "@ @@@@@ @" "@ @   @ @" "@@@   @@@" 
    "@ @   @ @" "@ @@@@@ @" "@   @   @" "@@@@@@@@@")]
  "Vector of rectangles containing some Life startup patterns.")

;; Macros are used macros for manifest constants instead of variables
;; because the compiler will convert them to constants, which should
;; eval faster than symbols.
;;
;; The (require) wrapping forces the compiler to eval these macros at
;; compile time.  This would not be necessary if we did not use macros
;; inside of macros, which the compiler doesn't seem to check for.
;;
;; Don't change any of the life-* macro constants unless you thoroughly
;; understand the `life-grim-reaper' function.
(require
 (progn
   (defmacro life-life-char () ?@)
   (defmacro life-death-char () (1+ (life-life-char)))
   (defmacro life-birth-char () 3)
   (defmacro life-void-char () ?\ )

   (defmacro life-life-string () (char-to-string (life-life-char)))
   (defmacro life-death-string () (char-to-string (life-death-char)))
   (defmacro life-birth-string () (char-to-string (life-birth-char)))
   (defmacro life-void-string () (char-to-string (life-void-char)))
   (defmacro life-not-void-regexp () (concat "[^" (life-void-string) "\n]"))

   ;; try to optimize the (goto-char (point-min)) & (goto-char (point-max))
   ;; idioms.  This depends on goto-char's not griping if we underrshoot
   ;; or overshoot beginning or end of buffer.
   (defmacro goto-beginning-of-buffer () '(goto-char 1))
   (defmacro maxint () (lsh (lsh (lognot 0) 1) -1))
   (defmacro goto-end-of-buffer () '(goto-char (maxint)))

   (defmacro increment (variable) (list 'setq variable (list '1+ variable)))
   'life))

;; list of numbers that tell how many characters to move to get to
;; each of a cell's eight neighbors.
(defconst life-neighbor-deltas nil)

;; window display always starts here.  Easier to deal with than
;; (scroll-up) and (scroll-down) when trying to center the display.
(defconst life-window-start nil)

;; For mode line
(defconst life-current-generation nil)
;; Sadly, mode-line-format won't display numbers.
(defconst life-generation-string nil)

(defun abs (n) (if (< n 0) (- n) n))

(defun life (&optional sleeptime)
  "Run Conway's Life simulation.
The starting pattern is randomly selected.  Prefix arg (optional first arg
non-nil from a program) is the number of seconds to sleep between
generations (this defaults to 1)."
  (interactive "p")
  (or sleeptime (setq sleeptime 1))
  (life-setup)
  (life-display-generation sleeptime)
  (while t
    (let ((inhibit-quit t))
      (life-grim-reaper)
      (life-expand-plane-if-needed)
      (life-increment-generation)
      (life-display-generation sleeptime))))

(fset 'life-mode 'life)
(put 'life-mode 'mode-class 'special)

(random t)

(defun life-setup ()
  (let (n)
    (switch-to-buffer (get-buffer-create "*Life*") t)
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search nil
	  mode-name "Life"
	  major-mode 'life-mode
	  truncate-lines t
	  life-current-generation 0
	  life-generation-string "0"
	  mode-line-buffer-identification '("Life: generation "
					    life-generation-string)
	  fill-column (1- (window-width))
	  life-window-start 1)
    (buffer-flush-undo (current-buffer))
    ;; stuff in the random pattern
    (life-insert-random-pattern)
    ;; make sure (life-life-char) is used throughout
    (goto-beginning-of-buffer)
    (while (re-search-forward (life-not-void-regexp) nil t)
      (replace-match (life-life-string) t t))
    ;; center the pattern horizontally
    (goto-beginning-of-buffer)
    (setq n (/ (- fill-column (save-excursion (end-of-line) (point))) 2))
    (while (not (eobp))
      (indent-to n)
      (forward-line))
    ;; center the pattern vertically
    (setq n (/ (- (1- (window-height))
		  (count-lines (point-min) (point-max)))
	       2))
    (goto-beginning-of-buffer)
    (newline n)
    (goto-end-of-buffer)
    (newline n)
    ;; pad lines out to fill-column
    (goto-beginning-of-buffer)
    (while (not (eobp))
      (end-of-line)
      (indent-to fill-column)
      (move-to-column fill-column)
      (delete-region (point) (progn (end-of-line) (point)))
      (forward-line))
    ;; expand tabs to spaces
    (untabify (point-min) (point-max))
    ;; before starting be sure the automaton has room to grow
    (life-expand-plane-if-needed)
    ;; compute initial neighbor deltas
    (life-compute-neighbor-deltas)))

(defun life-compute-neighbor-deltas ()
  (setq life-neighbor-deltas
	(list -1 (- fill-column)
	      (- (1+ fill-column)) (- (+ 2 fill-column))
	      1 fill-column (1+ fill-column)
	      (+ 2 fill-column))))

(defun life-insert-random-pattern ()
  (insert-rectangle
   (elt life-patterns (% (abs (random)) (length life-patterns))))
  (insert ?\n))

(defun life-increment-generation ()
  (increment life-current-generation)
  (setq life-generation-string (int-to-string life-current-generation)))

(defun life-grim-reaper ()
  ;; Clear the match information.  Later we check to see if it
  ;; is still clear, if so then all the cells have died.
  (store-match-data nil)
  (goto-beginning-of-buffer)
  ;; For speed declare all local variable outside the loop.
  (let (point char pivot living-neighbors list)
    (while (search-forward (life-life-string) nil t)
      (setq list life-neighbor-deltas
	    living-neighbors 0
	    pivot (1- (point)))
      (while list
	(setq point (+ pivot (car list))
	      char (char-after point))
	(cond ((eq char (life-void-char))
	       (subst-char-in-region point (1+ point)
				     (life-void-char) 1 t))
	      ((< char 3)
	       (subst-char-in-region point (1+ point) char (1+ char) t))
	      ((< char 9)
	       (subst-char-in-region point (1+ point) char 9 t))
	      ((>= char (life-life-char))
	       (increment living-neighbors)))
	(setq list (cdr list)))
      (if (memq living-neighbors '(2 3))
	  ()
	(subst-char-in-region pivot (1+ pivot)
			    (life-life-char) (life-death-char) t))))
  (if (null (match-beginning 0))
      (life-extinct-quit))
  (subst-char-in-region 1 (point-max) 9 (life-void-char) t)
  (subst-char-in-region 1 (point-max) 1 (life-void-char) t)
  (subst-char-in-region 1 (point-max) 2 (life-void-char) t)
  (subst-char-in-region 1 (point-max) (life-birth-char) (life-life-char) t)
  (subst-char-in-region 1 (point-max) (life-death-char) (life-void-char) t))

(defun life-expand-plane-if-needed ()
  (catch 'done
    (goto-beginning-of-buffer)
    (while (not (eobp))
      ;; check for life at beginning or end of line.  If found at
      ;; either end, expand at both ends,
      (cond ((or (eq (following-char) (life-life-char))
		 (eq (progn (end-of-line) (preceding-char)) (life-life-char)))
	     (goto-beginning-of-buffer)
	     (while (not (eobp))
	       (insert (life-void-char))
	       (end-of-line)
	       (insert (life-void-char))
	       (forward-char))
	   (setq fill-column (+ 2 fill-column))
	   (scroll-left 1)
	   (life-compute-neighbor-deltas)
	   (throw 'done t)))
      (forward-line)))
  (goto-beginning-of-buffer)
  ;; check for life within the first two lines of the buffer.
  ;; If present insert two lifeless lines at the beginning..
  (cond ((search-forward (life-life-string)
			 (+ (point) fill-column fill-column 2) t)
	 (goto-beginning-of-buffer)
	 (insert-char (life-void-char) fill-column)
	 (insert ?\n)
	 (insert-char (life-void-char) fill-column)
	 (insert ?\n)
	 (setq life-window-start (+ life-window-start fill-column 1))))
  (goto-end-of-buffer)
  ;; check for life within the last two lines of the buffer.
  ;; If present insert two lifeless lines at the end.
  (cond ((search-backward (life-life-string)
			  (- (point) fill-column fill-column 2) t)
	 (goto-end-of-buffer)
	 (insert-char (life-void-char) fill-column)
	 (insert ?\n)
	 (insert-char (life-void-char) fill-column)
	 (insert ?\n)
	 (setq life-window-start (+ life-window-start fill-column 1)))))

(defun life-display-generation (sleeptime)
  (goto-char life-window-start)
  (recenter 0)
  (sit-for sleeptime))

(defun life-extinct-quit ()
  (life-display-generation 0)
  (signal 'life-extinct nil))

(put 'life-extinct 'error-conditions '(life-extinct quit))
(put 'life-extinct 'error-message "All life has perished")


