;; Compare text between windows for Emacs.
;; Copyright (C) 1986 Free Software Foundation, Inc.

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


(defun compare-windows ()
  "Compare text in current window with text in next window.
Compares the text starting at point in each window,
moving over text in each one as far as they match."
  (interactive)
  (let (p1 p2 maxp1 maxp2 b1 b2 w2
	   success size
	   (opoint (point)))
    (setq p1 (point) b1 (current-buffer))
    (setq w2 (next-window (selected-window)))
    (if (eq w2 (selected-window))
	(error "No other window."))
    (setq p2 (window-point w2)
	  b2 (window-buffer w2))
    (setq maxp1 (point-max))
    (save-excursion
     (set-buffer b2)
     (setq maxp2 (point-max)))

    ;; Try advancing comparing 1000 chars at a time.
    ;; When that fails, go 500 chars at a time, and so on.
    (setq size 1000)
    (while (> size 0)
      (setq success t)
      (while success
	(setq size (min size (- maxp1 p1) (- maxp2 p2)))
	(save-excursion
	 (set-buffer b2)
	 (setq s2 (buffer-substring p2 (+ size p2))))
	(setq s1 (buffer-substring p1 (+ size p1)))
	(setq success (and (> size 0) (equal s1 s2)))
	(if success
	    (setq p1 (+ p1 size) p2 (+ p2 size))))
      (setq size (/ size 2)))

    (goto-char p1)
    (set-window-point w2 p2)
    (if (= (point) opoint)
	(ding))))
