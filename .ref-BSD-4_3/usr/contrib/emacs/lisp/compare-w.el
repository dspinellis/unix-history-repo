;; Compare text between windows for Emacs.
;; Copyright (C) 1985 Richard M. Stallman.

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


(defun compare-windows ()
  "Compare text in current window with text in next window.
Compares the text starting at point in each window,
moving over text in each one as far as they match."
  (interactive)
  (let (p1 p2 np1 np2 maxp1 maxp2 b1 b2 w2
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

    (setq losep1 (+ 10 maxp1) losep2 (+ 10 maxp2))

    ;; Try advancing comparing 1000 chars at a time.
    ;; When that fails, go 500 chars at a time, and so on.
    (setq size 1000)
    (while (> size 0)
      (setq success t)
      (while success
	(setq np2 (min (+ p2 size) maxp2))
	(setq np1 (min (+ p1 size) maxp1))
	(save-excursion
	 (set-buffer b2)
	 (setq s2 (buffer-substring p2 np2)))
	(setq s1 (buffer-substring p1 np1))
	(setq success (and (/= np1 p1) (equal s1 s2)))
	;; Don't bother retrying the last fraction of what already lost.
	(and success
	     (or (> (+ p2 size) (- losep2 3))
		 (> (+ p1 size) (- losep1 3)))
	     (> size 1)
	     (setq success nil))
	(if success
	    (setq p1 np1 p2 np2)
	  (setq losep1 np1 losep2 np2
		maxp1 np1 maxp2 np2)))
      (setq size (min (- maxp1 p1) (- maxp2 p2) (/ size 2))))

    (goto-char p1)
    (set-window-point w2 p2)
    (if (= (point) opoint)
	(ding))))
