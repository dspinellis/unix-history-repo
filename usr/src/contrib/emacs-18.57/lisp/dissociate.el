;; Scramble text amusingly for Emacs.
;; Copyright (C) 1985 Free Software Foundation, Inc.

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


(defun dissociated-press (&optional arg)
  "Dissociate the text of the current buffer.
Output goes in buffer named *Dissociation*,
which is redisplayed each time text is added to it.
Every so often the user must say whether to continue.
If ARG is positive, require ARG chars of continuity.
If ARG is negative, require -ARG words of continuity.
Default is 2."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 2))
  (let* ((inbuf (current-buffer))
	 (outbuf (get-buffer-create "*Dissociation*"))
	 (move-function (if (> arg 0) 'forward-char 'forward-word))
	 (move-amount (if (> arg 0) arg (- arg)))
	 (search-function (if (> arg 0) 'search-forward 'word-search-forward))
	 (last-query-point 0))
    (switch-to-buffer outbuf)
    (erase-buffer)
    (while
      (save-excursion
	(goto-char last-query-point)
	(vertical-motion (- (window-height) 4))
	(or (= (point) (point-max))
	    (and (progn (goto-char (point-max))
			(y-or-n-p "Continue dissociation? "))
		 (progn
		   (message "")
		   (recenter 1)
		   (setq last-query-point (point-max))
		   t))))
      (let (start end)
	(save-excursion
	 (set-buffer inbuf)
	 (setq start (point))
	 (if (eq move-function 'forward-char)
	     (progn
	       (setq end (+ start (+ move-amount (logand 15 (random)))))
	       (if (> end (point-max))
		   (setq end (+ 1 move-amount (logand 15 (random)))))
	       (goto-char end))
	   (funcall move-function
		    (+ move-amount (logand 15 (random)))))
	 (setq end (point)))
	(let ((opoint (point)))
	  (insert-buffer-substring inbuf start end)
	  (save-excursion
	   (goto-char opoint)
	   (end-of-line)
	   (and (> (current-column) fill-column)
		(do-auto-fill)))))
      (save-excursion
       (set-buffer inbuf)
       (if (eobp)
	   (goto-char (point-min))
	 (let ((overlap
		(buffer-substring (prog1 (point)
					 (funcall move-function
						  (- move-amount)))
				  (point))))
	   (let (ranval)
	     (while (< (setq ranval (random)) 0))
	     (goto-char (1+ (% ranval (1- (point-max))))))
	   (or (funcall search-function overlap nil t)
	       (let ((opoint (point)))
		 (goto-char 1)
		 (funcall search-function overlap opoint t))))))
      (sit-for 0))))
