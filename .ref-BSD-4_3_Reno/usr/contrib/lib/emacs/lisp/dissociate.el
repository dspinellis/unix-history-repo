;; Scramble text amusingly for Emacs.
;; Copyright (C) 1985 Free Software Foundation, Inc.

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
