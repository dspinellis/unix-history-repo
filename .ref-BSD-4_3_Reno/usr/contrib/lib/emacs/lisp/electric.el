;; electric -- Window maker and Command loop for `electric' modes.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;; Principal author K. Shane Hartman

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


(provide 'electric)                           ; zaaaaaaap

;; perhaps this should be in subr.el...
(defun shrink-window-if-larger-than-buffer (window)
  (save-excursion
    (set-buffer (window-buffer window))
    (let ((w (selected-window)) ;save-window-excursion can't win
	  (buffer-file-name buffer-file-name)
	  (p (point))
	  (n 0)
	  (window-min-height 0)
	  (buffer-read-only nil)
	  (modified (buffer-modified-p)))
      (unwind-protect
	  (progn
	    (select-window window)
	    (goto-char (point-min))
	    (while (pos-visible-in-window-p (point-max))
	      ;; defeat file locking... don't try this at home, kids!
	      (setq buffer-file-name nil)
	      (insert ?\n) (setq n (1+ n)))
	    (if (> n 0) (shrink-window (1- n))))
	(delete-region (point-min) (point))
	(set-buffer-modified-p modified)
	(goto-char p)
	(select-window w)))))
      
      
;; This loop is the guts for non-standard modes which retain control
;; until some event occurs.  It is a `do-forever', the only way out is to
;; throw.  It assumes that you have set up the keymap, window, and
;; everything else: all it does is read commands and execute them -
;; providing error messages should one occur (if there is no loop
;; function - which see).  The required argument is a tag which should
;; expect a value of nil if the user decides to punt. The
;; second argument is a prompt string (defaults to "->").  Given third
;; argument non-nil, it INHIBITS quitting unless the user types C-g at
;; toplevel.  This is so user can do things like C-u C-g and not get
;; thrown out.  Fourth argument, if non-nil, should be a function of two
;; arguments which is called after every command is executed.  The fifth
;; argument, if provided, is the state variable for the function.  If the
;; loop-function gets an error, the loop will abort WITHOUT throwing
;; (moral: use unwind-protect around call to this function for any
;; critical stuff).  The second argument for the loop function is the
;; conditions for any error that occurred or nil if none.

(defun Electric-command-loop (return-tag
			      &optional prompt inhibit-quit
					loop-function loop-state)
  (if (not prompt) (setq prompt "->"))
  (let (cmd (err nil))
    (while t
      (setq cmd (read-key-sequence (if (stringp prompt)
				       prompt (funcall prompt))))
      (setq last-command-char (aref cmd (1- (length cmd)))
	    this-command (key-binding cmd)
	    cmd this-command)
      (if (or (prog1 quit-flag (setq quit-flag nil))
	      (= last-input-char ?\C-g))
	  (progn (setq unread-command-char -1
		       prefix-arg nil)
		 ;; If it wasn't cancelling a prefix character, then quit.
		 (if (or (= (length (this-command-keys)) 1)
			 (not inhibit-quit)) ; safety
		     (progn (ding)
			    (message "Quit")
			    (throw return-tag nil))
		   (setq cmd nil))))
      (setq current-prefix-arg prefix-arg)
      (if cmd
	  (condition-case conditions
	      (progn (command-execute cmd)
		     (if (or (prog1 quit-flag (setq quit-flag nil))
			     (= last-input-char ?\C-g))
			 (progn (setq unread-command-char -1)
				(if (not inhibit-quit)
				    (progn (ding)
					   (message "Quit")
					   (throw return-tag nil))
				  (ding)))))
	    (buffer-read-only (if loop-function
				  (setq err conditions)
				(ding)
				(message "Buffer is read-only")
				(sit-for 2)))
	    (beginning-of-buffer (if loop-function
				     (setq err conditions)
				   (ding)
				   (message "Beginning of Buffer")
				   (sit-for 2)))
	    (end-of-buffer (if loop-function
			       (setq err conditions)
			     (ding)
			     (message "End of Buffer")
			     (sit-for 2)))
	    (error (if loop-function
		       (setq err conditions)
		     (ding)
		     (message "Error: %s"
			      (if (eq (car conditions) 'error)
				  (car (cdr conditions))
				(prin1-to-string conditions)))
		     (sit-for 2))))
	(ding))
      (if loop-function (funcall loop-function loop-state err))))
  (ding)
  (throw return-tag nil))

;; This function is like pop-to-buffer, sort of. 
;; The algorithm is
;; If there is a window displaying buffer
;; 	Select it
;; Else if there is only one window
;; 	Split it, selecting the window on the bottom with height being
;; 	the lesser of max-height (if non-nil) and the number of lines in
;;      the buffer to be displayed subject to window-min-height constraint.
;; Else
;; 	Switch to buffer in the current window.
;;
;; Then if max-height is nil, and not all of the lines in the buffer
;; are displayed, grab the whole screen.
;;
;; Returns selected window on buffer positioned at point-min.

(defun Electric-pop-up-window (buffer &optional max-height)
  (let* ((win (or (get-buffer-window buffer) (selected-window)))
	 (buf (get-buffer buffer))
	 (one-window (one-window-p t))
	 (pop-up-windows t)
	 (target-height)
	 (lines))
    (if (not buf)
	(error "Buffer %s does not exist" buffer)
      (save-excursion
	(set-buffer buf)
	(setq lines (count-lines (point-min) (point-max)))
	(setq target-height
	      (min (max (if max-height (min max-height (1+ lines)) (1+ lines))
			window-min-height)
		   (save-window-excursion
		     (delete-other-windows)
		     (1- (window-height (selected-window)))))))
      (cond ((and (eq (window-buffer win) buf))
	     (select-window win))
	    (one-window
	     (goto-char (window-start win))
	     (pop-to-buffer buffer)
	     (setq win (selected-window))
	     (enlarge-window (- target-height (window-height win))))
	    (t
	     (switch-to-buffer buf)))
      (if (and (not max-height)
	       (> target-height (window-height (selected-window))))
	  (progn (goto-char (window-start win))
		 (enlarge-window (- target-height (window-height win)))))
      (goto-char (point-min))
      win)))
