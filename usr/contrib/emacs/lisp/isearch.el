;; Incremental search
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

; in loaddefs.el
;(defvar search-last-string ""
;  "Last string search for by a search command.
;This does not include direct calls to the primitive search functions,
;and does not include searches that are aborted.")
;
;(defconst search-repeat-char ?\C-s
;  "Character to repeat incremental search forwards.")
;(defconst search-reverse-char ?\C-r
;  "Character to repeat incremental search backwards.")
;(defconst search-exit-char ?\e
;  "Character to exit incremental search.")
;(defconst search-delete-char ?\177
;  "Character to delete from incremental search string.")
;(defconst search-quote-char ?\C-q
;  "Character to quote special characters for incremental search.")
;(defconst search-yank-word-char ?\C-w
;  "Character to pull next word from buffer into search string.")
;(defconst search-yank-line-char ?\C-y
;  "Character to pull rest of line from buffer into search string.")
;(defconst search-exit-option t
;  "Non-nil means random control characters terminate incremental search.")
;
;(defvar isearch-slow-window-lines 1
;  "*Number of lines in slow search display windows.")
;(defconst isearch-slow-speed 1200
;  "*Highest terminal speed at which to use \"slow\" style incremental search.
;This is the style where a one-line window is created to show the line
;that the search has reached.")

;; This function does all the work of incremental search.
;; The functions attached to ^R and ^S are trivial,
;; merely calling this one, but they are always loaded by default
;; whereas this file can optionally be autoloadable.
;; This is the only entry point in this file.

(defun isearch (forward &optional regexp)
  (let ((search-string "")
	(search-message "")
	(cmds nil)
	(success t)
	(invalid-regexp nil)
	(slow-terminal-mode (<= (baud-rate) isearch-slow-speed))
	(other-end nil)    ;Start of last match if fwd, end if backwd.
	(small-window nil)		;if t, using a small window
	(window-min-height (min window-min-height (1+ isearch-slow-window-lines)))
					;so we can make small windows
	(found-point nil)			;to restore point from a small window
	;; This is the window-start value found by the search.
	(found-start nil)
	(opoint (point))
	(inhibit-quit t))  ;Prevent ^G from quitting immediately.
    (isearch-push-state)
    (save-window-excursion
     (catch 'search-done
       (while t
	 (or (>= unread-command-char 0)
	     (progn
	       (or (input-pending-p)
		   (isearch-message))
	       (if (and slow-terminal-mode
			(not (or small-window (pos-visible-in-window-p))))
		   (progn
		     (setq small-window t)
		     (setq found-point (point))
		     (move-to-window-line 0)
		     (split-window nil (- (window-height)
					  (1+ isearch-slow-window-lines)))
		     (other-window 1)
		     (goto-char found-point)))))
	 (let ((char (if quit-flag
			 ?\C-g
		       (read-char))))
	   (setq quit-flag nil
		 invalid-regexp nil)
	   ;; Meta character means exit search.
	   (cond ((and (>= char 128)
		       search-exit-option)
		  (setq unread-command-char char)
		  (throw 'search-done t))
		 ((eq char search-exit-char)
		  ;; Esc means exit search normally.
		  ;; Except, if first thing typed, it means do nonincremental
		  (if (= 0 (length search-string))
		      (nonincremental-search forward regexp))
		  (throw 'search-done t))
		 ((= char ?\C-g)
		  ;; ^G means the user tried to quit.
		  (ding)
		  (discard-input)
		  (if success
		      ;; If search is successful, move back to starting point
		      ;; and really do quit.
		      (progn (goto-char opoint)
			     (signal 'quit nil))
		    ;; If search is failing, rub out until it is once more
		    ;;  successful.
		    (while (not success) (isearch-pop))))
		 ((eq char search-repeat-char)
		  ;; ^S means search again, forward, for the same string.
		  (setq forward t)
		  (if (null (cdr cmds))
		      ;; If the first char typed,
		      ;; it means search for the string of the previous search
		      (progn
		        (setq search-string search-last-string
			      search-message
			        (mapconcat 'text-char-description
					   search-string ""))))
		  (isearch-search)
		  (isearch-push-state))
		 ((eq char search-reverse-char)
		  ;; ^R is similar but it searches backward.
		  (setq forward nil)
		  (if (null (cdr cmds))
		      (progn
			(setq search-string search-last-string
			      search-message
			        (mapconcat 'text-char-description
					   search-string ""))))
		  (isearch-search)
		  (isearch-push-state))
		 ((= char search-delete-char)
		  ;; Rubout means discard last input item and move point
		  ;; back.  If buffer is empty, just beep.
		  (if (null (cdr cmds))
		      (ding)
		    (isearch-pop)))
		 (t
		  (cond ((or (eq char search-yank-word-char)
			     (eq char search-yank-line-char))
			 ;; ^W means gobble next word from buffer.
			 ;; ^Y means gobble rest of line from buffer.
			 (let ((word (save-excursion
				       (and (not forward) other-end
					    (goto-char other-end))
				       (buffer-substring
					(point)
					(save-excursion
					  (if (eq char search-yank-line-char)
					      (end-of-line)
					    (forward-word 1))
					  (point))))))
			   (setq search-string (concat search-string word)
				 search-message
				   (concat search-message
					   (mapconcat 'text-char-description
						      word "")))))
			 ;; Any other control char =>
			 ;;  unread it and exit the search normally.
			 ((and search-exit-option
			       (/= char search-quote-char)
			       (< char ? ) (/= char ?\t) (/= char ?\r))
			  (setq unread-command-char char)
			  (throw 'search-done t))
			 (t
			  ;; Any other character => add it to the
			  ;;  search string and search.
			  (cond ((= char search-quote-char)
				 (setq char (read-quoted-char
					     (isearch-message t))))
				((= char ?\r)
				 ;; unix braindeath
				 (setq char ?\n)))
			  (setq search-string (concat search-string
						      (char-to-string char))
				search-message (concat search-message
						       (text-char-description char)))))
		  (if (and (not success)
			   ;; unsuccessful regexp search may become
			   ;;  successful by addition of characters which
			   ;;  make search-string valid
			   (not regexp))
		      nil
		    (if other-end
			(goto-char (if forward other-end
				     (min opoint (1+ other-end)))))
		    (isearch-search))
		  (isearch-push-state))))))
     (setq found-start (window-start (selected-window)))
     (setq found-point (point)))
    (setq search-last-string search-string)
    ;; If there was movement, mark the starting position.
    ;; Maybe should test difference between and set mark iff > threshold.
    (if (/= (point) opoint) (push-mark opoint))
    (if small-window
	(goto-char found-point)
      ;; Exiting the save-window-excursion clobbers this; restore it.
      (set-window-start (selected-window) found-start t))
    (message "")))

(defun isearch-message (&optional c-q-hack ing)
  (or success (setq ing nil))
  (let ((m (concat (if success "" "Failing ")
		   (if regexp (if success "Regexp " "regexp ") "")
		   "I-search"
		   (if forward ": " " backward: ")
		   search-message
		   (if c-q-hack "^Q" "")
		   (if invalid-regexp
		       (concat " [" invalid-regexp "]")
		     (if (and ing (not slow-terminal-mode)) " ..." "")))))
    (if c-q-hack m (message "%s" m))))

(defun isearch-pop ()
  (setq cmds (cdr cmds))
  (let ((cmd (car cmds)))
    (setq search-string (car cmd)
	  search-message (car (cdr cmd))
	  success (car (cdr (cdr (cdr cmd))))
	  forward (car (cdr (cdr (cdr (cdr cmd)))))
	  other-end (car (cdr (cdr (cdr (cdr (cdr cmd))))))
	  invalid-regexp (car (cdr (cdr (cdr (cdr (cdr (cdr cmd))))))))
    (goto-char (car (cdr (cdr cmd))))))

(defun isearch-push-state ()
  (setq cmds (cons (list search-string search-message (point)
			 success forward other-end invalid-regexp)
		   cmds)))

(defun isearch-search ()
  (isearch-message nil t)
  (if (setq success
	    (condition-case lossage
		(let ((inhibit-quit nil))
		  (if regexp (setq invalid-regexp nil))
		  (funcall
		    (if regexp
			(if forward 're-search-forward 're-search-backward)
		        (if forward 'search-forward 'search-backward))
		    search-string nil t))
	      (quit (setq unread-command-char ?\C-g)
		    nil)
	      (invalid-regexp (setq invalid-regexp (car (cdr lossage)))
			      nil)))
      (setq other-end
	    (if forward (match-beginning 0) (match-end 0)))
    (or invalid-regexp
	(not (car (cdr (cdr (cdr (car cmds)))))) ;unsuccusful last time
	(ding))
    (goto-char (car (cdr (cdr (car cmds)))))))

;; This is called from incremental-search
;; if the first input character is the exit character.
;; We store the search string in  search-string
;; which has been bound already by incremental-search
;; so that, when we exit, it is copied into search-last-string.
(defun nonincremental-search (forward regexp)
  (let (message char (inhibit-quit nil))
    ;; Prompt assuming not word search,
    (setq message (if regexp 
		      (if forward "Regexp search: "
			"Regexp search backward: ")
		    (if forward "Search: " "Search backward: ")))
    (message "%s" message)
    ;; Read 1 char and switch to word search if it is ^W.
    (setq char (read-char))
    (if (eq char search-yank-word-char)
	(setq message (if forward "Word search: " "Word search backward: "))
      ;; Otherwise let that 1 char be part of the search string.
      (setq unread-command-char char))
    ;; Read the search string with corrected prompt.
    (setq search-string (read-string message))
    ;; Empty means use default.
    (if (= 0 (length search-string))
	(setq search-string search-last-string)
      ;; Set last search string now so it is set even if we fail.
      (setq search-last-string search-string))
    ;; Go ahead and search.
    (funcall (if (eq char search-yank-word-char)
		 (if forward 'word-search-forward 'word-search-backward)
	       (if regexp
		   (if forward 're-search-forward 're-search-backward)
		 (if forward 'search-forward 'search-backward)))
	     search-string)))
