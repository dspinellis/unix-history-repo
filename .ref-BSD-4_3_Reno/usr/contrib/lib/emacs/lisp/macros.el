;; Non-primitive commands for keyboard macros.
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

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


(defun name-last-kbd-macro (symbol)
  "Assign a name to the last keyboard macro defined.
One arg, a symbol, which is the name to define.
The symbol's function definition becomes the keyboard macro string.
Such a \"function\" cannot be called from Lisp, but it is a valid command
definition for the editor command loop."
  (interactive "SName for last kbd macro: ")
  (or last-kbd-macro
      (error "No keyboard macro defined"))
  (and (fboundp symbol)
       (not (stringp (symbol-function symbol)))
       (error "Function %s is already defined and not a keyboard macro."
	      symbol))
  (fset symbol last-kbd-macro))

(defun insert-kbd-macro (macroname &optional keys)
  "Insert in buffer the definition of kbd macro NAME, as Lisp code.
Second argument KEYS non-nil means also record the keys it is on.
 (This is the prefix argument, when calling interactively.)

This Lisp code will, when executed, define the kbd macro with the
same definition it has now.  If you say to record the keys,
the Lisp code will also rebind those keys to the macro.
Only global key bindings are recorded since executing this Lisp code
always makes global bindings.

To save a kbd macro, visit a file of Lisp code such as your ~/.emacs,
use this command, and then save the file."
  (interactive "CInsert kbd macro (name): \nP")
  (insert "(fset '")
  (prin1 macroname (current-buffer))
  (insert "\n   ")
  (prin1 (symbol-function macroname) (current-buffer))
  (insert ")\n")
  (if keys
      (let ((keys (where-is-internal macroname nil)))
	(while keys
	  (insert "(global-set-key ")
	  (prin1 (car keys) (current-buffer))
	  (insert " '")
	  (prin1 macroname (current-buffer))
	  (insert ")\n")
	  (setq keys (cdr keys))))))

(defun kbd-macro-query (flag)
  "Query user during kbd macro execution.
With prefix argument, enters recursive edit,
 reading keyboard commands even within a kbd macro.
 You can give different commands each time the macro executes.
Without prefix argument, reads a character.  Your options are:
 Space -- execute the rest of the macro.
 DEL -- skip the rest of the macro; start next repetition.
 C-d -- skip rest of the macro and don't repeat it any more.
 C-r -- enter a recursive edit, then on exit ask again for a character
 C-l -- redisplay screen and ask again."
  (interactive "P")
  (or executing-macro
      defining-kbd-macro
      (error "Not defining or executing kbd macro"))
  (if flag
      (let (executing-macro defining-kbd-macro)
	(recursive-edit))
    (if (not executing-macro)
	nil
      (let ((loop t))
	(while loop
	  (let ((char (let ((executing-macro nil)
			    (defining-kbd-macro nil))
			(message "Proceed with macro? (Space, DEL, C-d, C-r or C-l) ")
			(read-char))))
	    (cond ((= char ? )
		   (setq loop nil))
		  ((= char ?\177)
		   (setq loop nil)
		   (setq executing-macro ""))
		  ((= char ?\C-d)
		   (setq loop nil)
		   (setq executing-macro t))
		  ((= char ?\C-l)
		   (recenter nil))
		  ((= char ?\C-r)
		   (let (executing-macro defining-kbd-macro)
		     (recursive-edit))))))))))
