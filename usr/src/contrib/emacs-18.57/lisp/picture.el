;; "Picture mode" -- editing using quarter-plane screen model.
;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Principal author K. Shane Hartman

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


(provide 'picture)

(defun move-to-column-force (column)
  "Move to column COLUMN in current line.
Differs from move-to-column in that it creates or modifies whitespace
if necessary to attain exactly the specified column."
  (move-to-column column)
  (let ((col (current-column)))
    (if (< col column)
	(indent-to column)
      (if (and (/= col column)
	       (= (preceding-char) ?\t))
	  (let (indent-tabs-mode)
	    (delete-char -1)
            (indent-to col)
            (move-to-column column))))))


;; Picture Movement Commands

(defun picture-end-of-line (&optional arg)
  "Position point after last non-blank character on current line.
With ARG not nil, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error."
  (interactive "P")
  (if arg (forward-line (1- (prefix-numeric-value arg))))
  (beginning-of-line)
  (skip-chars-backward " \t" (prog1 (point) (end-of-line))))

(defun picture-forward-column (arg)
  "Move cursor right, making whitespace if necessary.
With argument, move that many columns."
  (interactive "p")
  (move-to-column-force (+ (current-column) arg)))

(defun picture-backward-column (arg)
  "Move cursor left, making whitespace if necessary.
With argument, move that many columns."
  (interactive "p")
  (move-to-column-force (- (current-column) arg)))

(defun picture-move-down (arg)
  "Move vertically down, making whitespace if necessary.
With argument, move that many lines."
  (interactive "p")
  (let ((col (current-column)))
    (picture-newline arg)
    (move-to-column-force col)))

(defconst picture-vertical-step 0
  "Amount to move vertically after text character in Picture mode.")

(defconst picture-horizontal-step 1
  "Amount to move horizontally after text character in Picture mode.")

(defun picture-move-up (arg)
  "Move vertically up, making whitespace if necessary.
With argument, move that many lines."
  (interactive "p")
  (picture-move-down (- arg)))

(defun picture-movement-right ()
  "Move right after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 0 1))

(defun picture-movement-left ()
  "Move left after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 0 -1))

(defun picture-movement-up ()
  "Move up after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion -1 0))

(defun picture-movement-down ()
  "Move down after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 1 0))

(defun picture-movement-nw ()
  "Move up and left after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion -1 -1))

(defun picture-movement-ne ()
  "Move up and right after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion -1 1))

(defun picture-movement-sw ()
  "Move down and left after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 1 -1))

(defun picture-movement-se ()
  "Move down and right after self-inserting character in Picture mode."
  (interactive)
  (picture-set-motion 1 1))

(defun picture-set-motion (vert horiz)
  "Set VERTICAL and HORIZONTAL increments for movement in Picture mode.
The mode line is updated to reflect the current direction."
  (setq picture-vertical-step vert
	picture-horizontal-step horiz)
  (setq mode-name
	(format "Picture:%s"
		(car (nthcdr (+ 1 (% horiz 2) (* 3 (1+ (% vert 2))))
			     '(nw up ne left none right sw down se)))))
  ;; Kludge - force the mode line to be updated.  Is there a better
  ;; way to this?
  (set-buffer-modified-p (buffer-modified-p))
  (message ""))

(defun picture-move ()
  "Move in direction of  picture-vertical-step  and  picture-horizontal-step."
  (picture-move-down picture-vertical-step)
  (picture-forward-column picture-horizontal-step))

(defun picture-motion (arg)
  "Move point in direction of current picture motion in Picture mode.
With ARG do it that many times.  Useful for delineating rectangles in
conjunction with diagonal picture motion.
Do \\[command-apropos]  picture-movement  to see commands which control motion."
  (interactive "p")
  (picture-move-down (* arg picture-vertical-step))
  (picture-forward-column (* arg picture-horizontal-step)))

(defun picture-motion-reverse (arg)
  "Move point in direction opposite of current picture motion in Picture mode.
With ARG do it that many times.  Useful for delineating rectangles in
conjunction with diagonal picture motion.
Do \\[command-apropos]  picture-movement  to see commands which control motion."
  (interactive "p")
  (picture-motion (- arg)))


;; Picture insertion and deletion.

(defun picture-self-insert (arg)
  "Insert this character in place of character previously at the cursor.
The cursor then moves in the direction you previously specified
with the commands picture-movement-right, picture-movement-up, etc.
Do \\[command-apropos]  picture-movement  to see those commands."
  (interactive "p")
  (while (> arg 0)
    (setq arg (1- arg))
    (move-to-column-force (1+ (current-column)))
    (delete-char -1)
    (insert last-input-char)
    (forward-char -1)
    (picture-move)))

(defun picture-clear-column (arg)
  "Clear out ARG columns after point without moving."
  (interactive "p")
  (let* ((opoint (point))
	 (original-col (current-column))
	 (target-col (+ original-col arg)))
    (move-to-column-force target-col)
    (delete-region opoint (point))
    (save-excursion
     (indent-to (max target-col original-col)))))

(defun picture-backward-clear-column (arg)
  "Clear out ARG columns before point, moving back over them."
  (interactive "p")
  (picture-clear-column (- arg)))

(defun picture-clear-line (arg)
  "Clear out rest of line; if at end of line, advance to next line.
Cleared-out line text goes into the kill ring, as do
newlines that are advanced over.
With argument, clear out (and save in kill ring) that many lines."
  (interactive "P")
  (if arg
      (progn
       (setq arg (prefix-numeric-value arg))
       (kill-line arg)
       (newline (if (> arg 0) arg (- arg))))
    (if (looking-at "[ \t]*$")
	(kill-ring-save (point) (progn (forward-line 1) (point)))
      (kill-region (point) (progn (end-of-line) (point))))))

(defun picture-newline (arg)
  "Move to the beginning of the following line.
With argument, moves that many lines (up, if negative argument);
always moves to the beginning of a line."
  (interactive "p")
  (if (< arg 0)
      (forward-line arg)
    (while (> arg 0)
      (end-of-line)
      (if (eobp) (newline) (forward-char 1))
      (setq arg (1- arg)))))

(defun picture-open-line (arg)
  "Insert an empty line after the current line.
With positive argument insert that many lines."
  (interactive "p")
  (save-excursion
   (end-of-line)
   (open-line arg)))

(defun picture-duplicate-line ()
  "Insert a duplicate of the current line, below it."
  (interactive)
  (save-excursion
   (let ((contents
	  (buffer-substring
	   (progn (beginning-of-line) (point))
	   (progn (picture-newline 1) (point)))))
     (forward-line -1)
     (insert contents))))


;; Picture Tabs

(defvar picture-tab-chars "!-~"
  "*A character set which controls behavior of commands
\\[picture-set-tab-stops] and \\[picture-tab-search].  It is NOT a
regular expression, any regexp special characters will be quoted.
It defines a set of \"interesting characters\" to look for when setting
\(or searching for) tab stops, initially \"!-~\" (all printing characters).
For example, suppose that you are editing a table which is formatted thus:
| foo		| bar + baz | 23  *
| bubbles	| and + etc | 97  *
and that picture-tab-chars is \"|+*\".  Then invoking
\\[picture-set-tab-stops] on either of the previous lines would result
in the following tab stops
		:     :     :     :
Another example - \"A-Za-z0-9\" would produce the tab stops
  :		  :	:     :

Note that if you want the character `-' to be in the set, it must be
included in a range or else appear in a context where it cannot be
taken for indicating a range (e.g. \"-A-Z\" declares the set to be the
letters `A' through `Z' and the character `-').  If you want the
character `\\' in the set it must be preceded by itself: \"\\\\\".

The command \\[picture-tab-search] is defined to move beneath (or to) a
character belonging to this set independent of the tab stops list.")

(defun picture-set-tab-stops (&optional arg)
  "Set value of  tab-stop-list  according to context of this line.
This controls the behavior of \\[picture-tab].  A tab stop
is set at every column occupied by an \"interesting character\" that is
preceded by whitespace.  Interesting characters are defined by the
variable  picture-tab-chars,  see its documentation for an example
of usage.  With ARG, just (re)set  tab-stop-list  to its default value.
The tab stops computed are displayed in the minibuffer with `:' at
each stop."
  (interactive "P")
  (save-excursion
    (let (tabs)
      (if arg
	  (setq tabs (default-value 'tab-stop-list))
	(let ((regexp (concat "[ \t]+[" (regexp-quote picture-tab-chars) "]")))
	  (beginning-of-line)
	  (let ((bol (point)))
	    (end-of-line)
	    (while (re-search-backward regexp bol t)
	      (skip-chars-forward " \t")
	      (setq tabs (cons (current-column) tabs)))
	    (if (null tabs)
		(error "No characters in set %s on this line."
		       (regexp-quote picture-tab-chars))))))
      (setq tab-stop-list tabs)
      (let ((blurb (make-string (1+ (nth (1- (length tabs)) tabs)) ?\ )))
	(while tabs
	  (aset blurb (car tabs) ?:)
	  (setq tabs (cdr tabs)))
	(message blurb)))))

(defun picture-tab-search (&optional arg)
  "Move to column beneath next interesting char in previous line.
With ARG move to column occupied by next interesting character in this
line.  The character must be preceded by whitespace.
\"interesting characters\" are defined by variable  picture-tab-chars.
If no such character is found, move to beginning of line."
  (interactive "P")
  (let ((target (current-column)))
    (save-excursion
      (if (and (not arg)
	       (progn
		 (beginning-of-line)
		 (skip-chars-backward
		  (concat "^" (regexp-quote picture-tab-chars))
		  (point-min))
		 (not (bobp))))
	  (move-to-column target))
      (if (re-search-forward
	   (concat "[ \t]+[" (regexp-quote picture-tab-chars) "]")
	   (save-excursion (end-of-line) (point))
	   'move)
	  (setq target (1- (current-column)))
	(setq target nil)))
    (if target
	(move-to-column-force target)
      (beginning-of-line))))

(defun picture-tab (&optional arg)
  "Tab transparently (move) to next tab stop.
With ARG overwrite the traversed text with spaces.  The tab stop
list can be changed by \\[picture-set-tab-stops] and \\[edit-tab-stops].
See also documentation for variable  picture-tab-chars."
  (interactive "P")
  (let* ((opoint (point))
	 (target (prog2 (tab-to-tab-stop)
			(current-column)
			(delete-region opoint (point)))))
    (move-to-column-force target)
    (if arg
	(let (indent-tabs-mode)
	  (delete-region opoint (point))
	  (indent-to target)))))

;; Picture Rectangles

(defconst picture-killed-rectangle nil
  "Rectangle killed or copied by \\[picture-clear-rectangle] in Picture mode.
The contents can be retrieved by \\[picture-yank-rectangle]")

(defun picture-clear-rectangle (start end &optional killp)
  "Clear and save rectangle delineated by point and mark.
The rectangle is saved for yanking by \\[picture-yank-rectangle] and replaced
with whitespace.  The previously saved rectangle, if any, is lost.
With prefix argument, the rectangle is actually killed, shifting remaining
text."
  (interactive "r\nP")
  (setq picture-killed-rectangle (picture-snarf-rectangle start end killp)))

(defun picture-clear-rectangle-to-register (start end register &optional killp)
  "Clear rectangle delineated by point and mark into REGISTER.
The rectangle is saved in REGISTER and replaced with whitespace.
With prefix argument, the rectangle is actually killed, shifting remaining
text."
  (interactive "r\ncRectangle to register: \nP")
  (set-register register (picture-snarf-rectangle start end killp)))

(defun picture-snarf-rectangle (start end &optional killp)
  (let ((column (current-column))
	(indent-tabs-mode nil))
    (prog1 (save-excursion
             (if killp
                 (delete-extract-rectangle start end)
               (prog1 (extract-rectangle start end)
                      (clear-rectangle start end))))
	   (move-to-column-force column))))

(defun picture-yank-rectangle (&optional insertp)
  "Overlay rectangle saved by \\[picture-clear-rectangle]
The rectangle is positioned with upper left corner at point, overwriting
existing text.  With prefix argument, the rectangle is inserted instead,
shifting existing text.  Leaves mark at one corner of rectangle and
point at the other (diagonally opposed) corner."
  (interactive "P")
  (if (not (consp picture-killed-rectangle))
      (error "No rectangle saved.")
    (picture-insert-rectangle picture-killed-rectangle insertp)))

(defun picture-yank-rectangle-from-register (register &optional insertp)
  "Overlay rectangle saved in REGISTER.
The rectangle is positioned with upper left corner at point, overwriting
existing text.  With prefix argument, the rectangle is
inserted instead, shifting existing text.  Leaves mark at one corner
of rectangle and point at the other (diagonally opposed) corner."
  (interactive "cRectangle from register: \nP")
  (let ((rectangle (get-register register)))
    (if (not (consp rectangle))
	(error "Register %c does not contain a rectangle." register)
      (picture-insert-rectangle rectangle insertp))))

(defun picture-insert-rectangle (rectangle &optional insertp)
  "Overlay RECTANGLE with upper left corner at point.
Optional argument INSERTP, if non-nil causes RECTANGLE to be inserted.
Leaves the region surrounding the rectangle."
  (let ((indent-tabs-mode nil))
    (if (not insertp)
	(save-excursion
	  (delete-rectangle (point)
			    (progn
			      (picture-forward-column (length (car rectangle)))
			      (picture-move-down (1- (length rectangle)))
			      (point)))))
    (push-mark)
    (insert-rectangle rectangle)))


;; Picture Keymap, entry and exit points.

(defconst picture-mode-map nil)

(if (not picture-mode-map)
    (let ((i ?\ ))
      (setq picture-mode-map (make-keymap))
      (while (< i ?\177)
        (aset picture-mode-map i 'picture-self-insert)
	(setq i (1+ i)))
      (define-key picture-mode-map "\C-f" 'picture-forward-column)
      (define-key picture-mode-map "\C-b" 'picture-backward-column)
      (define-key picture-mode-map "\C-d" 'picture-clear-column)
      (define-key picture-mode-map "\C-c\C-d" 'delete-char)
      (define-key picture-mode-map "\177" 'picture-backward-clear-column)
      (define-key picture-mode-map "\C-k" 'picture-clear-line)
      (define-key picture-mode-map "\C-o" 'picture-open-line)
      (define-key picture-mode-map "\C-m" 'picture-newline)
      (define-key picture-mode-map "\C-j" 'picture-duplicate-line)
      (define-key picture-mode-map "\C-n" 'picture-move-down)
      (define-key picture-mode-map "\C-p" 'picture-move-up)
      (define-key picture-mode-map "\C-e" 'picture-end-of-line)
      (define-key picture-mode-map "\e\t" 'picture-toggle-tab-state)
      (define-key picture-mode-map "\t" 'picture-tab)
      (define-key picture-mode-map "\e\t" 'picture-tab-search)
      (define-key picture-mode-map "\C-c\t" 'picture-set-tab-stops)
      (define-key picture-mode-map "\C-c\C-k" 'picture-clear-rectangle)
      (define-key picture-mode-map "\C-c\C-w" 'picture-clear-rectangle-to-register)
      (define-key picture-mode-map "\C-c\C-y" 'picture-yank-rectangle)
      (define-key picture-mode-map "\C-c\C-x" 'picture-yank-rectangle-from-register)
      (define-key picture-mode-map "\C-c\C-c" 'picture-mode-exit)
      (define-key picture-mode-map "\C-c\C-f" 'picture-motion)
      (define-key picture-mode-map "\C-c\C-b" 'picture-motion-reverse)
      (define-key picture-mode-map "\C-c<" 'picture-movement-left)
      (define-key picture-mode-map "\C-c>" 'picture-movement-right)
      (define-key picture-mode-map "\C-c^" 'picture-movement-up)
      (define-key picture-mode-map "\C-c." 'picture-movement-down)
      (define-key picture-mode-map "\C-c`" 'picture-movement-nw)
      (define-key picture-mode-map "\C-c'" 'picture-movement-ne)
      (define-key picture-mode-map "\C-c/" 'picture-movement-sw)
      (define-key picture-mode-map "\C-c\\" 'picture-movement-se)))

(defvar edit-picture-hook nil
  "If non-nil, it's value is called on entry to Picture mode.
Picture mode is invoked by the command \\[edit-picture].")

(defun edit-picture ()
  "Switch to Picture mode, in which a quarter-plane screen model is used.
Printing characters replace instead of inserting themselves with motion
afterwards settable by these commands:
  C-c <	  Move left after insertion.
  C-c >	  Move right after insertion.
  C-c ^	  Move up after insertion.
  C-c .	  Move down after insertion.
  C-c `	  Move northwest (nw) after insertion.
  C-c '	  Move northeast (ne) after insertion.
  C-c /	  Move southwest (sw) after insertion.
  C-c \\   Move southeast (se) after insertion.
The current direction is displayed in the mode line.  The initial
direction is right.  Whitespace is inserted and tabs are changed to
spaces when required by movement.  You can move around in the buffer
with these commands:
  C-p	  Move vertically to SAME column in previous line.
  C-n	  Move vertically to SAME column in next line.
  C-e	  Move to column following last non-whitespace character.
  C-f	  Move right inserting spaces if required.
  C-b	  Move left changing tabs to spaces if required.
  C-c C-f Move in direction of current picture motion.
  C-c C-b Move in opposite direction of current picture motion.
  Return  Move to beginning of next line.
You can edit tabular text with these commands:
  M-Tab	  Move to column beneath (or at) next interesting character.
	    `Indents' relative to a previous line.
  Tab	  Move to next stop in tab stop list.
  C-c Tab Set tab stops according to context of this line.
	    With ARG resets tab stops to default (global) value.
	    See also documentation of variable	picture-tab-chars
	    which defines \"interesting character\".  You can manually
	    change the tab stop list with command \\[edit-tab-stops].
You can manipulate text with these commands:
  C-d	  Clear (replace) ARG columns after point without moving.
  C-c C-d Delete char at point - the command normally assigned to C-d.
  Delete  Clear (replace) ARG columns before point, moving back over them.
  C-k	  Clear ARG lines, advancing over them.	 The cleared
	    text is saved in the kill ring.
  C-o	  Open blank line(s) beneath current line.
You can manipulate rectangles with these commands:
  C-c C-k Clear (or kill) a rectangle and save it.
  C-c C-w Like C-c C-k except rectangle is saved in named register.
  C-c C-y Overlay (or insert) currently saved rectangle at point.
  C-c C-x Like C-c C-y except rectangle is taken from named register.
  \\[copy-rectangle-to-register]   Copies a rectangle to a register.
  \\[advertised-undo]   Can undo effects of rectangle overlay commands
	    commands if invoked soon enough.
You can return to the previous mode with:
  C-c C-c Which also strips trailing whitespace from every line.
	    Stripping is suppressed by supplying an argument.

Entry to this mode calls the value of  edit-picture-hook  if non-nil.

Note that Picture mode commands will work outside of Picture mode, but
they are not defaultly assigned to keys."
  (interactive)
  (if (eq major-mode 'edit-picture)
      (error "You are already editing a Picture.")
    (make-local-variable 'picture-mode-old-local-map)
    (setq picture-mode-old-local-map (current-local-map))
    (use-local-map picture-mode-map)
    (make-local-variable 'picture-mode-old-mode-name)
    (setq picture-mode-old-mode-name mode-name)
    (make-local-variable 'picture-mode-old-major-mode)
    (setq picture-mode-old-major-mode major-mode)
    (setq major-mode 'edit-picture)
    (make-local-variable 'picture-killed-rectangle)
    (setq picture-killed-rectangle nil)
    (make-local-variable 'tab-stop-list)
    (setq tab-stop-list (default-value 'tab-stop-list))
    (make-local-variable 'picture-tab-chars)
    (setq picture-tab-chars (default-value 'picture-tab-chars))
    (make-local-variable 'picture-vertical-step)
    (make-local-variable 'picture-horizontal-step)
    (picture-set-motion 0 1)
    (run-hooks 'edit-picture-hook)
    (message
     (substitute-command-keys
      "Type \\[picture-mode-exit] in this buffer to return it to %s mode.")
     picture-mode-old-mode-name)))

(fset 'picture-mode 'edit-picture)	; for the confused

(defun picture-mode-exit (&optional nostrip)
  "Undo edit-picture and return to previous major mode.
With no argument strips whitespace from end of every line in Picture buffer
  otherwise just return to previous mode."
  (interactive "P")
  (if (not (eq major-mode 'edit-picture))
      (error "You aren't editing a Picture.")
    (if (not nostrip) (picture-clean))
    (setq mode-name picture-mode-old-mode-name)
    (use-local-map picture-mode-old-local-map)
    (setq major-mode picture-mode-old-major-mode)
    (kill-local-variable 'tab-stop-list)
    ;; Kludge - force the mode line to be updated.  Is there a better
    ;; way to do this?
    (set-buffer-modified-p (buffer-modified-p))))

(defun picture-clean ()
  "Eliminate whitespace at ends of lines."
  (save-excursion
   (goto-char (point-min))
   (while (re-search-forward "[ \t][ \t]*$" nil t)
     (delete-region (match-beginning 0) (point)))))
