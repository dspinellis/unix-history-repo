;; Register commands for Emacs.
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


(defvar register-alist nil
  "Alist of elements (NAME . CONTENTS), one for each Emacs register.
NAME is a character (a number).  CONTENTS is a string, number,
mark or list.  A list represents a rectangle; its elements are strings.")

(defun get-register (char)
  "Return contents of Emacs register named CHAR, or nil if none."
  (cdr (assq char register-alist)))

(defun set-register (char value)
  "Set contents of Emacs register named CHAR to VALUE."
  (let ((aelt (assq char register-alist)))
    (if aelt
	(setcdr aelt value)
      (setq aelt (cons char value))
      (setq register-alist (cons aelt register-alist)))))

(defun point-to-register (char)
  "Store current location of point in a register.
Argument is a character, naming the register."
  (interactive "cPoint to register: ")
  (set-register char (point-marker)))

(defun register-to-point (char)
  "Move point to location stored in a register.
Argument is a character, naming the register."
  (interactive "cRegister to point: ")
  (let ((val (get-register char)))
    (if (markerp val)
	(progn
	  (switch-to-buffer (marker-buffer val))
	  (goto-char val))
      (error "Register doesn't contain a buffer position"))))

;(defun number-to-register (arg char)
;  "Store a number in a register.
;Two args, NUMBER and REGISTER (a character, naming the register).
;If NUMBER is nil, digits in the buffer following point are read
;to get the number to store.
;Interactively, NUMBER is the prefix arg (none means nil)."
;  (interactive "P\ncNumber to register: ")
;  (set-register char 
;		(if arg
;		    (prefix-numeric-value arg)
;		  (if (looking-at "[0-9][0-9]*")
;		      (save-excursion
;		       (save-restriction
;			(narrow-to-region (point)
;					  (progn (skip-chars-forward "0-9")
;						 (point)))
;			(goto-char (point-min))
;			(read (current-buffer))))
;		    0))))

;(defun increment-register (arg char)
;  "Add NUMBER to the contents of register REGISTER.
;Interactively, NUMBER is the prefix arg (none means nil)." 
;  (interactive "p\ncNumber to register: ")
;  (or (integerp (get-register char))
;      (error "Register does not contain a number"))
;  (set-register char (+ arg (get-register char))))

(defun view-register (char)
  "Display what is contained in register named REGISTER.
REGISTER is a character."
  (interactive "cView register: ")
  (let ((val (get-register char)))
    (if (null val)
	(message "Register %s is empty" (single-key-description char))
      (with-output-to-temp-buffer "*Output*"
	(princ "Register ")
	(princ (single-key-description char))
	(princ " contains ")
	(if (integerp val)
	    (princ val)
	  (if (markerp val)
	      (progn
		(princ "a buffer position:\nbuffer ")
		(princ (buffer-name (marker-buffer val)))
		(princ ", position ")
		(princ (+ 0 val)))
	    (if (consp val)
		(progn
		  (princ "the rectangle:\n")
		  (while val
		    (princ (car val))
		    (terpri)
		    (setq val (cdr val))))
	      (princ "the string:\n")
	      (princ val))))))))

(defun insert-register (char &optional arg)
  "Insert contents of register REG.  REG is a character.
Normally puts point before and mark after the inserted text.
If optional second arg is non-nil, puts mark before and point after.
Interactively, second arg is non-nil if prefix arg is supplied."
  (interactive "cInsert register: \nP")
  (push-mark)
  (let ((val (get-register char)))
    (if (consp val)
	(insert-rectangle val)
      (if (stringp val)
	  (insert val)
	(if (or (integerp val) (markerp val))
	    (princ (+ 0 val) (current-buffer))
	  (error "Register does not contain text")))))
  (or arg (exchange-point-and-mark)))

(defun copy-to-register (char start end &optional delete-flag)
  "Copy region into register REG.
With prefix arg, delete as well.
Called from program, takes four args:
REG, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to copy."
  (interactive "cCopy to register: \nr\nP")
  (set-register char (buffer-substring start end))
  (if delete-flag (delete-region start end)))

(defun append-to-register (char start end &optional delete-flag)
  "Append region to text in register REG.
With prefix arg, delete as well.
Called from program, takes four args:
REG, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to append."
  (interactive "cAppend to register: \nr\nP")
  (or (stringp (get-register char))
      (error "Register does not contain text"))
  (set-register char (concat (get-register char)
			     (buffer-substring start end)))
  (if delete-flag (delete-region start end)))

(defun prepend-to-register (char start end &optional delete-flag)
  "Prepend region to text in register REG.
With prefix arg, delete as well.
Called from program, takes four args:
REG, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to prepend."
  (interactive "cPrepend to register: \nr\nP")
  (or (stringp (get-register char))
      (error "Register does not contain text"))
  (set-register char (concat (buffer-substring start end)
			     (get-register char)))
  (if delete-flag (delete-region start end)))

(defun copy-rectangle-to-register (char start end &optional delete-flag)
  "Copy rectangular region into register REG.
With prefix arg, delete as well.
Called from program, takes four args:
REG, START, END and DELETE-FLAG.
START and END are buffer positions giving two corners of rectangle."
  (interactive "cCopy rectangle to register: \nr\nP")
  (set-register char
		(if delete-flag
		    (delete-extract-rectangle start end)
		  (extract-rectangle start end))))
