;; Basic lisp subroutines for Emacs
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


(defun read-quoted-char (&optional prompt)
  "Like  read-char, except that if the first character read is an octal
digit, we read up to two more octal digits and return the character
represented by the octal number consisting of those digits"
  (let ((count 0) (code 0) char)
    (while (< count 3)
      (let ((inhibit-quit (zerop count))
	    (help-form nil))
	(and prompt (message "%s-" prompt))
	(setq char (read-char))
	(if inhibit-quit (setq quit-flag nil)))
      (cond ((null char))
	    ((and (<= ?0 char) (<= char ?7))
	     (setq code (+ (* code 8) (- char ?0))
		   count (1+ count))
	     (and prompt (message (setq prompt
					(format "%s %c" prompt char)))))
	    ((> count 0)
	     (setq unread-command-char char count 259))
	    (t (setq code char count 259))))
    (logand 255 code)))

(defun error (&rest format-args)
  "Signal an error, making error message by passing all args to  format."
  (while t
    (signal 'error (list (apply 'format format-args)))))

(defun undefined ()
  (interactive)
  (ding))

;Prevent the \{...} documentation construct
;from mentioning keys that run this command.
(put 'undefined 'suppress-keymap t)

(defun suppress-keymap (map &optional nodigits)
  "Make MAP override all buffer-modifying commands to be undefined.
Works by knowing which commands are normally buffer-modifying.
Normally also makes digits set numeric arg,
but optional second arg NODIGITS non-nil prevents this."
  (let ((i ? ))
    (while (< i 127)
      (aset map i 'undefined)
      (setq i (1+ i))))
  (or nodigits
      (let (loop)
	(aset map ?- 'negative-argument)
	;; Make plain numbers do numeric args.
	(setq loop ?0)
	(while (<= loop ?9)
	  (aset map loop 'digit-argument)
	  (setq loop (1+ loop))))))

(defun copy-alist (alist)
  "Return a copy of ALIST.
This is a new alist which represents the same mapping
from objects to objects, but does not share the alist structure with ALIST.
The objects mapped (cars and cdrs of elements of the alist)
are shared, however."
  (setq alist (copy-sequence alist))
  (let ((tail alist))
    (while tail
      (if (consp (car tail))
	  (setcar tail (cons (car (car tail)) (cdr (car tail)))))
      (setq tail (cdr tail))))
  alist)

(defun copy-keymap (keymap)
  "Return a copy of KEYMAP"  
  (while (not (keymapp keymap))
    (setq keymap (signal 'wrong-type-argument (list 'keymapp keymap))))
  (if (vectorp keymap)
      (copy-sequence keymap)
      (copy-alist keymap)))

(fset 'beep 'ding) ;preserve lingual purtity

;; Avoids useless byte-compilation.
;; In the future, would be better to fix byte compiler
;; not to really compile in cases like this,
;; and use defun here.
(fset 'ignore '(lambda (&rest ignore) nil))

; old name
(fset 'make-syntax-table 'copy-syntax-table)


(defun run-hooks (&rest hooklist)
  "Takes hook names and runs each one in turn.  Major mode functions use this.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value is called
with no arguments to run the hook."
  (while hooklist
    (let ((sym (car hooklist)))
      (and (boundp sym)
	   (symbol-value sym)
	   (funcall (symbol-value sym))))
    (setq hooklist (cdr hooklist))))
