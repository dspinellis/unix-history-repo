;; Cursor definitions for Sun windows
;; Copyright (C) 1987 Free Software Foundation, Inc.

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

;;;
;;; Added some more cursors and moved the hot spots
;;; Cursor defined by 16 pairs of 16-bit numbers
;;;
;;; 9-dec-86 Jeff Peck, Sun Microsystems Inc. <peck@sun.com>

(provide 'sm-cursors)

(defvar sc::cursors nil "List of known cursors")

(defmacro defcursor (name x y string)
  (if (not (memq name sc::cursors)) 
      (setq sc::cursors (cons name sc::cursors)))
  (list 'defconst name (list 'vector x y string)))

;;; push should be defined in common lisp, but if not use this:
;(defmacro push (v l)
;  "The ITEM is evaluated and consed onto LIST, a list-valued atom"
;  (list 'setq l (list 'cons v l)))

;;;
;;; The standard default cursor
;;;
(defcursor sc:right-arrow 15 0
  (concat '(0 1 0 3 0 7 0 15 0 31 0 63 0 127 0 15
	      0 27 0 25 0 48 0 48 0 96 0 96 0 192 0 192)))

;;(sc:set-cursor sc:right-arrow)

(defcursor sc:fat-left-arrow 0 8
  (concat '(1 0 3 0 7 0 15 0 31 0 63 255 127 255 255 255
	      255 255 127 255 63 255 31 0 15 0 7 0 3 0 1 0)))

(defcursor sc:box 8 8
  (concat '(15 252 8 4 8 4 8 4 8 4 8 4 8 4 8 4
	       8 132 8 4 8 4 8 4 8 4 8 4 8 4 15 252)))

(defcursor sc:hourglass 8 8
  (concat "\177\376\100\002\040\014\032\070"
	  "\017\360\007\340\003\300\001\200"
	  "\001\200\002\100\005\040\010\020"
	  "\021\210\043\304\107\342\177\376"))

(defun sc:set-cursor (icon)
  "Change the Sun mouse cursor to ICON.
If ICON is nil, switch to the system default cursor,
Otherwise, ICON should be a vector or the name of a vector of [x y 32-chars]"
  (interactive "XIcon Name: ")
  (if (symbolp icon) (setq icon (symbol-value icon)))
  (sun-change-cursor-icon icon))

(make-local-variable '*edit-icon*)
(make-variable-buffer-local 'icon-edit)
(setq-default icon-edit nil)
(or (assq 'icon-edit minor-mode-alist)
    (push '(icon-edit " IconEdit") minor-mode-alist))

(defun sc:edit-cursor (icon)
  "convert icon to rectangle, edit, and repack"
  (interactive "XIcon Name: ")
  (if (not icon) (setq icon (sc::menu-choose-cursor (selected-window) 1 1)))
  (if (symbolp icon) (setq icon (symbol-value icon)))
  (if (get-buffer "icon-edit") (kill-buffer "icon-edit"))
  (switch-to-buffer "icon-edit")
  (local-set-mouse '(text right) 'sc::menu-function)
  (local-set-mouse '(text left) '(sc::pic-ins-at-mouse 32))
  (local-set-mouse '(text middle) '(sc::pic-ins-at-mouse 64))
  (local-set-mouse '(text left middle) 'sc::hotspot)
  (sc::display-icon icon)
  (picture-mode)
  (setq icon-edit t)	; for mode line display
)

(defun sc::pic-ins-at-mouse (char)
  "Picture insert char at mouse location"
  (mouse-move-point *mouse-window* (min 15 *mouse-x*) (min 15 *mouse-y*))
  (move-to-column-force (1+ (min 15 (current-column))))
  (delete-char -1)
  (insert char)
  (sc::goto-hotspot))
    
(defun sc::menu-function (window x y)
  (sun-menu-evaluate window (1+ x) y sc::menu))

(defmenu sc::menu
  ("Cursor Menu")
  ("Pack & Use" sc::pack-buffer-to-cursor)
  ("Pack to Icon" sc::pack-buffer-to-icon 
		  (sc::menu-choose-cursor *menu-window* *menu-x* *menu-y*))
  ("New Icon" call-interactively 'sc::make-cursor)
  ("Edit Icon" sc:edit-cursor 
	       (sc::menu-choose-cursor *menu-window* *menu-x* *menu-y*))
  ("Set Cursor" sc:set-cursor
		(sc::menu-choose-cursor *menu-window* *menu-x* *menu-y*)) 
  ("Reset Cursor" sc:set-cursor nil)
  ("Help". sc::edit-icon-help-menu)
  ("Quit" sc::quit-edit)
  )

(defun sc::quit-edit ()
  (interactive)
  (bury-buffer (current-buffer))
  (switch-to-buffer (other-buffer) 'no-record))

(defun sc::make-cursor (symbol)
  (interactive "SIcon Name: ")
  (eval (list 'defcursor symbol 0 0 ""))
  (sc::pack-buffer-to-icon (symbol-value symbol)))

(defmenu sc::edit-icon-help-menu
  ("Simple Icon Editor")
  ("Left     => CLEAR")
  ("Middle   => SET")
  ("L & M    => HOTSPOT")
  ("Right    => MENU"))

(defun sc::edit-icon-help ()
  (message "Left=> CLEAR  Middle=> SET  Left+Middle=> HOTSPOT  Right=> MENU"))

(defun sc::pack-buffer-to-cursor ()
    (sc::pack-buffer-to-icon *edit-icon*)
    (sc:set-cursor *edit-icon*))

(defun sc::menu-choose-cursor (window x y)
  "Presents a menu of cursor names, and returns one or nil"
  (let ((curs sc::cursors) 
	(items))
    (while curs
      (push (sc::menu-item-for-cursor (car curs)) items)
      (setq curs (cdr curs)))
    (push (list "Choose Cursor") items)
    (setq menu (menu-create items))
    (sun-menu-evaluate window x y menu)))

(defun sc::menu-item-for-cursor (cursor)
  "apply function to selected cursor"
  (list (symbol-name cursor) 'quote cursor))

(defun sc::hotspot (window x y)
  (aset *edit-icon* 0 x)
  (aset *edit-icon* 1 y)
  (sc::goto-hotspot))

(defun sc::goto-hotspot ()
  (goto-line (1+ (aref *edit-icon* 1)))
  (move-to-column (aref *edit-icon* 0)))

(defun sc::display-icon (icon)
  (setq *edit-icon* (copy-sequence icon))
  (let ((string (aref *edit-icon* 2))
	(index 0))
    (while (< index 32)
      (let ((char (aref string index))
	    (bit 128))
	(while (> bit 0)
	  (insert (sc::char-at-bit char bit))
	  (setq bit (lsh bit -1))))
      (if (eq 1 (mod index 2)) (newline))
      (setq index (1+ index))))
  (sc::goto-hotspot))

(defun sc::char-at-bit (char bit)
  (if (> (logand char bit) 0) "@" " "))

(defun sc::pack-buffer-to-icon (icon)
  "Pack 16 x 16 field into icon string"
  (goto-char (point-min))
  (aset icon 0 (aref *edit-icon* 0))
  (aset icon 1 (aref *edit-icon* 1))
  (aset icon 2 (mapconcat 'sc::pack-one-line "1234567890123456" ""))
  (sc::goto-hotspot)
  )
  
(defun sc::pack-one-line (dummy)
  (let* (char chr1 chr2)
    (setq char 0 chr1 (mapconcat 'sc::pack-one-char "12345678" "") chr1 char)
    (setq char 0 chr2 (mapconcat 'sc::pack-one-char "12345678" "") chr2 char)
    (forward-line 1)
    (concat (char-to-string chr1) (char-to-string chr2))
    ))

(defun sc::pack-one-char (dummy)
  "pack following char into char, unless eolp"
  (if (or (eolp) (char-equal (following-char) 32))
      (setq char (lsh char 1)) 
    (setq char (1+ (lsh char 1))))
  (if (not (eolp))(forward-char)))

