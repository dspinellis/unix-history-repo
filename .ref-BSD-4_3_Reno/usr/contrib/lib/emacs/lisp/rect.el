;; Rectangle functions for GNU Emacs.
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


(defun operate-on-rectangle (function start end coerce-tabs)
  "Call FUNCTION for each line of rectangle with corners at START, END.
If COERCE-TABS is non-nil, convert multi-column characters
that span the starting or ending columns on any line
to multiple spaces before calling FUNCTION.
FUNCTION is called with three arguments:
 position of start of segment of this line within the rectangle,
 number of columns that belong to rectangle but are before that position,
 number of columns that belong to rectangle but are after point.
Point is at the end of the segment of this line within the rectangle."
  (let (startcol startlinepos endcol endlinepos)
    (save-excursion
     (goto-char start)
     (setq startcol (current-column))
     (beginning-of-line)
     (setq startlinepos (point)))
    (save-excursion
     (goto-char end)
     (setq endcol (current-column))
     (forward-line 1)
     (setq endlinepos (point-marker)))
    (if (< endcol startcol)
	(let ((tem startcol))
	  (setq startcol endcol endcol tem)))
    (if (/= endcol startcol)
	(save-excursion
	 (goto-char startlinepos)
	 (while (< (point) endlinepos)
	   (let (startpos begextra endextra)
	     (move-to-column startcol)
	     (and coerce-tabs
		  (> (current-column) startcol)
		  (rectangle-coerce-tab startcol))
	     (setq begextra (- (current-column) startcol))
	     (setq startpos (point))
	     (move-to-column endcol)
	     (if (> (current-column) endcol)
		 (if coerce-tabs
		     (rectangle-coerce-tab endcol)
		   (forward-char -1)))
	     (setq endextra (- endcol (current-column)))
	     (if (< begextra 0)
		 (setq endextra (+ endextra begextra)
		       begextra 0))
	     (funcall function startpos begextra endextra))
	   (forward-line 1))))
    (- endcol startcol)))

(defun delete-rectangle-line (startdelpos ignore ignore)
  (delete-region startdelpos (point)))

(defun delete-extract-rectangle-line (startdelpos begextra endextra)
  (save-excursion
   (extract-rectangle-line startdelpos begextra endextra))
  (delete-region startdelpos (point)))

(defun extract-rectangle-line (startdelpos begextra endextra)
  (let ((line (buffer-substring startdelpos (point)))
	(end (point)))
    (goto-char startdelpos)
    (while (search-forward "\t" end t)
      (let ((width (- (current-column)
		      (save-excursion (forward-char -1)
				      (current-column)))))
	(setq line (concat (substring line 0 (- (point) end 1))
			   (spaces-string width)
			   (substring line (+ (length line) (- (point) end)))))))
    (if (or (> begextra 0) (> endextra 0))
	(setq line (concat (spaces-string begextra)
			   line
			   (spaces-string endextra))))
    (setq lines (cons line lines))))

(defconst spaces-strings
  '["" " " "  " "   " "    " "     " "      " "       " "        "])

(defun spaces-string (n)
  (if (<= n 8) (aref spaces-strings n)
    (let ((val ""))
      (while (> n 8)
	(setq val (concat "        " val)
	      n (- n 8)))
      (concat val (aref spaces-strings n)))))
    
(defun delete-rectangle (start end)
  "Delete (don't save) text in rectangle with point and mark as corners.
The same range of columns is deleted in each line
starting with the line where the region begins
and ending with the line where the region ends."
  (interactive "r")
  (operate-on-rectangle 'delete-rectangle-line start end t))

(defun delete-extract-rectangle (start end)
  "Return and delete contents of rectangle with corners at START and END.
Value is list of strings, one for each line of the rectangle."
  (let (lines)
    (operate-on-rectangle 'delete-extract-rectangle-line
			  start end t)
    (nreverse lines)))

(defun extract-rectangle (start end)
  "Return contents of rectangle with corners at START and END.
Value is list of strings, one for each line of the rectangle."
  (let (lines)
    (operate-on-rectangle 'extract-rectangle-line start end nil)
    (nreverse lines)))

(defvar killed-rectangle nil
  "Rectangle for yank-rectangle to insert.")

(defun kill-rectangle (start end)
  "Delete rectangle with corners at point and mark; save as last killed one.
Calling from program, supply two args START and END, buffer positions.
But in programs you might prefer to use delete-extract-rectangle."
  (interactive "r")
  (setq killed-rectangle (delete-extract-rectangle start end)))

(defun yank-rectangle ()
  "Yank the last killed rectangle with upper left corner at point."
  (interactive)
  (insert-rectangle killed-rectangle))

(defun insert-rectangle (rectangle)
  "Insert text of RECTANGLE with upper left corner at point.
RECTANGLE's first line is inserted at point,
its second line is inserted at a point vertically under point, etc.
RECTANGLE should be a list of strings."
  (let ((lines rectangle)
	(insertcolumn (current-column))
	(first t))
    (while lines
      (or first
	  (progn
	   (forward-line 1)
	   (or (bolp) (insert ?\n))
	   (move-to-column insertcolumn)
	   (if (> (current-column) insertcolumn)
	       (rectangle-coerce-tab insertcolumn))
	   (if (< (current-column) insertcolumn)
	       (indent-to insertcolumn))))
      (setq first nil)
      (insert (car lines))
      (setq lines (cdr lines)))))

(defun open-rectangle (start end)
  "Blank out rectangle with corners at point and mark, shifting text right.
The text previously in the region is not overwritten by the blanks,
but insted winds up to the right of the rectangle."
  (interactive "r")
  (operate-on-rectangle 'open-rectangle-line start end nil))

(defun open-rectangle-line (startpos begextra endextra)
  (let ((column (+ (current-column) begextra endextra)))
    (goto-char startpos)
    (let ((ocol (current-column)))
      (skip-chars-forward " \t")
      (setq column (+ column (- (current-column) ocol))))
    (delete-region (point)
                   (progn (skip-chars-backward " \t")
			  (point)))
    (indent-to column)))

(defun clear-rectangle (start end)
  "Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks."
  (interactive "r")
  (operate-on-rectangle 'clear-rectangle-line start end t))

(defun clear-rectangle-line (startpos begextra endextra)
  (skip-chars-forward " \t")
  (let ((column (+ (current-column) endextra)))
    (delete-region (point)
                   (progn (goto-char startpos)
			  (skip-chars-backward " \t")
			  (point)))
    (indent-to column)))

(defun rectangle-coerce-tab (column)
  (let ((aftercol (current-column))
	(indent-tabs-mode nil))
    (delete-char -1)
    (indent-to aftercol)
    (backward-char (- aftercol column))))
