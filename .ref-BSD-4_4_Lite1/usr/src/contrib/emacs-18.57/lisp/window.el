;; GNU Emacs window commands aside from those written in C.
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


(defun split-window-vertically (&optional arg)
  "Split current window into two windows, one above the other.
This window becomes the uppermost of the two, and gets
ARG lines.  No arg means split equally."
  (interactive "P")
  (split-window nil (and arg (prefix-numeric-value arg))))

(defun split-window-horizontally (&optional arg)
  "Split current window into two windows side by side.
This window becomes the leftmost of the two, and gets
ARG columns.  No arg means split equally."
  (interactive "P")
  (split-window nil (and arg (prefix-numeric-value arg)) t))

(defun enlarge-window-horizontally (arg)
  "Make current window ARG columns wider."
  (interactive "p")
  (enlarge-window arg t))

(defun shrink-window-horizontally (arg)
  "Make current window ARG columns narrower."
  (interactive "p")
  (shrink-window arg t))

(define-key ctl-x-map "2" 'split-window-vertically)
(define-key ctl-x-map "5" 'split-window-horizontally)
(define-key ctl-x-map "}" 'enlarge-window-horizontally)
(define-key ctl-x-map "{" 'shrink-window-horizontally)
