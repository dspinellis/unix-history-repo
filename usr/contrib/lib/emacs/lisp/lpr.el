;; Print Emacs buffer on line printer.
;; Copyright (C) 1985, 1988 Free Software Foundation, Inc.

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


;(defconst lpr-switches nil
;  "*List of strings to pass as extra switch args to lpr when it is invoked.")

(defvar lpr-command (if (eq system-type 'usg-unix-v)
			"lp" "lpr")
  "Shell command for printing a file")

(defun lpr-buffer ()
  "Print buffer contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (print-region-1 (point-min) (point-max) lpr-switches))

(defun print-buffer ()
  "Print buffer contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (print-region-1 (point-min) (point-max) (cons "-p" lpr-switches)))

(defun lpr-region (start end)
  "Print region contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive "r")
  (print-region-1 start end lpr-switches))

(defun print-region (start end)
  "Print region contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive "r")
  (print-region-1 start end (cons "-p" lpr-switches)))

(defun print-region-1 (start end switches)
  (let ((name (concat (buffer-name) " Emacs buffer"))
	(width tab-width))
    (save-excursion
     (message "Spooling...")
     (if (/= tab-width 8)
	 (let ((oldbuf (current-buffer)))
	  (set-buffer (get-buffer-create " *spool temp*"))
	  (widen) (erase-buffer)
	  (insert-buffer-substring oldbuf start end)
	  (setq tab-width width)
	  (untabify (point-min) (point-max))
	  (setq start (point-min) end (point-max))))
     (apply 'call-process-region
	    (nconc (list start end lpr-command
			 nil nil nil)
		   (nconc (and (eq system-type 'berkeley-unix)
			       (list "-J" name "-T" name))
			  switches)))
     (message "Spooling...done"))))
