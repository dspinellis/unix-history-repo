;; Print Emacs buffer on line printer.
;; Copyright (C) 1985, 1988 Free Software Foundation, Inc.

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
