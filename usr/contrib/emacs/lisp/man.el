;; Read in and display parts of Unix manual.
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


(defun manual-entry (topic &optional section)
  "Display the Unix manual entry for TOPIC."
  (interactive "sManual entry (topic): ")
  (if (and (null section)
	   (string-match "\\`[ \t]*\\([^( \t]+\\)[ \t]*(\\(.+\\))[ \t]*\\'" topic))
      (setq section (substring topic (match-beginning 2)
				     (match-end 2))
	    topic (substring topic (match-beginning 1)
				   (match-end 1))))
  (with-output-to-temp-buffer "*Manual Entry*"
    (buffer-flush-undo standard-output)
    (save-excursion
      (set-buffer standard-output)
      (message "Looking for formatted entry for %s%s..."
	       topic (if section (concat "(" section ")") ""))
      (let ((dirlist '("/usr/man/cat1"
		       "/usr/man/cat2" "/usr/man/cat3" "/usr/man/cat4"
		       "/usr/man/cat5" "/usr/man/cat6" "/usr/man/cat7"
		       "/usr/man/cat8" "/usr/man/catl" "/usr/man/catn"))
	    name)
	(if (and section (file-exists-p
			   (setq name (concat "/usr/man/cat" (aref section 0)
					      "/"
					      topic "." section))))
	    (insert-file-contents name)
	  (while dirlist
	    (let* ((dir (car dirlist))
		   (name1 (concat dir "/"
				  topic "." (or section (substring dir -1))))
		   completions)
	      (if (file-exists-p name1)
		  (insert-file-contents name1)
		(condition-case ()
		    (progn
		      (setq completions (file-name-all-completions topic dir))
		      (while completions
			(insert-file-contents (concat dir "/" (car completions)))
			(setq completions (cdr completions))))
		  (file-error nil)))
	      (goto-char (point-max)))
	    (setq dirlist (cdr dirlist)))))

      (if (= (buffer-size) 0)
	  (progn
	    (message "No formatted entry, invoking man %s%s..."
		     (if section (concat section " ") "") topic)
	    (if section
		(call-process "/usr/ucb/man" nil t nil section topic)
	        (call-process "/usr/ucb/man" nil t nil topic))
	    (if (< (buffer-size) 80)
		(progn
		  (goto-char (point-min))
		  (end-of-line)
		  (error (buffer-substring 1 (point)))))))

      (let ((case-fold-search nil))
	(message "Cleaning manual entry for %s..." topic)

	;; Nuke underlining
	(goto-char (point-min))
	(while (search-forward "_\b" nil t)
	  (replace-match ""))

	;; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
	(goto-char (point-min))
	(while (re-search-forward "^[A-Za-z][A-Za-z]*([0-9]*).*)$" nil t)
	  (replace-match ""))

	;; Nuke footers: "Printed 12/3/85	27 April 1981	1"
	(goto-char (point-min))
	(while (re-search-forward "^Printed [0-9].*[0-9]$" nil t)
	  (replace-match ""))

	;; Crunch blank lines
	(goto-char (point-min))
	(while (re-search-forward "\n\n\n\n*" nil t)
	  (replace-match "\n\n"))

	;; Nuke blanks lines at start.
	(goto-char (point-min))
	(skip-chars-forward "\n")
	(delete-region (point-min) (point))

	(set-buffer-modified-p nil)
	(message "")))))

