;; Read in and display parts of Unix manual.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

(defun manual-entry (topic &optional section)
  "Display the Unix manual entry for TOPIC.
TOPIC is either the title of the entry, or has the form TITLE(SECTION)
where SECTION is the desired section of the manual, as in `tty(4)'."
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
      (let ((dirlist manual-formatted-dirlist)
	    (case-fold-search nil)
	    name)
	(if (and section (or (file-exists-p
			       (setq name (concat manual-formatted-dir-prefix
						  (substring section 0 1)
						  "/"
						  topic "." section)))
			     (file-exists-p
			       (setq name (concat manual-formatted-dir-prefix
						  section
						  "/"
						  topic "." section)))))
	    (insert-man-file name)
	  (while dirlist
	    (let* ((dir (car dirlist))
		   (name1 (concat dir "/" topic "."
				  (or section
				      (substring
					dir
					(1+ (or (string-match "\\.[^./]*$" dir)
						-2))))))
		   completions)
	      (if (file-exists-p name1)
		  (insert-man-file name1)
		(condition-case ()
		    (progn
		      (setq completions (file-name-all-completions
					 (concat topic "." (or section ""))
					 dir))
		      (while completions
			(insert-man-file (concat dir "/" (car completions)))
			(setq completions (cdr completions))))
		  (file-error nil)))
	      (goto-char (point-max)))
	    (setq dirlist (cdr dirlist)))))

      (if (= (buffer-size) 0)
	  (progn
	    (message "No formatted entry, invoking man %s%s..."
		     (if section (concat section " ") "") topic)
	    (if section
		(call-process manual-program nil t nil section topic)
	        (call-process manual-program nil t nil topic))
	    (if (< (buffer-size) 80)
		(progn
		  (goto-char (point-min))
		  (end-of-line)
		  (error (buffer-substring 1 (point)))))))

      (message "Cleaning manual entry for %s..." topic)
      (nuke-nroff-bs)
      (set-buffer-modified-p nil)
      (message ""))))

;; Hint: BS stands form more things than "back space"
(defun nuke-nroff-bs ()
  (interactive "*")
  ;; Nuke underlining and overstriking (only by the same letter)
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (let* ((preceding (char-after (- (point) 2)))
	   (following (following-char)))
      (cond ((= preceding following)
	     ;; x\bx
	     (delete-char -2))
	    ((= preceding ?\_)
	     ;; _\b
	     (delete-char -2))
	    ((= following ?\_)
	     ;; \b_
	     (delete-region (1- (point)) (1+ (point)))))))

  ;; Nuke headers: "MORE(1) UNIX Programmer's Manual MORE(1)"
  (goto-char (point-min))
  (while (re-search-forward "^ *\\([A-Za-z][-_A-Za-z0-9]*([0-9A-Z]+)\\).*\\1$" nil t)
    (replace-match ""))
  
  ;; Nuke footers: "Printed 12/3/85	27 April 1981	1"
  ;;    Sun appear to be on drugz:
  ;;     "Sun Release 3.0B  Last change: 1 February 1985     1"
  ;;    HP are even worse!
  ;;     "     Hewlett-Packard   -1- (printed 12/31/99)"  FMHWA12ID!!
  ;;    System V (well WICATs anyway):
  ;;     "Page 1			  (printed 7/24/85)"
  ;;    Who is administering PCP to these corporate bozos?
  (goto-char (point-min))
  (while (re-search-forward
	   (cond ((eq system-type 'hpux)
		  "^[ \t]*Hewlett-Packard\\(\\| Company\\)[ \t]*- [0-9]* -.*$")
		 ((eq system-type 'usg-unix-v)
		  "^ *Page [0-9]*.*(printed [0-9/]*)$")
		 (t
		  "^\\(Printed\\|Sun Release\\) [0-9].*[0-9]$"))
	   nil t)
    (replace-match ""))

  ;; Crunch blank lines
  (goto-char (point-min))
  (while (re-search-forward "\n\n\n\n*" nil t)
    (replace-match "\n\n"))

  ;; Nuke blanks lines at start.
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (delete-region (point-min) (point)))


(defun insert-man-file (name)
  ;; Insert manual file (unpacked as necessary) into buffer
  (if (or (equal (substring name -2) ".Z")
	  (string-match "/cat[0-9][a-z]?\\.Z/" name))
      (call-process "zcat" name t nil)
    (if (equal (substring name -2) ".z")
	(call-process "pcat" nil t nil name)
      (insert-file-contents name))))
