;; Syntax checker for Mim (MDL).
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


(require 'mim-mode)

(defun slow-syntax-check-mim ()
  "Check Mim syntax slowly.
Points out the context of the error, if the syntax is incorrect."
  (interactive)
  (message "checking syntax...")
  (let ((stop (point-max)) point-stack current last-bracket whoops last-point)
    (save-excursion
      (goto-char (point-min))
      (while (and (not whoops)
		  (re-search-forward "\\s(\\|\\s)\\|\"\\|[\\]" stop t))
	(setq current (preceding-char))
	(cond ((= current ?\")
	       (condition-case nil
		   (progn (re-search-forward "[^\\]\"")
			  (setq current nil))
		 (error (setq whoops (point)))))
	      ((= current ?\\)
	       (condition-case nil (forward-char 1) (error nil)))
	      ((= (char-syntax current) ?\))
	       (if (or (not last-bracket)
		       (not (= (logand (lsh (aref (syntax-table) last-bracket) -8)
				       ?\177)
			       current)))
		   (setq whoops (point))
		 (setq last-point (car point-stack))
		 (setq last-bracket (if last-point (char-after (1- last-point))))
		 (setq point-stack (cdr point-stack))))
	      (t
	       (if last-point (setq point-stack (cons last-point point-stack)))
	       (setq last-point (point))
	       (setq last-bracket current)))))
    (cond ((not (or whoops last-point))
	   (message "Syntax correct"))
	  (whoops
	   (goto-char whoops)
	   (cond ((equal current ?\")
		  (error "Unterminated string"))
		 ((not last-point)
		  (error "Extraneous %s" (char-to-string current)))
		 (t
		  (error "Mismatched %s with %s"
			   (save-excursion
			     (setq whoops (1- (point)))
			     (goto-char (1- last-point))
			     (buffer-substring (point)
					       (min (progn (end-of-line) (point))
						    whoops)))
			   (char-to-string current)))))
	  (t
	   (goto-char last-point)
	   (error "Unmatched %s" (char-to-string last-bracket))))))
      
(defun fast-syntax-check-mim ()
  "Checks Mim syntax quickly.
Answers correct or incorrect, cannot point out the error context."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (state)
      (while (and (not (eobp))
		  (equal (car (setq state (parse-partial-sexp (point) (point-max) 0)))
			 0)))
      (if (equal (car state) 0)
	  (message "Syntax correct")
	(error "Syntax incorrect")))))


	
