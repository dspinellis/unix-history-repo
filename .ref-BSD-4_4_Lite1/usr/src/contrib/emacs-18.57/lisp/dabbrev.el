;; Dynamic abbreviation package for GNU Emacs.
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


; DABBREVS - "Dynamic abbreviations" hack, originally written by Don Morrison
; for Twenex Emacs.  Converted to mlisp by Russ Fish.  Supports the table
; feature to avoid hitting the same expansion on re-expand, and the search
; size limit variable.  Bugs fixed from the Twenex version are flagged by
; comments starting with ;;; .
; 
; converted to elisp by Spencer Thomas.
; Thoroughly cleaned up by Richard Stallman.
;  
; If anyone feels like hacking at it, Bob Keller (Keller@Utah-20) first
; suggested the beast, and has some good ideas for its improvement, but
; doesn?tknow TECO (the lucky devil...).  One thing that should definitely
; be done is adding the ability to search some other buffer(s) if you can?t
; find the expansion you want in the current one.

;; (defun dabbrevs-help ()
;;   "Give help about dabbrevs."
;;   (interactive)
;;   (&info "emacs" "dabbrevs")	; Select the specific info node.
;; )
(provide 'dabbrevs)

(defvar dabbrevs-limit nil
  "*Limits region searched by dabbrevs-expand to that many chars away (local).")
(make-variable-buffer-local 'dabbrevs-limit)

(defvar dabbrevs-backward-only nil
  "*If non-NIL, dabbrevs-expand only looks backwards.")

; State vars for dabbrevs-re-expand.
(defvar last-dabbrevs-table nil
  "Table of expansions seen so far. (local)")
(make-variable-buffer-local 'last-dabbrevs-table)

(defvar last-dabbrevs-abbreviation ""
  "Last string we tried to expand.  Buffer-local.")
(make-variable-buffer-local 'last-dabbrevs-abbreviation)

(defvar last-dabbrevs-direction 0
  "Direction of last dabbrevs search. (local)")
(make-variable-buffer-local 'last-dabbrevs-direction)

(defvar last-dabbrevs-abbrev-location nil
  "Location last abbreviation began (local).")
(make-variable-buffer-local 'last-dabbrevs-abbrev-location)

(defvar last-dabbrevs-expansion nil
    "Last expansion of an abbreviation. (local)")
(make-variable-buffer-local 'last-dabbrevs-expansion)

(defvar last-dabbrevs-expansion-location nil
  "Location the last expansion was found. (local)")
(make-variable-buffer-local 'last-dabbrevs-expansion-location)

(defun dabbrev-expand (arg)
  "Expand previous word \"dynamically\".
Expands to the most recent, preceding word for which this is a prefix.
If no suitable preceding word is found, words following point are considered.

A positive prefix argument, N, says to take the Nth backward DISTINCT
possibility.  A negative argument says search forward.  The variable
dabbrev-backward-only may be used to limit the direction of search to
backward if set non-nil.

If the cursor has not moved from the end of the previous expansion and
no argument is given, replace the previously-made expansion
with the next possible expansion not yet tried."
  (interactive "*P")
  (let (abbrev expansion old which loc n pattern
	(do-case (and case-fold-search case-replace)))
    ;; abbrev -- the abbrev to expand
    ;; expansion -- the expansion found (eventually) or nil until then
    ;; old -- the text currently in the buffer
    ;;    (the abbrev, or the previously-made expansion)
    ;; loc -- place where expansion is found
    ;;    (to start search there for next expansion if requested later)
    ;; do-case -- nil if should consider case significant.
    (save-excursion
      (if (and (null arg)
	       (eq last-command this-command)
	       last-dabbrevs-abbrev-location)
	  (progn
	    (setq abbrev last-dabbrevs-abbreviation)
	    (setq old last-dabbrevs-expansion)
	    (setq which last-dabbrevs-direction))
	(setq which (if (null arg)
			(if dabbrevs-backward-only 1 0)
		        (prefix-numeric-value arg)))
	(setq loc (point))
	(forward-word -1)
	(setq last-dabbrevs-abbrev-location (point)) ; Original location.
	(setq abbrev (buffer-substring (point) loc))
	(setq old abbrev)
	(setq last-dabbrevs-expansion-location nil)
	(setq last-dabbrev-table nil))  	; Clear table of things seen.

      (setq pattern (concat "\\b" (regexp-quote abbrev) "\\(\\sw\\|\\s_\\)+"))
      ;; Try looking backward unless inhibited.
      (if (>= which 0)
	  (progn 
	    (setq n (max 1 which))
	    (if last-dabbrevs-expansion-location
		(goto-char last-dabbrevs-expansion-location))
	    (while (and (> n 0)
			(setq expansion (dabbrevs-search pattern t do-case)))
	      (setq loc (point-marker))
	      (setq last-dabbrev-table (cons expansion last-dabbrev-table))
	      (setq n (1- n)))
	    (or expansion
		(setq last-dabbrevs-expansion-location nil))
	    (setq last-dabbrevs-direction (min 1 which))))

      (if (and (<= which 0) (not expansion)) ; Then look forward.
	  (progn 
	    (setq n (max 1 (- which)))
	    (if last-dabbrevs-expansion-location
		(goto-char last-dabbrevs-expansion-location))
	    (while (and (> n 0)
			(setq expansion (dabbrevs-search pattern nil do-case)))
	      (setq loc (point-marker))
	      (setq last-dabbrev-table (cons expansion last-dabbrev-table))
	      (setq n (1- n)))
	    (setq last-dabbrevs-direction -1))))

    (if (not expansion)
	(let ((first (string= abbrev old)))
	  (setq last-dabbrevs-abbrev-location nil)
	  (if (not first)
	      (progn (undo-boundary)
		     (delete-backward-char (length old))
		     (insert abbrev)))
	  (error (if first
		     "No dynamic expansion for \"%s\" found."
		     "No further dynamic expansions for \"%s\" found.")
		 abbrev))
      ;; Success: stick it in and return.
      (undo-boundary)
      (search-backward old)
      ;; Make case of replacement conform to case of abbreviation
      ;; provided (1) that kind of thing is enabled in this buffer
      ;; and (2) the replacement itself is all lower case
      ;; except perhaps for the first character.
      (let ((do-case (and do-case
			  (string= (substring expansion 1)
				   (downcase (substring expansion 1))))))
	;; First put back the original abbreviation with its original
	;; case pattern.
	(save-excursion
	  (replace-match abbrev t 'literal))
	(search-forward abbrev)
	(replace-match (if do-case (downcase expansion) expansion)
		       (not do-case)
		       'literal))
      ;; Save state for re-expand.
      (setq last-dabbrevs-abbreviation abbrev)
      (setq last-dabbrevs-expansion expansion)
      (setq last-dabbrevs-expansion-location loc))))

;; Search function used by dabbrevs library.  
;; First arg is string to find as prefix of word.  Second arg is
;; t for reverse search, nil for forward.  Variable dabbrevs-limit
;; controls the maximum search region size.

;; Table of expansions already seen is examined in buffer last-dabbrev-table,
;; so that only distinct possibilities are found by dabbrevs-re-expand.
;; Note that to prevent finding the abbrev itself it must have been
;; entered in the table.

;; Value is the expansion, or nil if not found.  After a successful
;; search, point is left right after the expansion found.

(defun dabbrevs-search (pattern reverse do-case)
  (let (missing result)
    (save-restriction 	    ; Uses restriction for limited searches.
      (if dabbrevs-limit
	  (narrow-to-region last-dabbrevs-abbrev-location
			    (+ (point)
			       (* dabbrevs-limit (if reverse -1 1)))))
      ;; Keep looking for a distinct expansion.
      (setq result nil)
      (setq missing nil)
      (while  (and (not result) (not missing))
	; Look for it, leave loop if search fails.
	(setq missing
	      (not (if reverse
		       (re-search-backward pattern nil t)
		       (re-search-forward pattern nil t))))

	(if (not missing)
	    (progn
	      (setq result (buffer-substring (match-beginning 0)
					     (match-end 0)))
	      (let* ((test last-dabbrev-table))
		(while (and test
			    (not
			     (if do-case
				 (string= (downcase (car test)) (downcase result))
			       (string= (car test) result))))
		  (setq test (cdr test)))
		(if test (setq result nil))))))	; if already in table, ignore
      result)))
