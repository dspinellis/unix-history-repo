;; Utility functions used both by rmail and rnews
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


(provide 'mail-utils)
		     
;; should be in loaddefs
(defvar mail-use-rfc822 nil
  "*If non-nil, use a full, hairy RFC822 parser on mail addresses.
Otherwise, (the default) use a smaller, somewhat faster and
often-correct parser.")

(defun mail-string-delete (string start end)
  "Returns a string containing all of STRING except the part
from START (inclusive) to END (exclusive)."
  (if (null end) (substring string 0 start)
    (concat (substring string 0 start)
	    (substring string end nil))))

(defun mail-strip-quoted-names (address)
  "Delete comments and quoted strings in an address list ADDRESS.
Also delete leading/trailing whitespace and replace FOO <BAR> with just BAR.
Return a modified address list."
  (if mail-use-rfc822
      (progn (require 'rfc822)
	     (mapconcat 'identity (rfc822-addresses address) ", "))
    (let (pos)
     (string-match "\\`[ \t\n]*" address)
     ;; strip surrounding whitespace
     (setq address (substring address
			      (match-end 0)
			      (string-match "[ \t\n]*\\'" address
					    (match-end 0))))
     ;; strip rfc822 comments
     (while (setq pos (string-match 
			;; This doesn't hack rfc822 nested comments
			;;  `(xyzzy (foo) whinge)' properly.  Big deal.
			"[ \t]*(\\([^)\"\\]\\|\\\\.\\|\\\\\n\\)*)"
			address))
       (setq address
	     (mail-string-delete address
				 pos (match-end 0))))
     ;; strip `quoted' names (This is supposed to hack `"Foo Bar" <bar@host>')
     (setq pos 0)
     (while (setq pos (string-match
			"[ \t]*\"\\([^\"\\]\\|\\\\.\\|\\\\\n\\)*\"[ \t\n]*"
			address pos))
       ;; If the next thing is "@", we have "foo bar"@host.  Leave it.
       (if (and (> (length address) (match-end 0))
		(= (aref address (match-end 0)) ?@))
	   (setq pos (match-end 0))
	 (setq address
	       (mail-string-delete address
				   pos (match-end 0)))))
     ;; Retain only part of address in <> delims, if there is such a thing.
     (while (setq pos (string-match "\\(,\\|\\`\\)[^,]*<\\([^>,]*>\\)"
				    address))
       (let ((junk-beg (match-end 1))
	     (junk-end (match-beginning 2))
	     (close (match-end 0)))
	 (setq address (mail-string-delete address (1- close) close))
	 (setq address (mail-string-delete address junk-beg junk-end))))
     address)))
  
(or (and (boundp 'rmail-default-dont-reply-to-names)
	 (not (null rmail-default-dont-reply-to-names)))
    (setq rmail-default-dont-reply-to-names "info-"))

; rmail-dont-reply-to-names is defined in loaddefs
(defun rmail-dont-reply-to (userids)
  "Returns string of mail addresses USERIDS sans any recipients
that start with matches for  rmail-dont-reply-to-names.
Usenet paths ending in an element that matches are removed also."
  (if (null rmail-dont-reply-to-names)
      (setq rmail-dont-reply-to-names
	    (concat (if rmail-default-dont-reply-to-names
			(concat rmail-default-dont-reply-to-names "\\|")
		        "")
		    (concat (regexp-quote
			      (or (getenv "USER") (getenv "LOGNAME")
				  (user-login-name)))
			    "\\>"))))
  (let ((match (concat "\\(^\\|,\\)[ \t\n]*\\([^,\n]*!\\|\\)\\("
		       rmail-dont-reply-to-names
		       "\\)"))
	(case-fold-search t)
	pos epos)
    (while (setq pos (string-match match userids))
      (if (> pos 0) (setq pos (1+ pos)))
      (setq epos
	    (if (string-match "[ \t\n,]+" userids (match-end 0))
		(match-end 0)
	      (length userids)))
      (setq userids
	    (mail-string-delete
	      userids pos epos)))
    ;; get rid of any trailing commas
    (if (setq pos (string-match "[ ,\t\n]*\\'" userids))
	(setq userids (substring userids 0 pos)))
    ;; remove leading spaces. they bother me.
    (if (string-match "\\s *" userids)
	(substring userids (match-end 0))
      userids)))

(defun mail-fetch-field (field-name &optional last all)
  "Return the value of the header field FIELD.
The buffer is expected to be narrowed to just the headers of the message.
If 2nd arg LAST is non-nil, use the last such field if there are several.
If 3rd arg ALL is non-nil, concatenate all such fields, with commas between."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (name (concat "^" (regexp-quote field-name) "[ \t]*:[ \t]*")))
      (goto-char (point-min))
      (if all
	  (let ((value ""))
	    (while (re-search-forward name nil t)
	      (let ((opoint (point)))
		(while (progn (forward-line 1)
			      (looking-at "[ \t]")))
		(setq value (concat value
				    (if (string= value "") "" ", ")
				    (buffer-substring opoint (1- (point)))))))
	    (and (not (string= value "")) value))
	(if (re-search-forward name nil t)
	    (progn
	      (if last (while (re-search-forward name nil t)))
	      (let ((opoint (point)))
		(while (progn (forward-line 1)
			      (looking-at "[ \t]")))
		(buffer-substring opoint (1- (point))))))))))

;; Parse a list of tokens separated by commas.
;; It runs from point to the end of the visible part of the buffer.
;; Whitespace before or after tokens is ignored,
;; but whitespace within tokens is kept.
(defun mail-parse-comma-list ()
  (let (accumulated
	beg)
    (skip-chars-forward " ")
    (while (not (eobp))
      (setq beg (point))
      (skip-chars-forward "^,")
      (skip-chars-backward " ")
      (setq accumulated
	    (cons (buffer-substring beg (point))
		  accumulated))
      (skip-chars-forward "^,")
      (skip-chars-forward ", "))
    accumulated))

(defun mail-comma-list-regexp (labels)
  (let (pos)
    (setq pos (or (string-match "[^ \t]" labels) 0))
    ;; Remove leading and trailing whitespace.
    (setq labels (substring labels pos (string-match "[ \t]*$" labels pos)))
    ;; Change each comma to \|, and flush surrounding whitespace.
    (while (setq pos (string-match "[ \t]*,[ \t]*" labels))
      (setq labels
	    (concat (substring labels 0 pos)
		    "\\|"
		    (substring labels (match-end 0))))))
  labels)
