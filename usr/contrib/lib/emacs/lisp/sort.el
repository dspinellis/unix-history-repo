;; Commands to sort text in an Emacs buffer.
;; Copyright (C) 1986, 1987 Free Software Foundation, Inc.

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

(provide 'sort)

;; Original version of most of this contributed by Howie Kaye

(defun sort-subr (reverse nextrecfun endrecfun &optional startkeyfun endkeyfun)
  "General text sorting routine to divide buffer into records and sort them.
Arguments are REVERSE NEXTRECFUN ENDRECFUN &optional STARTKEYFUN ENDKEYFUN.

We consider this portion of the buffer to be divided into disjoint pieces
called sort records.  A portion of each sort record (perhaps all of it)
is designated as the sort key.  The records are rearranged in the buffer
in order by their sort keys.  The records may or may not be contiguous.

Usually the records are rearranged in order of ascending sort key.
If REVERSE is non-nil, they are rearranged in order of descending sort key.

The next four arguments are functions to be called to move point
across a sort record.  They will be called many times from within sort-subr.

NEXTRECFUN is called with point at the end of the previous record.
It moves point to the start of the next record.
The first record is assumed to start at the position of point when sort-subr
is called.

ENDRECFUN is is called with point within the record.
It should move point to the end of the record.

STARTKEYFUN may moves from the start of the record to the start of the key.
It may return either return a non-nil value to be used as the key, or
else the key will be the substring between the values of point after
STARTKEYFUNC and ENDKEYFUN are called.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDRECFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN."
  (save-excursion
    (message "Finding sort keys...")
    (let* ((sort-lists (sort-build-lists nextrecfun endrecfun
					startkeyfun endkeyfun))
	   (old (reverse sort-lists)))
      (if (null sort-lists)
	  ()
	(or reverse (setq sort-lists (nreverse sort-lists)))
	(message "Sorting records...")
	(setq sort-lists
	      (if (fboundp 'sortcar)
		  (sortcar sort-lists
			   (cond ((numberp (car (car sort-lists)))
				  '<)
				 ((consp (car (car sort-lists)))
				  'buffer-substring-lessp)
				 (t
				  'string<)))
		  (sort sort-lists
			(cond ((numberp (car (car sort-lists)))
			       (function
				(lambda (a b)
				  (< (car a) (car b)))))
			      ((consp (car (car sort-lists)))
			       (function
				(lambda (a b)
				  (buffer-substring-lessp (car a) (car b)))))
			      (t
			       (function
				(lambda (a b)
				  (string< (car a) (car b)))))))))
	(if reverse (setq sort-lists (nreverse sort-lists)))
	(message "Reordering buffer...")
	(sort-reorder-buffer sort-lists old)))
    (message "Reordering buffer... Done")))

;; Parse buffer into records using the arguments as Lisp expressions;
;; return a list of records.  Each record looks like (KEY STARTPOS ENDPOS)
;; where KEY is the sort key (a number or string),
;; and STARTPOS and ENDPOS are the bounds of this record in the buffer.

;; The records appear in the list lastmost first!

(defun sort-build-lists (nextrecfun endrecfun startkeyfun endkeyfun)
  (let ((sort-lists ())
	(start-rec nil)
	done key)
    ;; Loop over sort records.
    ;(goto-char (point-min)) -- it is the caller's responsibility to
    ;arrange this if necessary
    (while (not (eobp))
      (setq start-rec (point))		;save record start
      (setq done nil)
      ;; Get key value, or move to start of key.
      (setq key (catch 'key
		  (or (and startkeyfun (funcall startkeyfun))
		      ;; If key was not returned as value,
		      ;; move to end of key and get key from the buffer.
		      (let ((start (point)))
			(funcall (or endkeyfun
				     (prog1 endrecfun (setq done t))))
			(if (fboundp 'buffer-substring-lessp)
			    (cons start (point))
			  (buffer-substring start (point)))))))
      ;; Move to end of this record (start of next one, or end of buffer).
      (cond ((prog1 done (setq done nil)))
	    (endrecfun (funcall endrecfun))
	    (nextrecfun (funcall nextrecfun) (setq done t)))
      (if key (setq sort-lists (cons
				 ;; consing optimization in case in which key
				 ;; is same as record.
				 (if (and (consp key)
					  (equal (car key) start-rec)
					  (equal (cdr key) (point)))
				     (cons key key)
				     (list key start-rec (point)))
				sort-lists)))
      (and (not done) nextrecfun (funcall nextrecfun)))
    sort-lists))

(defun sort-reorder-buffer (sort-lists old)
  (let ((inhibit-quit t)
	(last (point-min))
	(min (point-min)) (max (point-max)))
    (while sort-lists
      (goto-char (point-max))
      (insert-buffer-substring (current-buffer)
			       last
			       (nth 1 (car old)))
      (goto-char (point-max))
      (insert-buffer-substring (current-buffer)
			       (nth 1 (car sort-lists))
			       (nth 2 (car sort-lists)))
      (setq last (nth 2 (car old))
	    sort-lists (cdr sort-lists)
	    old (cdr old)))
    (goto-char (point-max))
    (insert-buffer-substring (current-buffer)
			     last
			     max)
    (delete-region min max)))	;get rid of old version

(defun sort-lines (reverse beg end) 
  "Sort lines in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort)."
  (interactive "P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (sort-subr reverse 'forward-line 'end-of-line)))

(defun sort-paragraphs (reverse beg end)
  "Sort paragraphs in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort)."
  (interactive "P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (sort-subr reverse
	       (function (lambda () (skip-chars-forward "\n \t\f")))
	       'forward-paragraph)))

(defun sort-pages (reverse beg end)
  "Sort pages in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort)."
  (interactive "P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (sort-subr reverse
	       (function (lambda () (skip-chars-forward "\n")))
	       'forward-page)))

(defvar sort-fields-syntax-table nil)
(if sort-fields-syntax-table nil
  (let ((table (make-syntax-table))
	(i 0))
    (while (< i 256)
      (modify-syntax-entry i "w" table)
      (setq i (1+ i)))
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\n " " table)
    (setq sort-fields-syntax-table table)))

(defun sort-numeric-fields (field beg end)
  "Sort lines in region numerically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
Specified field must contain a number in each line of the region.
With a negative arg, sorts by the -ARG'th field, in reverse order.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort."
  (interactive "p\nr")
  (sort-fields-1 field beg end
		 (function (lambda ()
			     (sort-skip-fields (1- field))
			     (string-to-int
			      (buffer-substring
			        (point)
				(save-excursion
				  (skip-chars-forward "[0-9]")
				  (point))))))
		 nil))

(defun sort-fields (field beg end)
  "Sort lines in region lexicographically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
With a negative arg, sorts by the -ARG'th field, in reverse order.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort."
  (interactive "p\nr")
  (sort-fields-1 field beg end
		 (function (lambda ()
			     (sort-skip-fields (1- field))
			     nil))
		 (function (lambda () (skip-chars-forward "^ \t\n")))))

(defun sort-fields-1 (field beg end startkeyfun endkeyfun)
  (let ((reverse (< field 0))
	(tbl (syntax-table)))
    (setq field (max 1 field (- field)))
    (unwind-protect
	(save-restriction
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (set-syntax-table sort-fields-syntax-table)
	  (sort-subr reverse
		     'forward-line 'end-of-line
		     startkeyfun endkeyfun))
      (set-syntax-table tbl))))

(defun sort-skip-fields (n)
  (let ((eol (save-excursion (end-of-line 1) (point))))
    (forward-word n)
    (if (> (point) eol)
	(error "Line has too few fields: %s"
	       (buffer-substring (save-excursion
				   (beginning-of-line) (point))
				 eol)))
    (skip-chars-forward " \t")))


(defun sort-regexp-fields (reverse record-regexp key-regexp beg end)
  "Sort the region lexicographically as specifed by RECORD-REGEXP and KEY.
RECORD-REGEXP specifies the textual units which should be sorted.
  For example, to sort lines RECORD-REGEXP would be \"^.*$\"
KEY specifies the part of each record (ie each match for RECORD-REGEXP)
  is to be used for sorting.
  If it is \"\\digit\" then the digit'th \"\\(...\\)\" match field from
  RECORD-REGEXP is used.
  If it is \"\\&\" then the whole record is used.
  Otherwise, it is a regular-expression for which to search within the record.
If a match for KEY is not found within a record then that record is ignored.

With a negative prefix arg sorts in reverse order.

For example: to sort lines in the region by the first word on each line
 starting with the letter \"f\",
 RECORD-REGEXP would be \"^.*$\" and KEY \"\\<f\\w*\\>\""
  (interactive "P\nsRegexp specifying records to sort: 
sRegexp specifying key within record: \nr")
  (cond ((or (equal key-regexp "") (equal key-regexp "\\&"))
	 (setq key-regexp 0))
	((string-match "\\`\\\\[1-9]\\'" key-regexp)
	 (setq key-regexp (- (aref key-regexp 1) ?0))))
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let (sort-regexp-record-end) ;isn't dynamic scoping wonderful?
      (re-search-forward record-regexp)
      (setq sort-regexp-record-end (point))
      (goto-char (match-beginning 0))
      (sort-subr reverse
		 (function (lambda ()
			     (and (re-search-forward record-regexp nil 'move)
				  (setq sort-regexp-record-end (match-end 0))
				  (goto-char (match-beginning 0)))))
		 (function (lambda ()
			     (goto-char sort-regexp-record-end)))
		 (function (lambda ()
			     (let ((n 0))
			       (cond ((numberp key-regexp)
				      (setq n key-regexp))
				     ((re-search-forward
				        key-regexp sort-regexp-record-end t)
				      (setq n 0))
				     (t (throw 'key nil)))
			       (condition-case ()
				   (if (fboundp 'buffer-substring-lessp)
				       (cons (match-beginning n)
					     (match-end n))
				       (buffer-substring (match-beginning n)
							 (match-end n)))
				 ;; if there was no such register
				 (error (throw 'key nil))))))))))


(defun sort-columns (reverse &optional beg end)
  "Sort lines in region alphabetically by a certain range of columns.
For the purpose of this command, the region includes
the entire line that point is in and the entire line the mark is in.
The column positions of point and mark bound the range of columns to sort on.
A prefix argument means sort into reverse order.

Note that sort-columns uses the sort utility program and therefore
cannot work on text containing TAB characters.  Use M-x untabify
to convert tabs to spaces before sorting."
  (interactive "P\nr")
  (save-excursion
    (let (beg1 end1 col-beg1 col-end1 col-start col-end)
      (goto-char (min beg end))
      (setq col-beg1 (current-column))
      (beginning-of-line)
      (setq beg1 (point))
      (goto-char (max beg end))
      (setq col-end1 (current-column))
      (forward-line)
      (setq end1 (point))
      (setq col-start (min col-beg1 col-end1))
      (setq col-end (max col-beg1 col-end1))
      (if (search-backward "\t" beg1 t)
	  (error "sort-columns does not work with tabs.  Use M-x untabify."))
      (call-process-region beg1 end1 "sort" t t nil
			   (if reverse "-rt\n" "-t\n")
                           (concat "+0." col-start)
                           (concat "-0." col-end)))))
