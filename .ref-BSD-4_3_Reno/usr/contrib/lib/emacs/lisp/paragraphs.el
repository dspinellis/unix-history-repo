;; Paragraph and sentence parsing.
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


(defvar paragraph-ignore-fill-prefix nil
  "Non-nil means the paragraph commands are not affected by fill-prefix.
This is desirable in modes where blank lines are the paragraph delimiters.")

(defun forward-paragraph (&optional arg)
  "Move forward to end of paragraph.  With arg, do it arg times.
A line which  paragraph-start  matches either separates paragraphs
\(if  paragraph-separate  matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer."
  (interactive "p")
  (or arg (setq arg 1))
  (let* ((fill-prefix-regexp
	  (and fill-prefix (not (equal fill-prefix ""))
	       (not paragraph-ignore-fill-prefix)
	       (regexp-quote fill-prefix)))
	 (paragraph-separate
	  (if fill-prefix-regexp
	      (concat paragraph-separate "\\|^"
		      fill-prefix-regexp "[ \t]*$")
	    paragraph-separate)))
    (while (< arg 0)
      (if (and (not (looking-at paragraph-separate))
	       (re-search-backward "^\n" (max (1- (point)) (point-min)) t))
	  nil
	(forward-char -1) (beginning-of-line)
	(while (and (not (bobp)) (looking-at paragraph-separate))
	  (forward-line -1))
	(end-of-line)
	;; Search back for line that starts or separates paragraphs.
	(if (if fill-prefix-regexp
		;; There is a fill prefix; it overrides paragraph-start.
		(progn
		 (while (progn (beginning-of-line)
			       (and (not (bobp))
				    (not (looking-at paragraph-separate))
				    (looking-at fill-prefix-regexp)))
		   (forward-line -1))
		 (not (bobp)))
	      (re-search-backward paragraph-start nil t))
	    ;; Found one.
	    (progn
	      (while (looking-at paragraph-separate)
		(forward-line 1))
	      (if (eq (char-after (- (point) 2)) ?\n)
		  (forward-line -1)))
	  ;; No starter or separator line => use buffer beg.
	  (goto-char (point-min))))
      (setq arg (1+ arg)))
    (while (> arg 0)
      (beginning-of-line)
      (while (prog1 (and (not (eobp))
			 (looking-at paragraph-separate))
		    (forward-line 1)))
      (if fill-prefix-regexp
	  ;; There is a fill prefix; it overrides paragraph-start.
	  (while (and (not (eobp))
		      (not (looking-at paragraph-separate))
		      (looking-at fill-prefix-regexp))
	    (forward-line 1))
	(if (re-search-forward paragraph-start nil t)
	    (goto-char (match-beginning 0))
	  (goto-char (point-max))))
      (setq arg (1- arg)))))

(defun backward-paragraph (&optional arg)
  "Move backward to start of paragraph.  With arg, do it arg times.
A paragraph start is the beginning of a line which is a first-line-of-paragraph
or which is ordinary text and follows a paragraph-separating line; except:
if the first real line of a paragraph is preceded by a blank line,
the paragraph starts at that blank line.
See forward-paragraph for more information."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-paragraph (- arg)))

(defun mark-paragraph ()
  "Put point at beginning of this paragraph, mark at end."
  (interactive)
  (forward-paragraph 1)
  (push-mark nil t)
  (backward-paragraph 1))

(defun kill-paragraph (arg)
  "Kill to end of paragraph."
  (interactive "*p")
  (kill-region (point) (progn (forward-paragraph arg) (point))))

(defun backward-kill-paragraph (arg)
  "Kill back to start of paragraph."
  (interactive "*p")
  (kill-region (point) (progn (backward-paragraph arg) (point))))

(defun transpose-paragraphs (arg)
  "Interchange this (or next) paragraph with previous one."
  (interactive "*p")
  (transpose-subr 'forward-paragraph arg))

(defun start-of-paragraph-text ()
  (let ((opoint (point)) npoint)
    (forward-paragraph -1)
    (setq npoint (point))
    (skip-chars-forward " \t\n")
    (if (>= (point) opoint)
	(progn
	  (goto-char npoint)
	  (if (> npoint (point-min))
	      (start-of-paragraph-text))))))

(defun end-of-paragraph-text ()
  (let ((opoint (point)))
    (forward-paragraph 1)
    (if (eq (preceding-char) ?\n) (forward-char -1))
    (if (<= (point) opoint)
	(progn
	  (forward-char 1)
	  (if (< (point) (point-max))
	      (end-of-paragraph-text))))))

(defun forward-sentence (&optional arg)
  "Move forward to next sentence-end.  With argument, repeat.
With negative argument, move backward repeatedly to sentence-beginning.
Sentence ends are identified by the value of sentence-end
treated as a regular expression.  Also, every paragraph boundary
terminates sentences as well."
  (interactive "p")
  (or arg (setq arg 1))
  (while (< arg 0)
    (let ((par-beg (save-excursion (start-of-paragraph-text) (point))))
      (if (re-search-backward (concat sentence-end "[^ \t\n]") par-beg t)
	  (goto-char (1- (match-end 0)))
	(goto-char par-beg)))
    (setq arg (1+ arg)))
  (while (> arg 0)
    (let ((par-end (save-excursion (end-of-paragraph-text) (point))))
      (if (re-search-forward sentence-end par-end t)
	  (skip-chars-backward " \t\n")
	(goto-char par-end)))
    (setq arg (1- arg))))

(defun backward-sentence (&optional arg)
  "Move backward to start of sentence.  With arg, do it arg times.
See forward-sentence for more information."
  (interactive "p")
  (or arg (setq arg 1))
  (forward-sentence (- arg)))

(defun kill-sentence (&optional arg)
  "Kill from point to end of sentence.
With arg, repeat, or backward if negative arg."
  (interactive "*p")
  (let ((beg (point)))
    (forward-sentence arg)
    (kill-region beg (point))))

(defun backward-kill-sentence (&optional arg)
  "Kill back from point to start of sentence.
With arg, repeat, or forward if negative arg."
  (interactive "*p")
  (let ((beg (point)))
    (backward-sentence arg)
    (kill-region beg (point))))

(defun mark-end-of-sentence (arg)
  "Put mark at end of sentence.  Arg works as in forward-sentence."
  (interactive "p")
  (push-mark
    (save-excursion
      (forward-sentence arg)
      (point))))

(defun transpose-sentences (arg)
  "Interchange this (next) and previous sentence."
  (interactive "*p")
  (transpose-subr 'forward-sentence arg))
