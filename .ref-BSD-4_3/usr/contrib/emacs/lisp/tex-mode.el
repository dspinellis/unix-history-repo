;; Copyright (C) 1985 Richard M. Stallman
;; Rewritten following contributions by William F. Schelter
;; and Dick King (king@kestrel).

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


(defvar TeX-mode-syntax-table nil
  "Syntax table used while in TeX mode.")

(defvar TeX-zap-file nil
  "Temporary file name used for text being sent as input to TeX.")
(defvar TeX-command "cd /tmp; tex"
  "The command to run TeX on a file in /tmp, to make output in /tmp.")
(defvar TeX-dvi-print-command "lpr -d"
  "Command string used to print a .dvi file.")
(defvar TeX-trailer "\\bye\n"
  "TeX input supplied after the end of a region sent to TeX by M-x TeX-region.")

(defvar TeX-mode-map nil)
(if TeX-mode-map 
    nil
  (setq TeX-mode-map (make-sparse-keymap))
  (define-key TeX-mode-map "\C-j" 'TeX-terminate-paragraph)
  (define-key TeX-mode-map "\e{" 'TeX-insert-braces)
  (define-key TeX-mode-map "\e}" 'up-list)
  (define-key TeX-mode-map "\"" 'TeX-insert-quote)
  (define-key TeX-mode-map "\C-c\C-r" 'TeX-region)
  (define-key TeX-mode-map "\C-c\C-b" 'TeX-buffer)
  (define-key TeX-mode-map "\C-c\C-p" 'TeX-print))

(defun TeX-insert-quote (count)
  "Insert ``, '' or \" according to preceding character.
With numeric arg N, always insert N \" characters."
  (interactive "P")
  (if count
      (self-insert-command count)
    (insert
     (cond
      ((or (bobp)
	   (save-excursion
	     (forward-char -1)
	     (looking-at "[ \t\n]\\|\\s(")))
       "``")
      ((= (preceding-char) ?\\)
       ?\")
      (t "''")))))

(defun validate-TeX-buffer ()
  "Check current buffer for paragraphs containing mismatched $'s.
As each such paragraph is found, a mark is pushed at its beginning,
and the location is displayed for a few seconds."
  (interactive)
  (let ((opoint (point)))
    (goto-char (point-max))
    ;; Does not use save-excursion
    ;; because we do not want to save the mark.
    (unwind-protect
	(while (and (not (input-pending-p)) (not (bobp)))
	  (let ((end (point)))
	    (search-backward "\n\n" nil 'move)
	    (or (TeX-validate-paragraph (point) end)
		(progn
		  (push-mark (point))
		  (message "Mismatch found in pararaph starting here")
		  (sit-for 4)))))
      (goto-char opoint))))

(defun TeX-validate-paragraph (start end)
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (forward-sexp (- end start))
	  t))
    (error nil)))

(defun TeX-terminate-paragraph (inhibit-validation)
  "Insert two newlines, breaking a paragraph for TeX.
Check for mismatched braces/$'s in paragraph being terminated.
A prefix arg inhibits the checking."
  (interactive "P")
  (or inhibit-validation
      (TeX-validate-paragraph
       (save-excursion
	 (search-backward "\n\n" nil 'move)
	 (point))
       (point))
      (message "Paragraph being closed appears to contain a mismatch"))
  (insert "\n\n"))

(defun TeX-insert-braces ()
  "Make a pair of braces and be poised to type inside of them."
  (interactive)
  (insert ?\{)
  (save-excursion
    (insert ?})))
    
;(fset 'TeX-mode 'tex-mode) in loaddefs.
(defun tex-mode ()
  "Major mode for editing files of input for TeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use M-x validate-TeX-buffer to check buffer for paragraphs containing
mismatched $'s or braces.

Use C-c C-r to run TeX on the current region, plus a \"header\"
copied from the top of the file (containing macro definitions, etc.),
running TeX under a special subshell.  C-c C-b does the whole buffer.
C-c C-p prints the .dvi file made by either of those.

Special commands:
\\{TeX-mode-map}

Entering TeX mode calls the value of text-mode-hook,
and then the value of TeX-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (use-local-map TeX-mode-map)
  (setq mode-name "TeX")
  (setq major-mode 'TeX-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (if (null TeX-mode-syntax-table)
      (progn
	(setq TeX-mode-syntax-table (make-syntax-table))
	(set-syntax-table TeX-mode-syntax-table)
	(modify-syntax-entry ?\\ "\\   ")
	(modify-syntax-entry ?\$ "$$  ")
	(modify-syntax-entry ?\% "<   ")
	(modify-syntax-entry ?\f ">   ")
	(modify-syntax-entry ?\n ">   ")
	(modify-syntax-entry ?' "w   "))
    (set-syntax-table TeX-mode-syntax-table))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^\n")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "[^\\]\\(\\\\\\\\\\)*%+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'TeX-comment-indent)
  (run-hooks 'text-mode-hook 'TeX-mode-hook))

(defun TeX-comment-indent ()
  (if (looking-at "%%%")
      (current-column)
    (skip-chars-backward " \t")
    (max (1+ (current-column)) comment-column)))

;; Invoking TeX in an inferior shell.

(defun TeX-region (beg end)
  "Run TeX on current region.  Optionally process buffer's header first.
The buffer's header is everything up to a line saying \"%**end of header\".
It is processed as input by TeX before the region itself.
The file has a header if one of the first ten lines says \"%**start of header\".
The value of TeX-trailer is supplied as input to TeX after the region.
It defaults to \"\\bye\\n\"."
  (interactive "r")
  (or (get-buffer "*TeX-shell*")
      (progn
	(require 'shell)
	(make-shell "TeX-shell" "csh")))
  (or TeX-zap-file (setq TeX-zap-file (make-temp-name "/tmp/tz")))
  (let ((tex-out-file (concat TeX-zap-file ".tex")))
    (save-excursion
      (goto-char (point-min))
      (forward-line 10)
      (let ((search-end (point))
	    hbeg)
	(goto-char (point-min))
	;; Initialize the temp file with either the header or nothing
	(if (and (search-forward "%**start of header" search-end t)
		 (< (point) beg))
	    (progn
	      (forward-line 1)
	      (setq hbeg (point))
	      (search-forward "%**end of header")
	      (beginning-of-line)
	      (write-region hbeg (point) tex-out-file))
	  (write-region (point) (point) tex-out-file))
	;; Append the region to be printed.
	(write-region beg end tex-out-file t)))
    (send-string "TeX-shell" (concat TeX-command " " tex-out-file "\n")))
  (if TeX-trailer
      (send-string "TeX-shell" TeX-trailer))
  (pop-to-buffer "*TeX-shell*"))

(defun TeX-buffer ()
  "Run TeX on current buffer."
  (interactive)
  (let (TeX-trailer)
    (TeX-region (point-min) (point-max))))

(defun TeX-print ()
  "Print the .dvi file made by \\[TeX-region] or \\[TeX-buffer]."
  (interactive)
  (send-string "TeX-shell"
	       (concat TeX-dvi-print-command " " TeX-zap-file ".dvi\n")))
