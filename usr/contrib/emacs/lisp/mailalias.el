;; Expand mailing address aliases defined in ~/.mailrc.
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


;; Called from sendmail-send-it, or similar functions,
;; only if some mail aliases are defined.
(defun expand-mail-aliases (beg end)
  "Expand all mail aliases in suitable header fields found between BEG and END.
Suitable header fields are To, Cc and Bcc."
  (goto-char beg)
  (setq end (set-marker (make-marker) end))
  (let ((case-fold-search t))
    (while (progn (setq case-fold-search t)
		  (re-search-forward "^\\(to\\|cc\\|bcc\\):" end t))
      (setq case-fold-search nil)
      (skip-chars-forward " \t")
      (let ((beg1 (point))
	    end1 pos epos seplen translation)
	(re-search-forward "^[^ \t]" end 'move)
	(beginning-of-line)
	(skip-chars-backward " \t\n")
	(setq end1 (point-marker))
	(goto-char beg1)
	(while (< (point) end1)
	  (setq pos (point))
	  (if (re-search-forward "[ \t]*[\n,][ \t]*" end1 t)
	      (setq epos (match-beginning 0)
		    seplen (- (point) epos))
	    (setq epos end1 seplen 0))
	  (setq translation
		(cdr (assoc (buffer-substring pos epos) mail-aliases)))
	  (if translation
	      (progn
		(delete-region pos epos)
		(goto-char pos)
		(insert translation))
	    (goto-char epos)
	    (forward-char seplen)))
	(set-marker end1 nil)))
    (set-marker end nil)))

;; Called by mail-setup, or similar functions, only if ~/.mailrc exists.
(defun build-mail-aliases ()
  "Read mail aliases from ~/.mailrc and set mail-aliases."
  (let (buffer exists name (file "~/.mailrc"))
    (setq exists (get-file-buffer file))
    (setq buffer (find-file-noselect file))
    (unwind-protect
	(save-excursion
	  (set-buffer buffer)
	  (goto-char (point-min))
	  (while (re-search-forward "^alias[ \t]*\\|^a[ \t]*" nil t)
	    (re-search-forward "[^ \t]+")
	    (setq name (buffer-substring (match-beginning 0) (match-end 0)))
	    (skip-chars-forward " \t")
	    (define-mail-alias
	     name
	     (buffer-substring (point) (progn (end-of-line) (point))))))
      (or exists (kill-buffer buffer)))))

;; Always autoloadable in case the user wants to define aliases
;; interactively or in .emacs.
(defun define-mail-alias (name definition)
  "Define NAME as a mail-alias that translates to DEFINITION."
  (interactive "sDefine mail alias: \nsDefine %s as mail alias for: ")
  (let ((aelt (assoc name mail-aliases)))
    (if aelt
	(rplacd aelt definition)
      (setq mail-aliases (cons (cons name definition) mail-aliases)))))
