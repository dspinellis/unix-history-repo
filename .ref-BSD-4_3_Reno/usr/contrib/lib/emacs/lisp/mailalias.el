;; Expand mailing address aliases defined in ~/.mailrc.
;; Copyright (C) 1985, 1987 Free Software Foundation, Inc.

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
  (if (eq mail-aliases t)
      (progn (setq mail-aliases nil) (build-mail-aliases)))
  (goto-char beg)
  (setq end (set-marker (make-marker) end))
  (let ((case-fold-search nil))
    (while (let ((case-fold-search t))
	     (re-search-forward "^\\(to\\|cc\\|bcc\\):" end t))
      (skip-chars-forward " \t")
      (let ((beg1 (point))
	    end1 pos epos seplen
	    ;; DISABLED-ALIASES records aliases temporarily disabled
	    ;; while we scan text that resulted from expanding those aliases.
	    ;; Each element is (ALIAS . TILL-WHEN), where TILL-WHEN
	    ;; is where to reenable the alias (expressed as number of chars
	    ;; counting from END1).
	    (disabled-aliases nil))
	(re-search-forward "^[^ \t]" end 'move)
	(beginning-of-line)
	(skip-chars-backward " \t\n")
	(setq end1 (point-marker))
	(goto-char beg1)
	(while (< (point) end1)
	  (setq pos (point))
	  ;; Reenable any aliases which were disabled for ranges
	  ;; that we have passed out of.
	  (while (and disabled-aliases (> pos (- end1 (cdr (car disabled-aliases)))))
	    (setq disabled-aliases (cdr disabled-aliases)))
	  ;; EPOS gets position of end of next name;
	  ;; SEPLEN gets length of whitespace&separator that follows it.
	  (if (re-search-forward "[ \t]*[\n,][ \t]*" end1 t)
	      (setq epos (match-beginning 0)
		    seplen (- (point) epos))
	    (setq epos (marker-position end1) seplen 0))
	  (let (translation
		(string (buffer-substring pos epos)))
	    (if (and (not (assoc string disabled-aliases))
		     (setq translation
			   (cdr (assoc string mail-aliases))))
		(progn
		  ;; This name is an alias.  Disable it.
		  (setq disabled-aliases (cons (cons string (- end1 epos))
					       disabled-aliases))
		  ;; Replace the alias with its expansion
		  ;; then rescan the expansion for more aliases.
		  (goto-char pos)
		  (insert translation)
		  (delete-region (point) (+ (point) (- epos pos)))
		  (goto-char pos))
	      ;; Name is not an alias.  Skip to start of next name.
	      (goto-char epos)
	      (forward-char seplen))))
	(set-marker end1 nil)))
    (set-marker end nil)))

;; Called by mail-setup, or similar functions, only if ~/.mailrc exists.
(defun build-mail-aliases (&optional file)
  "Read mail aliases from ~/.mailrc and set mail-aliases."
  (setq file (expand-file-name (or file "~/.mailrc")))
  (let ((buffer nil)
	(obuf (current-buffer)))
    (unwind-protect
	(progn
	  (setq buffer (generate-new-buffer "mailrc"))
	  (buffer-flush-undo buffer)
	  (set-buffer buffer)
	  (cond ((get-file-buffer file)
		 (insert (save-excursion
			   (set-buffer (get-file-buffer file))
			   (buffer-substring (point-min) (point-max)))))
		((not (file-exists-p file)))
		(t (insert-file-contents file)))
	  ;; Don't lose if no final newline.
	  (goto-char (point-max))
	  (or (eq (preceding-char) ?\n) (newline))
	  (goto-char (point-min))
	  ;; handle "\\\n" continuation lines
	  (while (not (eobp))
	    (end-of-line)
	    (if (= (preceding-char) ?\\)
		(progn (delete-char -1) (delete-char 1) (insert ?\ ))
	        (forward-char 1)))
	  (goto-char (point-min))
	  (while (or (re-search-forward "^a\\(lias\\|\\)[ \t]+" nil t)
		     (re-search-forward "^g\\(roup\\|\\)[ \t]+" nil t))
	    (re-search-forward "[^ \t]+")
	    (let* ((name (buffer-substring (match-beginning 0) (match-end 0)))
		   (start (progn (skip-chars-forward " \t") (point))))
	      (end-of-line)
	      (define-mail-alias
		name
		(buffer-substring start (point)))))
	  mail-aliases)
      (if buffer (kill-buffer buffer))
      (set-buffer obuf))))

;; Always autoloadable in case the user wants to define aliases
;; interactively or in .emacs.
(defun define-mail-alias (name definition)
  "Define NAME as a mail-alias that translates to DEFINITION."
  (interactive "sDefine mail alias: \nsDefine %s as mail alias for: ")
  ;; Read the defaults first, if we have not done so.
  (if (eq mail-aliases t)
      (progn
	(setq mail-aliases nil)
	(if (file-exists-p "~/.mailrc")
	    (build-mail-aliases))))
  (let (tem)
    ;; ~/.mailrc contains addresses separated by spaces.
    ;; mailers should expect addresses separated by commas.
    (while (setq tem (string-match "[^ \t,][ \t,]+" definition tem))
      (if (= (match-end 0) (length definition))
	  (setq definition (substring definition 0 (1+ tem)))
	(setq definition (concat (substring definition
					    0 (1+ tem))
				 ", "
				 (substring definition (match-end 0))))
	(setq tem (+ 3 tem))))
    (setq tem (assoc name mail-aliases))
    (if tem
	(rplacd tem definition)
      (setq mail-aliases (cons (cons name definition) mail-aliases)))))
