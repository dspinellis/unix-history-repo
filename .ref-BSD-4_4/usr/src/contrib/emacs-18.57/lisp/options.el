;; Edit Options command for Emacs.
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


(defun list-options ()
  "Display a list of Emacs user options, with values and documentation."
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*List Options*"))
    (Edit-options-mode))
  (with-output-to-temp-buffer "*List Options*"
    (let (vars)
      (mapatoms (function (lambda (sym)
			    (if (user-variable-p sym)
				(setq vars (cons sym vars))))))
      (setq vars (sort vars 'string-lessp))
      (while vars
	(let ((sym (car vars)))
	  (princ ";; ")
	  (prin1 sym)
	  (princ ":\n\t")
	  (prin1 (symbol-value sym))
	  (terpri)
	  (princ (substitute-command-keys 
		  (documentation-property sym 'variable-documentation)))
	  (princ "\n;;\n"))
	(setq vars (cdr vars))))))

(defun edit-options ()
  "Edit a list of Emacs user option values.
Selects a buffer containing such a list,
in which there are commands to set the option values.
Type \\[describe-mode] in that buffer for a list of commands."
  (interactive)
  (list-options)
  (pop-to-buffer "*List Options*"))

(defvar Edit-options-mode-map
  (let ((map (make-keymap)))
    (define-key map "s" 'Edit-options-set)
    (define-key map "x" 'Edit-options-toggle)
    (define-key map "1" 'Edit-options-t)
    (define-key map "0" 'Edit-options-nil)
    (define-key map "p" 'backward-paragraph)
    (define-key map " " 'forward-paragraph)
    (define-key map "n" 'forward-paragraph)
    map)
  "")

;; Edit Options mode is suitable only for specially formatted data.
(put 'Edit-options-mode 'mode-class 'special)

(defun Edit-options-mode ()
  "Major mode for editing Emacs user option settings.
Special commands are:
s -- set variable point points at.  New value read using minibuffer.
x -- toggle variable, t -> nil, nil -> t.
1 -- set variable to t.
0 -- set variable to nil.
Each variable description is a paragraph.
For convenience, the characters p and n move back and forward by paragraphs."
  (kill-all-local-variables)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map Edit-options-mode-map)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "[^\^@-\^?]")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^\t")
  (setq truncate-lines t)
  (setq major-mode 'Edit-options-mode)
  (setq mode-name "Options"))

(defun Edit-options-set () (interactive)
  (Edit-options-modify
   '(lambda (var) (eval-minibuffer (concat "New " (symbol-name var) ": ")))))

(defun Edit-options-toggle () (interactive)
  (Edit-options-modify '(lambda (var) (not (symbol-value var)))))

(defun Edit-options-t () (interactive)
  (Edit-options-modify '(lambda (var) t)))

(defun Edit-options-nil () (interactive)
  (Edit-options-modify '(lambda (var) nil)))

(defun Edit-options-modify (modfun)
  (save-excursion
   (let (var pos)
     (re-search-backward "^;; ")
     (forward-char 3)
     (setq pos (point))
     (save-restriction
      (narrow-to-region pos (progn (end-of-line) (1- (point))))
      (goto-char pos)
      (setq var (read (current-buffer))))
     (goto-char pos)
     (forward-line 1)
     (forward-char 1)
     (save-excursion
      (set var (funcall modfun var)))
     (kill-sexp 1)
     (prin1 (symbol-value var) (current-buffer)))))

