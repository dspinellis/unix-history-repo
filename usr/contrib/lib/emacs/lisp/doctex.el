;;; Grind GNU Emacs DOC file into LaTeX input
;;; Copyright (C) 1987 Kyle E. Jones, Tor Lillqvist

;;; This file may be redistributed provided the above copyright
;;; notice appears on all copies and that the further free redistribution
;;; of this file is not in any way restricted by those who
;;; redistribute it.

;;; Based on Kyle E. Jones's grind-DOC package.

;;; This software is distributed 'as is', without warranties of any kind.

;;; This file is not part of GNU Emacs.

;;; The document that is the output from the (LaTeXify-DOC) function is
;;; part of GNU Emacs.


(defvar LaTeXify-DOC-style "report"
  "*Should be bound to a string indicating what LaTeX document style
should be used to format the DOC file.  If this variable is set to nil
the report style will be used.")

(defvar LaTeXify-DOC-style-options ""
  "*A string containing a list of document style options for LaTeX")

(defun LaTeXify-DOC () (interactive)
  "Reads the etc/DOC-xx.xx.x file into a buffer and converts it to a form
suitable as LaTeX input."
  ;
  ; Make sure we can deal with the macro package and the point size.
  ;
  (cond
   ((not (stringp LaTeXify-DOC-style))
    (error "LaTeXify-DOC-style must be a string")))
  ;
  ; Select the DOC file.
  ;
  (find-file (expand-file-name
	      (if (fboundp 'dump-emacs)
		  (concat "DOC-" emacs-version)
		"DOC")
	      exec-directory))
  (setq buffer-read-only nil)
  (auto-save-mode 0)
  (set-visited-file-name (concat (buffer-file-name) ".tex"))
  (delete-other-windows)
  ;
  ; Save-excursion just in case the DOC file was already selected.
  ;
  (save-excursion
    (let (case-fold-search mode-line-format varstart-point bufstring name odot)
      ;
      ; The first thing we must do is convert the \[COMMAND] sequences
      ; into the keys that the COMMANDs are bound to.
      ;
      (setq mode-line-format
	    "                     Grinding the DOC file... be patient.")
      (sit-for 0)
      (replace-regexp "\\\\{\\(\\s_\\|\\sw\\)*}"
		      "]]bgroup]]obeylines\\&]]egroup")
      (setq bufstring (substitute-command-keys (buffer-string)))
      (erase-buffer)
      (insert bufstring)
      ;
      ; Here we make each docstring begin and end with C-_ for
      ; easier manipulation.  This is undone later.
      ;
      (goto-char (1+ (point-min)))
      (replace-string "\C-_" "\C-_\C-_" nil)
      (goto-char (point-max))
      (insert "\C-_")
      ;
      ; Sort the docstrings.  This implicitly separates function
      ; documentation from the variable documentation.
      ;
      (sort-regexp-fields nil "\C-_\\([FV].*\\)[^\C-_]*\C-_" "\\1"
			  (point-min) (point-max))
      ;
      ; Handle TeX special characters
      ;
      (goto-char (point-min))
      (mapcar
       '(lambda (x) (save-excursion (eval x)))
       '((replace-string "#" "]]#")
         (replace-string "$" "]]$")
	 (replace-string "%" "]]%")
	 (replace-string "&" "]]&")
         (replace-string "~" "]]verb+~+")
	 (replace-string "_" "]]verb+_+")
	 (replace-string "^" "]]verb+^+")
	 (replace-string "\\" "]]verb+]]+")
	 (replace-string "{" "]]{")
	 (replace-string "}" "]]}")
	 (replace-string "<" "]]verb+<+")
	 (replace-string ">" "]]verb+>+")
	 (replace-string "]]" "\\")))
      ;
      ; Now add the indentation commands and put ( ...) around the functions
      ;
      (save-restriction
	(goto-char (point-min))
	(search-forward "\C-_V" (point-max) nil 1)
	(backward-char 2)
	(narrow-to-region (point-min) (dot))
	(goto-char (point-min))
	(insert "\\section*{Functions}\n"
		"\\begin{description}\n")
	(while (search-forward "\C-_F" (point-max) t 1)
	  (delete-char -2)
	  (insert "\n\\item[\\sf(")
	  (end-of-line 1)
	  (insert " ...)]")
	  (search-forward "\C-_" (point-max) nil 1)
	  (delete-char -1))
	(insert "\\end{description}\n"))
      (insert "\\section*{Variables}
Variables whose documentation begins with an
asterisk `*' are user definable options.  These variables are
used to customize Emacs.  Other variables are generally of
interest only to Emacs Lisp programmers.
\\begin{description}\n")
      (while (search-forward "\C-_V" (point-max) t 1)
	(delete-char -2)
	(insert "\n\\item[\\sf ")
	(end-of-line 1)
	(insert "]")
	(search-forward "\C-_" (point-max) nil 1)
	(delete-char -1))
      (insert "\\end{description}\n"
	      "\\end{document}\n")
      ;
      ; Try to make those parameters that are in all-caps look better
      ;
      (goto-char (point-min))
      (mapcar
       '(lambda (x) (save-excursion (eval x)))
       '((replace-regexp "[A-Z][A-Z]+" "\n{\\\\lowercase{\\\\sf \\&}}" nil)
	 (replace-string "\\lowercase{\\sf TAB}" "{\\tt TAB}")
	 (replace-string "\\lowercase{\\sf LFD}" "{\\tt LFD}")
	 (replace-string "\\lowercase{\\sf RET}" "{\\tt RET}")
	 (replace-string "\\lowercase{\\sf ESC}" "{\\tt ESC}")
	 (replace-string "\\lowercase{\\sf SPC}" "{\\tt SPC}")
	 (replace-string "\\lowercase{\\sf DEL}" "{\\tt DEL}")))
      ;
      ; Handle document style and front matter
      ;
      (goto-char (point-min))
      (insert "\\documentstyle["
	      LaTeXify-DOC-style-options
	      "]{" LaTeXify-DOC-style "}\n"
	      "\\begin{document}\n"
	      "\\title{GNU Emacs Lisp Reference \\\\\n"
	      "Version " emacs-version " \\\\\n"
	      "\\large (gouged with a blunt instrument from the DOC file)}\n"
	      "\\author{Richard M. Stallman}\n"
	      "\\date{" (substring emacs-build-time 4 8)
	      (substring emacs-build-time 20) "}\n"
	      "\\maketitle\n")
      ;
      ; Insert the GNU Emacs copyright notice.
      ;
      (insert
       "\\begin{centering}\n"
       "Copyright \\copyright" (substring emacs-build-time 20)
       " Free Software Foundation, Inc. \\\\\n"
       "\\end{centering}
\\vspace{\\baselineskip}
\\noindent
This document is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.
\\newpage\\sloppy\n")
      ;
      ; That's it
      ;
      (message "Grinding completed.  Behold!"))))
