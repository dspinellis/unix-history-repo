;; Major mode for editing texinfo files.
;; Copyright (C) 1985, 1988 Free Software Foundation, Inc.

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


(defvar texinfo-mode-syntax-table nil)

(if texinfo-mode-syntax-table
    nil
  (setq texinfo-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" " " texinfo-mode-syntax-table)
  (modify-syntax-entry ?\\ " " texinfo-mode-syntax-table)
  (modify-syntax-entry ?@ "\\" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\^q "\\" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" texinfo-mode-syntax-table)
  (modify-syntax-entry ?{ "(}" texinfo-mode-syntax-table)
  (modify-syntax-entry ?} "){" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\' "w" texinfo-mode-syntax-table))

(defvar texinfo-mode-map nil)

(if texinfo-mode-map
    nil
  (setq texinfo-mode-map (make-sparse-keymap))
  (define-key texinfo-mode-map "\C-c\C-f" 'texinfo-format-region)
  (define-key texinfo-mode-map "\C-c\C-s" 'texinfo-show-structure)
  (define-key texinfo-mode-map "\e}"          'up-list)
  (define-key texinfo-mode-map "\e{"          'texinfo-insert-braces)
  (define-key texinfo-mode-map "\C-c\C-cv"    'texinfo-insert-@var)
  (define-key texinfo-mode-map "\C-c\C-cs"    'texinfo-insert-@samp)
  (define-key texinfo-mode-map "\C-c\C-cn"    'texinfo-insert-@node)
  (define-key texinfo-mode-map "\C-c\C-ci"    'texinfo-insert-@item)
  (define-key texinfo-mode-map "\C-c\C-ce"    'texinfo-insert-@end)
  (define-key texinfo-mode-map "\C-c\C-cd"    'texinfo-insert-@dfn)
  (define-key texinfo-mode-map "\C-c\C-cc"    'texinfo-insert-@code))

(defun texinfo-insert-@var ()
  "Insert the string @var in a texinfo buffer."
  (interactive)
  (insert "@var{}")
  (backward-char))

(defun texinfo-insert-@samp ()
  "Insert the string @samp in a texinfo buffer."
  (interactive)
  (insert "@samp{}")
  (backward-char))

(defun texinfo-insert-@node ()
  "Insert the string @node in a texinfo buffer, 
along with a comment indicating the arguments to @node."
  (interactive)
  (insert "@node     \n@comment  node-name,  next,  previous,  up")
  (forward-line -1)
  (forward-char 6))

(defun texinfo-insert-@item ()
  "Insert the string @item in a texinfo buffer."
  (interactive)
  (insert "@item")
  (newline))

(defun texinfo-insert-@end ()
  "Insert the string @end in a texinfo buffer."
  (interactive)
  (insert "@end "))

(defun texinfo-insert-@dfn ()
  "Insert the string @dfn in a texinfo buffer."
  (interactive)
  (insert "@dfn{}")
  (backward-char))

(defun texinfo-insert-@code ()
  "Insert the string @code in a texinfo buffer."
  (interactive)
  (insert "@code{}")
  (backward-char))

(defun texinfo-insert-braces ()
  "Make a pair of braces and be poised to type inside of them.
Use \\[up-list] to move forward out of the braces."
  (interactive)
  (insert "{}")
  (backward-char))

(defun texinfo-mode ()
  "Major mode for editing texinfo files.

  It has these extra commands:
\\{texinfo-mode-map}

  These are files that are used as input for TeX to make printed manuals
and also to be turned into Info files by \\[texinfo-format-buffer].
These files must be written in a very restricted and modified version
of TeX input format.

  Editing commands are like text-mode except that the syntax table is
set up so expression commands skip Texinfo bracket groups.  To see
what the Info version of a region of the Texinfo file will look like,
use \\[texinfo-format-region].  This command runs Info on the current region
of the Texinfo file and formats it properly.

  You can show the structure of a Texinfo file with \\[texinfo-show-structure].
This command shows the structure of a Texinfo file by listing the
lines with the @-sign commands for @node, @chapter, @section and the
like.  These lines are displayed in another window called the *Occur*
window.  In that window, you can position the cursor over one of the
lines and use \\[occur-mode-goto-occurrence], to jump to the
corresponding spot in the Texinfo file.

  In addition, Texinfo mode provides commands that insert various
frequently used @-sign commands into the buffer.  You can use these
commands to save keystrokes.  And you can insert balanced braces with
\\[texinfo-insert-braces] and later use the command \\[up-list] to
move forward past the closing brace.

Entering Texinfo mode calls the value of text-mode-hook, and then the
value of texinfo-mode-hook."
  (interactive)
  (text-mode)
  (setq mode-name "Texinfo")
  (setq major-mode 'texinfo-mode)
  (use-local-map texinfo-mode-map)
  (set-syntax-table texinfo-mode-syntax-table)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat "^\b\\|^@[a-zA-Z]*[ \n]\\|" paragraph-separate))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^\b\\|^@[a-zA-Z]*[ \n]\\|" paragraph-start))
  (make-local-variable 'fill-column)
  (setq fill-column 72)
  (make-local-variable 'comment-start)
  (setq comment-start "@c ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "@c +")
  (run-hooks 'text-mode-hook 'texinfo-mode-hook))

(defvar texinfo-heading-pattern
  "^@\\(chapter\\|unnum\\|appendix\\|sect\\|sub\\|heading\\|major\\|node\\)"
  "This is a regular expression to match Texinfo lines that are chapter
or sections headings or like such.")

(defun texinfo-show-structure () 
  "Show the structure of a Texinfo file by listing the lines with the
@-sign commands for @node, @chapter, @section and the like.  Lines
with structuring commands in them are displayed in another window
called the *Occur* window.  In that window, you can position the
cursor over one of the lines and use \\[occur-mode-goto-occurrence],
to jump to the corresponding spot in the Texinfo file."
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (occur texinfo-heading-pattern))
  (pop-to-buffer "*Occur*")
  (goto-char (point-min))
  (flush-lines "-----"))
