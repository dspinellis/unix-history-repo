;; Text mode, and its ideosyncratic commands.
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


(defvar text-mode-syntax-table nil
  "Syntax table used while in text mode.")

(defvar text-mode-abbrev-table nil
  "Abbrev table used while in text mode.")
(define-abbrev-table 'text-mode-abbrev-table ())

(if text-mode-syntax-table
    ()
  (setq text-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " text-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " text-mode-syntax-table)
  (modify-syntax-entry ?' "w   " text-mode-syntax-table))

(defvar text-mode-map nil "")
(if text-mode-map
    ()
  (setq text-mode-map (make-sparse-keymap))
  (define-key text-mode-map "\t" 'tab-to-tab-stop)
  (define-key text-mode-map "\es" 'center-line)
  (define-key text-mode-map "\eS" 'center-paragraph))


;(defun non-saved-text-mode ()
;  "Like text-mode, but delete auto save file when file is saved for real."
;  (text-mode)
;  (make-local-variable 'delete-auto-save-files)
;  (setq delete-auto-save-files t))

(defun text-mode ()
  "Major mode for editing text intended for humans to read.  Special commands:\\{text-mode-map}
Turning on text-mode calls the value of the variable text-mode-hook,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map text-mode-map)
  (setq mode-name "Text")
  (setq major-mode 'text-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'text-mode-hook))

(defvar indented-text-mode-map ())
(if indented-text-mode-map
    ()
  (setq indented-text-mode-map (make-sparse-keymap))
  (define-key indented-text-mode-map "\t" 'indent-relative)
  (define-key indented-text-mode-map "\es" 'center-line)
  (define-key indented-text-mode-map "\eS" 'center-paragraph))

(defun indented-text-mode ()
  "Major mode for editing indented text intended for humans to read.\\{indented-text-mode-map}
Turning on indented-text-mode calls the value of the variable text-mode-hook,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map text-mode-map)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (use-local-map indented-text-mode-map)
  (setq mode-name "Indented Text")
  (setq major-mode 'indented-text-mode)
  (run-hooks 'text-mode-hook))

(defun center-paragraph ()
  "Center each line in the paragraph at or after point.
See center-line for more info."
  (interactive)
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)))
      (backward-paragraph)
      (center-region (point) end))))

(defun center-region (from to)
  "Center each line starting in the region.
See center-line for more info."
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (while (not (eobp))
	(center-line)
	(forward-line 1)))))

(defun center-line ()
  "Center the line point is on, within the width specified by `fill-column'.
This means adjusting the indentation to match
the distance between the end of the text and `fill-column'."
  (interactive)
  (save-excursion
    (let (line-length)
      (beginning-of-line)
      (delete-horizontal-space)
      (end-of-line)
      (delete-horizontal-space)
      (setq line-length (current-column))
      (beginning-of-line)
      (indent-to 
	(+ left-margin 
	   (/ (- fill-column left-margin line-length) 2))))))
