;; Automatic mode-dependent insertion of text into new files.
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

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

;;; autoinsert.el

;;;  Abstract:
;;;
;;;  The following defines an association list for files to be
;;;  automatically inserted when a new file is created, and a function
;;;  which automatically inserts these files; the idea is to insert
;;;  default files much as the mode is automatically set using
;;;  auto-mode-alist.
;;;
;;;  The auto-insert-alist consists of dotted pairs of
;;;  ( REGEXP . FILENAME ) where REGEXP is a regular expression, and
;;;  FILENAME is the file name of a file which is to be inserted into
;;;  all new files matching the regular expression with which it is
;;;  paired.
;;;
;;;  To use: 
;;;     load autoinsert.el
;;;     setq auto-insert-directory to an appropriate value, which
;;;       must end in "/"
;;;
;;;  Author:  Charlie Martin
;;;           Department of Computer Science and
;;;           National Biomedical Simulation Resource
;;;           Box 3709
;;;           Duke University Medical Center
;;;           Durham, NC 27710
;;;	      (crm@cs.duke.edu,mcnc!duke!crm) 
;;;
;;;  Date: Fri Jul  1 16:15:31 EDT 1988

(defvar auto-insert-alist '(("\\.tex$" . "tex-insert.tex")
			    ("\\.c$" . "c-insert.c")
			    ("\\.h$" . "h-insert.c")
			    ("[Mm]akefile" . "makefile.inc")
			    ("\\.bib$" . "tex-insert.tex"))
  "Alist specifying text to insert by default into a new file.
Elements look like (REGEXP . FILENAME); if the new file's name
matches REGEXP, then the file FILENAME is inserted into the buffer.
Only the first matching element is effective.")

;;; Establish a default value for auto-insert-directory
(defvar auto-insert-directory "~/insert/"
  "Directory from which auto-inserted files are taken.")

(defun insert-auto-insert-files ()
  "Insert default contents into a new file.
Matches the visited file name against the elements of `auto-insert-alist'."
  (let ((alist auto-insert-alist)
	;; remove backup suffixes from file name
        (name (file-name-sans-versions buffer-file-name))
        (insert-file nil))

    ;; find first matching alist entry
    (while (and (not insert-file) alist)
      (if (string-match (car (car alist)) name)
          (setq insert-file (cdr (car alist)))
        (setq alist (cdr alist))))

    ;; Now, if we found an appropriate insert file, insert it
    (if insert-file
        (let ((file (concat auto-insert-directory insert-file)))
          (if (file-readable-p file)
              (insert-file-contents file)
            (message "Auto-insert: file %s not found" file)
	    (sleep-for 1))))))

;; Make this feature take effect when a nonexistent file is visited.
(setq find-file-not-found-hooks
      (cons 'insert-auto-insert-files
	    find-file-not-found-hooks))
