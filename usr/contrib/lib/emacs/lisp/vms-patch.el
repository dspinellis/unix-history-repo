;; Override parts of files.el for VMS.
;; Copyright (C) 1986 Free Software Foundation, Inc.

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


;;; Functions that need redefinition

;;; VMS file names are upper case, but buffer names are more
;;; convenient in lower case.

(defun create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return it.
FILENAME (sans directory) is used unchanged if that name is free;
otherwise a string <2> or <3> or ... is appended to get an unused name."
  (generate-new-buffer (downcase (file-name-nondirectory filename))))

;;; Given a string FN, return a similar name which is a legal VMS filename.
;;; This is used to avoid invalid auto save file names.
(defun make-legal-file-name (fn)
  (setq fn (copy-sequence fn))
  (let ((dot nil) (indx 0) (len (length fn)) chr)
    (while (< indx len)
      (setq chr (aref fn indx))
      (cond
       ((eq chr ?.) (if dot (aset fn indx ?_) (setq dot t)))
       ((not (or (and (>= chr ?a) (<= chr ?z)) (and (>= chr ?A) (<= chr ?Z))
		 (and (>= chr ?0) (<= chr ?9))
		 (eq chr ?$) (eq chr ?_) (and (eq chr ?-) (> indx 0))))
	(aset fn indx ?_)))
      (setq indx (1+ indx))))
  fn)

;;; Auto save filesnames start with _$ and end with $.

(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.
Does not consider auto-save-visited-file-name; that is checked
before calling this function.
This is a separate function so your .emacs file or site-init.el can redefine it.
See also auto-save-file-name-p."
  (if buffer-file-name
      (concat (file-name-directory buffer-file-name)
	      "_$"
	      (file-name-nondirectory buffer-file-name)
	      "$")
    (expand-file-name (concat "_$_" (make-legal-file-name (buffer-name)) "$"))))

(defun auto-save-file-name-p (filename)
  "Return t if FILENAME can be yielded by make-auto-save-file-name.
FILENAME should lack slashes.
This is a separate function so your .emacs file or site-init.el can redefine it."
  (string-match "^_\\$.*\\$" filename))

(defun vms-suspend-resume-hook ()
  "When resuming suspended Emacs, check for file to be found.
If the logical name `EMACS_FILE_NAME' is defined, `find-file' that file."
  (let ((file (vms-system-info "LOGICAL" "EMACS_FILE_NAME")))
    (if file (find-file file))))

(setq suspend-resume-hook 'vms-suspend-resume-hook)

(defun vms-suspend-hook ()
  "Don't allow suspending if logical name `DONT_SUSPEND_EMACS' is defined."
  (if (vms-system-info "LOGICAL" "DONT_SUSPEND_EMACS")
      (error "Can't suspend this emacs"))
  nil)

(setq suspend-hook 'vms-suspend-hook)
