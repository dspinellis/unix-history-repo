;Load up standardly loaded Lisp files for Emacs.
;; This is loaded into a bare Emacs to make a dumpable one.
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


(load "subr")
(load "simple")
(garbage-collect)
(load "files")
(garbage-collect)
(load "indent")
(load "window")
(load "paths.el")  ;Don't get confused if someone compiled paths by mistake.
(garbage-collect)
(load "loaddefs.el")  ;Don't get confused if someone compiled loaddefs by mistake.
(garbage-collect)
(load "startup")
(load "lisp")
(garbage-collect)
(load "page")
(load "register")
(garbage-collect)
(load "paragraphs")
(load "lisp-mode")
(garbage-collect)
(load "text-mode")
(load "fill")
(garbage-collect)
(load "c-mode")
(garbage-collect)
(load "isearch")
(garbage-collect)
(load "replace")
(garbage-collect)
(load "abbrev")
(garbage-collect)
(load "buff-menu")

(load "version.el")  ;Don't get confused if someone compiled version.el by mistake.

;; Note: all compiled Lisp files loaded above this point
;; must be among the ones parsed by make-docfile
;; to construct DOC.  Any that are not processed
;; for DOC will not have doc strings in the dumped Emacs.

(message "Finding pointers to doc strings...")
(if (fboundp 'dump-emacs)
    (progn
      (copy-file (expand-file-name "../etc/DOC")
		 (concat (expand-file-name "../etc/DOC.") emacs-version)
		 t)
      (Snarf-documentation (concat "DOC." emacs-version)))
    (Snarf-documentation "DOC"))
(message "Finding pointers to doc strings...done")

(lisp-interaction-mode)
(load "site-init" t)
(garbage-collect)

(if (or (equal (nth 3 command-line-args) "dump")
	(equal (nth 4 command-line-args) "dump"))
    (progn
      (message "Dumping under names xemacs and emacs-%s" emacs-version)
      (condition-case ()
	  (delete-file "xemacs")
	(file-error nil))
      (dump-emacs "xemacs" "temacs")
      (add-name-to-file "xemacs" (concat "emacs-" emacs-version) t)
      (kill-emacs)))

;; Avoid error if user loads some more libraries now.
(setq purify-flag nil)

;; For machines with CANNOT_DUMP defined in config.h,
;; this file must be loaded each time Emacs is run.
;; So run the startup code now.

(or (fboundp 'dump-emacs)
    (eval top-level))
