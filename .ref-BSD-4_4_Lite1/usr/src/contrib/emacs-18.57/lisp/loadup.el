;Load up standardly loaded Lisp files for Emacs.
;; This is loaded into a bare Emacs to make a dumpable one.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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


(load "subr")
(garbage-collect)
(load "loaddefs.el")  ;Don't get confused if someone compiled loaddefs by mistake.
(garbage-collect)
(load "simple")
(garbage-collect)
(load "help")
(garbage-collect)
(load "files")
(garbage-collect)
(load "indent")
(load "window")
(load "paths.el")  ;Don't get confused if someone compiled paths by mistake.
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
(if (eq system-type 'vax-vms)
    (progn
      (garbage-collect)
      (load "vmsproc")))
(garbage-collect)
(load "abbrev")
(garbage-collect)
(load "buff-menu")
(if (eq system-type 'vax-vms)
    (progn
      (garbage-collect)
      (load "vms-patch")))

;If you want additional libraries to be preloaded and their
;doc strings kept in the DOC file rather than in core,
;you may load them with a "site-load.el" file.
;But you must also cause them to be scanned when the DOC file
;is generated.  For VMS, you must edit ../etc/makedoc.com.
;For other systems, you must edit ../src/ymakefile.
(if (load "site-load" t)
    (garbage-collect))

(load "version.el")  ;Don't get confused if someone compiled version.el by mistake.

;; Note: all compiled Lisp files loaded above this point
;; must be among the ones parsed by make-docfile
;; to construct DOC.  Any that are not processed
;; for DOC will not have doc strings in the dumped Emacs.

(message "Finding pointers to doc strings...")
(if (fboundp 'dump-emacs)
    (let ((name emacs-version))
      (while (string-match "[^-+_.a-zA-Z0-9]+" name)
	(setq name (concat (downcase (substring name 0 (match-beginning 0)))
			   "-"
			   (substring name (match-end 0)))))
      (copy-file (expand-file-name "../etc/DOC")
		 (concat (expand-file-name "../etc/DOC-") name)
		 t)
      (Snarf-documentation (concat "DOC-" name)))
    (Snarf-documentation "DOC"))
(message "Finding pointers to doc strings...done")

;Note: You can cause additional libraries to be preloaded
;by writing a site-init.el that loads them.
;See also "site-load" above.
(load "site-init" t)
(garbage-collect)

(if (or (equal (nth 3 command-line-args) "dump")
	(equal (nth 4 command-line-args) "dump"))
    (if (eq system-type 'vax-vms)
	(progn 
	  (message "Dumping data as file temacs.dump")
	  (dump-emacs "temacs.dump" "temacs")
	  (kill-emacs))
      (if (fboundp 'dump-emacs-data)
	  ;; Handle the IBM RS/6000, and perhaps eventually other machines.
	  (progn
	    ;; This strange nesting is so that the variable `name'
	    ;; is not bound when the data is dumped.
	    (message "Dumping data as file ../etc/EMACS-DATA")
	    (dump-emacs-data "../etc/EMACS-DATA")
	    (kill-emacs))
	(let ((name (concat "emacs-" emacs-version)))
	  (while (string-match "[^-+_.a-zA-Z0-9]+" name)
	    (setq name (concat (downcase (substring name 0 (match-beginning 0)))
			       "-"
			       (substring name (match-end 0)))))
	  (message "Dumping under names xemacs and %s" name))
	(condition-case ()
	    (delete-file "xemacs")
	  (file-error nil))
	(dump-emacs "xemacs" "temacs")
	;; Recompute NAME now, so that it isn't set when we dump.
	(let ((name (concat "emacs-" emacs-version)))
	  (while (string-match "[^-+_.a-zA-Z0-9]+" name)
	    (setq name (concat (downcase (substring name 0 (match-beginning 0)))
			       "-"
			       (substring name (match-end 0)))))
	  (add-name-to-file "xemacs" name t))
	(kill-emacs))))

;; Avoid error if user loads some more libraries now.
(setq purify-flag nil)

;; For machines with CANNOT_DUMP defined in config.h,
;; this file must be loaded each time Emacs is run.
;; So run the startup code now.

(or (fboundp 'dump-emacs)
    (eval top-level))
