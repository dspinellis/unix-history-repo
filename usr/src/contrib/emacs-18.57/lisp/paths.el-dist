;; Define pathnames for use by various Emacs commands.
;; Copyright (C) 1986, 1988 Free Software Foundation, Inc.

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


;; These are default settings for names of certain files and directories
;; that Emacs needs to refer to from time to time.

;; If these settings are not right, override them with `setq'
;; in site-init.el.  Do not change this file.

(defvar Info-directory (expand-file-name "../info/" exec-directory))

(defvar news-path "/usr/spool/news/"
  "The root directory below which all news files are stored.")
(defvar news-inews-program
  (cond ((file-exists-p "/usr/bin/inews") "/usr/bin/inews")
	((file-exists-p "/usr/local/inews") "/usr/local/inews")
	((file-exists-p "/usr/local/bin/inews") "/usr/local/bin/inews")
	((file-exists-p "/usr/lib/news/inews") "/usr/lib/news/inews")
	(t "inews"))
  "Program to post news.")

(defvar mh-progs
  (cond ((file-exists-p "/usr/new/mh") "/usr/new/mh/")
	((file-exists-p "/usr/local/bin/mh") "/usr/local/bin/mh/")
	(t "/usr/local/mh/"))
  "Directory containing MH commands")

(defvar mh-lib
  (cond ((file-exists-p "/usr/new/lib/mh") "/usr/new/lib/mh/")
	((file-exists-p "/usr/local/lib/mh") "/usr/local/lib/mh/")
	(t "/usr/local/bin/mh/"))
  "Directory of MH library")

(defconst rmail-file-name "~/RMAIL"
  "Name of user's primary mail file.")

(defconst rmail-spool-directory
  (if (memq system-type '(hpux usg-unix-v unisoft-unix rtu
			       silicon-graphics-unix))
      "/usr/mail/"
    "/usr/spool/mail/")
  "Name of directory used by system mailer for delivering new mail.
Its name should end with a slash.")

(defconst sendmail-program
  (if (file-exists-p "/usr/lib/sendmail")
      "/usr/lib/sendmail"
    "fakemail")			;In ../etc, to interface to /bin/mail.
  "Program used to send messages.")

(defconst term-file-prefix (if (eq system-type 'vax-vms) "[.term]" "term/")
  "If non-nil, Emacs startup does (load (concat term-file-prefix (getenv \"TERM\")))
You may set this variable to nil in your `.emacs' file if you do not wish
the terminal-initialization file to be loaded.")

(defconst manual-program (if (eq system-type 'berkeley-unix)
			     "/usr/ucb/man" "/usr/bin/man")
  "Program to run to print man pages.")

;; Note that /usr/man/cat is not really right for this on sysV; nothing is,
;; judging by the list of directories below.  You can't get the dir
;; for a section by appending the section number to any one prefix.
;; But it turns out that a string that's wrong does no harm here.
(defconst manual-formatted-dir-prefix
  (if (file-exists-p "/usr/man/cat.C")  ;; Check for Xenix.
      "/usr/man/cat." "/usr/man/cat")
  "Prefix for directories containing formatted manual pages.
Append a section-number or section-name to get a directory name.")

(defconst manual-formatted-dirlist
  (cond ((eq system-type 'hpux)
	 '("/usr/man/cat1" "/usr/man/cat2" "/usr/man/cat3"
	   "/usr/man/cat4" "/usr/man/cat5" "/usr/man/cat6"
	   "/usr/man/cat7" "/usr/man/cat1m" "/usr/man/cat8"
	   "/usr/local/man/cat1" "/usr/local/man/cat2" "/usr/local/man/cat3"
	   "/usr/local/man/cat4" "/usr/local/man/cat5" "/usr/local/man/cat6"
	   "/usr/local/man/cat7" "/usr/local/man/cat1m" "/usr/local/man/cat8"
	   "/usr/contrib/man/cat1" "/usr/contrib/man/cat2"
	   "/usr/contrib/man/cat3" "/usr/contrib/man/cat4"
	   "/usr/contrib/man/cat5" "/usr/contrib/man/cat6"
	   "/usr/contrib/man/cat7" "/usr/contrib/man/cat1m"
	   "/usr/contrib/man/cat8"))
	 ((file-exists-p "/usr/man/cat.C")  ; Xenix
	  '("/usr/man/cat.C" "/usr/man/cat.CP" "/usr/man/cat.CT"
	    "/usr/man/cat.DOS/" "/usr/man/cat.F" "/usr/man/cat.HW"
	    "/usr/man/cat.M/" "/usr/man/cat.S" "/usr/man/cat.LOCAL"))
	 ((file-exists-p "/usr/man/cat1")
	  '("/usr/man/cat1" "/usr/man/cat2" "/usr/man/cat3"
	    "/usr/man/cat4" "/usr/man/cat5" "/usr/man/cat6"
	    "/usr/man/cat7" "/usr/man/cat8" "/usr/man/catl" "/usr/man/catn"))
	 (t
	   '("/usr/catman/u_man/man1" "/usr/catman/u_man/man6"
	     "/usr/catman/p_man/man2" "/usr/catman/p_man/man3"
	     "/usr/catman/p_man/man4" "/usr/catman/p_man/man5"
	     "/usr/catman/a_man/man1" "/usr/catman/a_man/man7"
	     "/usr/catman/a_man/man8" "/usr/catman/local")))
  "List of directories containing formatted manual pages.")

(defconst abbrev-file-name 
  (if (eq system-type 'vax-vms)
      "~/abbrev.def"
    "~/.abbrev_defs")
  "*Default name of file to read abbrevs from.")
