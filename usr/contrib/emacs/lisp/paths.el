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
	(t "/usr/local/mh/"))
  "Directory containing MH commands")
(defvar mh-lib
  (cond ((file-exists-p "/usr/new/lib/mh") "/usr/new/lib/mh/")
	(t "/usr/local/lib/mh/"))
  "Directory of MH library")

(defconst rmail-file-name "~/RMAIL"
  "Name of user's primary mail file.")

(defconst rmail-spool-directory
  (if (memq system-type '(hpux usg-unix-v))
      "/usr/mail/"
    "/usr/spool/mail/")
  "Name of directory used by system mailer for delivering new mail.
Its name should end with a slash.")

(defconst rmail-primary-inbox-list 
  (if (memq system-type '(hpux usg-unix-v))
      '("~/mbox" "/usr/mail/$LOGNAME")
    '("~/mbox" "/usr/spool/mail/$USER"))
 "List of files which are inboxes for user's primary mail file ~/RMAIL.")

(defconst sendmail-program
  (if (or (eq system-type 'berkeley-unix)
	  (file-exists-p "/usr/lib/sendmail"))
      "/usr/lib/sendmail"
    "fakemail")			;In ../etc, to interface to /bin/mail.
  "Program used to send messages.")

(defconst term-file-prefix "term/"
  "If non-nil, Emacs startup does (load (concat term-file-prefix (getenv \"TERM\")))
You may set this variable to nil in your `.emacs' file if you do not wish
the terminal-initialization file to be loaded.")
