(setq auto-mode-alist (cons '("\\.tex\$" . tex-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sty\$" . tex-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbl\$" . tex-mode) auto-mode-alist))
(autoload 'tex-mode "TeX-mode" 
	"Major mode for editing TeX-based documents" t)
(setq auto-mode-alist (cons '("\\.bib\$" . bibtex-mode) auto-mode-alist))
(autoload 'bibtex-mode "BibTeX-mode" 
	"Major mode for editing BibTeX database files" t)

; for 4.4 bsd:
(setq news-path "/var/spool/news/")
(setq mh-progs "/usr/contrib/mh/bin")
(setq mh-lib "/usr/contrib/lib/mh")
(setq rmail-spool-directory "/var/mail/")
(setq sendmail-program "/usr/sbin/sendmail")
(setq manual-program "/usr/bin/man")
(setq mainual-formatted-dirlist
 	   '("/usr/share/man/cat1" "/usr/share/man/cat2"
 	   "/usr/share/man/cat3"
 	   "/usr/share/man/cat3f"
 	   "/usr/share/man/cat4"
 	   "/usr/share/man/cat5" "/usr/share/man/cat6"
 	   "/usr/share/man/cat7" "/usr/share/man/cat8"
 	   "/usr/contrib/man/cat1" "/usr/contrib/man/cat2"
 	   "/usr/contrib/man/cat3" "/usr/contrib/man/cat4"
 	   "/usr/contrib/man/cat5" "/usr/contrib/man/cat6"
 	   "/usr/contrib/man/cat7" "/usr/contrib/man/cat8"
 	   "/usr/local/man/cat1" "/usr/local/man/cat2"
 	   "/usr/local/man/cat3" "/usr/local/man/cat4"
 	   "/usr/local/man/cat5" "/usr/local/man/cat6"
 	   "/usr/local/man/cat7" "/usr/local/man/cat8"))
