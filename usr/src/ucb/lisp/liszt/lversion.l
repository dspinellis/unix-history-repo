;; lversion.l				-[Tue Feb 22 08:43:32 1983 by jkf]-
;;
;; this defines this symbols:
;;   compiler-name: the banner printed out when liszt starts up
;;   this-liszt-version-built: ctime string which tells when this liszt
;;	was built.

;; this file is not sccsed because it only contains version number
;; information.

(setq this-liszt-version-built (status ctime))

(setq compiler-name "Liszt version 8.22")

