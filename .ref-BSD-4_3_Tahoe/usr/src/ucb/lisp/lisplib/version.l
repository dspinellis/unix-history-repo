;; version.l
;;			-[ Wed May 22 09:30:58 PDT 1985 by sklower]-
;;
;; This file is edited after every modification is made to the
;; lisp system.
;; Variables defined:
;;   this-lisp-version-built: a symbols whose pname is the date when
;;	the lisp system was build.  People who care about such things
;;	can have their .lisprc file print it out at startup
;;   franz-minor-version-number.  This is printed after the opus number
;;	upon startup.  It is incremented after each fix or feature addition
;;

(setq this-lisp-version-built (status ctime)
      franz-minor-version-number ".92")
