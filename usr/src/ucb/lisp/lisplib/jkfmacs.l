(setq SCCS-jkfmacs "%Z%%M%	%I%	%G%")

;------ jkfmacs :: common and useful macros
;
;; as of Franz opus 38.36 all the macros in this file all available in
;; the standard lisp system, so there is no reason to load this file.
;;

(msg "message from jkfmacs: " N
     "All the macros in this file are now available in the default franz." N
     "Thus you should not be loading jkfmacs." N
     "One warning: the order of the arguments is different in the version " N
     "of the push macro in the default franz.  it is now (push val stack) " N
     "You should check your code " N
     "Also, there isn't an 'unpush' macro any more " N)
