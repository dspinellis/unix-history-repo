;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pearl.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This file is the file that should be handed to Liszt for compiling.
;    It "includes" all of the files that need to be used to make
;    a complete PEARL object file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; After compiling this file with the -r option,
;    run it,
; and then run the function
;       (savepearl) to save a version in the current directory
;       	    under the name "pearl"
;       or
;       (savepearl <pathandfilename>) to save a version under that name
; This will then give you a (very large) runnable version of
;    Franz plus PEARL.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile)
  ; To cut down on the number of garbage collects during compilation.
  (allocate 'list 800)
  (allocate 'symbol 200)
  (declare (special defmacro-for-compiling))
  (setq defmacro-for-compiling t))

(declare (macros t))
(declare (localf enforcetype buildvalue
		 insidescopy insidepatternize insidevarreplace
		 followpath convertpreds revassq
		 recursetoinsidestandardfetch gethashvalue insertbyfocus
		 removebyfocus
		 prefix addhistory read-in-startprl-file read-in-initprl-file))

(include ucisubset.l)
; Version numbers are in here.
(include franz.l)
(include lowlevel.l)
(include db.l)
(include vars.l)
(include symord.l)
(include hook.l)
(include create.l)
(include scopy.l)
(include path.l)
(include print.l)
(include hash.l)
(include match.l)
(include history.l)
(include toplevel.l)

; This is a version of the usual library file fix.l
;   with "print"s changed to "pearlfixprintfn".
; It should only be left out after changing the toplevel stuff to
;   not call the fixit debugger.
(include fix.l)

(include alias.l)
(include inits.l)

; vi: set lisp:
