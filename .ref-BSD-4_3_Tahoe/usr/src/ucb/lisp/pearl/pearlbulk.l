;;;;;;;;;;;;;;;;;;;;;;;;;;; pearlbulk.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This file is the second half of PEARL for compiling in two pieces.
;    It loads "pearlbase.o" and then "includes" the rest of PEARL.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

(eval-when (compile)
  ; To cut down on the number of garbage collects during compilation.
  (allocate 'list 800)
  (allocate 'symbol 200)
  (allocate 'fixnum 10)
  (fasl 'pearlbase)
  (load 'franz.l)
  (declare (special defmacro-for-compiling))
  (setq defmacro-for-compiling t))

(declare (macros t))
(declare (localf enforcetype buildvalue
		 insidescopy insidepatternize insidevarreplace
		 followpath convertpreds revassq
		 recursetoinsidestandardfetch gethashvalue insertbyfocus
		 removebyfocus
		 prefix addhistory read-in-startprl-file read-in-initprl-file))

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
