;;;;;;;;;;;;;;;;;;;;;;;;;;; pearlbase.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This file is the first half of PEARL for compiling in two steps.
;    After it is compiled, it can be loaded into a liszt that is
;    compiling pearlbulk.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

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


; vi: set lisp:
