;;;;;;;;;;;;;;;;;;;;;;;;;;;; pearllib.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This file loads the two halves of PEARL from /usr/lib/lisp when it
;    was compiled in two pieces on a machine with small memory
;    and/or tempfile space.  It is renamed "pearl.o" in /usr/lib/lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

(eval-when (load)
  (fasl '/usr/lib/lisp/pearlbase.o)
  (fasl '/usr/lib/lisp/pearlbulk.o))

; vi: set lisp:
