;;;;;;;;;;;;;;;;;;;;;;;;;;; pearlsmall.l ;;;;;;;;;;;;;;;;;;;;;;;;;;
; This file loads the two halves of PEARL when it is compiled in
;    two pieces on a machine with small memory and/or tempfile space.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

(eval-when (load)
  (fasl 'pearlbase.o)
  (fasl 'pearlbulk.o))

; vi: set lisp:
