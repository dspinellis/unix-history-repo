;;;;;;;;;;;;;;;;;;;;;;;;;;;;; inits.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Expressions which set the values of special variables and create
;    nilsym and nilstruct and which use PEARL functions and so must
;    be done AFTER everything is loaded.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

(progn
  ; A very special cell.
  (setq *any*conscell* (cons '*any* (punbound)))
  ; Define the default symbol.
  (or (boundp (symatom 'nilsym))
      (symbol nilsym))
  ; Define the default empty structure.
  (or (boundp (defatom 'nilstruct))
      (create base nilstruct))
  ; Values that should not take part in hashing.
  ;   Used to include ",(eval (defatom 'nilstruct)) ,(eval (symatom 'nilsym))".
  (setq *unhashablevalues* `(nil ,(punbound) ,(unbound)))
  (putprop 'history '(history) 'alias)
  (putprop 'h '(history 22) 'alias)
  (defprop quote "'" printmacro)
  (defprop pearlequals "=" printmacro)
  (defprop *var* "?" printmacro)
  (defprop *global* "?" printmacro)
  (setdbsize 7.)
  (builddb *maindb*)
  (setq *db* *maindb*)
  )

; vi: set lisp:
