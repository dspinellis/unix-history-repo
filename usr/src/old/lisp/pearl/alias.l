;;;;;;;;;;;;;;;;;;;;;;;;;;;;; alias.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Aliases for various functions -- some for history's sake, some
;    for abbreviation's sake 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright (c) 1983 ,  The Regents of the University of California.
; All rights reserved.  
; Authors: Joseph Faletti and Michael Deering.

; For history:
;(aliasdef 'powercopy 'copy 'subr)
(aliasdef 'minform 'valform 'subr)
(aliasdef 'minprint 'valprint 'subr)
(aliasdef 'listform 'fullform 'subr)
(aliasdef 'shortform 'valform 'subr)
(aliasdef 'listprint 'fullprint 'subr)
(aliasdef 'shortprint 'valprint 'subr)
(aliasdef 'insert-db 'insertdb 'subr)
(aliasdef 'next-item 'nextitem 'subr)
(aliasdef 'remove-db 'removedb 'subr)
(aliasdef 'expanded-fetch 'expandedfetch 'subr)
(aliasdef 'symbol-e 'symbole 'subr)
(aliasdef 'combine-skels 'combineskels 'subr)
(aliasdef 'quasi-quote 'quasiquote 'subr)
(aliasdef 'define-set 'ordinal 'fsubr)
(aliasdef 'usersave 'savefresh 'subr)
(aliasdef 'user-save 'usersave 'subr)
(aliasdef 'pearl-rep-loop 'pearlreploop 'subr)

; Abbreviations:
(aliasdef 'cr 'create 'fsubr)
(aliasdef 'dbcr 'dbcreate 'macro)
(aliasdef 'ppath 'path 'macro)
(aliasdef 'vp 'valprint 'subr)
(aliasdef 'fp 'fullprint 'subr)
(aliasdef 'ap 'abbrevprint 'subr)
(aliasdef 'dp 'debugprint 'subr)
(aliasdef 'pdb 'printdb 'subr)

; vi: set lisp:
