;;; ----	c h e a d		header file for inclusion

; $Header: /na/franz/liszt/RCS/chead.l,v 1.1 83/01/26 12:12:39 jkf Exp $

; Copyright (c) 1980 ,  The Regents of the University of California.
; All rights reserved.  
; author: j. foderaro

(putprop 'chead t 'version)	; flag that this file has been loaded

; global franz special variables
(declare (special $gcprint 	; t means print stats when  gc occurs.
		  $ldprint	; t means print fasl messages
		  $gccount$	; incremented every gc
		  float-format	; printf string used to print flonums
		  lisp-library-directory ; contains as assembler
		  lisp-object-directory  ; contains lisp for -r option
		  franz-minor-version-number ; just what it says
		  ))


(declare (special v-form g-loc g-cc g-locs g-loccnt g-labs g-args
		  g-ret g-skipcode g-dropnpcnt v-ifile g-arginfo
		  g-bindloc fl-comments g-tran g-trancnt g-tranloc
		  g-topsym g-reguse g-ignorereg g-arrayspecs
		  g-didvectorcode g-optionalp
		  fl-inter fl-xref fl-anno fl-asm v-sfile
		  g-trueop g-falseop
		  g-complrname
		  g-lits g-litcnt g-funcs g-spec Np-reg
		  Lbot-reg bind-reg np-plus bNp-reg
		  oLbot-reg Bnp-val ch-newline 
		  compiler-name internal-macros
		  old-top-level poport piport compiler-name
		  readtable original-readtable er-fatal ibase
		  fl-macl fl-tty
		  
		  g-functype g-bindtype g-calltype g-vartype
		  g-compfcn	; t if compiling a function
		  g-decls	; stack of local declaration info
		  g-spec g-litcnt
		  g-trueloc g-lits g-funcs g-fname g-comments
		  g-localf g-flocal g-ftype
		  g-stdref      ; on prop list of symbols called
		  		; as fcns in a non-local way.
		  twa-list old-declare-fcn vps-include vns-include fl-verb
		  user-top-level $gccount$ macros g-current
		  fl-profile fl-tran
		  g-allf fl-vms vp-xfile v-xfile 
		  vms-pointers g-refseen g-reflst fl-warn
		  g-currentargs
		  $gc-count$
		  special
		  liszt-eof-forms
		  liszt-process-forms	; list of forms to be compiled
		  liszt-root-name
		  liszt-file-name
		  k-ftype vp-sfile formsiz 
		  Cstack unCstack ; should be constants
		  arithequiv in-line-lambda-number
		  Liszt-file-names OF_olderrp
		  )
  )


;--- parameters: these must be evaluated at compile time so readmacros will
; work

(eval-when (compile load eval)
  (setq Np-reg 'r6
	Lbot-reg 'r7
	bind-reg 'r8
	np-plus '(+ r6)
	bNp-reg 'r5
	oLbot-reg 'r10
        Bnp-val '_bnp
	ch-newline (ascii #\lf)))


;--- evaluation frame parameters.  These must correspond to what is 
; in h/frame.h in the C system

(eval-when (compile load eval)
  (setq C_GO 1   	C_RET 2   	C_THROW 3   	C_RESET 4)
  (setq F_PROG 1 	F_CATCH 2 	F_RESET 3))

; offsets in frame
(setq OF_olderrp 12)	; 12 bytes from base of error frame


; load in the macro files if compiling or interpreting.

(eval-when (compile eval)
   (or (get 'cmacros 'version) (load 'cmacros)))
