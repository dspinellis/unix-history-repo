;;; ----	c h e a d		header file for inclusion

;$Header: chead.l,v 1.9 87/12/16 11:56:14 sklower Exp $
;
;			-[Tue Nov 22 08:32:26 1983 by jkf]-

; Copyright (c) 1982 ,  The Regents of the University of California.
; Copyright (c) 1980 ,  The Regents of the University of California.
; All rights reserved.  

; authors: John K. Foderaro and Kevin Layer

(putprop 'chead t 'version)	; flag that this file has been loaded

;--- build tahoe by default
;
(cond ((not (or (status feature for-vax) (status feature for-68k)
		(status feature for-tahoe)))
       (sstatus feature for-tahoe)))

; global franz special variables
(declare (special $gcprint 	; t means print stats when  gc occurs.
		  $ldprint	; t means print fasl messages
		  $gccount$	; incremented every gc
		  $global-reg$	; t means that np and lbot are in global regs
		  float-format	; printf string used to print flonums
		  lisp-library-directory ; contains as assembler
		  lisp-object-directory  ; contains lisp for -r option
		  franz-minor-version-number ; just what it says
		  ))

; keep 'em sorted please!
(declare (special
	     Liszt-file-names
	     arithequiv
	     bnp-sym
	     ch-newline
	     compiler-name
	     er-fatal		;; # of fatal erros
	     er-warn		;; # of warnings
	     fl-anno
	     fl-asm
	     fl-comments
	     fl-inter
	     fl-macl
	     fl-profile
	     fl-tran
	     fl-tty
	     fl-verb
	     fl-vms
	     fl-warn
	     fl-xref
	     formsiz
	     g-allf
	     g-arginfo
	     g-args
	     g-arrayspecs
	     g-bindloc
	     g-bindtype
	     g-calltype
	     g-cc
	     g-comments
	     g-compfcn			; t if compiling a function
	     g-complrname
	     g-current
	     g-currentargs
	     g-decls
	     g-didvectorcode
	     g-dropnpcnt
	     g-falseop
	     g-flocal
	     g-fname
	     g-ftype
	     g-funcs
	     g-functype
	     g-ignorereg
	     g-labs
	     g-litcnt
	     g-lits
	     g-loc
	     g-localf
	     g-loccnt
	     g-locs
	     g-masklab
	     g-optionalp
	     g-reflst
	     g-refseen
	     g-regmaskvec
	     g-reguse
	     g-ret
	     g-skipcode
	     g-spec
	     g-stackspace
	     g-stdref
	     g-topsym
	     g-tran
	     g-tranloc
	     g-trancnt
	     g-trueloc
	     g-trueop
	     g-vartype
	     ibase
	     in-line-lambda-number
	     internal-macros
	     k-ftype
	     liszt-eof-forms
	     liszt-file-name
	     liszt-process-forms
	     liszt-root-name
	     macros
	     old-declare-fcn
	     old-top-level
	     original-readtable
	     piport
	     poport
	     readtable
	     special
	     twa-list
	     user-top-level
	     v-form
	     v-ifile
	     v-sfile
	     v-xfile
	     vms-pointers
	     vns-include
	     vp-sfile
	     vp-xfile
	     vps-include))

(eval-when (compile eval)
   (or (get 'const 'loaded) (load '../const.l)))

; load in the macro files if compiling or interpreting.
;
(eval-when (compile eval)
   (or (get 'cmacros 'version) (load 'cmacros)))
