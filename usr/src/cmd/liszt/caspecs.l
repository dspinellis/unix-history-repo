
;------ special variables --------



;sccs id:  @(#)caspecs.l	5.3	10/22/80

; Copyright (c) 1980 ,  The Regents of the University of California.
; All rights reserved.  
; author: j. foderaro

(declare (special v-form g-loc g-cc g-locs g-loccnt g-labs g-args
		  g-ret g-skipcode g-dropnpcnt
		  g-bindloc fl-comments g-tran g-trancnt g-tranloc
		  g-topsym g-reguse g-ignorereg
		  fl-inter fl-xref
		 g-lits g-litcnt g-funcs g-spec Np-reg
		 Lbot-reg bind-reg np-plus bNp-reg
		 oLbot-reg Bnp-val ch-newline 
		 compiler-name internal-macros
		 old-top-level poport piport compiler-name
		 readtable original-readtable er-fatal ibase
		 fl-macl fl-tty g-functype g-spec g-litcnt
		 g-trueloc g-lits g-funcs g-fname g-comments
		 g-localf g-flocal g-ftype
		 twa-list old-declare-fcn vps-include fl-verb
		 user-top-level $gccount$ macros g-current
		 fl-profile fl-tran
 		 g-allf fl-vms vp-xfile v-xfile 
		 vms-pointers g-refseen g-reflst fl-warn
		 g-currentargs
		 $gc-count$
		 special
		 liszt-eof-forms
		 liszt-root-name
		 liszt-file-name
		 k-ftype vp-sfile formsiz)
  ;local functions in car.l
  (localf lcfform dodef d-prelude d-fini d-bindtab 
	  d-asciiout 
	  doevalwhen lambdacvt)
  ;local functions for cadr and cddr must be calculated
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
	ch-newline (ascii #\lf)))

(eval-when (compile load eval)
  (setq Bnp-val '_bnp))


