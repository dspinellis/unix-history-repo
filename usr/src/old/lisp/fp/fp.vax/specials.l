;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-specials.l "@(#)specials.l	5.1 (Berkeley) 5/31/85")

(declare (special
	  DynTraceFlg Measures TracedFns debug traceport level
	  piport poport ptport readtable infile outfile  newreadtable
	  char_num  fn_name  in_def
	  rsrvd  e_rsrvd  usr_fn_name in_buf  char_set
	  num_delim$  outfile builtins  wslen  stk  tkn timeIt))

(declare (macros t))

(eval-when (compile eval load)
  (or (get 'fpMacs 'loaded) (load 'fpMacs)))
