(setq SCCS-specials.l "@(#)specials.l	4.1	12/14/82")
;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California

(declare (special
	  DynTraceFlg Measures TracedFns debug traceport level
	  piport poport ptport readtable infile outfile  newreadtable
	  char_num  fn_name  in_def
	  rsrvd  e_rsrvd  usr_fn_name in_buf  char_set
	  num_delim$  outfile builtins  wslen  stk  tkn timeIt))

(declare (macros t))

(eval-when (compile eval load)
  (or (get 'fpMacs 'loaded) (load 'fpMacs)))
