;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-runFp.l "@(#)runFp.l	5.1 (Berkeley) 5/31/85")

; FASL (or load if no object files exist) then run FP.
; also set up  user-top-level to 'runFp'.

(include specials.l)

(declare
  (localf make_chset setup init addHelp initHelp)
  (special user-top-level))

(sstatus translink on)

(mapcar  'load 
  '(fpMain handlers scanner parser codeGen primFp utils fpPP fpMeasures))


(defun runFp nil
  (cond ((null (make_chset))
	 (patom "Illegal Character set")
	 (terpri)
	 (exit))
	
	(t
	 (setup)					; set up FP syntax funnies
	 (init)
	 (Tyi)
	 (msg N "FP, v. 4.2, (4/28/83)" N (B 6))))
  
  (setq user-top-level 'res_fp)		; from now on just resume FP--
  ; no need for extensive initializations
  
  (signal 2 'break-resp)
  (fpMain nil t))			; invoke fp, exit to shell when done

(defun res_fp nil			; restart fp after infinite recursion,
					; simpler initializatin than runFp.
   (signal 2 'break-resp)
   (msg N (B 6))
   (setq in_def nil infile nil outfile nil fn_name 'tmp$$ in_buf nil)
   (setq level 0)
   (fpMain nil t))


(defun make_chset nil
  (putprop 'fonts "+-,>!%&*/:=@{}()[]?~TF;#" 'asc)
  (cond ((null (setq rsrvd (get 'fonts char_set))))
	(t (setq e_rsrvd (explodec rsrvd)))))


(defun setup nil
  (setq newreadtable (makereadtable nil))
  (let ((readtable newreadtable))
       (mapcar '(lambda (z) (setsyntax z 66)) (exploden rsrvd))
       (setsyntax #/< 'macro 'readit))
  
  (setsyntax #/< 'macro 'readit))


(defun init nil
  ; these are the only chars which may delimit numbers
  ; (select operator)
  
  (setq num_delim$ '(#/, #/] #/@ #/: #/} 41 59 32 9 10 #/-))
  
  (setq timeIt nil)
  (setq char_set (concat 'scan$ char_set))
  (setq in_def nil)
  (setq infile nil)
  (setq outfile nil)
  (setq fn_name 'tmp$$)
  (setq in_buf nil)
  (setq level 0) 		; initialize level to 0
  (setq TracedFns nil) ; just to make sure TracedFns is defined
  (setq DynTraceFlg nil) ; default of no dynamic tracing
  
  
  
  ; These are the builtin function names
  
  (setq builtins 
	'(
	  out					; output fn - for debug only
	  tl					; left tail
	  id					; id
	  atom					; atom
	  eq					; equal
	  not					; not
	  and					; and
	  or					; or
	  xor					; xor
	  null					; null
	  iota					; counting sequence generator
	  ; (library functions)
	  sin
	  asin
	  cos
	  acos
	  log					; natural
	  exp
	  mod
	  ; (unary origin)
	  first					; the first element
	  last					; the last element
	  front					; all except last
	  pick					; get nth element
	  concat				; concat
	  pair					; makes pairs
	  split					; splits into two
	  reverse				; reverse
	  distl					; distribute left
	  distr					; distribute right
	  length				; length
	  trans					; transpose
	  while					; while
	  apndl					; append left
	  apndr					; append right
	  tlr					; right tail
	  rotl					; rotate left
	  rotr))				; rotate right
  
  (initStats)
  (initHelp))

(defun addHelp (text cmd)
  (putprop 'helpCmd text cmd))

(defun initHelp nil
  (addHelp "fsave <file>			Same as csave except without pretty-printing" 'fsave)
  (addHelp "cload <file>			Load Lisp code from a file (may be compiled)" 'cload)
  (addHelp "csave <file>			Output Lisp code for all user-defined fns" 'csave)
  (addHelp "debug on/off			Turn debugger output on/off" 'debug)
  (addHelp "lisp				Exit to the lisp system (return with '^D')" 'help)
  (addHelp "help		This text" 'help)
  (addHelp "script open/close/append [file] Open or close a script-file" 'script)
  (addHelp "timer on/off			Turn timer on/off" 'timing)
  (addHelp "trace on/off <fn1> ...		Start/Stop exec trace of <fn1> ..." 'trace)
  (addHelp "stats on/off/reset/print [file] collect and output dynamic stats" 'stats)
  (addHelp "fns				List all functions" 'fns)
  (addHelp "delete <fn1> ...		Delete <fn1> ..." 'delete)
  (addHelp "pfn <fn1> ...			Print source text of <fn1> ..." 'pfn)
  (addHelp "save <file>			Save defined fns in <file>" 'save)
  (addHelp "load <file>			Redirect input from <file>" 'load)
  )


  (setq user-top-level 'runFp)
  (setq char_set 'asc)			; set to the type of character set
					; desired at the moment only ascii (asc)
					; supported (no APL at this time).

