(include-if (null (get 'chead 'version)) "../chead.l")
(Liszt-file io
   "$Header: io.l,v 1.17 87/12/15 17:03:20 sklower Exp $")

;;; ---- 	i o				input output
;;;
;;;				-[Fri Sep  2 21:37:05 1983 by layer]-


;--- d-prelude :: emit code common to beginning of all functions
;
(defun d-prelude nil
   (let ((loada-op #+(or for-vax for-tahoe) 'movab #+for-68k 'lea)
	 (sub2-op  #+(or for-vax for-tahoe) 'subl2 #+for-68k 'subl)
	 (add2-op  #+(or for-vax for-tahoe) 'addl2 #+for-68k 'addl)
	 (temp-reg #+(or for-vax for-tahoe) '#.fixnum-reg #+for-68k 'a5))
       #+for-68k (setq g-stackspace (d-genlab) g-masklab (d-genlab))
       (if g-flocal
	   then #+for-tahoe (e-write2 '".word" '0x0)
		(C-push '#.olbot-reg)
		(e-write3 loada-op
			  `(,(* -4 g-currentargs) #.np-reg) '#.olbot-reg)
		(e-writel g-topsym)
	   else #+(or for-vax for-tahoe) (e-write2 '".word" '0x5c0)
		#+for-68k
		(progn
		    (e-write3 'link 'a6 (concat "#-" g-stackspace))
		    (e-write2 'tstb '(-132 sp))
		    (e-write3 'moveml `($ ,g-masklab)
			      (concat "a6@(-" g-stackspace ")"))
		    (e-move '#.Nilatom '#.nil-reg))
		(if fl-profile
		    then (e-write3 loada-op 'mcnts
				   #+(or for-vax for-tahoe) 'r0 #+for-68k 'a0)
			 (e-quick-call 'mcount))
		(e-write3 loada-op 'linker '#.bind-reg)
		(if (eq g-ftype 'lexpr)
		    then ; Here is the method:
			 ;  We push the number of arguments, nargs,
			 ; on the name stack twice, setting olbot-reg
			 ; to point to the second one, so that the user
			 ; has a copy that he can set, and we have
			 ; one that we can use for address calcs.
			 ;  So, the stack will look like this, after
			 ; the setup:
			 ;np    ->
			 ;olbot -> nargs (II)
			 ;      -> nargs (I)
			 ;      -> (arg nargs)
			 ;      -> (arg nargs-1)
			 ;...
			 ;      -> (arg 1)
			 ;
			 (if (null $global-reg$)
			     then (e-move '#.np-sym '#.np-reg))
			 (e-writel g-topsym)
			 (e-move '#.np-reg temp-reg)
			 (e-write3 sub2-op
				   (if $global-reg$
				       then '#.lbot-reg
				       else '#.lbot-sym) temp-reg)
			 (e-write3 add2-op (e-cvt '(fixnum 0)) temp-reg)
			 (L-push temp-reg)
			 (e-move '#.np-reg '#.olbot-reg)
			 (L-push temp-reg)
		    else ;   Set up old lbot register, base reg for variable
			 ; references, and make sure the np points where
			 ; it should since the caller might
			 ; have given too few or too many args.
			 (e-move
				   (if $global-reg$
				       then '#.lbot-reg
				       else '#.lbot-sym)
				   '#.olbot-reg)
			 #+for-68k
			 (e-write3 loada-op
				   `(,(* 4 g-currentargs) #.olbot-reg)
				   '#.np-reg)
			 (e-writel g-topsym)))))

;--- d-fini :: emit code  at end of function
;
(defun d-fini nil
   (if g-flocal
       then (C-pop '#.olbot-reg)
	    (e-write1 #+for-vax 'rsb #+for-tahoe 'ret #+for-68k 'rts)
       else #+for-68k
	    (progn
		(e-write3 'moveml (concat "a6@(-" g-stackspace ")")
			  `($ ,g-masklab))
		(e-write2 'unlk 'a6))
	    (e-return)))

;--- d-bindtab :: emit binder table when all functions compiled
;
(defun d-bindtab nil
  (setq g-skipcode nil)	  ; make sure this isnt ignored	
  (e-writel "bind_org")
  #+(or for-vax for-tahoe)
  (progn
      (e-write2 ".set linker_size," (length g-lits))
      (e-write2 ".set trans_size," (length g-tran)))
  #+for-68k
  (progn
      (e-write2 "linker_size = " (length g-lits))
      (e-write2 "trans_size = " (length g-tran)))
  (do ((ll (setq g-funcs (nreverse g-funcs)) (cdr ll)))
      ((null ll))
      (if (memq (caar ll) '(lambda nlambda macro eval))
	  then (e-write2 '".long"
			 (cdr (assoc (caar ll)
				     '((lambda . 0) (nlambda . 1)
				       (macro . 2) (eval . 99)))))
	  else (comp-err " bad type in lit list " (car ll))))
  
  (e-write1 ".long -1")
  (e-writel "lit_org")
  (d-asciiout (nreverse g-lits))
  (if g-tran then (d-asciiout (nreverse g-tran)))
  (d-asciiout (mapcar '(lambda (x) (if (eq (car x) 'eval)
				       then (cadr x)
				       else (caddr x)))
		      g-funcs))
  (e-writel "lit_end"))

;--- d-asciiout :: print a list of asciz strings
;
(defun d-asciiout (args)
       (do ((lits args (cdr lits))
	    (form))
	   ((null lits))
	   (setq form (explode (car lits))
		 formsiz (length form))
	   (do ((remsiz formsiz)
		(curform form)
		(thissiz))
	       ((zerop remsiz))
	       (if (greaterp remsiz 60) then (sfilewrite '".ascii \"")
		   else (sfilewrite '".asciz \""))
	       (setq thissiz (min 60 remsiz))
	       (do ((count thissiz (1- count)))
		   ((zerop count)
		    (sfilewrite (concat '\" (ascii 10)))
		    (setq remsiz (difference remsiz thissiz)))
		   (if (eq '#.ch-newline (car curform))
		       then (sfilewrite '\\012)
		    else (if (or (eq '\\ (car curform))
				 (eq '\" (car curform)))
			     then (sfilewrite '\\))
			 (sfilewrite (car curform)))
		   (setq curform (cdr curform))))))

;--- d-autorunhead
;
; Here is the C program to generate the assembly language:
;	(after some cleaning up)
;
;main(argc,argv,arge)
;register char *argv[];
;register char **arge;
;{
;	*--argv = "-f";
;	*--argv = "/usr/ucb/lisp";
;	execve("/usr/ucb/lisp",argv,arge);
;	exit(0);
;}
;
(defun d-printautorun nil
   (let ((readtable (makereadtable t))	; in raw readtable
	 tport ar-file)
      (setsyntax #/; 'vsplicing-macro 'zapline)
      (setq ar-file (concat lisp-library-directory
			    #+for-vax "/autorun/vax"
			    #+for-tahoe "/autorun/tahoe"
			    #+for-68k "/autorun/68k"))
      (if (null (errset (setq tport (infile ar-file))))
	 then (comp-err "Can't open autorun header file " ar-file))
      (do ((x (read tport '<eof>) (read tport '<eof>)))
	  ((eq '<eof> x) (close tport))
	  (sfilewrite x))))

(defun e-cvt (arg)
   (if     (eq 'reg arg) then #+(or for-vax for-tahoe) 'r0 #+for-68k 'd0
    elseif (eq 'areg arg) then #+(or for-vax for-tahoe) 'r0 #+for-68k 'a0
    elseif (eq 'Nil arg) then #+(or for-vax for-tahoe) '($ 0)
    			      #+for-68k '#.nil-reg
    elseif (eq 'T arg)
       then (if g-trueloc
		thenret
		else (setq g-trueloc (e-cvt (d-loclit t nil))))
    elseif (eq 'stack arg) then '(+ #.np-reg)
    elseif (eq 'unstack arg) then (progn #+for-tahoe (e-sub '($ 4) '#.np-reg)
					 '(- #.np-reg))
    elseif (or (atom arg) (symbolp arg)) then arg
    elseif (dtpr arg)
       then (caseq (car arg)
		   (stack	`(,(* 4 (1- (cadr arg))) #.olbot-reg))
		   (vstack	`(* ,(* 4 (1- (cadr arg))) #.olbot-reg))
		   (bind	`(* ,(* 4 (1- (cadr arg))) #.bind-reg))
		   (lbind	`(,(* 4 (1- (cadr arg))) #.bind-reg))
		   (fixnum	`(\# ,(cadr arg)))
		   (immed	`($ ,(cadr arg)))
		   (racc	(cdr arg))
		   (t		(comp-err " bad arg to e-cvt : "
					  (or arg))))
       else  (comp-warn "bad arg to e-cvt : " (or arg))))

;--- e-uncvt :: inverse of e-cvt, used for making comments pretty
;
(defun e-uncvt (arg)
   (if (atom arg)
       then (if (eq 'Nil arg)
		then nil
		else arg)
    elseif (eq 'stack (car arg))
       then (do ((i g-loccnt)
		 (ll g-locs))
		((and (equal i (cadr arg)) (atom (car ll))) (car ll))
		(if (atom (car ll))
		    then (setq ll (cdr ll)
			       i (1- i))
		    else (setq ll (cdr ll))))
    elseif (or (eq 'bind (car arg)) (eq 'lbind (car arg)))
       then (do ((i g-litcnt (1- i))
		 (ll g-lits (cdr ll)))
		((equal i (cadr arg))
		 (cond ((eq 'lbind (car arg))
			(list 'quote (car ll)))
		       (t (car ll)))))
       else arg))

;--- e-cvtas :: convert an EIADR to vax unix assembler fmt and print it
;	- form : an EIADR form
;
#+(or for-vax for-tahoe)
(defun e-cvtas (form)
  (if (atom form)
      then (sfilewrite form)
      else (if (eq '* (car form))
	       then (if (eq '\# (cadr form))
			then (setq form `($ ,(caddr form)))
			else (sfilewrite "*")
			     (setq form (cdr form))))
	   (if (numberp (car form))
	       then (sfilewrite (car form))
		    (sfilewrite "(")
		    (sfilewrite (cadr form))
		    (sfilewrite ")")
		    (if (caddr form)
			then (sfilewrite "[")
			     (sfilewrite (caddr form))
			     (sfilewrite "]"))
	    elseif (eq '+ (car form))
	       then (sfilewrite '"(")
		    (sfilewrite (cadr form))
		    (sfilewrite '")")
		    #-for-tahoe (sfilewrite '"+")
	    elseif (eq '- (car form))
	       then #-for-tahoe (sfilewrite '"-")
		    (sfilewrite '"(")
		    (sfilewrite (cadr form))
		    (sfilewrite '")")
	    elseif (eq '\# (car form))	; 5120 is base of small fixnums
	       then (sfilewrite (concat "$" (+ (* (cadr form) 4) 5120)))
	    elseif (eq '$ (car form))
	       then (sfilewrite '"$")
		    (sfilewrite (cadr form)))))

#+for-68k
(defun e-cvtas (form)
   (if (atom form)
       then (sfilewrite form)
       else (if (eq '* (car form))
		then (if (eq '\# (cadr form))
			 then (setq form `($ ,(caddr form)))))
	    (if (numberp (car form))
		then (sfilewrite (cadr form))
		     (sfilewrite "@")
		     (if (not (zerop (car form)))
			 then (sfilewrite "(")
			      (sfilewrite (car form))
			      (sfilewrite ")"))
	    elseif (eq '% (car form))
	       then (setq form (cdr form))
		    (sfilewrite (cadr form))
		    (sfilewrite "@(")
		    (sfilewrite (car form))
		    (sfilewrite ",")
		    (sfilewrite (caddr form))
		    (sfilewrite ":L)")
	     elseif (eq '+ (car form))
		then (sfilewrite (cadr form))
		     (sfilewrite '"@+")
	     elseif (eq '- (car form))
		then (sfilewrite (cadr form))
		     (sfilewrite '"@-")
	     elseif (eq '\# (car form))
		then (sfilewrite (concat '#.Nilatom "+0x1400"
					 (if (null (signp l (cadr form)))
					     then "+" else "")
					 (* (cadr form) 4)))
	     elseif (eq '$ (car form))
		then (sfilewrite '"#")
		     (sfilewrite (cadr form))
	       else (comp-err " bad arg to e-cvtas : " (or form)))))

;--- e-postinc :: handle postincrement for the tahoe machine
;

#+for-tahoe
(defun e-postinc (addr)
   (if (and (dtpr addr) (eq (car addr) '+))
       (e-add '($ 4) (cadr addr))))


;--- e-docomment :: print any comment lines
;
(defun e-docomment nil
  (if g-comments
      then (do ((ll (nreverse g-comments) (cdr ll)))
	       ((null ll))
	       (sfilewrite "	")
	       (sfilewrite #.comment-char)
	       (do ((ll (exploden (car ll)) (cdr ll)))
		   ((null ll))
		   (tyo (car ll) vp-sfile)
		   (cond ((eq #\newline (car ll))
			  (sfilewrite #.comment-char))))
	       (terpr vp-sfile))
	   (setq g-comments nil)
     else (terpr vp-sfile)))

;--- e-goto :: emit code to jump to the location given
;
(defun e-goto (lbl)
  (e-jump lbl))

;--- e-gotonil :: emit code to jump if nil was last computed
;
(defun e-gotonil (lbl)
  (e-write2 g-falseop lbl))

;--- e-gotot :: emit code to jump if t was last computed
(defun e-gotot (lbl)
  (e-write2  g-trueop lbl))

;--- e-label :: emit a label
(defun e-label (lbl)
  (setq g-skipcode nil)
  (e-writel lbl))

;--- e-pop :: pop the given number of args from the stack
; g-locs is not! fixed
;
(defun e-pop (nargs)
  (if (greaterp nargs 0)
      then (e-dropnp nargs)))

;--- e-pushnil :: push a given number of nils on the stack
;
#+for-vax
(defun e-pushnil (nargs)
   (do ((i nargs))
       ((zerop i))
       (if (>& i 1)
	   then (e-write2 'clrq '#.np-plus)
		(setq i (- i 2))
	elseif (equal i 1)
	   then (e-write2 'clrl '#.np-plus)
		(setq i (1- i)))))

#+for-tahoe
(defun e-pushnil (nargs)
  (do ((i nargs))
      ((zerop i))
      (e-write2 'clrl '#.np-plus)
      (setq i (1- i))))

#+for-68k
(defun e-pushnil (nargs)
  (do ((i nargs))
      ((zerop i))
      (L-push '#.nil-reg)
      (setq i (1- i))))

;--- e-setupbind :: setup for shallow binding
;
(defun e-setupbind nil
  (e-move '#.bnp-sym '#.bnp-reg))

;--- e-unsetupbind :: restore temp value of bnp to real loc
;
(defun e-unsetupbind nil
  (e-move '#.bnp-reg '#.bnp-sym))

;--- e-shallowbind :: shallow bind value of variable and initialize it
;	- name : variable name
;	- val : IADR value for variable
;
#+(or for-vax for-68k)
(defun e-shallowbind (name val)
  (let ((vloc (d-loclit name t)))
       (e-move (e-cvt vloc) '(+ #.bnp-reg))	; store old val
       (e-move (e-cvt `(lbind ,@(cdr vloc)))
		       '(+ #.bnp-reg))		; now name
       (d-move val vloc)))		

#+for-tahoe
(defun e-shallowbind (name val)
  (let ((vloc (d-loclit name t)))
       (e-move (e-cvt vloc) '(0 #.bnp-reg))	; store old val
       (e-add '($ 4) '#.bnp-reg)
       (e-move (e-cvt `(lbind ,@(cdr vloc)))
		       '(0 #.bnp-reg))		; now name
       (e-add '($ 4) '#.bnp-reg)
       (d-move val vloc)))		

;--- e-unshallowbind :: un shallow bind n variable from top of stack
;
#+(or for-vax for-tahoe)
(defun e-unshallowbind (n)
  (e-setupbind)		; set up binding register
  (do ((i 1 (1+ i)))
      ((greaterp i n))
      (e-move `(,(* -8 i) #.bnp-reg) `(* ,(+ 4 (* -8 i)) #.bnp-reg)))
  (e-sub3 `($ ,(* 8 n)) '#.bnp-reg '#.bnp-sym))

#+for-68k
(defun e-unshallowbind (n)
  (makecomment "e-unshallowbind begin...")
  (e-setupbind)		; set up binding register
  (do ((i 1 (1+ i)))
      ((greaterp i n))
      (e-move `(,(* -8 i) #.bnp-reg) `(* ,(+ 4 (* -8 i)) #.bnp-reg)))
  (e-move '#.bnp-reg '#.bnp-sym)
  (e-sub `($ ,(* 8 n)) '#.bnp-sym)
  (makecomment "...end e-unshallowbind"))

;----------- very low level routines
; all output to the assembler file goes through these routines.
; They filter out obviously extraneous instructions as well as 
; combine sequential drops of np.

;--- e-dropnp :: unstack n values from np.
; rather than output the instruction now, we just remember that it
; must be done before any other instructions are done.  This will
; enable us to catch sequential e-dropnp's
;
(defun e-dropnp (n)
  (if (not g-skipcode)
      then (setq g-dropnpcnt (+ n (if g-dropnpcnt thenret else 0)))))

;--- em-checknpdrop :: check if we have a pending npdrop
; and do it if so.
;
(defmacro em-checknpdrop nil
   `(if g-dropnpcnt
	then (let ((dr g-dropnpcnt))
		 (setq g-dropnpcnt nil)
		 (e-sub `($ ,(* dr 4)) '#.np-reg))))

;--- em-checkskip :: check if we are skipping this code due to jump
;
(defmacro em-checkskip nil
  '(if g-skipcode then (sfilewrite #.comment-char)))


;--- e-jump :: jump to given label
; and set g-skipcode so that all code following until the next label
; will be skipped.
;
(defun e-jump (l)
  (em-checknpdrop)
  (e-write2 #+(or for-vax for-tahoe) 'jbr #+for-68k 'jra l)
  (setq g-skipcode t))

;--- e-return :: do return, and dont check for np drop
;
(defun e-return nil
  (setq g-dropnpcnt nil)  ; we dont need to worry about nps
  #+(or for-vax for-tahoe) (e-write1 'ret)
  #+for-68k (progn  (e-write1 'rts)
		    (sfilewrite
		       (concat g-masklab " = " (d-makemask) '#.ch-newline))
		    (sfilewrite
		       (concat g-stackspace " = "
			       (Cstackspace) '#.ch-newline))))

;--- e-writel :: write out a label
;
(defun e-writel (label)
  (setq g-skipcode nil)
  (em-checknpdrop)
  (sfilewrite label)
  (sfilewrite ":")
  (e-docomment))

;--- e-write1 :: write out one litteral
;
(defun e-write1 (lit)
  (em-checkskip)
  (em-checknpdrop)
  (sfilewrite "	")
  (sfilewrite lit)
  (e-docomment))

;--- e-write2 :: write one one litteral, and one operand
;
#+(or for-vax for-tahoe)
(defun e-write2 (lit frm)
  (em-checkskip)
  (em-checknpdrop)
  (sfilewrite "	")
  (sfilewrite lit)
  (sfilewrite "	")
  (e-cvtas frm)
  (e-docomment)
  #+for-tahoe (e-postinc frm))

#+for-68k
(defun e-write2 (lit frm)
  (em-checkskip)
  (em-checknpdrop)
  (if (and (dtpr frm) (eq (car frm) '*))
      then (e-move (cdr frm) 'a5)
	   (sfilewrite "	")
	   (sfilewrite lit)
	   (sfilewrite '"	")
	   (e-cvtas '(0 a5))
      else (sfilewrite "	")
	   (sfilewrite lit)
	   (sfilewrite '"	")
	   (e-cvtas frm))
  (e-docomment))

;--- e-write3 :: write one one litteral, and two operands
;
#+(or for-vax for-tahoe)
(defun e-write3 (lit frm1 frm2)
  (em-checkskip)
  (em-checknpdrop)
  (sfilewrite "	")
  (sfilewrite lit)
  (sfilewrite "	")
  (e-cvtas frm1)
  (sfilewrite ",")
  (e-cvtas frm2)
  (e-docomment)
  #+for-tahoe (e-postinc frm1)
  #+for-tahoe (e-postinc frm2))

#+for-68k
(defun e-write3 (lit frm1 frm2)
   (em-checkskip)
   (em-checknpdrop)
   (if (and (dtpr frm1) (eq (car frm1) '*)
	    (not (and (dtpr frm2) (eq (car frm2) '*))))
       then (e-move (cdr frm1) 'a5)
	    (sfilewrite "	")
	    (sfilewrite lit)
	    (sfilewrite '"	")
	    (e-cvtas '(0 a5))
	    (sfilewrite '",")
	    (e-cvtas frm2)
	    (e-docomment)
    elseif (and (not (and (dtpr frm1) (eq (car frm1) '*)))
		(dtpr frm2) (eq (car frm2) '*))
       then (e-move (cdr frm2) 'a5)
	    (sfilewrite "	")
	    (sfilewrite lit)
	    (sfilewrite '"	")
	    (e-cvtas frm1)
	    (sfilewrite '",")
	    (e-cvtas '(0 a5))
	    (e-docomment)
    elseif (and (dtpr frm1) (eq (car frm1) '*)
		(dtpr frm2) (eq (car frm2) '*))
       then (d-regused 'd6)
	    (e-move (cdr frm1) 'a5)
	    (e-move '(0 a5) 'd6)
	    (e-move (cdr frm2) 'a5)
	    (sfilewrite "	")
	    (sfilewrite lit)
	    (sfilewrite '"	")
	    (e-cvtas 'd6)
	    (sfilewrite '",")
	    (e-cvtas '(0 a5))
	    (e-docomment)
       else (sfilewrite "	")
	    (sfilewrite lit)
	    (sfilewrite '"	")
	    (e-cvtas frm1)
	    (sfilewrite '",")
	    (e-cvtas frm2)
	    (e-docomment)))

;--- e-write4 :: write one one litteral, and three operands
;
#+(or for-vax for-tahoe)
(defun e-write4 (lit frm1 frm2 frm3)
  (em-checkskip)
  (em-checknpdrop)
  (sfilewrite "	")
  (sfilewrite lit)
  (sfilewrite "	")
  (e-cvtas frm1)
  (sfilewrite ",")
  (e-cvtas frm2)
  (sfilewrite ",")
  (e-cvtas frm3)
  (e-docomment)
  #+for-tahoe (e-postinc frm1)
  #+for-tahoe (e-postinc frm2)
  #+for-tahoe (e-postinc frm3))


;--- e-write5 :: write one one litteral, and four operands
;
#+(or for-vax for-tahoe)
(defun e-write5 (lit frm1 frm2 frm3 frm4)
  (em-checkskip)
  (em-checknpdrop)
  (sfilewrite "	")
  (sfilewrite lit)
  (sfilewrite "	")
  (e-cvtas frm1)
  (sfilewrite ",")
  (e-cvtas frm2)
  (sfilewrite ",")
  (e-cvtas frm3)
  (sfilewrite ",")
  (e-cvtas frm4)
  (e-docomment)
  #+for-tahoe (e-postinc frm1)
  #+for-tahoe (e-postinc frm2)
  #+for-tahoe (e-postinc frm3)
  #+for-tahoe (e-postinc frm4))

;--- d-printdocstuff
;
; describe this version
;
(defun d-printdocstuff nil
   (sfilewrite (concat ".data "
		       #.comment-char
		       " this is just for documentation "))
   (terpr vp-sfile)
   (sfilewrite (concat ".asciz \"@(#)Compiled by " compiler-name
                " on " (status ctime) '\"))
   (terpr vp-sfile)
   (do ((xx Liszt-file-names (cdr xx)))
       ((null xx))
       (sfilewrite (concat ".asciz \"" (car xx) '\"))
       (terpr vp-sfile)))
