(include "chead.l")
(Liszt-file io  "@(#)io.l	1.1	9/25/81")

;;; ---- 	i o				input output
;;;


;--- d-prelude :: emit code common to beginning of all functions
;
(defun d-prelude nil
  (If g-flocal
      then (e-write3 'movl 'r10 '"-(sp)")	; (faster than pushl)
	   (e-write3 'movab `(,(* -4 g-currentargs) r6) 'r10)
	   (e-writel g-topsym)
      else
  	   (e-write2 '".word" '0x5c0)
	   (If fl-profile
	       then (e-write3 'movab 'mcounts 'r0)
		    (e-write2 'jsb 'mcount))
	   (e-write3 'movab 'linker '#.bind-reg)
	   (If (eq g-ftype 'lexpr) 
	     then 
	       (e-write4 'subl3 '$4 Lbot-reg '"-(sp)")  ; set up base for (arg)
	       (e-writel g-topsym)
	       (e-write3 'movl Np-reg oLbot-reg)	; will stack num of args
	       (e-write4 'subl3 Lbot-reg Np-reg 'r0)	; arg cnt again
	       (e-write3 'movab '"0x1400(r0)" np-plus)	; stack lispval
	       (e-write3 'movl '(0 #.oLbot-reg) '"-(sp)") ; also on runtime stk
	     else
	       ; set up old lbot register, base register for variable
	       ; references
	       (e-write3 'movl '#.Lbot-reg '#.oLbot-reg)
	       ; make sure the np register points where it should since
	       ; the caller might have given too few or too many args
	       (e-write3 'movab `(,(* 4 g-currentargs) #.oLbot-reg) 
			        '#.Np-reg)
	       (e-writel g-topsym))))

;--- d-fini :: emit code  at end of function

(defun d-fini nil
  (If g-flocal then (e-write3 'movl '"(sp)+" 'r10)
		    (e-write1 'rsb)
	       else (e-return)))


;--- d-bindtab :: emit binder table when all functions compiled
;
(defun d-bindtab nil
  (setq g-skipcode nil)	  ; make sure this isnt ignored	
  (e-writel "bind_org")
  (e-write2 ".set linker_size," (length g-lits))
  (e-write2 ".set trans_size," (length g-tran))
  (do ((ll (setq g-funcs (nreverse g-funcs)) (cdr ll)))
      ((null ll))
      (If (memq (caar ll) '(lambda nlambda macro eval))
	  then (e-write2 '".long" (cdr (assoc (caar ll) 
					      '((lambda . 0)
						(nlambda . 1)
						(macro . 2)
						(eval . 99)))))
	  else (comp-err " bad type in lit list " (car ll))))
  
  (e-write1 ".long -1")
  (e-write1 '"lit_org:")
  (d-asciiout (nreverse g-lits))
  (If g-tran then (d-asciiout (nreverse g-tran)))
  (d-asciiout (mapcar '(lambda (x) (If (eq (car x) 'eval)
				       then (cadr x)
				       else (caddr x)))
		      g-funcs))
  
  (e-write1 '"lit_end:"))

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
	       (If (greaterp remsiz 60) then (sfilewrite '".ascii \"")
		   else (sfilewrite '".asciz \""))
	       (setq thissiz (min 60 remsiz))
	       (do ((count thissiz (1- count)))
		   ((zerop count)
		    (sfilewrite (concat '\" (ascii 10)))
		    (setq remsiz (difference remsiz thissiz)))
		   (If (eq ch-newline (car curform))
		       then (sfilewrite '\\012)
		    else (If (or (eq '\\ (car curform))
				 (eq '\" (car curform)))
			     then (sfilewrite '\\))
			 (sfilewrite (car curform)))
		   (setq curform (cdr curform))))))

;--- d-autorunhead
;
(defun d-printautorun nil
  (sfilewrite
".set	exit,1
.word	0x0000
subl2	$8,sp
movl	8(sp),(sp)  #  argc
movab	12(sp),r0
movl	r0,4(sp)  #  argv
QL1:
tstl	(r0)+  #  null args term ?
bneq	QL1
cmpl	r0,*4(sp)  #  end of 'env' or 'argv' ?
blss	QL2
tstl	-(r0)  # envp's are in list
QL2:
movab	dr,8(sp)
movab	ln,4(sp)
movab	4(sp),r1
movl	sp,r2
pushl	r0	#stack environment
pushl	r1
pushab	ln
calls	$3,_execve
chmk	$exit
ln:
")
(sfilewrite (concat ".asciz	\"" lisp-object-directory "/lisp\"
dr:
.asciz	\"-f\"
.set	exece,59
_execve:
.word	0x0000
chmk	$exece
chmk	$exit
ret
")))



(defun e-cvt (arg)
  (If     (eq 'reg arg) then 'r0
   elseif (eq 'Nil arg) then '$0
   elseif (eq 'T arg) then (If g-trueloc thenret
			       else (setq g-trueloc (e-cvt (d-loclit t nil))))
   elseif (eq 'stack arg) then '(+ #.Np-reg)
   elseif (eq 'unstack arg) then '(- #.Np-reg)
   elseif (atom arg) then arg
   elseif (dtpr arg) then (If     (eq 'stack (car arg))
			      then `(,(* 4 (1- (cadr arg))) #.oLbot-reg)
			   elseif (eq 'vstack (car arg))
			      then `(* ,(* 4 (1- (cadr arg))) #.oLbot-reg)
			   elseif (eq 'bind (car arg))
			      then `(* ,(* 4 (1- (cadr arg))) #.bind-reg)
			   elseif (eq 'lbind (car arg))
			      then `( ,(* 4 (1- (cadr arg))) #.bind-reg)
			   elseif (eq 'fixnum (car arg))
			      then `(\# ,(cadr arg))
			   elseif (eq 'immed (car arg))
			      then `($ ,(cadr arg))
			   elseif (eq 'racc (car arg))
			      then (cdr arg)
			   else (comp-err " bad arg to e-cvt : "
					  (or arg)))
   else  (comp-warn "bad arg to e-cvt : " (or arg))))


;--- e-uncvt :: inverse of e-cvt, used for making comments pretty
;
(defun e-uncvt (arg)
  (If (atom arg) then (If (eq 'Nil arg) then nil
			  else arg)
   elseif (eq 'stack (car arg))
	  then (do ((i g-loccnt)
		    (ll g-locs))
		   ((and (equal i (cadr arg)) (atom (car ll))) (car ll))
		   (If (atom (car ll)) then (setq ll (cdr ll)
						  i (1- i))
					else (setq ll (cdr ll))))
   elseif (or (eq 'bind (car arg)) (eq 'lbind (car arg)))
	  then (do ((i g-litcnt (1- i))
		    (ll g-lits (cdr ll)))
		   ((equal i (cadr arg)) (cond ((eq 'lbind (car arg))
						(list 'quote (car ll)))
					       (t (car ll)))))
   else arg))

;--- e-cvtas :: convert an EIADR to vax unix assembler fmt and print it
;	- form : an EIADR form
;
(defun e-cvtas (form)
  (If (atom form)
      then (sfilewrite form)
   else (If (eq '* (car form)) then (If (eq '\# (cadr form))
					then (setq form `($ ,(caddr form)))
					else (sfilewrite "*")
					     (setq form (cdr form))))
	(If (numberp (car form))
	    then (sfilewrite (car form))
		 (sfilewrite "(")
		 (sfilewrite (cadr form))
		 (sfilewrite ")")
		 (If (caddr form)
		     then (sfilewrite "[")
			  (sfilewrite (caddr form))
			  (sfilewrite "]"))
	elseif (eq '+ (car form))
	    then (sfilewrite '"(")
		 (sfilewrite (cadr form))
		 (sfilewrite '")+")
	elseif (eq '- (car form))
	    then (sfilewrite '"-(")
		 (sfilewrite (cadr form))
		 (sfilewrite '")")
	elseif (eq '\# (car form))	; 5120 is base of small fixnums
	    then (sfilewrite (concat "$" (+ (* (cadr form) 4) 5120)))
	elseif (eq '$ (car form))
	    then (sfilewrite '"$")
		 (sfilewrite (cadr form)))))
;--- e-cmp :: emit code to compare the two given args
;	- arg1, arg2 : EIADRs
;
(defun e-cmp (arg1 arg2)
  (e-write3 'cmpl arg1 arg2))

;--- e-docomment :: print any comment lines
;
(defun e-docomment nil
  (If g-comments
      then (do ((ll (nreverse g-comments) (cdr ll)))
	       ((null ll))
	       (sfilewrite '"	#")
	       (do ((ll (exploden (car ll)) (cdr ll)))
		   ((null ll))
		   (tyo (car ll) vp-sfile)
		   (cond ((eq #\newline (car ll))
			  (sfilewrite "#"))))
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

;--- e-move :: move value from one place to anther
; this corresponds to d-move except the args are EIADRS
;
(defun e-move (from to)
  (If (equal 0 from) then (e-write2 'clrl to)
		     else (e-write3 'movl from to)))

;--- e-pop :: pop the given number of args from the stack
; g-locs is not! fixed
;
(defun e-pop (nargs)
  (If (greaterp nargs 0)
      then (e-dropnp nargs)))


;--- e-pushnil :: push a given number of nils on the stack
;
(defun e-pushnil (nargs)
  (do ((i nargs))
      ((zerop i))
      (If (greaterp i 1) then (e-write2  'clrq np-plus)
			      (setq i (- i 2))
	elseif (equal i 1) then (e-write2 'clrl np-plus)
				(setq i (1- i)))))

;--- e-tst :: test a value, arg is an EIADR
;
(defun e-tst (arg)
  (e-write2 'tstl arg))
;--- e-setupbind :: setup for shallow binding
;
(defun e-setupbind nil
  (e-write3 'movl '#.Bnp-val '#.bNp-reg))

;--- e-unsetupbind :: restore temp value of bnp to real loc
;
(defun e-unsetupbind nil
  (e-write3 'movl '#.bNp-reg '#.Bnp-val))

;--- e-shallowbind :: shallow bind value of variable and initialize it
;	- name : variable name
;	- val : IADR value for variable
;
(defun e-shallowbind (name val)
  (let ((vloc (d-loclit name t)))
       (e-write3 'movl (e-cvt vloc) '(+ #.bNp-reg))	; store old val
       (e-write3 'movl (e-cvt `(lbind ,@(cdr vloc)))
		       '(+ #.bNp-reg))		; now name
       (d-move val vloc)))		

;--- e-unshallowbind :: un shallow bind n variable from top of stack
;
(defun e-unshallowbind (n)
  (e-setupbind)		; set up binding register
  (do ((i 1 (1+ i)))
      ((greaterp i n))
      (e-write3 'movl `(,(* -8 i) ,bNp-reg) `(* ,(+ 4 (* -8 i)) ,bNp-reg)))
  (e-write4 'subl3 `($ ,(* 8 n)) bNp-reg Bnp-val))

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
  (If (not g-skipcode)
      then (setq g-dropnpcnt (+ n (If g-dropnpcnt thenret else 0)))))

;--- em-checknpdrop :: check if we have a pending npdrop
; and do it if so.
;
(defmacro em-checknpdrop nil
  `(If g-dropnpcnt then (let ((dr g-dropnpcnt))
			     (setq g-dropnpcnt nil)
			     (e-write3 'subl2 `($ ,(* dr 4)) Np-reg))))

;--- em-checkskip :: check if we are skipping this code due to jump
;
(defmacro em-checkskip nil
  '(If g-skipcode then (sfilewrite "# ")))


;--- e-jump :: jump to given label
; and set g-skipcode so that all code following until the next label
; will be skipped.
;
(defun e-jump (l)
  (em-checknpdrop)
  (e-write2 'jbr l)
  (setq g-skipcode t))

;--- e-return :: do return, and dont check for np drop
;
(defun e-return nil
  (setq g-dropnpcnt nil)  ; we dont need to worry about nps
  (e-write1 'ret))


;--- e-writel :: write out a label
;
(defun e-writel (label)
  (setq g-skipcode nil)
  (em-checknpdrop)
  (sfilewrite label)
  (sfilewrite '":")
  (e-docomment))

;--- e-write1 :: write out one litteral
;
(defun e-write1 (lit)
  (em-checkskip)
  (em-checknpdrop)
  (sfilewrite lit)
  (e-docomment))

;--- e-write2 :: write one one litteral, and one operand
;
(defun e-write2 (lit frm)
  (em-checkskip)
  (em-checknpdrop)
  (sfilewrite lit)
  (sfilewrite '"	")
  (e-cvtas frm)
  (e-docomment))

;--- e-write3 :: write one one litteral, and two operands
;
(defun e-write3 (lit frm1 frm2)
  (em-checkskip)
  (em-checknpdrop)
  (sfilewrite lit)
  (sfilewrite '"	")
  (e-cvtas frm1)
  (sfilewrite '",")
  (e-cvtas frm2)
  (e-docomment))

;--- e-write4 :: write one one litteral, and three operands
;
(defun e-write4 (lit frm1 frm2 frm3)
  (em-checkskip)
  (em-checknpdrop)
  (sfilewrite lit)
  (sfilewrite '"	")
  (e-cvtas frm1)
  (sfilewrite '",")
  (e-cvtas frm2)
  (sfilewrite '",")
  (e-cvtas frm3)
  (e-docomment))


;--- e-write5 :: write one one litteral, and four operands
;
(defun e-write5 (lit frm1 frm2 frm3 frm4)
  (em-checkskip)
  (em-checknpdrop)
  (sfilewrite lit)
  (sfilewrite '"	")
  (e-cvtas frm1)
  (sfilewrite '",")
  (e-cvtas frm2)
  (sfilewrite '",")
  (e-cvtas frm3)
  (sfilewrite '",")
  (e-cvtas frm4)
  (e-docomment))



;--- d-printdocstuff
;
; describe this version
;
(defun d-printdocstuff nil
   (sfilewrite ".data  # this is just for documentation ")
   (terpr vp-sfile)
   (sfilewrite (concat ".asciz \"@(#)Compiled by " compiler-name
                " on " (status ctime) '\"))
   (terpr vp-sfile)
   (do ((xx Liszt-file-names (cdr xx)))
       ((null xx))
       (sfilewrite (concat ".asciz \"" (car xx) '\"))
       (terpr vp-sfile)))
