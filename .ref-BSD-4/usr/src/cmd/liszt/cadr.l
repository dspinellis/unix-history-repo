

;		l i s z t   v 4




; Copyright (c) 1980 ,  The Regents of the University of California.
; All rights reserved.  
; author: j. foderaro

; Section EXPR  -- general expression compiler

(include "caspecs.l")

(eval-when (compile eval)
  (cond ((not (getd 'If))
	 (fasl 'camacs))))

(setq sectioncadrid "@(#)cadr.l	5.4	10/22/80")  ; id for SCCS

;--- d-exp :: compile a lisp expression				= d-exp =
;	v-form : a lisp expression to compile
; returns an IADR which tells where the value was located.
;
(defun d-exp (v-form)
  (prog (first resloc tmp ftyp)

    begin
	(If (atom v-form)
	    then (setq tmp (d-loc v-form))		;locate vrble
		 (If (null g-loc)
		     then (If g-cc then (d-tst tmp))
		     else (d-move tmp g-loc))
		 (d-handlecc)
		 (return tmp)

	 elseif (atom (setq first (car v-form)))
	    then (If (and fl-xref (not (get first g-refseen)))
		     then (Push g-reflst first)
			  (putprop first t g-refseen))
	         (setq ftyp (d-functyp first))
		 (If (eq 'macro ftyp)
		     then (setq v-form (apply first v-form))
			  (go begin)
		  elseif (setq tmp (get first 'fl-exprcc))
		     then (return (funcall tmp))
		  elseif (setq tmp (get first 'fl-exprm))
		     then (setq v-form (funcall tmp))
		  	  (go begin)
		  elseif (setq tmp (get first 'fl-expr))
		     then (funcall tmp)
		  elseif (setq tmp (or (and (eq 'car first)
					    '( a ))
				       (and (eq 'cdr first)
					    '( d ))
				       (d-cxxr first)))
		     then (return (cc-cxxr (cadr v-form) tmp))
		   elseif (eq 'nlambda ftyp)
		     then (d-callbig first `(',(cdr v-form)))
		   elseif (or (eq 'lambda ftyp) (eq 'lexpr ftyp))
		     then (setq tmp (length v-form))
		     
			  (d-callbig first (cdr v-form)))
	 elseif (eq 'lambda (car first))
	    then (c-lambexp)

	 elseif (or (eq 'quote (car first)) (eq 'function (car first)))
	    then (comp-warn "bizzare function name " (or first))
		 (setq v-form (cons (cadr first) (cdr v-form)))
		 (go begin)
		
	 else (comp-err "bad expression" (or v-form)))

(If (null g-loc)
    then (If g-cc then (d-tst 'reg))
 elseif (eq g-loc 'reg)
    then (If g-cc then (d-tst 'reg))
 else (d-move 'reg g-loc))
(If g-cc then (d-handlecc))))

;--- d-functyp :: return the type of function
;	- name : function name
;
(defun d-functyp (name)
  (let (ftyp )
       (If (atom name) then 
	   (If (setq ftyp (getd name))
	       then (If (bcdp ftyp)
			then (getdisc ftyp)
		     elseif (dtpr ftyp)
			then (car ftyp))
	     elseif (get name g-functype) thenret
	     else 'lambda))))		; default is lambda


;--- d-exps :: compile a list of expressions
;	- exps : list of expressions
; the last expression is evaluated according to g-loc and g-cc, the others
; are evaluated with g-loc and g-cc nil.
;
(defun d-exps (exps)
  (d-exp (do ((ll exps (cdr ll))
	      (g-loc nil)
	      (g-cc  nil)
	      (g-ret nil))
	     ((null (cdr ll)) (car ll))
	     (d-exp (car ll)))))


;--- d-pushargs :: compile and push a list of expressions
;	- exps : list of expressions
; compiles and stacks a list of expressions
;
(defun d-pushargs (args)
  (If args then (do ((ll args (cdr ll))
		     (g-loc 'stack)
		     (g-cc nil)
		     (g-ret nil))
		    ((null ll))
		    (d-exp (car ll))
		    (Push g-locs nil)
		    (incr g-loccnt))))

;--- d-cxxr :: split apart a cxxr function name
;	- name : a possible cxxr function name
; returns the a's and d's between c and r in reverse order, or else
;  returns nil if this is not a cxxr name
;
(defun d-cxxr (name)
  (let ((expl (explodec name)))
       (If (eq 'c (car expl))			; must begin with c
	   then (do ((ll (cdr expl) (cdr ll))
		     (tmp)
		     (res))
		    (nil)
		    (setq tmp (car ll))
		    (If (null (cdr ll))	
			then (If (eq 'r tmp)	; must end in r
				 then (return res)
				 else (return nil))
		     elseif (or (eq 'a tmp)	; and contain only a's and d's
				(eq 'd tmp))
			then (setq res (cons tmp res))
		     else (return nil))))))

;--- d-call :: call another function
;	- name : name of funtion to call
;	- nargs : number of args stacked (including the function name)
;
(defun d-call (name nargs)
  (prog (tmp)
	(forcecomment `(calling ,name))
	(If (null (setq tmp (cdr (assoc nargs
					'( (1 . (* -8 #.bind-reg))
					   (2 . (* -12 #.bind-reg))
					   (3 . (* -16 #.bind-reg))
					   (4 . (* -20 #.bind-reg))
					   (5 . (* -24 #.bind-reg)))))))
	    then   ; lbot will not be set up automatically
		   (e-write3 'movab		; must set up lbot
			     `(,(* -4 nargs) #.Np-reg)
			     '#.Lbot-reg)
		   (setq tmp '(* -28 #.bind-reg)))
	(e-write2 'jsb tmp)))

;--- d-callbig :: call a local or global function	
;
;
(defun d-callbig (name args)
  (let ((tmp (get name g-localf))
	c)
       (forcecomment `(calling ,name))
       (If (d-dotailrecursion name args) thenret
        elseif tmp then ;-- local function call
		    (d-pushargs args)
		    (e-write2 'jsb (car tmp))
		    (setq g-locs (nthcdr (setq c (length args)) g-locs))
		    (setq g-loccnt (- g-loccnt c))
	else (If fl-tran	;-- transfer table linkage
	       then (d-pushargs args)
		    (setq c (length args))
		    (d-calltran name c)
	       else ;--- standard function call
		    (d-pushargs `(',name ,@args))
		    (d-call name (setq c (1+ (length args)))))
	     (setq g-locs (nthcdr c g-locs))
	     (setq g-loccnt (- g-loccnt c)))
       (d-clearreg)))
	

;--- d-calltran :: call a function through the transfer table  	  = d-calltran =
;  name - name of function to call
;  c - number of arguments to the function
;
(defun d-calltran (name c)
  (e-write3 'movab `(,(* -4 c) #.Np-reg) '#.Lbot-reg)
  (e-write3 'calls '$0 (concat "*trantb+" (d-tranloc name)))
  (e-write3 'movl '#.Lbot-reg '#.Np-reg))

;--- d-tranloc :: locate a function in the transfer table	  = d-tranloc =
;
; return the offset we should use for this function call
;
(defun d-tranloc (fname)
  (cond ((get fname g-tranloc))
	(t (Push g-tran fname)
	   (let ((newval (* 8 g-trancnt)))
		(putprop fname newval g-tranloc)
		(incr g-trancnt)
		newval))))

;--- d-dotailrecursion :: do tail recursion if possible
; name - function name we are to call
; args - arguments to give to function
;
; return t iff we were able to do tail recursion
; We can do tail recursion if:
;  g-ret is set indicating that the result of this call will be returned
;	 as the value of the function we are compiling
;  the function we are calling, name, is the same as the function we are
;	 compiling, g-fname
;  there are no variables shallow bound, since we would have to unbind
;	 them, which may cause problems in the function.
;
(defun d-dotailrecursion (name args)
  (If (and g-ret 
	   (eq name g-fname)
           (do ((loccnt 0)
		(ll g-locs (cdr ll)))
	       ((null ll) (return t))
	       (If (dtpr (car ll))
		   then (If (or (eq 'catcherrset (caar ll))
				(greaterp (cdar ll) 0))
			    then (return nil))
		   else (incr loccnt))))
      then 
	    ; evalate the arguments and pop them back to the location of
	    ; the original args.
	    (makecomment '(tail merging))
	    (comp-note "Tail merging being done: " v-form)
	    (let ((g-locs g-locs)
		  (g-loccnt g-loccnt))
		 (d-pushargs args))     ; push then forget about
	    (let (base-reg nargs)
	         (If (eq g-ftype 'lexpr)
		     then ; the beginning of the local variables 
			  ;has been stacked
		          (e-write3 'addl2 '$4 'sp)	; pop off arg count
			  (e-write4 'addl3 '$4 "(sp)" Lbot-reg) 
			  (setq base-reg Lbot-reg)	; will push from   bot
		     else (setq base-reg oLbot-reg)) ; will push from olbot
		 (setq nargs (length args))
		 (do ((i nargs (1- i))
		      (top (* nargs -4) (+ top 4))
		      (bot 0 (+ bot 4)))
		     ((zerop i))
		     (e-write3 'movl `(,top ,Np-reg) `(,bot ,base-reg)))
		 (e-write3 'movab `(,(* 4 nargs) ,base-reg) Np-reg)
		 (e-goto g-topsym))
	    t)) ; return t to indicate that tailrecursion was successful




; Section xxx -- specific function compilers
;

;--- cc-and :: compile an and expression
; We evaluate forms from left to right as long as they evaluate to
; a non nil value.  We only have to worry about storing the value of
; the last expression in g-loc.
;
(defun cc-and nil
  (let ((finlab (d-genlab))
	(finlab2)
	(exps (If (cdr v-form) thenret else '(t))))	; (and) ==> t
       (If (null (cdr g-cc))
	   then (d-exp (do ((g-cc (cons nil finlab))
			    (g-loc)
			    (g-ret)
			    (ll exps (cdr ll)))
			   ((null (cdr ll)) (car ll))
			   (d-exp (car ll))))
		(If g-loc then (setq finlab2 (d-genlab))
			       (e-goto finlab2)
			       (e-label finlab)
			       (d-move 'Nil g-loc)
			       (e-label finlab2)
			  else (e-label finlab))
	   else ;--- cdr g-cc is non nil, thus there is
		; a quick escape possible if one of the
		; expressions evals to nil

		(If (null g-loc) then (setq finlab (cdr g-cc)))
	        (d-exp (do ((g-cc (cons nil finlab))
			    (g-loc)
			    (g-ret)
			    (ll exps (cdr ll)))
			   ((null (cdr ll)) (car ll))
			   (d-exp (car ll))))
		; if g-loc is non nil, then we have evaled the and
		; expression to yield nil, which we must store in
		; g-loc and then jump to where the cdr of g-cc takes us
		(If g-loc then (setq finlab2 (d-genlab))
			       (e-goto finlab2)
			       (e-label finlab)
			       (d-move 'Nil g-loc)
			       (e-goto (cdr g-cc))
			       (e-label finlab2))))
  (d-clearreg))	 ; we cannot predict the state of the registers
	    
			  


;--- cc-arg  :: get the nth arg from the current lexpr		= cc-arg =
;
; the syntax for Franz lisp is (arg i)
; for interlisp the syntax is (arg x i) where x is not evaluated and is
; the name of the variable bound to the number of args.  We can only handle
; the case of x being the variable for the current lexpr we are compiling
;
(defun cc-arg nil 
  (let ((nillab (d-genlab)) (finlab (d-genlab)))
       (If (not (eq 'lexpr g-ftype)) 
	   then (comp-err " arg only allowed in lexprs"))
       (If (and (eq (length (cdr v-form)) 2)  fl-inter)
	   then (If (not (eq (car g-args) (cadr v-form)))
		    then (comp-err " arg expression is for non local lexpr "
				   v-form)
		    else (setq v-form (cdr v-form))))
       (If (or g-loc g-cc)
	   then (let ((g-loc 'reg) 
		      (g-cc (cons nil nillab))
		      (g-ret))
		     (d-exp `(cdr ,(cadr v-form))))	; calc the numeric arg
	        (If g-loc then (d-move '"*-4(fp)[r0]" g-loc)
		   else (e-tst '"*-4(fp)[r0]"))
	        (d-handlecc)
		(e-goto finlab)
		(e-label nillab)
		; here we are doing (arg nil) which returns the number of args
		; which is always true if anyone is testing 
		(If g-loc then (d-move '"-8(fp)" g-loc)
			       (d-handlecc)
		   elseif (car g-cc) then (e-goto (car g-cc))) ;always true
		(e-label finlab))))


;--- cc-atom :: test for atomness				= cc-atom  =
;
(defun cc-atom nil
  (d-typecmplx (cadr v-form) 
	       '#.(concat '$ (plus 1_0 1_1 1_2 1_4 1_5 1_6 1_7 1_9 1_10))))


;--- cc-bcdp :: check for bcdpness			   	= cc-bcdp  =
;
(defun cc-bcdp nil
  (d-typesimp (cadr v-form) '$5))


;--- cc-bigp :: check for bignumness				= cc-bigp =
;
(defun cc-bigp nil
  (d-typesimp (cadr v-form) '$9))

;--- c-*catch :: compile a *catch expression 			= c-*catch =
;
; the form of *catch is (*catch 'tag 'val)
; we evaluate 'tag and set up a catch frame, and then eval 'val
;
(defun c-*catch nil
  (let ((g-loc 'reg)
	(g-cc nil)
	(g-ret nil)
	(finlab (d-genlab)))
       (d-exp (cadr v-form))		; calculate tag into r0
       (d-catcherrset finlab 'reg 'T (caddr v-form))
       (e-label finlab)))



;--- d-catcherrset :: common code to catch and errset 
;
(defun d-catcherrset (finlab tagloc  flagloc expr)
       (e-write2 'pushab finlab)
       (e-write2 'pushr '$0x2540)	; save registers
       (e-write2 'jsb '_svkludg)		; save rest of state
       (e-write2 'pushl Bnp-val)
       (e-write2 'pushl (e-cvt tagloc))	; push tag
       (e-write2 'pushl (e-cvt flagloc))	; non-nil flag
       (e-write2 'pushl '_errp)		; old error pointer
       (e-write3 'movl 'sp '_errp)	; set up new error pointer
       (Push g-locs '(catcherrset . 0))
       (d-exp expr)			; now do the expression
       (unpush g-locs)
       (e-write3 'movl '"(sp)" '_errp)	; unlink this error frame
       (e-write3 'addl2 '$80 'sp)
       (d-clearreg))			; cant predict contents after retune 	


;--- c-cond :: compile a "cond" expression			= c-cond =
;
; not that this version of cond is a 'c' rather than a 'cc' . 
; this was done to make coding this routine easier and because
; it is believed that it wont harm things much if at all
;
(defun c-cond nil
  (makecomment '(beginning cond))
  (do ((clau (cdr v-form) (cdr clau))
       (finlab (d-genlab))
       (nxtlab)
       (save-reguse)
       (seent))
      ((or (null clau) seent)
       ; end of cond
       ; if haven't seen a t must store a nil in r0
       (If (null seent)  then (d-move 'Nil 'reg))
       (e-label finlab))

      ; case 1 - expr
      (If (atom (car clau))
	  then (comp-err "bad cond clause " (car clau))
      ; case 2 - (expr)
       elseif (null (cdar clau))
	  then (let ((g-loc (If (or g-cc g-loc) then 'reg))
		     (g-cc (cons finlab nil))
		     (g-ret))
		    (d-exp (caar clau)))
      ; case 3 - (t expr1 expr2 ...)
       elseif (or (eq t (caar clau))
		  (equal ''t (caar clau)))
	  then (let ((g-loc (If (or g-cc g-loc) then 'reg))
		     g-cc)
		    (d-exps (cdar clau)))
	       (setq seent t)
      ; case 4 - (expr1 expr2 ...)
       else (let ((g-loc nil)
		  (g-cc (cons nil (setq nxtlab (d-genlab))))
		  (g-ret nil))
		 (d-exp (caar clau)))
	    (setq save-reguse (copy g-reguse))
	    (let ((g-loc (If (or g-cc g-loc) then 'reg))
		  g-cc)
		 (d-exps (cdar clau)))
	    (If (or (cdr clau) (null seent)) then (e-goto finlab))
	    (e-label nxtlab)
	    (setq g-reguse save-reguse)))
  
  (d-clearreg))
	      


;--- c-cons :: do a cons instruction quickly			= c-cons =
;
(defun c-cons nil
  (d-pushargs (cdr v-form))		; there better be 2 args
  (e-write2 'jsb '_qcons)
  (setq g-locs (cddr g-locs))
  (setq g-loccnt (- g-loccnt 2))
  (d-clearreg))


;--- c-cxr :: compile a cxr instruction				= c-cxr =
; 
; this code would also be useful for accessing any vector of lispvals.
;
(defun c-cxr nil
  (prog (arg1 arg2 arg1loc arg2loc)
	(setq arg1loc (d-simple (setq arg1 (list 'cdr (cadr v-form))))
	      arg2loc (d-simple (setq arg2 (caddr v-form))))

	(If (not (and (dtpr arg1loc) (eq 'immed (car arg1loc))))
	    then
	       (If arg2loc 
		   then (If (null arg1loc) 
			    then (let ((g-loc 'r1)
				       (g-cc))
				      (d-exp arg1))
			    else (d-move arg1loc 'r1))
			(d-move arg2loc 'r0)
		   else (d-pushargs (ncons arg1))
			(let ((g-loc 'r0)
			      (g-cc))
			     (d-exp arg2))
			(d-move 'unstack 'r1)
			(decr g-loccnt)
			(Pop g-locs))
	       (d-inreg 'r1 nil)		; register clobbered
	       (If g-loc then (e-move `(0 r0 r1) (e-cvt g-loc))
			      (d-handlecc)
		elseif g-cc then (e-tst `(0 r0 r1))
				 (d-handlecc))
	    else (let ((g-loc 'r0)
		       (g-cc))
		      (d-exp arg2))
		 (setq arg1loc (list (* 4 (cadr arg1loc)) 'r0))
		 (If g-loc then (e-move arg1loc (e-cvt g-loc))
				(d-handlecc)
		  elseif g-cc then (e-tst arg1loc)
				   (d-handlecc)))))
			
			    
;--- cc-cxxr :: compile a "c*r" instr where * 			= c-cxxr =
;		is any sequence of a's and d's
;	- arg : argument of the cxxr function
;	- pat : a list of a's and d's in the reverse order of that
;			which appeared between the c and r
;
(defun cc-cxxr (arg pat)
  (prog (resloc loc qloc sofar togo keeptrack)
	; check for the special case of nil, since car's and cdr's
	; are nil anyway
	(If (null arg) then (If g-loc then (d-move 'Nil g-loc)
					   (d-handlecc)
			     elseif (cdr g-cc) then (e-goto (cdr g-cc)))
			    (return))
				      
	(If (and (symbolp arg) (setq qloc (d-bestreg arg pat)))
	    then (setq resloc (car qloc)
		       loc   resloc
		       sofar  (cadr qloc)
		       togo   (caddr qloc))
        else (setq resloc (If (d-simple arg) thenret
				else (let ((g-loc 'reg)
					   (g-cc nil)
					   (g-ret nil))
					  (d-exp arg))
				'r0))
	       (setq sofar nil
		     togo  pat))

	(If (and arg (symbolp arg)) then (setq keeptrack t))

       ; if resloc is a global variable, we must move it into a register
       ; right away to be able to do car's and cdr's
       (If (and (dtpr resloc) (or (eq (car resloc) 'bind)
				  (eq (car resloc) 'vstack)))
	   then (d-move resloc 'reg)
		(setq resloc 'r0))

       ; now do car's and cdr's .  Values are placed in r0. We stop when
       ; we can get the result in one machine instruction.  At that point
       ; we see whether we want the value or just want to set the cc's.
       ; If the intermediate value is in a register, 
       ; we can do : car cdr cddr cdar
       ; If the intermediate value is on the local vrbl stack or lbind
       ; we can do : cdr
       (do ((curp togo newp)
	    (newp))
	   ((null curp) (If g-loc then (d-movespec loc g-loc)
			    elseif g-cc then (e-tst loc))
	                (d-handlecc))
	   (If (symbolp resloc)
	       then (If (eq 'd (car curp))
			then (If (or (null (cdr curp))
				     (eq 'a (cadr curp)))
				 then (setq newp (cdr curp)   ; cdr
					    loc `(0 ,resloc)
					    sofar (append sofar (list 'd)))
				 else (setq newp (cddr curp)  ; cddr
					    loc `(* 0 ,resloc)
					    sofar (append sofar (list 'd 'd))))
			else (If (or (null (cdr curp))
				     (eq 'a (cadr curp)))
				 then (setq newp (cdr curp)   ; car
					    loc `(4 ,resloc)
					    sofar (append sofar (list 'a)))
				 else (setq newp (cddr curp)  ; cdar
					    loc `(* 4 ,resloc)
					    sofar (append sofar (list 'a 'd)))))
	       elseif (and (eq 'd (car curp))
			   (not (eq '* (car (setq loc (e-cvt resloc))))))
		 then (setq newp (cdr curp)	; (cdr <local>)
			    loc (cons '* loc)
			    sofar (append sofar (list 'd)))
	       else  (setq loc (e-cvt resloc)
			   newp curp))
	   (If newp			; if this is not the last move
	       then (setq resloc (d-allocreg (If keeptrack then nil else 'r0)))
		    (d-movespec loc resloc)
		    (If keeptrack then (d-inreg resloc (cons arg sofar)))))))

;--- c-declare :: handle the "declare" form
; if a declare is seen inside a function definition, we just 
; ignore it.  We probably should see what it is declareing, as it
; might be declaring a special.
;
(defun c-declare nil)

;--- c-do :: compile a "do" expression				= c-do =
;
; a do has this form:
;  (do vrbls tst . body)
; we note the special case of tst being nil, in which case the loop
; is evaluated only once, and thus acts like a let with labels allowed.
; The do statement is a cross between a prog and a lambda. It is like
; a prog in that labels are allowed. It is like a lambda in that
; we stack the values of all init forms then bind to the variables, just
; like a lambda expression (that is the initial values of even specials
; are stored on the stack, and then copied into the value cell of the
; atom during the binding phase. From then on the stack location is
; not used).
;
(defun c-do nil
  (prog (b-vrbls b-tst b-body chklab bodylab x-repeat x-vrbs x-fst
	g-loc g-cc oldreguse)
	(forcecomment '(beginning do))
	(setq g-loc 'reg  chklab (d-genlab)   bodylab (d-genlab))

	(If (and (cadr v-form) (atom (cadr v-form)))
	    then (setq v-form (d-olddo-to-newdo (cdr v-form))))

	(Push g-locs (cons 'do 0 ))		; begin our frame

	(setq b-vrbls (cadr v-form)
	      b-tst   (caddr v-form)
	      b-body  (cdddr v-form))

	; push value of init forms on stack
	(d-pushargs (mapcar '(lambda (x)
				     (If (atom x) then nil ; no init form => nil
					 else (cadr x)))
			    b-vrbls))

	; now bind to  the variables in the vrbls form
	(d-bindlamb (mapcar '(lambda (x)
				     (If (atom x) then x
					 else (car x)))
			    b-vrbls))

	; search through body for all labels and assign them gensymed labels
	(Push g-labs (cons (d-genlab)
			   (do ((ll b-body (cdr ll))
				(res))
			       ((null ll) res)
			       (If (and (car ll) (symbolp (car ll)))
				   then (Push res  (cons (car ll) (d-genlab)))))))

	; if the test is non nil, we do the test
	; another strange thing, a test form of (pred) will not return
	; the value of pred if it is not nil! it will return nil (in this
	; way, it is not like a cond clause)
	(d-clearreg)
	(If b-tst then (e-label chklab)
		       (let ((g-cc (cons nil bodylab)) g-loc g-ret)
			    (d-exp (car b-tst)))	; eval test
			   				; if false, do body
	               (If (cdr b-tst) 
			   then (setq oldreguse (copy g-reguse))
				(d-exps (cdr b-tst))
				(setq g-reguse oldreguse)
			   else  (d-move 'Nil 'reg))
		       (e-goto (caar g-labs))		; leave do
		       (e-label bodylab))		; begin body

	; process body
	(do ((ll b-body (cdr ll))
	     (g-cc) (g-loc)(g-ret))
	    ((null ll))
	    (If (or (null (car ll)) (not (symbolp (car ll))))
		then (d-exp (car ll))
		else (e-label (cdr (assoc (car ll) (cdar g-labs))))
		     (d-clearreg)))

	(If b-tst then ; determine all repeat forms which must be 
		       ; evaluated, and all the variables affected.
		       ; store the results in x-repeat and  x-vrbs
		       ; if there is just one repeat form, we calculate
		       ; its value directly into where it is stored,
		       ; if there is more than one, we stack them
		       ; and then store them back at once.
		       (do ((ll b-vrbls (cdr ll)))
			   ((null ll))
			   (If (and (dtpr (car ll)) (cddar ll))
			       then (Push x-repeat (caddar ll))
				    (Push x-vrbs   (caar ll))))
		       (If x-vrbs 
			    then (If (null (cdr x-vrbs))  ; if just one repeat..
				     then (let ((g-loc (d-locv (car x-vrbs)))
						(g-cc nil))
					       (d-exp (car x-repeat)))
				     else (setq x-fst (car x-repeat))
					  (d-pushargs (nreverse (cdr x-repeat)))
					  (let ((g-loc (d-locv (car x-vrbs)))
						(g-cc)
						(g-ret))
					       (d-exp x-fst))
					  (do ((ll (cdr x-vrbs) (cdr ll)))
					      ((null ll))
					      (d-move 'unstack (d-locv (car ll)))
					      (setq g-locs (cdr g-locs))
					      (decr g-loccnt))))
		      (e-goto chklab))

	(e-label (caar g-labs))			; end of do label
	(d-clearreg)
	(d-unbind)
	(setq g-labs (cdr g-labs))))


;--- d-olddo-to-newdo  :: map old do to new do
;
; form of old do is  (do var tst . body)
; where var is a symbol, not nil
;
(defun d-olddo-to-newdo (v-l)
  `(do ((,(car v-l) ,(cadr v-l) ,(caddr v-l)))
       (,(cadddr v-l))
       ,@(cddddr v-l)))



;--- cc-dtpr :: check for dtprness				= cc-dtpr =
;
(defun cc-dtpr nil
  (d-typesimp (cadr v-form) '$3))


;--- cc-eq :: compile an "eq" expression			= cc-eq =
;
(defun cc-eq nil
  (let ((arg1 (cadr v-form))
	(arg2 (caddr v-form))
	arg1loc
	arg2loc)
       (If (setq arg2loc (d-simple arg2))
	   then (If (setq arg1loc (d-simple arg1))
		    then ; eq <simple> <simple>
			 (d-cmp arg1loc arg2loc)
		    else ; eq <nonsimple> <simple>
			 (let ((g-loc 'reg)	; put <nonsimple> in r0
			       g-cc
			       g-ret)
			      (d-exp arg1))
			 (d-cmp 'reg arg2loc))
	   else ; since second is nonsimple, must stack first
		; arg out of harms way
		(let ((g-loc 'stack)
		      g-cc
		      g-ret)
		     (d-exp arg1)
		     (Push g-locs nil)
		     (incr g-loccnt)
		     (setq g-loc 'reg)		; second arg to r0
		     (d-exp arg2))
		(d-cmp 'unstack 'reg)
		(setq g-locs (cdr g-locs))
		(decr g-loccnt)))

  (d-invert))

(defun cc-equal nil
  (let ((lab1 (d-genlab))
	(lab11 (d-genlab))
	lab2)
       (d-pushargs (cdr v-form))
       (e-write3 'cmpl "-8(r6)" "-4(r6)")
       (e-gotonil lab1)
       (d-calltran 'equal '2)		 ; not eq, try equal.
       (d-clearreg)
       (e-write2 'tstl 'r0)
       (e-gotot lab11)		
       (If g-loc then (d-move 'Nil g-loc))
       (If (cdr g-cc) then (e-goto (cdr g-cc))
	   else (e-goto (setq lab2 (d-genlab))))
       (e-writel lab1)
       (e-dropnp 2)
       (e-writel lab11)
       (If g-loc then (d-move 'T g-loc))
       (If (car g-cc) then (e-goto (car g-cc)))
       (If lab2 then (e-writel lab2))
       (setq g-locs (cddr g-locs))
       (setq g-loccnt (- g-loccnt 2))))




;--- c-errset :: compile an errset expression			= c-errset =
;
; the errset has this form: (errset 'value ['tag])
; where tag defaults to t.
;
(defun c-errset nil
  (let ((g-loc 'reg)
	(g-cc nil)
	(g-ret nil)
	(finlab (d-genlab)))
       (d-exp (If (cddr v-form) then (caddr v-form) else t))
       (d-catcherrset finlab (d-loclit '(ER%all) nil) 'reg (cadr v-form))
       (d-move 'reg 'stack)
       (d-calltran 'ncons 1)
       (e-label finlab)
       (d-clearreg)))


;--- cc-fixp :: check for a fixnum or bignum			= cc-fixp =
;
(defun cc-fixp nil
  (d-typecmplx (cadr v-form) 
	       '#.(concat '$ (plus 1_2 1_9))))


;--- cc-floatp :: check for a flonum				= cc-floatp =
;
(defun cc-floatp nil
  (d-typesimp (cadr v-form) '$4))


;--- c-get :: do a get from the prop list
;
(defun c-get nil
  (If (not (eq 2 (length (cdr v-form))))
      then (comp-err "Wrong number of args to get " v-form))
  (d-pushargs (cdr v-form))		; there better be 2 args
  (e-write2 'jsb '_qget)
  (d-clearreg)
  (setq g-locs (cddr g-locs))
  (setq g-loccnt (- g-loccnt 2)))

;--- c-go :: compile a "go" expression				= c-go =
;
; we only compile the (go symbol)type expression, we do not
; allow symbol to be anything by a non null symbol.
;
(defun c-go nil
  ; find number of frames we have to go down to get to the label
  (do ((labs g-labs (cdr labs))
       (locs g-locs)
       (locals 0)
       (specials 0)
       (catcherrset 0)
       (label))
      ((null labs) (comp-err "go label not found for expression: " (or v-form)))
      ; if there are any enclosing *catches or errsets, they will be
      ; first in g-locs
      (do nil 
	  ((not (and (dtpr (car locs)) (eq (caar locs) 'catcherrset))))
	  (incr catcherrset)
	  (unpush locs))

      (If (car labs) 
	  then (If (setq label (do ((lbs (cdar labs) (cdr lbs)))
				   ((null lbs))
				   (If (eq (caar lbs) (cadr v-form))
				       then (return (cdar lbs)))))
		   then (If (not (eq labs g-labs))
			    then (comp-warn "non local go used : " (or v-form)))
			(If (greaterp catcherrset 0)
			    then (comp-warn "Go through a catch or errset " v-form)
				 (do ((i 0 (1+ i)))
				     ((equal catcherrset i))
				     (e-write3 'movl "(sp)" '_errp)
				     (e-write3 'addl2 '$80 'sp)))
			(e-pop locals)
			(If (greaterp specials 0)
			    then (e-unshallowbind specials))
			(e-goto label)
			(return)))
      ; tally all locals and specials used in this frame
      (do ()
	  ((dtpr (car locs)) (setq specials (+ specials (cdar locs))
				   locs (cdr locs)))
	  (setq locs (cdr locs))
	  (incr locals))))
			

;--- cc-ingnore :: just ignore this code
;
(defun cc-ignore nil
  nil)

;--- c-lambexp :: compile a lambda expression			= c-lambexp =
;
(defun c-lambexp nil
  (let ((g-loc (If (or g-loc g-cc) then 'reg))
	(g-cc nil))
       (Push g-locs (cons 'lambda  0))	; add null lambda header
       (d-pushargs (cdr v-form))		; then push vals
       (d-lambbody (car v-form))
       (d-clearreg)))

;--- d-lambbody :: do a lambda body
;	- body : body of lambda expression, eg (lambda () dld)
;
(defun d-lambbody (body)
  (d-bindlamb (cadr body))		; bind locals
  (setq g-labs (cons nil g-labs))	; no labels allowed
  (d-clearreg)
  (d-exp (do ((ll (cddr body) (cdr ll))
	      (g-loc)
	      (g-cc)
	      (g-ret))
	     ((null (cdr ll)) (car ll))
	     (d-exp (car ll))))

  (setq g-labs (cdr g-labs))
  (d-unbind))				; unbind this frame


;--- d-bindlamb :: bind  variables in lambda list
;	- vrbs : list of lambda variables, may include nil meaning ignore
;
(defun d-bindlamb (vrbs)
  (let ((res (d-bindlrec (reverse vrbs) g-locs 0 g-loccnt)))
       (If res then (e-setupbind)
		    (mapc '(lambda (vrb) (e-shallowbind (car vrb) (cdr vrb)))
			  res)
		    (e-unsetupbind))))
  
;--- d-bindlrec :: recusive routine to bind lambda variables
;	- vrb : list of variables yet to bind
;	- locs : current location in g-loc
;	- specs : number of specials seen so far
;	- lev  : how far up from the bottom of stack we are.
; returns: list of elements, one for each special, of this form:
;		(<specialvrbname> stack <n>)
;	where specialvrbname is the name of the special variable, and n is
;	the distance from the top of the stack where its initial value is 
;	located
; also: puts the names of the local variables in the g-locs list, as well
;	as placing the number of special variables in the lambda header.
;
(defun d-bindlrec (vrb locs specs lev)
  (If vrb 
      then (let ((spcflg (d-specialp (car vrb)))
		 retv)
		(If spcflg then (setq specs (1+ specs)))
		
		(If (cdr vrb)		; if more vrbls to go ...
		    then (setq retv (d-bindlrec (cdr vrb) 
						(cdr locs)
						specs
						(1- lev)))
		    else (rplacd (cadr locs) specs))	; else fix up lambda hdr
		
		(If (not spcflg) then (rplaca locs (car vrb))
		    else (Push retv `(,(car vrb) stack ,lev)))
		
		retv)))
;--- c-list :: compile a list expression			= c-list =
;
; this is compiled as a bunch of conses with a nil pushed on the
; top for good measure
;
(defun c-list nil
  (prog (nargs)
	(setq nargs (length (cdr v-form)))
	(makecomment '(list expression))
	(If (zerop nargs) then (d-move 'Nil 'reg)	; (list) ==> nil
			       (return))
	(d-pushargs (cdr v-form))
	(e-write2 'clrl '(+ #.Np-reg))	; stack one nil

       ; now do the consing
       (do ((i (max 1 nargs) (1- i)))
	   ((zerop i))
	   (e-write2 'jsb '_qcons)
	   (d-clearreg)
	   (If (> i 1) then (d-move 'reg 'stack)))

       (setq g-locs (nthcdr nargs g-locs)
	     g-loccnt (- g-loccnt nargs))))



;--- d-mapconvert - access : function to access parts of lists
;		  - join	 : function to join results
;		  - resu	 : function to apply to result
;		- form	 : mapping form
;	This function converts maps to an equivalent do form.
;
(defun d-mapconvert (access join resu form )
	  (prog (vrbls finvar acc accform compform tmp)

		(setq finvar (gensym 'X)   ; holds result

		      vrbls (reverse
			     (maplist '(lambda (arg)
					((lambda (temp)
					    (cond ((or resu (cdr arg))
						   `(,temp ,(car arg)
							   (cdr ,temp)))
						  (t `(,temp 
						       (setq ,finvar ,(car arg))
						       (cdr ,temp)))))
					 (gensym 'X)))
				    (reverse (cdr form))))

		      acc (mapcar '(lambda (tem)
					   (cond (access `(,access ,(car tem)))
						 (t (car tem))))
				  vrbls)

		      accform (cond ((or (atom (setq tmp (car form)))
					 (null (setq tmp (d-macroexpand tmp)))
					 (not (member (car tmp) '(quote function))))
				     `(funcall ,tmp ,@acc))
				    (t `(,(cadr tmp) ,@acc))))
		(return
		 `((lambda (,finvar)
		    (do ( ,@vrbls)
			((null ,(caar vrbls)))
			,(cond ((eq join 'nconc)
				`(setq ,finvar (nconc ,finvar ,accform)))
			       (join `(setq ,finvar (,join ,accform ,finvar)))
			       (t accform)))
		    ,(cond ((eq resu 'identity) finvar)
			   (resu `(,resu ,finvar))
			   (t finvar)))
		   nil ))))
; apply to successive elements, return second arg
(defun cm-mapc nil
	  (d-mapconvert 'car nil nil (cdr v-form)))

; apply to successive elements, return list of results
(defun cm-mapcar nil
	  (d-mapconvert 'car 'cons 'nreverse (cdr v-form)))

; apply to successive elements, returned nconc of results
(defun cm-mapcan nil
	  (d-mapconvert 'car 'nconc 'identity (cdr v-form)))


; apply to successive sublists, return second arg
(defun cm-map nil
	  (d-mapconvert nil nil nil (cdr v-form)))


; apply to successive sublists, return list of results
(defun cm-maplist nil
	  (d-mapconvert nil 'cons 'reverse (cdr v-form)))

; apply to successive sublists, return nconc of results
(defun cm-mapcon nil
	  (d-mapconvert nil 'nconc 'identity (cdr v-form)))


;--- cc-memq :: compile a memq expression			= cc-memq =
;
(defun cc-memq nil
  (let ((loc1 (d-simple (cadr v-form)))
	(loc2 (d-simple (caddr v-form)))
	looploc finlab)
       (If loc2 then (d-clearreg 'r1)
		     (If loc1 then (d-move loc1 'r1)
			      else (let ((g-loc 'r1)
					 g-cc
					 g-ret)
					(d-exp (cadr v-form))))
		     (d-move loc2 'reg)
		else (let ((g-loc 'stack)
			   g-cc
			   g-ret)
			  (d-exp (cadr v-form)))
		     (Push g-locs nil)
		     (incr g-loccnt)
		     (let ((g-loc 'reg)
			   g-cc
			   g-ret)
			  (d-exp (caddr v-form)))
		     (d-move 'unstack 'r1)
		     (d-clearreg 'r1)
		     (unpush g-locs)
		     (decr g-loccnt))
       ; now set up the jump addresses
       (If (null g-loc)
	   then (setq loc1 (If (car g-cc) thenret
			       else (d-genlab))
		      loc2 (If (cdr g-cc) thenret
			       else (d-genlab)))
	   else (setq loc1 (d-genlab)
		      loc2 (d-genlab)))

       (setq looploc (d-genlab))

       (e-write2 'tstl 'r0)
       (e-write2 'jeql loc2)
       (e-label looploc)
       (e-write3 'cmpl 'r1 "4(r0)")
       (e-write2 'jeql loc1)
       (e-write3 'movl "(r0)" 'r0)
       (e-write2 'jneq looploc)
       (If g-loc then (e-label loc2)		; nil result
		      (d-move 'reg g-loc)
		      (If (cdr g-cc) then (e-goto (cdr g-cc))
				     else (e-goto (setq finlab (d-genlab))))
		 else (If (cdr g-cc) then (e-goto (cdr g-cc))
				     else (e-label loc2)))
       (If g-loc then (e-label loc1)		; non nil result
		      (d-move 'reg g-loc)
		      (If (car g-cc) then (e-goto (car g-cc)))
		 else (If (null (car g-cc)) then (e-label loc1)))
       (If finlab then (e-label finlab))))

