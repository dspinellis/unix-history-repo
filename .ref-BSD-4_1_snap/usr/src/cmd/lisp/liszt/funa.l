(include "chead.l")
(Liszt-file funa  "@(#)funa.l	1.3	2/10/82")

;;; ----	f u n a				function compilation
;;;
;;;				-[Wed Feb 10 21:13:29 1982 by jkf]-


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
	   then (If (and (fixp (cadr v-form)) (greaterp (cadr v-form) 0))
		    then ; simple case (arg n) for positive n
			(d-move `(immed ,(cadr v-form)) 'reg)
			(d-clearreg 'r0)
			(If g-loc then (d-move '"*-4(fp)[r0]" g-loc)
			    else (e-tst '"*-4(fp)[r0]"))
			(d-handlecc)
		 elseif (or (null (cadr v-form)) (zerop (cadr v-form)))
		    then ; get number of arguments (arg nil) or (arg) or (arg 0)
			(If g-loc then (d-move "-8(fp)" g-loc))
			; (arg) will always return a non nil value, we
			; don't even have to test it.
			(If (car g-cc) then (e-goto (car g-cc)))
		 else	; general (arg <form>)
		      (let ((g-loc 'reg) 
			    (g-cc (cons nil nillab))
			    (g-ret))
			   (d-exp `(cdr ,(cadr v-form)))) ; calc the numeric arg
		    (If g-loc then (d-move '"*-4(fp)[r0]" g-loc)
			else (e-tst '"*-4(fp)[r0]"))
		    (d-handlecc)
		    (e-goto finlab)
		    (e-label nillab)
		    ; here we are doing (arg nil) which returns the number 
		    ; of args
		    ; which is always true if anyone is testing 
		    (If g-loc then (d-move '"-8(fp)" g-loc)
				   (d-handlecc)
		     elseif (car g-cc) then (e-goto (car g-cc))) ;always true
		    (e-label finlab)))))


;--- c-assembler-code
; the args to assembler-code are a list of assembler language 
; statements.  This statements are put directly in the code
; stream produced by the compiler.  Beware: The interpreter cannot
; interpret the assembler-code function.
;
(defun c-assembler-code nil
  (setq g-skipcode nil)		; turn off code skipping
  (makecomment '(assembler code start))
  (do ((xx (cdr v-form) (cdr xx)))
      ((null xx))
      (e-write1 (car xx)))
  (makecomment '(assembler code end)))



;--- cm-assq :: assoc with eq for testing			= cm-assq  =
;
; form: (assq val list)
;
(defun cm-assq nil
  `(do ((xx-val ,(cadr v-form))
	(xx-lis ,(caddr v-form) (cdr xx-lis)))
       ((null xx-lis))
       (cond ((eq xx-val (caar xx-lis)) (return (car xx-lis))))))


;--- cc-atom :: test for atomness				= cc-atom  =
;
(defun cc-atom nil
  (d-typecmplx (cadr v-form) 
	       '#.(concat '$ (plus 1_0 1_1 1_2 1_4 1_5 1_6 1_7 1_9 1_10))))


;--- c-bcdcall :: do a bcd call					= c-bcdcall =
;
; a bcdcall is the franz equivalent of the maclisp subrcall.
; it is called with
; (bcdcall 'b_obj 'arg1 ...)
;  where b_obj must be a binary object. no type checking is done.
;
(defun c-bcdcall nil
  (d-callbig 1 (cdr v-form) t))

;--- cc-bcdp :: check for bcdpness			   	= cc-bcdp  =
;
(defun cc-bcdp nil
  (d-typesimp (cadr v-form) '$5))


;--- cc-bigp :: check for bignumness				= cc-bigp =
;
(defun cc-bigp nil
  (d-typesimp (cadr v-form) '$9))

;--- c-boole :: compile
;
(defun c-boole nil
   (cond ((fixp (cadr v-form))
	  (setq v-form (d-boolexlate (d-booleexpand v-form)))))
   (cond ((eq 'boole (car v-form)) 	;; avoid recursive calls to d-exp
	  (d-callbig 'boole (cdr v-form) nil))
	 (t (let ((g-loc 'reg) (g-cc nil) (g-ret nil))  ; eval answer
	       (d-exp v-form)))))

;--- d-booleexpand :: make sure boole only has three args
;  we use the identity (boole k x y z) == (boole k (boole k x y) z)
; to make sure that there are exactly three args to a call to boole
;
(defun d-booleexpand (form)
   (If (and (dtpr form) (eq 'boole (car form)))
      then (If (< (length form) 4)
	      then (comp-err "Too few args to boole : " form)
	    elseif (= (length form) 4)
	      then form
	      else (d-booleexpand `(boole ,(cadr form)
					   (boole ,(cadr form)
						   ,(caddr form)
						   ,(cadddr form))
					   ,@(cddddr form))))
	   else form))

(declare (special x y))
(defun d-boolexlate (form)
   (If (atom form)
       then form
    elseif (and (eq 'boole (car form))
		(fixp (cadr form)))
      then (let ((key (cadr form))
		 (x (d-boolexlate (caddr form)))
		 (y (d-boolexlate (cadddr form)))
		 (res))
	      (makecomment `(boole key = ,key))
	      (If (eq key 0) 		;; 0
		 then `(progn ,x ,y 0)
	       elseif (eq key 1) 	;; x * y
		 then `(fixnum-BitAndNot ,x (fixnum-BitXor ,y -1))
	       elseif (eq key 2) 	;; !x * y
		 then `(fixnum-BitAndNot (fixnum-BitXor ,x -1)
					 (fixnum-BitXor ,y -1))
	       elseif (eq key 3) 	;; y
		 then `(progn ,x ,y)
	       elseif (eq key 4)	;; x * !y
		 then `(fixnum-BitAndNot ,x ,y)
	       elseif (eq key 5) 	;; x
		 then `(prog1 ,x ,y)
	       elseif (eq key 6)        ;; x xor y
		 then `(fixnum-BitXor ,x ,y)
	       elseif (eq key 7) 	;; x + y
		 then `(fixnum-BitOr ,x ,y)
	       elseif (eq key 8)	;; !(x xor y)
		 then `(fixnum-BitXor (fixnum-BitOr ,x ,y) -1)
	       elseif (eq key 9) 	;; !(x xor y)
		 then `(fixnum-BitXor (fixnum-BitXor ,x ,y) -1)
	       elseif (eq key 10) 	;; !x
		 then `(prog1 (fixnum-BitXor ,x -1) ,y)
	       elseif (eq key 11) 	;; !x + y
		 then `(fixnum-BitOr (fixnum-BitXor ,x -1) ,y)
	       elseif (eq key 12) 	;; !y
		 then `(progn ,x (fixnum-BitXor ,y -1))
	       elseif (eq key 13) 	;; x + !y
		 then `(fixnum-BitOr ,x (fixnum-BitXor ,y -1))
	       elseif (eq key 14) 	;; !x + !y
		 then `(fixnum-BitOr (fixnum-BitXor ,x -1)
				     (fixnum-BitXor ,y -1))
	       elseif (eq key 15) 	;; -1
		 then `(progn ,x ,y -1)
		 else form))
      else form))

(declare (unspecial x y))



;--- c-*catch :: compile a *catch expression 			= c-*catch =
;
; the form of *catch is (*catch 'tag 'val)
; we evaluate 'tag and set up a catch frame, and then eval 'val
;
(defun c-*catch nil
  (let ((g-loc 'reg)
	(g-cc nil)
	(g-ret nil)
	(finlab (d-genlab))
	(beglab (d-genlab)))
       (d-exp (cadr v-form))		; calculate tag into r0
       (d-pushframe #.F_CATCH 'reg 'Nil) ; the Nil is a don't care
       (Push g-labs nil)		; disallow labels
       ; retval will be non 0 if we were thrown to, in which case the value
       ; thrown is in _lispretval.
       ; If we weren't thrown-to the value should be calculated in r0.
       (e-write2 'tstl '_retval)
       (e-write2 'jeql beglab)
       (e-write3 'movl '_lispretval 'r0)
       (e-write2 'jbr finlab)
       (e-label beglab)
       (d-exp (caddr v-form))
       (e-label finlab)
       (d-popframe)	; remove catch frame from stack
       (unpush g-locs)	; remove (catcherrset . 0)
       (unpush g-labs)  ; allow labels again
       (d-clearreg)))



;--- d-pushframe :: put an evaluation frame on the stack
;
; This is equivalant in the C system to 'errp = Pushframe(class,arg1,arg2);'
; We stack a frame which describes the class (will always be F_CATCH)
; and the other option args.
; 2/10/82 - it is a bad idea to stack a variable number of arguments, since
; this makes it more complicated to unstack frames.  Thus we will always
; stack the maximum --jkf
(defun d-pushframe (class arg1 arg2)
  (e-write2 'pushl (e-cvt arg2))
  (e-write2 'pushl (e-cvt arg1))
  (e-write2 'pushl (concat '$ class))
  (e-write2 'jsb '_qpushframe)
  (e-write3 'movl 'r0 '_errp)
  (Push g-locs '(catcherrset . 0)))

;--- d-popframe :: remove an evaluation frame from the stack
;
; This is equivalent in the C system to 'errp = Popframe();'
;  n is the number of arguments given to the pushframe which
; created this frame.  We have to totally remove this frame from
; the stack only if we are in a local function, but for now, we just
; do it all the time.
;
(defun d-popframe ()
  (e-write3 'movl '_errp 'r1)
  (e-write3 'movl `(,OF_olderrp r1) '_errp)
  ; there are always 3 arguments pushed, and the frame contains 5
  ; longwords.  We should make these parameters into manifest
  ; constants --jkf
  (e-write4 'addl3 `($ ,(+ (* 3 4) (* 5 4))) 'r1 'sp))



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
		     (g-ret (and g-ret (null (cdr clau)))))
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
;
			

(defun cc-cxr nil
  (d-supercxr t nil))


;--- d-supercxr :: do a general struture reference
;  	type - one of fixnum-block,flonum-block,<other-symbol>
; the type is that of an array, so <other-symbol> could be t, nil
; or anything else, since anything except *-block is treated the same
;
; the form of a cxr is (cxr index hunk) but supercxr will handle
; arrays too, so hunk could be (getdata (getd 'arrayname))
;
; offsetonly is t if we only care about the offset of this element from
; the beginning of the data structure.  If offsetonly is t then type
; will be nil.
;
; Note: this takes care of g-loc and g-cc 

(defun d-supercxr (type offsetonly)
  (let ((arg1 (cadr v-form))
	(arg2 (caddr v-form))
	lop rop semisimple)

       (If (fixp arg1) then (setq lop `(immed ,arg1))
	   else (d-fixnumexp arg1)	; calculate index into r5
		(setq lop 'r5))		; and remember that it is there

       ; before we calculate the second expression, we may have to save
       ; the value just calculated into r5.  To be safe we stack away
       ; r5 if the expression is not simple or semisimple.
       (If (not (setq rop (d-simple arg2)))	
	   then (If (and (eq lop 'r5) 
			 (not (setq semisimple (d-semisimple arg2))))
		    then (d-move lop Cstack))
	        (let ((g-loc 'reg) g-cc)
		     (d-exp arg2))
	        (setq rop 'r0)

		(If (and (eq lop 'r5) (not semisimple))
		    then (d-move unCstack lop)))

       (If (eq type 'flonum-block)
	  then (setq lop (d-structgen lop rop 8))
	       (e-write3 'movq lop 'r4)
	       (e-write2 'jsb '"_qnewdoub")	; box number
	       (d-clearreg)			; clobbers all regs
	       (If (and g-loc (not (eq g-loc 'reg)))
		  then (d-move 'reg g-loc))
	       (If (car g-cc) then (e-goto (car g-cc)))
	  else (setq lop (d-structgen lop rop 4)
		     rop (If g-loc then
			     (If (eq type 'fixnum-block) then 'r5 
				else (e-cvt g-loc))))
	       (If rop 
		  then (If offsetonly
			  then (e-write3 'moval lop rop)
			  else (e-write3 'movl lop rop))
		       (If (eq type 'fixnum-block) 
			  then (e-write2 'jsb '"_qnewint")
			       (d-clearreg)
			       (If (not (eq g-loc 'reg))
				  then (d-move 'reg g-loc))
			       ; result is always non nil.
			       (If (car g-cc) then (e-goto (car g-cc)))
			  else (d-handlecc))
		elseif g-cc 
		  then (If (eq type 'fixnum-block)
			  then (If (car g-cc) 
				  then (e-goto (car g-cc)))
			  else (e-write2 'tstl lop)
				(d-handlecc))))))


;--- d-semisimple :: check if result is simple enough not to clobber r5
; currently we look for the case of (getdata (getd 'foo))
; since we know that this will only be references to r0.
; More knowledge can be added to this routine.
;
(defun d-semisimple (form)
  (or (d-simple form)
      (and (dtpr form) 
	   (eq 'getdata (car form))
	   (dtpr (cadr form))
	   (eq 'getd (caadr form))
	   (dtpr (cadadr form))
	   (eq 'quote (caadadr form)))))

;--- d-structgen :: generate appropriate address for indexed access
;	index - index address, must be (immed n) or r5 (which contains int)
;	base  - address of base
;	width - width of data element
; want to calculate appropriate address for base[index]
; may require emitting instructions to set up registers
; returns the address of the base[index] suitable for setting or reading
;
; the code sees the base as a stack value as a special case since it
; can generate (perhaps) better code for that case.

(defun d-structgen (index base width)
  (If (and (dtpr base) (eq (car base) 'stack))
      then (If (dtpr index)	; i.e if index = (immed n)
	       then (d-move index 'r5))	; get immed in register
	   ;  the result is always *n(r6)[r5]
	   (append (e-cvt `(vstack ,(cadr base))) '(r5))
      else (If (not (atom base))	; i.e if base is not register
	       then (d-move base 'r0)	; (if nil gets here we will fail)
		    (d-clearreg 'r0)
		    (setq base 'r0))
	   (If (dtpr index) then `(,(* width (cadr index)) ;immed index
				    ,base)
			    else `(0 ,base r5))))

;--- c-rplacx :: complile a rplacx expression			= c-rplacx =
;
;  This simple calls the general structure hacking function, d-superrplacx
;  The argument, hunk, means that the elements stored in the hunk are not
;  fixum-block or flonum-block arrays.
(defun c-rplacx nil
  (d-superrplacx 'hunk))

;--- d-superrplacx :: handle general setting of things in structures
;	type - one of fixnum-block, flonum-block, hunk
; see d-supercxr for comments
; form of rplacx is (rplacx index hunk valuetostore)
(defun d-superrplacx (type)
	 (let ((arg1 (cadr v-form))
	       (arg2 (caddr v-form))
	       (arg3 (cadddr v-form))
	       lop rop semisimple)
	      
	      ; calulate index and put it in r5 if it is not an immediate
	      ; set lop to the location of the index
	      (If (fixp arg1) then (setq lop `(immed ,arg1))
		  else (d-fixnumexp arg1)
		       (setq lop 'r5))	
	      
	      ; set rop to the location of the hunk.  If we have to 
	      ; calculate the hunk, we may have to save r5.
	      ; If we are doing a rplacx (type equals hunk) then we must
	      ; return the hunk in r0.
	      (If (or (eq type 'hunk) (not (setq rop (d-simple arg2))))
		  then (If (and (eq lop 'r5) 
				(not (setq semisimple (d-semisimple arg2))))
			   then (d-move lop Cstack))
		       (let ((g-loc 'r0) g-cc)
			    (d-exp arg2))
		       (setq rop 'r0)
		  
		       (If (and (eq lop 'r5) (not semisimple))
			   then (d-move unCstack lop)))

	      ; now that the index and data block locations are known, we 
	      ; caclulate the location of the index'th element of hunk
	      (setq rop (d-structgen lop rop (If (eq type 'flonum-block)
						 then 8
						 else 4)))

	      ; the code to calculate the value to store and the actual
	      ; storing depends on the type of data block we are storing in.
	      (If (eq type 'flonum-block) 
		  then (If (setq lop (d-simple `(cdr ,arg3)))
			   then (e-write3 'movq (e-cvt lop) rop)
			   else ; preserve rop since it may be destroyed
				; when arg3 is calculated
				(e-write3 'movaq rop "-(sp)")
				(let ((g-loc 'r0) g-cc)
				     (d-exp arg3))
				(d-clearreg 'r0)
				(e-write3 'movq '(0 r0) "*(sp)+"))
	       elseif (and (eq type 'fixnum-block)
			   (setq arg3 `(cdr ,arg3))
			   nil)
		      ; fixnum-block is like hunk except we must grab the
		      ; fixnum value out of its box, hence the (cdr arg3)
		   thenret
	       else (If (setq lop (d-simple arg3))
			then (e-write3 'movl (e-cvt lop) rop)
			else ; if we are dealing with hunks, we must save
			     ; r0 since that contains the value we want to
			     ; return.
			     (If (eq type 'hunk) then (d-move 'reg 'stack)
						      (Push g-locs nil)
						      (incr g-loccnt))
			     (e-write3 'moval rop "-(sp)")
			     (let ((g-loc "*(sp)+") g-cc)
				  (d-exp arg3))
			     (If (eq type 'hunk) then (d-move 'unstack 'reg)
						      (unpush g-locs)
						      (decr g-loccnt))
			     (d-clearreg 'r0)))))

			    
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
