;--- file: complrd.l
(include "compmacs.l")

(def e-bind 
  (lambda (v-v v-n) 
	  (setq k-bind (cons (cons v-v v-n) k-bind)))) 

(def e-reg 
  (lambda (v-r v-t) 
	  (prog (v-v) 
		(cond ((setq v-v (get v-r x-reg)) (return v-v))) 
		(setq v-v 
		      (cond (v-t) 
			    ((prog (v-e v-l) 
				   (setq v-e '(4 5 2 3 1 0))
			      next 
				   (setq v-l k-regs) 
			      loop 
				   (cond ((null v-l) (return (car v-e))) 
					 ((not (equal (cdar v-l) (car v-e))) 
					  (setq v-l (cdr v-l)) 
					  (go loop)) 
					 ((setq v-e (cdr v-e)) (go next))))) 
			    (t (cdar (nth k-regs -1))))) 
		(f-make v-r v-v) 
		(return v-v)))) 
;--- e-addr - v-v : s-exp 
;	      v-r : ?
;	      v-t : ?
;	return the address in assembler format of the s-exp in v-v.
;	If the s-exp is a list or number then it must be on the
;	alist, else we look for it on the local variable stack.
;
(def e-addr 
  (lambda (v-v v-r v-t) 
	  (cond ((not (atom v-v)) (cdr (e-alist (cadr v-v))))	; (quote arg)
		((numberp v-v) (cdr (e-alist v-v)))		;number	
		((prog (v-l) 
		       (cond ((setq v-l (assoc v-v k-bind)) 
			      (return 
			       (cond ((ifflag v-v x-spec) 
				      (e-alist v-v))
				     (t  `(,(times 4 (cdr v-l)) 
					    ,lpar
					    ,olbot-reg
					    ,rpar))))))))
		((symbolp v-v) (e-alist v-v)) 
		; how is this reachable ??
		(t (emit3 'movl
			  (list '$ v-v)
			  (cond (v-t (list 'r r-xv))
				((equal v-r r-xv) (list 'r r-xv+1))
				(t (emit3 'movl (list 'r v-r) 'r0)
				   (list 'r r-xv+1))))))))

;--- e-alist - v-v : s-exp to look for on the alist
;	returns an assembler address of the s-exp as an offset off the
;	link register ln-reg. If the given s-exp is not on the alist yet,
;	it is added to it, thus this routine never fails
;
(def e-alist
  (lambda (v-v)
	  (prog (v-x)
		(setq v-x 
		      (cond ((cadr (assoc v-v k-ptrs)))
			    (t (setq k-ptrs
				     (cons (list v-v (setq k-disp (add k-disp 4)))
					   k-ptrs))
			       k-disp)))
		(return (cond ((zerop v-x) `(* (,ln-reg)))
			      (t `(* ,v-x (,ln-reg))))))))


;--- e-have - v-e : name of value (how generated?)
;	returns the register which contains this value, else nil if
;	this value is not in  a register
;
(def e-have 
  (lambda (v-e) 
	  (cond ((setq v-e (assoc v-e k-regs)) (cdr v-e))))) 

;--- e-note - v-r : register name
;	      v-e : name of value
;	returns v-r
;	This makes us remember that register v-r contains value v-e
;	by placing it in the k-regs assoc list
;
(def e-note 
  (lambda (v-r v-e) 
	  (setq k-regs (cons (cons v-e v-r) k-regs)) 
	  v-r)) 

;--- e-lose - v-r : register name
;	returns v-r
;	This says that register v-r is clobbered and no longer contains
;	any known value.
;
(def e-lose 
  (lambda (v-r) 
	  (setq k-regs (e-drop k-regs v-r)) 
	  v-r)) 

;--- e-drop - v-r : register name (in general, anything)
;	      v-l : list of registers (in general, any assoc list)
;	returns v-l with all entries with v-r as cadr removed.
;	
(def e-drop 
  (lambda (v-l v-r) 
	  (cond ((null v-l) nil) 
		((equal (cdar v-l) v-r) (e-drop (cdr v-l) v-r)) 
		(t (rplacd v-l (e-drop (cdr v-l) v-r)))))) 


;--- e-type - v-r : register containing a lispval
;	emits instructions which replace that register with the type
;	 number of the lispval it contained.
;
(def e-type
  (lambda (v-r)
	  (setq v-r (list 'r v-r))
	  (emit4 'ashl '$-9 v-r v-r)
	  (emit3 'cvtbl (list '"_typetable+1[r" (cadr v-r) '"]") v-r)))

(putprop 'get 'e-get 'x-emit)

(def e-get 
  (lambda (v-r v-v) 
	  (prog (v-cou)
		(setq v-cou (get v-r 'x-count))
		
		(cond ((null v-cou) 
		       (comp-warn " value lost " (or v-v) " from reg " (or v-r)
				  " plist " (plist v-r) N))
		      ((and (eq 'used v-cou)     	; if only used once
			    (eq (cadar k-code) v-r)
			    (or (eq 'set (caar k-code))
				(eq 'push (caar k-code))))
		       (cond ((eq 'set (caar k-code))
			      (e-setnoreg v-v))
			     (t (e-pushnoreg v-v))))
		      (t (setq v-cou (e-have v-v))
			 
			 (cond ((equal v-cou (setq v-r (e-reg v-r v-cou)))
				(return t)) 
			       ((null v-v) (emit2 'clrl (list 'r v-r)))
			       ((setq v-cou (e-addr v-v v-r t))
				(emit3 'movl v-cou (list 'r v-r))))
			 (e-note (e-lose v-r) v-v))))))

;--- e-setnoreg - v-fromv : value want to set 
;	This is used to shorcut the setting of a value. We bypass teh
;	pseudo register.  the set instruction is in the car of k-code.
;
(def e-setnoreg
  (lambda (v-fromv)
	  (prog (v-tov v-toadr v-floc)
		(setq v-tov (caddar k-code)	; get loc to set to
		      v-toadr (e-addr v-tov nil nil) ;loc of it
		      v-floc (e-have v-fromv)	; reg location if exists
		      k-code (cdr k-code))

		(cond ((null v-fromv) (emit2 'clrl v-toadr))
		      (t (cond (v-floc (emit3 'movl `(r ,v-floc) v-toadr))
			       (t (emit3 'movl (e-addr v-fromv nil nil)
					       v-toadr)))))

	loop	; remove alloc occuraces of v-v from the registers
		(cond ((null (setq v-toadr (e-have v-tov)))
		       (return nil))
		      (t (e-lose v-toadr)))
		(go loop))))
(putprop 'set 'e-set 'x-emit)
;--- e-set - v-r : (actrnum) register number with value in it
;	   - v-v : (actvname) name whose value will be replaced
;	emits an instruction to replace the value of v-v with
;	the value in v-r.  Then we remove all mention of v-v
;	in the registers since we have changed the value.
;	Finally we note that the value is stored in v-r since
;	that is where it came from
;
(def e-set 
  (lambda (v-r v-v) 
	  (prog (v-t) 
		(setq v-t (e-addr v-v v-r nil)) 
		(cond (v-t (emit3 'movl (list 'r v-r) v-t)) 
		      (t (return))) 
	   loop 
		(cond ((setq v-t (e-have v-v)) 
		       (e-lose v-t) 
		       (go loop))) 
		(e-note v-r v-v)))) 

(putprop 'push 'e-push 'x-emit)


;--- e-push - v-r : register number
;	emits an instruction to push the value in the given register
;	 on the name stack
(def e-push 
  (lambda (v-r) 
	  (emit3  'movl 
		 (list 'r v-r) push-np)
	  (setq k-stak (add1 k-stak)))) 


;--- e-pushnoreg - v-fromv : value we wish to stack
;	we stack a value without going through a intermediate register.
;
(def e-pushnoreg
  (lambda (v-fromv)
	  (prog (v-floc)
		(setq v-floc (e-have v-fromv)	; see if from is in regis
		      k-code (cdr k-code))

		(cond ((null v-fromv) (emit2 'clrl push-np))
		      (v-floc (emit3 'movl `(r ,v-floc) push-np))
		      (t (emit3 'movl (e-addr v-fromv nil nil)
				      push-np)))
		(setq k-stak (add1 k-stak)))))


(putprop 'fpush 'e-fpush 'x-emit)

(def e-fpush 
  (lambda (v-r)
	  (emit3 'movl (list 8 '"(" v-r '")") push-np)))

(putprop 'gpush 'e-gpush 'x-emit)

(def e-gpush 
  (lambda (v-r v-v)
	  (prog (v-t)
		(setq v-t (e-have v-v))
		(cond ((null v-v) (emit2  i-clr push-np))
		      ((equal v-t (setq v-r (e-reg v-r v-t)))
		       (emit3 i-mov (list 'r v-r) push-np))
		      ((setq v-t (e-addr v-v v-r t))
		       (emit3 i-mov v-t push-np))
		      ((zerop v-r))
		      (t (emit3 i-mov 'r0 push-np)))
		(setq k-nargs (add1 k-nargs))
		(setq k-stak (add1 k-stak)))))

(putprop 'gfpush 'e-gfpush 'x-emit)

(def e-gfpush
  (lambda (v-r v-v)
	  (prog (v-t)
		(setq v-t (e-have v-v))
		(cond ((null v-v) (emit2 i-clr push-np))
		      ((equal v-t (setq v-r (e-reg v-r v-t)))
		       (emit3 i-mov (list 'r v-r) push-np))
		      ((setq v-t (cdr (e-addr v-v v-r t)))
		       ; mod by jkf, new calling seq, push atom addr
		       ; on stack, let qfuncl look 8 beyond
		       (emit3 i-mov v-t push-np)
		       ;(emit3 'movl v-t (list 'r v-r))
		       ;(emit3 i-mov (list 8 '"(r" v-r '")") push-np)
		       )
		      ((zerop v-r))
		      (t (emit3 i-mov '"8(r0)" push-np)))
		(setq k-nargs (add1 k-nargs))
		(setq k-stak (add1 k-stak)))))


(putprop 'mark 'e-mark 'x-emit)
;--- e-mark - 
;	emit instructions to begin to call a function. This involves
;	 setting lbot in Opus30, and saving the old lbot in Opus 20.
;	 Also, some global variables are set.
;	details: In opus 30, np points to the next free loc, we set
;	 lbot to one beyond that since where np points we will place
;	 the address of the function to call.  If we adopt a xfer
;	 table scheme for calling, this would be different since
;	 we wouldn't stack the address of the function.
;
(def e-mark 
  (lambda nil 
	  nil))			; no-op

(putprop 'call 'e-call 'x-emit)

;--- e-call - v-r : register where result will go, this will always be 0
;	    - v-a : nil if calling throught the oblist, non nil then
;		    this is the address of a system function to call
;	Calls a routine, eithere system or through the oblist.
;	In the former case, we have only stacked the args, in the
;	latter case, lbot points to the function code to call.
;	If we are calling a non system function with 4 or less args
;	we do not set up lbot, instead we enter qfuncl at a special
;	entry point which does the set up.
;
(def e-call	
  (lambda (v-r v-a v-nargs) 
	(prog (v-temp)
	  (setq k-stak (difference k-stak v-nargs)) 
	  (setq k-regs nil) 
	  (cond ((or v-a (null (setq v-temp (get 'qfs (sub1 v-nargs)))))
		 (emit3 'movab `(- ,(times 4 v-nargs) ,lpar ,np-reg ,rpar)
				lbot-reg)))	; set up lbot
	  (cond (v-a (emit3 'calls '$0 v-a))	; system fcn
		(v-temp (emit2 'jsb v-temp))
		(t (emit2 'jsb qfuncl))) ; else non sys fcn
	  (cond (v-a (emit3 'movl lbot-reg np-reg))))))	; fix up lbot if sys

(putprop 'minus 'e-minus 'x-emit)

(def e-minus 
  (lambda (v-r v-v) 
	  (cond ((eq (caar k-code) 'get) 
		 (prog (v-i v-b)
		       (setq v-i (cdar k-code))
		       (setq v-b (e-reg (car v-i) nil)) 
		       (setq k-code (cdr k-code)) 
		       (e-lose v-b)
		       (cond ((equal v-r v-b) 
			      (setq v-r (e-reg (Gensym nil) nil)) 
			      (cond ((equal v-r v-b) 
				     (setq v-r (remainder (add1 v-r) 6) ))) 
			      (emit3 'movl 
				     (list 'r v-b)
				     (list 'r (e-lose v-r)))
			      (e-note v-r (Gensym nil)))) 
		       (cond ((null (cadr v-i)) (emit2 'clrl (list 'r v-b)))
			     (t (emit3 'movl
				       (e-addr (cadr v-i) v-b t)
				       (list 'r v-b)))))))
	  (cond ((null v-v) (emit2 'tstl (list 'r v-r)))
		(t (emit3 'cmpl (e-addr v-v v-r t) (list 'r v-r))))))

(putprop 'true 'e-true 'x-emit)

(def e-true	
  (lambda (v-l v-dv) 
	  (emit2 'jneq v-l)))

(putprop 'false 'e-false 'x-emit)

(def e-false 
  (lambda (v-l v-dv) 
	  (emit2 'jeql v-l)))

(putprop 'go 'e-go 'x-emit)

(def e-go 
  (lambda (v-l) 
	  (emit2 'jbr v-l)))

(putprop 'skip 'e-skip 'x-emit)

(def e-skip 
  (lambda (v-r v-l) 
	  (prog (v-x)
		(e-lose v-r) 
		(setq v-x (Gensym nil))
		(emit3 'movab v-x (list 'r v-r))
		(emit2 'jbr v-l)
		(emit1 (list v-x ':)))))

(putprop 'return 'e-rtn 'x-emit) 

(putprop 'bind 'e-xbind 'x-emit)

;--- e-xbind  - v-v : act varname to bind
;	Emits instrutions to bind v-v to the current top of stack.
;	it is possible for v-v to be nil, this means we should ignore
;	this value on the stack (but we remember that it is still on
;	the stack).
;
(def e-xbind
  (lambda (v-vrbl)
	  (prog (v-loc)
		(cond ((null v-vrbl))		; ignore if nil
		      ((ifflag v-vrbl x-spec)
		       ; if first bound, get val of bnp in bnp-reg
		       (cond ((zerop k-regf) (emit3 'movl bnp-val bnp-reg)))


		       (setq k-regf (add1 k-regf)	; count specials bound
			     v-loc (e-alist v-vrbl))	; addr of vars value
		       (emit3 'movl v-loc '"(r11)+")	; stack value
		       (emit3 'movl (cdr v-loc) '"(r11)+") ; now addr
		       (emit3 'movl bnp-reg bnp-val) ; keep current
		       (emit3 'movl `(,(times 4 k-stak) ,lpar ,olbot-reg ,rpar)
				    v-loc))
		      (t (e-bind v-vrbl k-stak)))		; update k-bind
		(setq k-stak (add1 k-stak)))))



(putprop 'label 'e-label 'x-emit)

(def e-label 
  (lambda (v-l) 
	  (put v-l x-lab 1)
	  (emit1 (list v-l ':))
	  (setq k-regs nil))) 

(putprop 'entry 'e-entry 'x-emit)

(def e-entry
  (lambda (type) 
	  (setq k-bind nil) 
	  (setq k-stak 0) 
	  (emit2 '".word" '"0xdc0") ; save 11,10,8,7,6
	  (emit3 'movab '"linker" ln-reg)
          (cond ((eq type 'lexpr)
		 (emit4 'subl3 '$4 lbot-reg `"-(sp)")	; stack num of args
		 (emit3 'movl np-reg  olbot-reg)	; np is top
		 (emit4 'subl3 lbot-reg np-reg 'r0)	; stack numb of args
		 (emit3 'movab '"0x400(r0)" `(,lpar ,np-reg ,rpar +))
		 (emit3 'movl `(,lpar ,olbot-reg ,rpar) '"-(sp)"))
		(t
		 (emit3 'movl `( ,lbot-reg) `( ,olbot-reg))))
	  (setq k-name (Gensym nil))
	  (emit1 (list k-name ':))))

(putprop 'repeat 'e-repeat 'x-emit)

(def e-repeat 
  (lambda nil 
	  (emit2 'jbr k-name)))

(putprop 'begin 'e-begin 'x-emit)

(def e-begin 
  (lambda (v-nargs) 
	  (setq k-stak (difference k-stak v-nargs))  ; make up for stacked args
	  (e-save) 
	  (setq k-prog (Gensym nil))
	  (setq k-regf 0)))			; counts specials bound

(putprop 'end 'e-end 'x-emit)

(def e-end
  (lambda (v-lab)
	  (cond (v-lab (emit1 `(,v-lab :))))	; if label, put out

	  (cond ((not (zerop k-regf))		; see of special to unbind
		 (emit3 'movl bnp-val bnp-reg)
		 (do ((i k-regf (sub1 i)))
		     ((zerop i) (emit3 'movl bnp-reg bnp-val))	
		     (emit3 'movl
			    `(-8 ,lpar ,bnp-reg ,rpar)
			    `(*-4 ,lpar ,bnp-reg ,rpar))
		     (emit3 'subl2 '$8 bnp-reg))))

	  ; fix up np-reg to reflect poping off of local variables if
	  ; we are not at the end of the function and there are some to
	  ; pop off
	  (cond ((and (not (eq (caar k-code) 'fini))
		      (not (zerop (difference k-stak (cadr k-save)))))
		 (emit3 'subl2 `($ ,(times 4 (difference k-stak (cadr k-save))))
			       np-reg)))
	  (e-unsave)))
			     
(putprop 'unbind 'e-unbind 'x-emit)

;--- e-unbind - levnum : number of contexts to unbind through
;	this is used to unbind specials when you don't want to
;	go to then end of the current context to do so.  this
;	is used, for example, to handle non-local returns
;
(def e-unbind
  (lambda (v-n)
	  (do ((numb k-regf)		; number of specials to unbind
	       (ll k-save (car ll))	; stack of info
	       (count v-n (sub1 count))) ; index vrbl
	      ((zerop count)
	       ; if any specials were bound in the contexts, emit
	       ; the proper instructions to unbind them
	       (cond ((greaterp numb 0)
		      (emit3 'movl bnp-val bnp-reg)
		      (do ((cnt numb (sub1 cnt)))
			  ((zerop cnt)
			   (emit3 'movl bnp-reg bnp-val))
			  (emit3 'movl
				 `(-8 ,lpar ,bnp-reg ,rpar)
				 `(*-4 ,lpar ,bnp-reg ,rpar))
			  (emit3 'subl2 '$8 bnp-reg))))
	       ; pop off the namestack
	       (cond ((not (zerop (setq ll (difference k-stak (cadr ll)))))
		      (emit3 'subl2 `($ ,(times 4 ll)) np-reg))))
	      (setq numb (plus numb (caddr ll))))))	; total k-regf

;--- e-unsave : restore the state variables. Occurs when we leave one
;		frame and pop off to the next one
;
(def e-unsave
  (lambda nil
	  (prog (tem)
		(setq tem k-save
		      k-save (car tem)   tem (cdr tem)
		      k-stak (car tem)   tem (cdr tem)
		      k-regf (car tem)   tem (cdr tem)
		      k-bind (car tem)))))

(def e-save
  (lambda nil
	  (setq k-save `(,k-save ,k-stak ,k-regf ,k-bind))))


(def e-eq
  (lambda (v-r1 v-r2)
	  (cond ((eq (caar k-code) 'get) 
		 (prog (v-i v-b)
		       (setq v-i (cdar k-code))
		       (setq v-b (e-reg (car v-i) nil)) 
		       (e-lose v-b)
		       (setq k-code (cdr k-code)) 
		       (cond ((null (cadr v-i)) (emit2 'clrl (list 'r v-b)))
			     (t (emit3 'movl (e-addr (cadr v-i) v-b t)
				       (list 'r v-b)))))))
	  (cond ((eq (caar k-code) 'false)
		 (rplaca (car k-code) 'true))
		((eq (caar k-code) 'true)
		 (rplaca (car k-code) 'false)))
	  (emit3 'cmpl v-r1 v-r2)))

(putprop 'eqs 'e-eqs 'x-emit)

;--- e-eqs
;	emits instructions to compare the top two items on the stack.
;	 note that it updates np first before poping the items from 
;	 the stack so if an interrupt occured here the top two values
;	 would be clobbered, this must be fixed.
;
(def e-eqs 
  (lambda nil
	  (setq k-stak (difference k-stak 2))
	  (emit3 'subl2 '"$8"
			np-reg)
	  (e-eq `(,lpar ,np-reg ,rpar)  ; compare top two times (above stack)
		`(4 ,lpar ,np-reg ,rpar))))

(putprop 'eqv 'e-eqv 'x-emit)

(def e-eqv 
  (lambda (v-r1 v-r2)
	  (e-eq (e-addr v-r1 nil t) (e-addr v-r2 nil t))))

(putprop 'fixup 'e-fixup 'x-emit)




(putprop 'seta 'e-seta 'x-emit)

;--- e-seta - v-r1 : dtpr lispval
;	      v-r2 : lispval
;	emits an instruction to replace the car of v-r1 with v-r2
;
(def e-seta 
  (lambda (v-r1 v-r2)
	  (emit3 'movl 
		 (list 'r (e-reg v-r2 nil))
		 (list 4 '"(r" (e-reg v-r1 nil) '")"))))

(putprop 'setas 'e-setas 'x-emit)

;--- e-setas - v-r : result register
;	       top-of-stack: lispval
;	       top-of-stack - 1 : dtpr lispval
;	emits instructions to replace the car of the top-of-stack -1 lispval
;	 with the top-of-stack lispval, then pops the stack of those two
; 	 lispval as put the top-of-stack - 1 lispval in v-r.
;	note: here again we pop np too soon which could result in big
;	 problem if an interrupt occured in the middle of the instruction
;	 sequence.
;
(def e-setas 
  (lambda (v-r)
	  (setq v-r (e-reg v-r nil))
	  (setq k-stak (difference k-stak 2))
	  (emit3 'subl2 '"$8"
			np-reg)
	  (emit3 'movl `(,lpar ,np-reg ,rpar) (list 'r v-r))
	  (emit3 'movl `( 4 ,lpar ,np-reg ,rpar) 
			(list 4 '"(r" v-r '")"))))

(putprop 'setd 'e-setd 'x-emit)

;--- e-setd - v-r1 : dtpr lispval
;	      v-r2 : lispval
;	emits instructions to replace the car of v-r1 with v-r2
;
(def e-setd 
  (lambda (v-r1 v-r2)
	  (emit3 'movl 
		 (list 'r (e-reg v-r2 nil))
		 (list '"(r" (e-reg v-r1 nil) '")"))))

(putprop 'setds 'e-setds 'x-emit)

;--- e-setds - v-r : result register
;	       top-of-stack : lispval
;	       top-of-stack - 1 : dtpr lisval
;	emits instructions to replace the cdr of the top-of-stack -1
;	 lispval with the top of stack lispval. The result is placed
;	 in v-r
(def e-setds 
  (lambda (v-r)
	  (setq v-r (e-reg v-r nil))
	  (setq k-stak (difference k-stak 2))
	  (emit3 'subl2 '"$8" np-reg)
	  (emit3 'movl `(,lpar ,np-reg ,rpar) (list 'r v-r))
	  (emit3 'movl `( 4 ,lpar ,np-reg ,rpar)
			(list '"(r" v-r '")"))))




(putprop 'dopop 'e-dopop 'x-emit)

(def e-dopop 
  (lambda (v-l)
	  (mapc '(lambda (v-x)
			 (emit3 'movl `( - ,lpar ,np-reg ,rpar)
				       (e-addr v-x nil t))
			 (setq k-stak (sub1 k-stak)))
		(reverse v-l))))

(putprop 'list 'e-list 'x-emit)

(def e-list (lambda nil nil)) 

(putprop 'chain 'e-chain 'x-emit)

;--- e-chain - v-r : result lispval
;	       v-e : dtpr lispval
;	       v-b : an atom of the form cxxr where the x's are a's and d's
;	emits instructions to put the cxxr of v-e in v-r
;
(def e-chain 
  (lambda (v-r v-e v-b) 
	  (setq v-r (e-reg v-r nil))
	  (setq v-e (e-reg v-e nil))
	  (cond ((setq v-b (cdr (reverse (cdr (explode v-b))))) 
		 (e-lose v-e) 
		 (e-note (e-lose v-r) (Gensym nil)) 
		 (setq v-r (concat 'r v-r)) 
		 (setq v-e (concat 'r v-e)) 
		 (prog (op)

		   loop 
		       (cond ((null v-b) (return))) 
		       (cond ((eq (car v-b) 'd)
			      (setq op (list '"(" v-e '")" )))
			     (t (setq op (list 4 '"(" v-e '")" ))))
		       (setq v-b (cdr v-b))
		       (cond ((and (not (null v-b)) (eq (car v-b) 'd))
			      (setq v-b (cdr v-b))
			      (setq op (cons '* op))))
		       (emit3 'movl op v-r)
		       (setq v-e v-r) 
		       (go loop))) 

		((equal v-r v-e)) 

		(t (emit3 'movl (list 'r v-e) (list 'r v-r))))))


(putprop 'getype 'e-getype 'x-emit)

(def e-getype 
  (lambda (v-r v-n) 
	  (prog (v-i v-b v-x v-x1) 
		(setq v-r (e-reg v-r nil))
		(setq v-x1 (setq v-x (list 'r v-r)))
		(cond ((eq (caar k-code) 'get) 
		       (setq v-i (cdar k-code)) 
		       (setq k-code (cdr k-code)) 
		       (e-type v-r)
		       (cond ((equal (e-note (e-lose 
					      (setq v-b 
						    (e-reg (car v-i) nil))) 
				      (setq v-i (cadr v-i))) 
				     v-r) 
			      (emit2 'pushl v-x)
			      (setq v-x '"(sp)")
			      (setq v-x1 '"(sp)+")))
		       (cond ((null v-i) (emit2 'clrl (list 'r v-b)))
			     (t (emit3 'movl (e-addr v-i v-b t)
				       (list 'r v-b)))))
		      (t (e-type v-r)))
		(e-lose v-r)
		(cond ((eq v-n 'name)
		       (emit3 'movl (list '"_tynames+4[r" v-r '"]")
			      (list 'r v-r))
		       (emit3 'movl (list '"(r" v-r '")") (list 'r v-r)))
		      ((atom v-n) (emit3 'cmpl (list '$ v-n) v-x1)
		       (cond ((eq (caar k-code) 'false)
			      (rplaca (car k-code) 'true))
			     ((eq (caar k-code) 'true)
			      (rplaca (car k-code) 'false))))
		      (t (prog nil
			       (emit4 'ashl v-x '$1 v-x)
			       (setq v-i 0)
			   loop
			       (cond ((null v-n) (go out)))
			       (setq v-i (mylogor v-i (leftshift 1 (car v-n))))
			       (setq v-n (cdr v-n))
			       (go loop)
			   out
			       (emit3 'bitw (list '$ v-i) v-x1)))))))



(putprop 'catchent 'e-catchent 'x-emit)

;--- e-catchent - v-l : label throw should go to
;	        - v-t : tag to be caught
;		- v-f : if non nil reg which contains flag to store in frame
;	We create a catch frame, the form is this:
;	 ---------------
;	| return addr  	|
;	 ---------------
;	| reg r13 (fp) 	|
;	 ---------------
;	|   reg r10 	|
;	 ---------------
;	|   reg r8	|		^
;	 ---------------		|  high addresses, bottom of stack
;	|   reg r6	|
;	 ---------------
;	|   Saved	|
;	|   (return)    |   (10 words) (kls CROCK fix)
;	|   dope 	|
;	 ---------------
;	|    bnp 	|
;	 ---------------
;	|    tag	|
;	 ---------------
;	|    flag	|
;	 ---------------
;	|    link 	|  <-- errp points here
;	 ---------------
;
; due to bad operation of e-addr (which returns addr of list or number,
; and value of atom), we must carefully check v-t
;
(def e-catchent
  (lambda (v-l v-t v-f)
	  (emit2 'pushab v-l)
	  (emit2 'pushr	'"$0x2540")	; register save mask
;	  (emit2 'subl2	'"$40,sp")
;	  (emit2 'movc3	'"$40,_setsav,(sp)") ; this won't work since lisp
					     ; may user register 0 - 5
					     ; the whole thing is a crock anyhow
			
	  (emit2 'jsb '_svkludg)
	  (emit2 'pushl	bnp-val)	; push value of bnp
	  (cond ((or (numberp v-t) (not (atom v-t)))
		 (emit2 'pushl (e-addr v-t nil nil)))
		(v-t (emit2 'pushl `(r ,(e-reg v-t nil))))
		(t   (emit2 'clrl '"-(sp)")))	; tag is nil
	  (cond (v-f (setq v-f (e-reg v-f nil)) ; if flag, find loc
		     (emit2 'pushl `(r ,v-f)))
		(t (emit2 'pushl '$1)))		; non flag, assume true
	  (emit2 'pushl '_errp)	; sav current errp value
	  (emit3 'movl 'sp '_errp)))

(putprop 'catchexit 'e-catchexit 'x-emit)

;--- e-catchexit - do catchexit stuff. This code is hit if we exit
;	a catch by just falling through, instead of via a throw.
;
(def e-catchexit
  (lambda nil
	  (emit3 'movl '"(sp)" '_errp)	; unstack error frame
	  (emit3 'addl2 '$76    'sp)))	; pop off 9 entries 
					; + 10 for (return) context


(putprop '*throw 'e-*throw 'x-emit)

;--- e-*throw - v-r : pseudo reg containing value to throw
;	     - v-nr : pseudo reg containing tag to throw
;
(def e-*throw
  (lambda (v-r v-nr)
	  (setq v-r (e-reg v-r nil)	; get real regis
		v-nr (e-reg v-nr nil))
	  (emit2 'pushl `(r ,v-r))
	  (emit2 'pushl `(r ,v-nr))
	  (emit3 'calls '$0 '_Idothrow)
	  (emit2 'clrl '"-(sp)")
	  (emit2 'pushab '__erthrow)
	  (emit3 'calls '$2 '_error)))
(putprop 'pushnil 'e-pushnil 'x-emit)
;--- e-pushnil - v-num : number of nils to push
;	pushs nils on the np stack in the most efficient way possible
;
(def e-pushnil
  (lambda (v-num)
	  (do ((i v-num (difference i 2)))
	      ((lessp i 2) (cond ((equal i 1) (emit2 'clrl push-np))))

	      (emit2 'clrq push-np))

	  (setq k-stak (plus k-stak v-num))))

(putprop 'fini 'e-fini 'x-emit)

;--- e-fini 
;	called at the end of a function, just emits a ret
;
(def e-fini
  (lambda nil
	  (emit1 'ret)))

(putprop 'arg 'e-arg 'x-emit)

;--- e-arg
;	form is (arg psreg)
;
(def e-arg
  (lambda (v-r)
	  (prog (tmp tmp2)
		(setq v-r (e-reg v-r nil))
		(emit3 'movl `(,lpar r ,v-r ,rpar) `(r ,v-r))
		(emit2 'jeql  (setq tmp (Gensym nil)))
		(emit3 'movl `("*-4(fp)[r" ,v-r "]") `(r ,v-r))
		(emit2 'jmp (setq tmp2 (Gensym nil)))
		(emit1 `(,tmp :))
		(emit3 'movl '"-8(fp)" `(r ,v-r))
		(emit1 `(,tmp2 :))
		(e-lose v-r))))



;; special system functions

(defsysf 'minus '_Lminus)
(defsysf 'add1  '_Ladd1)
(defsysf 'sub1  '_Lsub1)
(defsysf 'plist '_Lplist)
(defsysf 'cons  '_Lcons)
(defsysf 'putprop '_Lputprop)
(defsysf 'print '_Lprint)
(defsysf 'patom '_Lpatom)
(defsysf 'read '_Lread)
(defsysf 'concat '_Lconcat)
(defsysf 'get   '_Lget)
(defsysf 'mapc '_Lmapc)
(defsysf 'mapcan '_Lmapcan)
(defsysf 'list   '_Llist)
(defsysf 'add   '_Ladd)
(defsysf 'plus  '_Ladd)
(defsysf '>     '_Lgreaterp)
(defsysf '=     '_Lequal)
(defsysf 'times '_Ltimes)
(defsysf 'difference '_Lsub)

(flag 'set 'x-asg) 
(flag 'push 'x-asg) 
(flag 'minus 'x-asg) 
(flag 'skip 'x-asg) 
(flag 'set 'x-dont) 
(flag 'setq 'x-dont)
(flag 'prog 'x-dont) 
(flag 'lambda 'x-dont) 
(flag 'go 'x-dont) 
(flag 'return 'x-dont) 
(put 'go 'x-leap 'go) 
(put 'return 'x-leap 'return) 
(put 'label 'x-leap 'go) 
(setq x-spf 'x-spf) 
(setq x-spfq 'x-spfq)
(setq x-spfn 'x-spfn) 
(setq x-spfh 'x-spfh) 
(setq x-con 'x-con) 
(setq x-leap 'x-leap) 
(setq x-reg 'x-reg) 
(setq x-indx 'x-indx) 
(setq x-opt 'x-opt) 
(setq x-emit 'x-emit) 
(setq x-asg 'x-asg) 
(setq x-lab 'x-lab) 
(setq x-dont 'x-dont) 
(setq g-xv 'xv) 
(setq g-xv+1 'xv+1) 
(setq g-xv+2 'xv+2) 
(setq k-regf nil) 
(setq k-free 'nil) 
(setq k-nargs nil)
(setq k-cnargs nil)
(setq k-stak 'nil) 
(setq k-cstk 'nil) 
(setq k-prog 'nil) 
(setq k-undo 'nil) 
(setq k-bind 'nil) 
(setq k-back 'nil) 
(setq k-save 'nil) 
(setq k-code 'nil) 
(setq k-name 'nil) 
(setq k-args 'nil) 
(setq k-regs 'nil) 
(setq push-np '"(r6)+")
(setq r-xv 0) 
(setq r-xv+1 'r1) 
(put 'xv 'x-reg 0) 
(putprop 'xv 'force 'x-count)
(put 'xv+1 'x-reg 1) 
(put 'xv+2 'x-reg 2) 

(setq $gccount$ 0)		; incase auxfns0 is old
; macros are not compiled by default
(setq macros nil)
