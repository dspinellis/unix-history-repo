(include-if (null (get 'chead 'version)) "chead.l")
(Liszt-file expr
   "$Header: /na/franz/liszt/RCS/expr.l,v 1.1 83/01/26 12:14:06 jkf Exp $")

;;; ----	e x p r				expression compilation
;;;


;--- d-exp :: compile a lisp expression				= d-exp =
;	v-form : a lisp expression to compile
; returns an IADR which tells where the value was located.
;

;.. c-*catch, c-*throw, c-boole, c-cond, c-do, c-errset, c-funcall
;.. c-internal-fixnum-box, c-prog, c-progv, c-return, c-rplaca
;.. c-rplacd, c-setarg, cc-and, cc-arg, cc-cxxr, cc-eq
;.. cc-function, cc-memq, cc-not, cc-oneminus, cc-oneplus, cc-or
;.. cc-setq, cc-typep, d-exps, d-fixnumcode, d-fixop, d-lambbody
;.. d-pushargs, d-supercxr, d-superrplacx, d-typecmplx, d-typesimp
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
	         (setq ftyp (d-functyp first 'macros-ok))
		 ; If the resulting form is type macro or cmacro,
		 ; then call the appropriate function to macro-expand
		 ; it.
		 (If (memq ftyp '(macro cmacro))
		    then (setq tmp v-form)	; remember original form
			 (If (eq 'macro ftyp)
			     then (setq v-form (apply first v-form))
			   elseif (eq 'cmacro ftyp)
			     then (setq v-form (apply (get first 'cmacro)
						      v-form)))
			  ; If the resulting form is the same as
			  ; the original form, then we don't want to
			  ; macro expand again.  We call d-functyp and tell
			  ; it that we want a second opinion
			  (If (and (eq (car v-form) first)
				   (equal tmp v-form))
			     then (setq ftyp (d-functyp first nil))
			     else (go begin))) ; retry with what we have

		 (If (and (setq tmp (get first 'if-fixnum-args))
			      (d-allfixnumargs (cdr v-form)))
		    then (setq v-form (cons tmp (cdr v-form)))
			 (go begin)
		  elseif (setq tmp (get first 'fl-exprcc))
		    then (d-argnumchk 'hard)
			 (return (funcall tmp))
		  elseif (setq tmp (get first 'fl-exprm))
		    then (d-argnumchk 'hard)
			 (setq v-form (funcall tmp))
			 (go begin)
		  elseif (setq tmp (get first 'fl-expr))
		    then (d-argnumchk 'hard)
			 (funcall tmp)
		  elseif (setq tmp (or (and (eq 'car first)
					    '( a ))
				       (and (eq 'cdr first)
					    '( d ))
				       (d-cxxr first)))
		    then (d-argcheckit '(1 . 1) (length (cdr v-form)) 'hard)
			 (return (cc-cxxr (cadr v-form) tmp))
		   elseif (eq 'nlambda ftyp)
		    then (d-argnumchk 'soft)
			 (d-callbig first `(',(cdr v-form)) nil)
		   elseif (or (eq 'lambda ftyp) (eq 'lexpr ftyp))
		     then (setq tmp (length v-form))
		     	  (d-argnumchk 'soft)
			  (d-callbig first (cdr v-form) nil)
		   elseif (eq 'array ftyp)
		    then (d-handlearrayref)
		  elseif (eq 'macro ftyp)
		    then (comp-err "infinite macro expansion " v-form)
		    else (comp-err "internal liszt err in d-exp" v-form))

	 elseif (eq 'lambda (car first))
	    then (c-lambexp)

	 elseif (or (eq 'quote (car first)) (eq 'function (car first)))
	    then (comp-warn "bizzare function name " (or first))
		 (setq v-form (cons (cadr first) (cdr v-form)))
		 (go begin)
		
	 else (comp-err "bad expression" (or v-form)))

	(If (null g-loc)
	    then (If g-cc then (d-tst 'reg))
	 elseif (memq g-loc '(reg r0))
	    then (If g-cc then (d-tst 'reg))
	  else (d-move 'reg g-loc))
	(If g-cc then (d-handlecc))))

;--- d-exps :: compile a list of expressions
;	- exps : list of expressions
; the last expression is evaluated according to g-loc and g-cc, the others
; are evaluated with g-loc and g-cc nil.
;
;.. c-cond, c-do
(defun d-exps (exps)
  (d-exp (do ((ll exps (cdr ll))
	      (g-loc nil)
	      (g-cc  nil)
	      (g-ret nil))
	     ((null (cdr ll)) (car ll))
	     (d-exp (car ll)))))


;--- d-argnumchk :: check that the correct number of arguments are given
; v-form (global) contains the expression to check
; class: hard or soft, hard means that failure is an error, soft means
;	warning
;.. d-exp
(defun d-argnumchk (class)
   (let ((info (car (get (car v-form) 'fcn-info)))
	 (argsize (length (cdr v-form))))
      (If info then (d-argcheckit info argsize class))))

;--- d-argcheckit
; info - arg information form:  (min# . max#)  max# of nil means no max
; numargs - number of arguments given
; class - hard or soft
; v-form(global) - expression begin checked
;
;.. d-argnumchk, d-exp
(defun d-argcheckit (info numargs class)
   (If (and (car info) (< numargs (car info)))
      then (If (eq class 'hard)
	      then (comp-err
		      (difference (car info) numargs)
		      " too few argument(s) given in this expression:" N
		      v-form)
	      else (comp-warn
		      (difference (car info) numargs)
		      " too few argument(s) given in this expression:" N
		      v-form))
    elseif (and (cdr info) (> numargs (cdr info)))
      then (If (eq class 'hard)
	      then (comp-err
		      (difference numargs (cdr info))
		      " too many argument(s) given in this expression:" N
		      v-form)
	      else (comp-warn
		      (difference numargs (cdr info))
		      " too many argument(s) given in this expression:" N
		      v-form))))

;--- d-pushargs :: compile and push a list of expressions
;	- exps : list of expressions
; compiles and stacks a list of expressions
;
;.. c-cons, c-do, c-funcall, c-get, c-internal-bind-vars
;.. c-internal-unbind-vars, c-lambexp, c-list, c-prog, c-setarg
;.. cc-equal, d-callbig, d-dostore, d-dotailrecursion
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
;.. d-exp
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


;--- d-callbig :: call a local, global or bcd  function	
;
; name is the name of the function we are to call
; args are the arguments to evaluate and call the function with
; if bcdp is t then we are calling through a binary object and thus
; name is ingored.
;
;.. c-bcdcall, c-boole, d-exp, d-fixop
(defun d-callbig (name args bcdp)
  (let ((tmp (get name g-localf))
	c)
       (forcecomment `(calling ,name))
       (If (d-dotailrecursion name args) thenret
        elseif tmp then ;-- local function call
		    (d-pushargs args)
		    (e-write2 'jsb (car tmp))
		    (setq g-locs (nthcdr (setq c (length args)) g-locs))
		    (setq g-loccnt (- g-loccnt c))
	else (If bcdp 		;-- bcdcall
		 then (d-pushargs args)
		      (setq c (length args))
		      (d-bcdcall c)
	       elseif fl-tran	;-- transfer table linkage
	         then (d-pushargs args)
		    (setq c (length args))
		    (d-calltran name c)
		    (putprop name t g-stdref)	; remember we've called this
	       else ;--- shouldn't get here
		    (comp-err " bad args to d-callbig : "
			      (or name args)))
	     (setq g-locs (nthcdr c g-locs))
	     (setq g-loccnt (- g-loccnt c)))
       (d-clearreg)))
	

;--- d-calltran :: call a function through the transfer table  	  = d-calltran =
;  name - name of function to call
;  c - number of arguments to the function
;
;.. c-Internal-bcdcall, cc-equal, d-callbig
(defun d-calltran (name c)
  (e-write3 'movab `(,(* -4 c) #.Np-reg) '#.Lbot-reg)
  (e-write3 'calls '$0 (concat "*trantb+" (d-tranloc name)))
  (e-write3 'movl '#.Lbot-reg '#.Np-reg))

;--- d-calldirect :: call a function directly
;
;  name - name of a function in the C code (known about by fasl)
;    c  - number of args
;
;.. c-internal-bind-vars, c-internal-unbind-vars
(defun d-calldirect (name c)
  (e-write3 'movab `(,(* -4 c) #.Np-reg) '#.Lbot-reg)
  (e-write3 'calls '$0  name)
  (e-write3 'movl '#.Lbot-reg '#.Np-reg))

;--- d-bcdcall :: call a function through a binary data object
;  
; at this point the stack contains n-1 arguments and a binary object which
; is the address of the compiled lambda expression to go to.  We set
; up lbot right above the binary on the stack and call the function.
;
;.. c-Internal-bcdcall, d-callbig
(defun d-bcdcall (n)
  (e-write3 'movab `(,(* -4 (- n 1)) #.Np-reg) '#.Lbot-reg)
  (e-write3 'movl '(* -4 #.Lbot-reg) 'r0)	; get address to call to
  (e-write3 'calls '$0 "(r0)")
  (e-write3 'movab '(-4 #.Lbot-reg) '#.Np-reg)	; set up np after call
  )

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
;.. d-callbig
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
	    (comp-note g-fname ": Tail merging being done: " v-form)
	    (let ((g-locs g-locs)
		  (g-loccnt g-loccnt))
		 (d-pushargs args))     ; push then forget about
	    (let (base-reg nargs)
	         (If (eq g-ftype 'lexpr)
		    then ; the beginning of the local variables
			 ;has been stacked
			 (e-write3 'addl2 '$4 'sp)	; pop off arg count
			 (e-write4 'addl3 '$4 "(sp)" '#.Lbot-reg)
			 (setq base-reg '#.Lbot-reg)	; will push from   bot
		    else (e-write3 'movl '#.oLbot-reg '#.Lbot-reg)
			 (setq base-reg '#.Lbot-reg)) ; will push from lbot
		 (setq nargs (length args))
		 (do ((i nargs (1- i))
		      (top (* nargs -4) (+ top 4))
		      (bot 0 (+ bot 4)))
		     ((zerop i))
		     (e-write3 'movl `(,top #.Np-reg) `(,bot ,base-reg)))
		 (e-write3 'movab `(,(* 4 nargs) ,base-reg) '#.Np-reg)
		 (e-goto g-topsym))
	    t)) ; return t to indicate that tailrecursion was successful
