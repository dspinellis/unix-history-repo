(include-if (null (get 'chead 'version)) "../chead.l")
(Liszt-file expr
   "$Header: expr.l,v 1.13 87/12/15 17:01:08 sklower Exp $")

;;; ----	e x p r				expression compilation
;;;
;;;				-[Fri Sep  2 22:10:20 1983 by layer]-


;--- d-exp :: compile a lisp expression
;	v-form : a lisp expression to compile
; returns an IADR which tells where the value was located.
;

(defun d-exp (v-form)
  (prog (first resloc tmp ftyp nomacrop)
    begin
	(if (atom v-form)
	    then (setq tmp (d-loc v-form))		;locate vrble
		 (if (null g-loc)
		     then (if g-cc then (d-cmpnil tmp))
		    else (d-move tmp g-loc)
			 #+for-68k (if g-cc then (d-cmpnil tmp)))
		 (d-handlecc)
		 (return tmp)

	 elseif (atom (setq first (car v-form)))
	   then ; the form (*no-macroexpand* <expr>)
		; turns into <expr>, and prevents <expr> from
		; being macroexpanded (at the top level)
		(if (eq '*no-macroexpand* first)
		   then (setq v-form (cadr v-form)
			      nomacrop t)
			(go begin))
		(if (and fl-xref (not (get first g-refseen)))
		     then (Push g-reflst first)
			  (putprop first t g-refseen))
	         (setq ftyp (d-functyp first (if nomacrop then nil
						else 'macros-ok)))
		 ; if nomacrop is t, then under no circumstances
		 ; permit the form to be macroexpanded
		 (if (and nomacrop (eq ftyp 'macro))
		     then (setq ftyp 'lambda))
		 ; If the resulting form is type macro or cmacro,
		 ; then call the appropriate function to macro-expand
		 ; it.
		 (if (memq ftyp '(macro cmacro))
		    then (setq tmp v-form)	; remember original form
			 (if (eq 'macro ftyp)
			     then (setq v-form (apply first v-form))
			   elseif (eq 'cmacro ftyp)
			     then (setq v-form (apply (get first 'cmacro)
						      v-form)))
			  ; If the resulting form is the same as
			  ; the original form, then we don't want to
			  ; macro expand again.  We call d-functyp and tell
			  ; it that we want a second opinion
			  (if (and (eq (car v-form) first)
				   (equal tmp v-form))
			     then (setq ftyp (d-functyp first nil))
			     else (go begin))) ; retry with what we have

		 (if (and (setq tmp (get first 'if-fixnum-args))
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

	(if (null g-loc)
	    then (if g-cc then (d-cmpnil 'reg))
	 elseif (memq g-loc '(reg #+(or for-vax for-tahoe) r0 #+for-68k d0))
	    then (if g-cc then (d-cmpnil 'reg))
	   else (d-move 'reg g-loc)
		#+for-68k (if g-cc then (d-cmpnil 'reg)))
	(if g-cc then (d-handlecc))))

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


;--- d-argnumchk :: check that the correct number of arguments are given
; v-form (global) contains the expression to check
; class: hard or soft, hard means that failure is an error, soft means
;	warning
(defun d-argnumchk (class)
   (let ((info (car (get (car v-form) 'fcn-info)))
	 (argsize (length (cdr v-form))))
      (if info then (d-argcheckit info argsize class))))

;--- d-argcheckit
; info - arg information form:  (min# . max#)  max# of nil means no max
; numargs - number of arguments given
; class - hard or soft
; v-form(global) - expression begin checked
;
(defun d-argcheckit (info numargs class)
   (if (and (car info) (< numargs (car info)))
      then (if (eq class 'hard)
	      then (comp-err
		      (difference (car info) numargs)
		      " too few argument(s) given in this expression:" N
		      v-form)
	      else (comp-warn
		      (difference (car info) numargs)
		      " too few argument(s) given in this expression:" N
		      v-form))
    elseif (and (cdr info) (> numargs (cdr info)))
      then (if (eq class 'hard)
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
(defun d-pushargs (args)
   (if args then
       (do ((ll args (cdr ll))
	    (g-loc 'stack)
	    (g-cc nil)
	    (g-ret nil))
	   ((null ll))
	   (d-exp (car ll))
	   (push nil g-locs)
	   (incr g-loccnt))))

;--- d-cxxr :: split apart a cxxr function name
;	- name : a possible cxxr function name
; returns the a's and d's between c and r in reverse order, or else
;  returns nil if this is not a cxxr name
;
(defun d-cxxr (name)
  (let ((expl (explodec name)))
       (if (eq 'c (car expl))			; must begin with c
	   then (do ((ll (cdr expl) (cdr ll))
		     (tmp)
		     (res))
		    (nil)
		    (setq tmp (car ll))
		    (if (null (cdr ll))	
			then (if (eq 'r tmp)	; must end in r
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
#+(or for-vax for-tahoe)
(defun d-callbig (name args bcdp)
  (let ((tmp (get name g-localf))
	c)
       (forcecomment `(calling ,name))
       (if (d-dotailrecursion name args) thenret
        elseif tmp then ;-- local function call
		    (d-pushargs args)
		    (e-quick-call (car tmp))
		    (setq g-locs (nthcdr (setq c (length args)) g-locs))
		    (setq g-loccnt (- g-loccnt c))
	else (if bcdp 		;-- bcdcall
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

#+for-68k
(defun d-callbig (name args bcdp)
  (let ((tmp (get name g-localf))
	c)
       (forcecomment `(calling ,name))
       (if (d-dotailrecursion name args)
	   thenret
        elseif tmp then ;-- local function call
		    (d-pushargs args)
		    (setq c (length args))
		    (if (null $global-reg$) then
			(e-write3 'lea `(,(* -4 c) #.np-reg) 'a5)
			(e-move 'a5 '#.lbot-sym)
			(e-move '#.np-reg '#.np-sym))
		    (e-quick-call (car tmp))
		    (setq g-locs (nthcdr c g-locs))
		    (setq g-loccnt (- g-loccnt c))
	else (if bcdp 		;-- bcdcall
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

;--- d-calltran :: call a function through the transfer table
;  name - name of function to call
;  c - number of arguments to the function
;
#+(or for-vax for-tahoe)
(defun d-calltran (name c)
   (if $global-reg$
       then (e-write3 'movab `(,(* -4 c) #.np-reg) '#.lbot-reg)
       else (e-write3 'movab `(,(* -4 c) #.np-reg) '#.lbot-sym)
	    (e-move '#.np-reg '#.np-sym))
   #+for-vax (e-write3 'calls '$0 (concat "*trantb+" (d-tranloc name)))
   #+for-tahoe (progn (e-write3 'movab (concat "trantb+" (d-tranloc name)) 'r2)
		      (e-write3 'calls '$4 '"*(r2)"))
   (if $global-reg$
       then (e-move '#.lbot-reg '#.np-reg)
       else (e-write3 'movab `(,(* -4 c) #.np-reg) '#.np-reg)))

#+for-68k
(defun d-calltran (name c)
   (if $global-reg$
       then (e-write3 'lea `(,(* -4 c) #.np-reg) 'a5)
	    (e-move 'a5 '#.lbot-reg)
       else (e-write3 'lea `(,(* -4 c) #.np-reg) 'a5)
	    (e-move 'a5 '#.lbot-sym)
	    (e-move '#.np-reg '#.np-sym))
   (e-move (concat "trantb+" (d-tranloc name)) 'a5)
   (e-quick-call '(0 a5))
   (if $global-reg$
       then (e-move '#.lbot-reg '#.np-reg)
       else (e-write3 'lea `(,(* -4 c) #.np-reg) '#.np-reg)))

;--- d-calldirect :: call a function directly
;
;  name - name of a function in the C code (known about by fasl)
;    c  - number of args
;
#+(or for-vax for-tahoe)
(defun d-calldirect (name c)
   (if $global-reg$
       then (e-write3 'movab `(,(* -4 c) #.np-reg) '#.lbot-reg)
       else (e-write3 'movab `(,(* -4 c) #.np-reg) '#.lbot-sym)
	    (e-move '#.np-reg '#.np-sym))
#+for-vax (e-write3 'calls '$0  name)
#+for-tahoe (e-write3 'callf '$4  name)
   (if $global-reg$
       then (e-move '#.lbot-reg '#.np-reg)
       else (e-write3 'movab `(,(* -4 c) #.np-reg) '#.np-reg)))

#+for-68k
(defun d-calldirect (name c)
   (if $global-reg$
       then (e-write3 'lea `(,(* -4 c) #.np-reg) 'a5)
	    (e-move 'a5 '#.lbot-reg)
       else (e-write3 'lea `(,(* -4 c) #.np-reg) 'a5)
	    (e-move 'a5 '#.lbot-sym)
	    (e-move '#.np-reg '#.np-sym))
   (e-quick-call name)
   (if $global-reg$
       then (e-move '#.lbot-reg '#.np-reg)
       else (e-write3 'lea `(,(* -4 c) #.np-reg) '#.np-reg)))

;--- d-bcdcall :: call a function through a binary data object
;  
; at this point the stack contains n-1 arguments and a binary object which
; is the address of the compiled lambda expression to go to.  We set
; up lbot right above the binary on the stack and call the function.
;
#+(or for-vax for-tahoe)
(defun d-bcdcall (n)
   (if $global-reg$
       then (e-write3 'movab `(,(* -4 (- n 1)) #.np-reg) '#.lbot-reg)
       else (e-write3 'movab `(,(* -4 (- n 1)) #.np-reg) '#.lbot-sym)
	    (e-move '#.np-reg '#.np-sym))
   (e-move  `(* ,(* -4 n) #.np-reg) 'r0)    ;get address to call to
#+for-vax   (e-write3 'calls '$0 "(r0)")
#+for-tahoe (e-write3 'calls '$4 "(r0)")
   (if $global-reg$
       then (e-write3 'movab '(-4 #.lbot-reg) '#.np-reg)
       else (e-write3 'movab `(,(* -4 n) #.np-reg) '#.np-reg)))

#+for-68k
(defun d-bcdcall (n)
   (if $global-reg$
       then (e-write3 'lea `(,(* -4 (- n 1)) #.np-reg) 'a5)
	    (e-move 'a5 '#.lbot-reg)
       else (e-write3 'lea `(,(* -4 (- n 1)) #.np-reg) 'a5)
	    (e-move 'a5 '#.lbot-sym)
	    (e-move '#.np-reg '#.np-sym))
   (e-move `(,(* -4 n) #.np-reg) 'a5)	; get address to call to
   (e-move `(0 a5) 'a5)
   (e-quick-call '(0 a5))
   (if $global-reg$
       then (e-move '#.lbot-reg 'a5)
	    (e-write3 'lea '(-4 a5) '#.np-reg)
       else (e-write3 'lea `(,(* -4 n) #.np-reg) '#.np-reg)))

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
   (prog (nargs lbot)
       (if (null (and g-ret
		      (eq name g-fname)
		      (do ((loccnt 0)
			   (ll g-locs (cdr ll)))
			  ((null ll) (return t))
			  (if (dtpr (car ll))
			      then (if (or (eq 'catcherrset (caar ll))
					   (greaterp (cdar ll) 0))
				       then (return nil))
			      else (incr loccnt)))))
	   then (return nil))

       (makecomment '(tail merging))
       (comp-note g-fname ": Tail merging being done: " v-form)

       (setq nargs (length args))
       
       ; evalate the arguments, putting them above the arguments to the
       ; function we are executing...
       (let ((g-locs g-locs)
	     (g-loccnt g-loccnt))
	   (d-pushargs args))

       (if $global-reg$
	   then (setq lbot #+for-68k 'a5 #+(or for-vax for-tahoe) '#.lbot-reg)
		#+for-68k (e-move '#.lbot-reg lbot)
	   else (setq lbot #+for-68k 'a5 #+(or for-vax for-tahoe) '#.fixnum-reg)
		(e-move '#.lbot-sym lbot))

       ; setup lbot-reg to point to the bottom of the original
       ;args...
       (if (eq 'lexpr g-ftype)
	   then #+for-vax
		(e-write4 'ashl '($ 2) '(* -4 #.olbot-reg) lbot)
		#+for-tahoe
		(e-write4 'shal '($ 2) '(* -4 #.olbot-reg) lbot)
		#+for-68k
		(progn
		 (d-regused 'd6)
		 (e-move '(* -4 #.olbot-reg) 'd6)
		 (e-write3 'asll '($ 2) 'd6)
		 (e-move 'd6 lbot))
		(e-sub lbot '#.olbot-reg)
		(e-sub3 '($ 4) '#.olbot-reg lbot)
	   else (e-move '#.olbot-reg lbot))

       ; copy the new args down into the place of the original ones...
       (do ((i nargs (1- i))
	    (off-top (* nargs -4) (+ off-top 4))
	    (off-bot 0 (+ off-bot 4)))
	   ((zerop i))
	   (e-move `(,off-top #.np-reg) `(,off-bot ,lbot)))

       ; setup np for the coming call...
       (e-add3 `($ ,(* 4 nargs)) lbot '#.np-reg)

       (e-goto g-topsym)
       ;return t to indicate that tailrecursion was successful
       (return t)))
