(include "chead.l")
(Liszt-file func  "@(#)func.l	1.4	5/7/82")

;;; ----	f u n c				function compilation
;;;

; cm-ncons :: macro out an ncons expression			= cm-ncons =
;
(defun cm-ncons nil
  `(cons ,(cadr v-form) nil))

; cc-not :: compile a "not" or "null" expression		= cc-not =
;
(defun cc-not nil
  (makecomment '(beginning not))
  (If (null g-loc)
      then (let ((g-cc (cons (cdr g-cc) (car g-cc)))
		 (g-ret nil))
		(d-exp (cadr v-form)))
      else (let ((finlab (d-genlab))
		 (finlab2 (d-genlab))
		 (g-ret nil))
		; eval arg and jump to finlab if nil
		(let ((g-cc (cons finlab nil))
		      g-loc)
		     (d-exp (cadr v-form)))
		; didn't jump, answer must be t
		(d-move 'T g-loc)
		(If (car g-cc) then (e-goto (car g-cc))
			       else (e-goto finlab2))
		(e-label finlab)
		; answer is nil
		(d-move 'Nil g-loc)
		(If (cdr g-cc) then (e-goto (cdr g-cc)))
		(e-label finlab2))))


;--- cc-numberp :: check for numberness				= cc-numberp =
;
(defun cc-numberp nil
  (d-typecmplx (cadr v-form) 
	       '#.(concat '$ (plus 1_2 1_4 1_9))))


;--- cc-or :: compile an "or" expression			= cc-or =
;
(defun cc-or nil
  (let ((finlab (d-genlab))
	(finlab2)
	(exps (If (cdr v-form) thenret else '(nil)))) ; (or) => nil
       (If (null (car g-cc))
	   then (d-exp (do ((g-cc (cons finlab nil))
			    (g-loc (If g-loc then 'reg))
			    (g-ret nil)
			    (ll exps (cdr ll)))
			   ((null (cdr ll)) (car ll))
			   (d-exp (car ll))))
		(If g-loc then (setq finlab2 (d-genlab))
			       (e-goto finlab2)
			       (e-label finlab)
			       (d-move 'reg g-loc)
			       (e-label finlab2)
			  else (e-label finlab))
	   else (If (null g-loc) then (setq finlab (car g-cc)))
		(d-exp (do ((g-cc (cons finlab nil))
			    (g-loc (If g-loc then 'reg))
			    (g-ret nil)
			    (ll exps (cdr ll)))
			   ((null (cdr ll)) (car ll))
			   (d-exp (car ll))))
		(If g-loc then (setq finlab2 (d-genlab))
			       (e-goto finlab2)
			       (e-label finlab)
			       (d-move 'reg g-loc)
			       (e-goto (car g-cc))	; result is t
			       (e-label finlab2)))
       (d-clearreg)))  ; we are not sure of the state due to possible branches.
			       

;--- c-prog :: compile a "prog" expression			= c-prog =
;
; for interlisp compatibility, we allow the formal variable list to
; contain objects of this form (vrbl init) which gives the initial value
; for that variable (instead of nil)
;
(defun c-prog nil
   (let ((g-decls g-decls))
      (let (g-loc g-cc seeninit initf
	    (p-rettrue g-ret) (g-ret nil)
	    ((spcs locs initsv . initsn) (d-classify (cadr v-form))))

	 (e-pushnil (length locs))	; locals initially nil
	 (d-bindprg spcs locs)		; bind locs and specs

	 (cond (initsv (d-pushargs initsv)
		       (mapc '(lambda (x)
				 (d-move 'unstack (d-loc x))
				 (decr g-loccnt)
				 (unpush g-locs))
			     (nreverse initsn))))

	 ; determine all possible labels
	 (do ((ll (cddr v-form) (cdr ll))
	      (labs nil))
	     ((null ll) (setq g-labs `((,(d-genlab) ,@labs)
				       ,@g-labs)))
	     (If (and (car ll) (symbolp (car ll)))
		then (If (assq (car ll) labs)
			then (comp-err "label is mulitiply defined " (car ll))
			else (setq labs (cons (cons (car ll) (d-genlab))
					      labs)))))

	 ; compile each form which is not a label
	 (d-clearreg)		; unknown state after binding
	 (do ((ll (cddr v-form) (cdr ll)))
	     ((null ll))
	     (If (or (null (car ll)) (not (symbolp (car ll))))
		then (d-exp (car ll))
		else (e-label (cdr (assq (car ll) (cdar g-labs))))
		     (d-clearreg))))		; dont know state after label

      ; result is nil if fall out and care about value
      (If (or g-cc g-loc) then (d-move 'Nil 'reg))

      (e-label (caar g-labs))		; return to label
      (setq g-labs (cdr g-labs))
      (d-unbind)))			; unbind our frame


;--- d-bindprg :: do binding for a prog expression
;	- spcs : list of special variables
;	- locs : list of local variables
;	- specinit : init values for specs (or nil if all are nil)
;
(defun d-bindprg (spcs locs)


	; place the local vrbls and prog frame entry on the stack
	(setq g-loccnt (+ g-loccnt (length locs))
	      g-locs (nconc locs `((prog . ,(length spcs)) ,@g-locs)))
	 
	; now bind the specials, if any, to nil
	(If spcs then (e-setupbind)
	    	      (mapc '(lambda (vrb)
				     (e-shallowbind vrb 'Nil))
			    spcs)
	              (e-unsetupbind)))

;--- d-unbind :: remove one frame from g-locs
;
(defun d-unbind nil
  (do ((count 0 (1+ count)))
      ((dtpr (car g-locs))
       (If (not (zerop (cdar g-locs)))
	   then (e-unshallowbind (cdar g-locs)))
       (cond ((not (zerop count))
	      (e-dropnp count)
	      
	      (setq g-loccnt (- g-loccnt count))))
       (setq g-locs (cdr g-locs)))
      (setq g-locs (cdr g-locs))))
	

;--- d-classify :: seperate variable list into special and non-special
;	- lst : list of variables
; returns ( xxx yyy zzz . aaa) 
;		where xxx is the list of special variables and
;		yyy is the list of local variables
;		zzz are the non nil initial values for prog variables
;		aaa are the names corresponding to the values in zzz
;
(defun d-classify (lst)
  (do ((ll lst (cdr ll))
       (locs) (spcs) (init) (initsv) (initsn) 
         (name))
      ((null ll) (cons spcs (cons locs (cons initsv initsn))))
      (If (atom (car ll)) then (setq name (car ll))
			  else (setq name (caar ll))
			       (Push initsn name)
			       (Push initsv (cadar ll)))
      (If (d-specialp name)
	  then (Push spcs name)
	  else (Push locs name))))

; cm-progn :: compile a "progn" expression			= cm-progn =
;
(defun cm-progn nil
  `((lambda nil ,@(cdr v-form))))


; cm-prog1 :: compile a "prog1" expression			= cm-prog1 =
;
(defun cm-prog1 nil
  (let ((gl (d-genlab)))
       `((lambda (,gl) 
		 ,@(cddr v-form)
		 ,gl)
	 ,(cadr v-form))))


; cm-prog2 :: compile a "prog2" expression			= cm-prog2 =
;
(defun cm-prog2 nil
  (let ((gl (d-genlab)))
       `((lambda (,gl) ,(cadr v-form)
		       (setq ,gl ,(caddr v-form))
		       ,@(cdddr v-form)
		       ,gl)
	 nil)))


;--- cm-progv :: compile a progv form				= cm-progv =
;  a progv form looks like (progv 'l-vars 'l-inits 'g-exp1 ... 'g-expn)
; l-vars should be a list of variables, l-inits a list of initial forms
; We cannot permit returns and go-s through this form.
;
; we stack a (progv . 0) form on g-locs so that return and go will know
; not to try to go through this form.
;
(defun c-progv nil
   (let ((gl (d-genlab))
	 (g-labs (cons nil g-labs))
	 (g-locs (cons '(progv . 0) g-locs)))
      (d-exp `((lambda (,gl)
		  (prog1 (progn ,@(cdddr v-form))
			 (internal-unbind-vars ,gl)))
	       (internal-bind-vars ,(cadr v-form) ,(caddr v-form))))))

(defun c-internal-bind-vars nil
   (let ((g-locs g-locs)
	 (g-loccnt g-loccnt))
      (d-pushargs (cdr v-form))
      (d-calldirect '_Ibindvars (length (cdr v-form)))))

(defun c-internal-unbind-vars nil
   (let ((g-locs g-locs)
	 (g-loccnt g-loccnt))
      (d-pushargs (cdr v-form))
      (d-calldirect '_Iunbindvars (length (cdr v-form)))))

;--- cc-quote : compile a "quote" expression			= cc-quote =
; 
; if we are just looking to set the ; cc, we just make sure 
; we set the cc depending on whether the expression quoted is
; nil or not.
(defun cc-quote nil
  (let ((arg (cadr v-form))
	argloc)

       (If (null g-loc) 
	   then (If (and (null arg) (cdr g-cc)
		    then (e-goto (cdr g-cc))
		 elseif (and arg (car g-cc))
		    then (e-goto (car g-cc)))
		 elseif (null g-cc)
		    then (comp-warn "losing the value of this expression " (or v-form)))
	else (d-move (d-loclit arg nil) g-loc)
	     (d-handlecc))))


;--- c-setarg :: set a lexpr's arg				= cc-setarg  =
; form is (setarg index value)
;
(defun c-setarg nil
  (If (not (eq 'lexpr g-ftype))
      then (comp-err "setarg only allowed in lexprs"))
  (If (and fl-inter (eq (length (cdr v-form)) 3))	; interlisp setarg
      then (If (not (eq (cadr v-form) (car g-args)))
	       then (comp-err "setarg: can only compile local setargs " v-form)
	       else (setq v-form (cdr v-form))))
   (d-pushargs (list (cadr v-form)))    ; stack index
   (let ((g-loc 'reg)
	 (g-cc nil)
	 (g-ret nil))
	(d-exp (caddr v-form)))
   (d-clearreg 'r1)			; indicate we are clobbering r1
   (e-write3 'movl `(* -4 #.Np-reg) 'r1)	; actual number to r1
   (e-write3 'movl 'r0 "*-4(fp)[r1]")	; store value in
   (e-pop 1)
   (unpush g-locs)
   (decr g-loccnt))

;--- cc-stringp :: check for string ness			= cc-stringp =
;
(defun cc-stringp nil
  (d-typesimp (cadr v-form) '$0))


;--- cc-symbolp :: check for symbolness				= cc-symbolp =
;
(defun cc-symbolp nil
  (d-typesimp (cadr v-form) '$1))



;--- c-return :: compile a "return" statement			= c-return =
;
(defun c-return nil
  ; value is always put in r0
  (let ((g-loc 'reg)
	g-cc
	g-ret)
       (d-exp (cadr v-form)))

  ; if we are doing a non local return, compute number of specials to unbind
  ; and locals to pop
  (If (car g-labs) then (e-goto (caar g-labs))
      else (do ((loccnt 0)		;; locals
		(speccnt 0)		;; special
		(catcherrset 0)		;; catch/errset frames
		(ll g-labs (cdr ll))
		(locs g-locs))
	       ((null ll) (comp-err "return used not within a prog or do"))
	       (If (car ll)
		  then  (comp-note g-fname ": non local return used ")
		       ; unbind down to but not including
		       ; this frame.
		       (If (greaterp loccnt 0)
			  then (e-pop loccnt))
		       (If (greaterp speccnt 0)
			  then (e-unshallowbind speccnt))
		       (If (greaterp catcherrset 0)
			  then (comp-note g-fname
					  ": return through a catch or errset"
					  v-form)
			       (do ((i 0 (1+ i)))
				   ((=& catcherrset i))
				   (d-popframe)))
		       (e-goto (caar ll))
		       (return)
		  else ; determine number of locals and special on
		       ; stack for this frame, add to running
		       ; totals
		       (do ()
			   ((dtpr (car locs))
			    (If (eq 'catcherrset (caar locs)) ; catchframe
			       then (incr catcherrset)
			     elseif (eq 'progv (caar locs))
			       then (comp-err "Attempt to 'return' through a progv"))
			    (setq speccnt (+ speccnt (cdar locs))
				  locs (cdr locs)))
			   (incr loccnt)
			   (setq locs (cdr locs)))))))
					     
	 
; c-rplaca :: compile a "rplaca" expression			= c-rplaca =
;
(defun c-rplaca nil
  (let ((ssimp (d-simple (caddr v-form)))
	(g-ret nil))
       (let ((g-loc (If ssimp then 'reg else 'stack))
	     (g-cc nil))
	    (d-exp (cadr v-form)))
       (If (null ssimp) then (Push g-locs nil)
	                     (incr g-loccnt)
			     (let ((g-loc 'r1)
				   (g-cc nil))
				  (d-exp (caddr v-form)))
	                     (d-move 'unstack 'reg)
			     (unpush g-locs)
			     (decr g-loccnt)
			     (e-move 'r1 '(4 r0))
	   else (e-move (e-cvt ssimp)  '(4 r0)))
       (d-clearreg)))		; cant tell what we are clobbering


; c-rplacd :: compile a "rplacd" expression			= c-rplacd =
;
(defun c-rplacd nil
  (let ((ssimp (d-simple (caddr v-form)))
	(g-ret nil))
       (let ((g-loc (If ssimp then 'reg else 'stack))
	     (g-cc nil))
	    (d-exp (cadr v-form)))
       (If (null ssimp) then (Push g-locs nil)
	                     (incr g-loccnt)
			     (let ((g-loc 'r1)
				   (g-cc nil))
				  (d-exp (caddr v-form)))
	                     (d-move 'unstack 'reg)
			     (unpush g-locs)
			     (decr g-loccnt)
			     (e-move 'r1 '(0 r0))
	   else (e-move (e-cvt ssimp)  '(0 r0)))
       (d-clearreg)))

; c-set :: compile a "set" expression				= c-set =


;--- cc-setq :: compile a "setq" expression			= c-setq = 
;
(defun cc-setq nil
  (let (tmp)
       (If (oddp (length (cdr v-form)))
	   then (comp-err "wrong number of args to setq "
			  (or v-form))
	elseif (cdddr v-form)		; if multiple setq's
	   then (do ((ll (cdr v-form) (cddr ll))
		     (g-loc)
		     (g-cc nil))
		    ((null (cddr ll)) (setq tmp ll))
		    (setq g-loc (d-locv (car ll)))
		    (d-exp (cadr ll))
		    (d-clearuse (car ll)))
	else (setq tmp (cdr v-form)))

       ; do final setq
       (let ((g-loc (d-locv (car tmp)))
	     (g-cc (If g-loc then nil else g-cc))
	     (g-ret nil))
	    (d-exp (cadr tmp))
	    (d-clearuse (car tmp)))
       (If g-loc then (d-move (d-locv (car tmp)) g-loc)
		      (If g-cc then (d-handlecc)))))



; cc-typep :: compile a "typep" expression			= cc-typep =
; 
; this returns the type of the expression, it is always non nil
;
(defun cc-typep nil
  (let ((argloc (d-simple (cadr v-form)))
	(g-ret))
       (If (null argloc) then (let ((g-loc 'reg) g-cc)
				   (d-exp (cadr v-form)))
			      (setq argloc 'reg))
       (If g-loc then (e-write4 'ashl '$-9 (e-cvt argloc) 'r0)
	              (e-write3 'cvtbl "_typetable+1[r0]" 'r0)
		      (e-write3 'movl "_tynames+4[r0]" 'r0)
		      (e-write3 'movl "(r0)" (e-cvt g-loc)))
       (If (car g-cc) then (e-goto (car g-cc)))))



; cm-symeval :: compile a symeval expression.
; the symbol cell in franz lisp is just the cdr.
;
(defun cm-symeval nil
  `(cdr ,(cadr v-form)))


; c-*throw :: compile a "*throw" expression			=c-*throw =
;
; the form of *throw is (*throw 'tag 'val) .
; we calculate and stack the value of tag, then calculate val 
; we call Idothrow to do the actual work, and only return if the
; throw failed.
;
(defun c-*throw nil
  (let ((arg2loc (d-simple (caddr v-form)))
	g-cc
	g-ret
	arg1loc)
       ; put on the C runtime stack 
       ;  value to throw,
       ;  tag to throw to
       (If arg2loc then (If (setq arg1loc (d-simple (cadr v-form)))
			    then (e-write2 'pushl (e-cvt arg2loc))
				 (e-write2 'pushl (e-cvt arg1loc))
			    else (let ((g-loc 'reg))
				      (d-exp (cadr v-form))	; calc tag
				      (e-write2 'pushl (e-cvt arg2loc))
				      (e-write2 'pushl (e-cvt 'reg))))
		   else (let ((g-loc 'stack))
			     (d-exp (cadr v-form))	; calc tag to stack
			     (Push g-locs nil)
			     (incr g-loccnt)
			     (setq g-loc 'reg)	
			     (d-exp (caddr v-form))	; calc value into r0
			     (e-write2 'pushl (e-cvt 'reg))
			     (e-write2 'pushl (e-cvt 'unstack))
			     (unpush g-locs)
			     (decr g-loccnt)))
       ; now push the type of non local go we are doing, in this case
       ; it is a C_THROW
       (e-write2 'pushl '#.(concat '$ C_THROW))
       (e-write3 'calls '$3 '_Inonlocalgo) ; never returns
       ))



;--- cm-zerop ::  convert zerop to a quick test			= cm-zerop =
; zerop is only allowed on fixnum and flonum arguments.  In both cases,
; if the value of the first 32 bits is zero, then we have a zero.
; thus we can define it as a macro:
(defun cm-zerop nil
  (cond ((atom (cadr v-form))
	 `(and (null (cdr ,(cadr v-form))) (not (bigp ,(cadr v-form)))))
	(t (let ((gnsy (gensym)))
		`((lambda (,gnsy)
			  (and (null (cdr ,gnsy)) 
				(not (bigp ,gnsy))))
		  ,(cadr v-form))))))

