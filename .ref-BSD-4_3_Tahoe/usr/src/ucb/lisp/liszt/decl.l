(include-if (null (get 'chead 'version)) "../chead.l")
(Liszt-file decl
   "$Header: decl.l,v 1.9 87/12/15 17:00:21 sklower Exp $")

;;; ----	d e c l		declaration handling
;;;
;;;				-[Sat Aug  6 23:58:35 1983 by layer]-


(setq original-readtable readtable)
(setq raw-readtable (makereadtable t))

;--- compile-fcn  :: declare a open coded function
; name - name of the function
; fcnname - function to be funcall'ed to handle the open coding
; indicator -  describes what the fcnname will do, one of
;		fl-expr : will compile the expression and leave the
;			result in r0.  Will ignore g-cc and g-loc
;		fl-exprcc: will compile the expression and leave the
;			result in g-loc.  Will handle g-cc
;		fl-exprm: will just return another form to be d-exp'ed
; args - (optional) description of the arguments to this function.
;	form: (min-args . max-args) .  If max-args is nil, then there is
;		no max.  This is usually done in /usr/lib/lisp/fcninfo.l.
;
(defmacro compile-fcn (name fcnname indicator &optional (args nil args-p))
   `(progn (putprop ',name ',fcnname ',indicator)
	   ;; don't do this here, done in fcn-info
	   ,@(cond (args-p `((putprop ',name (list ',args) 'fcn-info))))))

	   
;--- special handlers
(compile-fcn and	cc-and		fl-exprcc)
(compile-fcn arg  	cc-arg		fl-exprcc)
(compile-fcn assq 	cm-assq		fl-exprm)
(compile-fcn atom 	cc-atom		fl-exprcc)
(compile-fcn bigp	cc-bigp		fl-exprcc)
(compile-fcn bcdcall	c-bcdcall	fl-expr)
(compile-fcn Internal-bcdcall c-Internal-bcdcall fl-expr)
(compile-fcn bcdp	cc-bcdp		fl-exprcc)
#+(or for-vax for-tahoe)
(compile-fcn boole 	c-boole		fl-expr)
(compile-fcn *catch	c-*catch 	fl-expr)
(compile-fcn comment	cc-ignore	fl-exprcc)
(compile-fcn cond	c-cond  	fl-expr)
(compile-fcn cons	c-cons		fl-expr)
(compile-fcn cxr 	cc-cxr		fl-exprcc)
(compile-fcn declare	c-declare	fl-expr)
(compile-fcn do		c-do     	fl-expr)
(compile-fcn liszt-internal-do 	c-do   	fl-expr)
(compile-fcn dtpr	cc-dtpr		fl-exprcc)
(compile-fcn eq		cc-eq    	fl-exprcc)
(compile-fcn equal 	cc-equal	fl-exprcc)
(compile-fcn errset 	c-errset 	fl-expr)
(compile-fcn fixp	cc-fixp		fl-exprcc)
(compile-fcn floatp	cc-floatp	fl-exprcc)
(compile-fcn funcall	c-funcall 	fl-expr)
(compile-fcn function	cc-function 	fl-exprcc)
(compile-fcn get	c-get		fl-expr)
(compile-fcn getaccess  cm-getaccess    fl-exprm)
(compile-fcn getaux	cm-getaux 	fl-exprm)
(compile-fcn getd 	cm-getd 	fl-exprm)
(compile-fcn getdata	cm-getdata 	fl-exprm)
(compile-fcn getdisc	cm-getdisc 	fl-exprm)
(compile-fcn go		c-go	  	fl-expr)
(compile-fcn list	c-list	  	fl-expr)
(compile-fcn map	cm-map	  	fl-exprm)
(compile-fcn mapc	cm-mapc  	fl-exprm)
(compile-fcn mapcan 	cm-mapcan 	fl-exprm)
(compile-fcn mapcar 	cm-mapcar 	fl-exprm)
(compile-fcn mapcon	cm-mapcon 	fl-exprm)
(compile-fcn maplist	cm-maplist 	fl-exprm)
(compile-fcn memq	cc-memq		fl-exprcc)
(compile-fcn ncons	cm-ncons	fl-exprm)
(compile-fcn not	cc-not   	fl-exprcc)
(compile-fcn null	cc-not   	fl-exprcc)
(compile-fcn numberp	cc-numberp	fl-exprcc)
(compile-fcn or		cc-or    	fl-exprcc)
(compile-fcn prog	c-prog   	fl-expr)
(compile-fcn progn	cm-progn 	fl-exprm)
(compile-fcn prog1 	cm-prog1	fl-exprm)
(compile-fcn prog2	cm-prog2 	fl-exprm)
(compile-fcn progv 	c-progv		fl-expr)
(compile-fcn quote	cc-quote 	fl-exprcc)
(compile-fcn return 	c-return 	fl-expr)
(compile-fcn rplaca 	c-rplaca 	fl-expr)
(compile-fcn rplacd 	c-rplacd 	fl-expr)
(compile-fcn rplacx 	c-rplacx 	fl-expr)
(compile-fcn *rplacx 	c-rplacx 	fl-expr)
(compile-fcn setarg 	c-setarg	fl-expr)
(compile-fcn setq	cc-setq  	fl-exprcc)
(compile-fcn stringp 	cc-stringp 	fl-exprcc)
(compile-fcn symbolp 	cc-symbolp	fl-exprcc)
(compile-fcn symeval 	cm-symeval	fl-exprm)
(compile-fcn *throw 	c-*throw 	fl-expr)
(compile-fcn typep   	cc-typep	fl-exprcc)
(compile-fcn vectorp   	cc-vectorp	fl-exprcc)
(compile-fcn vectorip  	cc-vectorip	fl-exprcc)
(compile-fcn vset	cc-vset 	fl-exprcc)
(compile-fcn vseti-byte cc-vseti-byte 	fl-exprcc)
(compile-fcn vseti-word cc-vseti-word 	fl-exprcc)
(compile-fcn vseti-long cc-vseti-long 	fl-exprcc)
(compile-fcn vref	cc-vref 	fl-exprcc)
(compile-fcn vrefi-byte cc-vrefi-byte 	fl-exprcc)
(compile-fcn vrefi-word cc-vrefi-word 	fl-exprcc)
(compile-fcn vrefi-long cc-vrefi-long 	fl-exprcc)
(compile-fcn vsize	c-vsize		fl-expr)
(compile-fcn vsize-byte	c-vsize-byte	fl-expr)
(compile-fcn vsize-word	c-vsize-word	fl-expr)

(compile-fcn zerop   	cm-zerop	fl-exprm)
; functions which expect fixnum operands 


(compile-fcn + c-fixnumop  fl-expr)
#+(or for-vax for-tahoe) (putprop '+ 'addl3 'fixop)
#+for-68k (putprop '+ 'addl 'fixop)

(compile-fcn - c-fixnumop fl-expr)
#+(or for-vax for-tahoe) (putprop '- 'subl3 'fixop)
#+for-68k (putprop '- 'subl 'fixop)

#+(or for-vax for-tahoe)
(progn 'compile
   (compile-fcn * c-fixnumop fl-expr)
   (putprop '* 'mull3 'fixop)

   (compile-fcn / c-fixnumop fl-expr)
   (putprop '/ 'divl3 'fixop))

;-- boole's derivatives
#+for-vax
(progn 'compile
   (compile-fcn fixnum-BitOr c-fixnumop fl-expr)
   (putprop 'fixnum-BitOr 'bisl3 'fixop)

   (compile-fcn fixnum-BitAndNot c-fixnumop fl-expr)
   (putprop 'fixnum-BitAndNot 'bicl3 'fixop)

   (compile-fcn fixnum-BitXor c-fixnumop fl-expr)
   (putprop 'fixnum-BitXor 'xorl3 'fixop))

#+for-tahoe
(progn 'compile
   (compile-fcn fixnum-BitOr c-fixnumop fl-expr)
   (putprop 'fixnum-BitOr 'orl3 'fixop)

   (compile-fcn fixnum-BitAnd c-fixnumop fl-expr)
   (putprop 'fixnum-BitAnd 'andl3 'fixop)

   (compile-fcn fixnum-BitXor c-fixnumop fl-expr)
   (putprop 'fixnum-BitXor 'xorl3 'fixop))

(compile-fcn 1+ 	cc-oneplus  fl-exprcc)
(compile-fcn 1-		cc-oneminus fl-exprcc)

#+(or for-vax for-tahoe)
(compile-fcn \\	c-\\	fl-expr)   ; done in the old way, should be modified

; these have typically fixnum operands, but not always 


; these without the & can be both fixnum or both flonum
;
(compile-fcn 	< 	cm-< 	fl-exprm)
(compile-fcn 	<& 	cc-<& 	fl-exprcc)

(compile-fcn 	> 	cm-> 	fl-exprm)
(compile-fcn 	>& 	cc->& 	fl-exprcc)

(compile-fcn 	= 	cm-=		fl-exprm)
(compile-fcn 	=&	cm-=&		fl-exprm)

; functions which can only be compiled
(compile-fcn assembler-code c-assembler-code fl-expr)
(compile-fcn fixnum-cxr cm-fixnum-cxr fl-exprm)
(compile-fcn internal-fixnum-box c-internal-fixnum-box fl-expr)
(compile-fcn offset-cxr cc-offset-cxr fl-exprcc)
(compile-fcn internal-bind-vars c-internal-bind-vars fl-expr)
(compile-fcn internal-unbind-vars c-internal-unbind-vars fl-expr)

; functions which can be converted to fixnum functions if
; proper declarations are done
(mapc
   '(lambda (arg) (putprop (car arg) (cdr arg) 'if-fixnum-args))
   '((lessp . <&) (greaterp . >&) (= . =&) (equal . =&)))
     

;--- doevalwhen, process evalwhen directive. This is inadequate.
;
(def doevalwhen 
      (lambda (v-f)
	      (prog (docom dolod)
		    (setq docom (memq 'compile (cadr v-f))
			  
			  dolod (memq 'load (cadr v-f)))
		    (mapc '(lambda (frm) (cond (docom (eval frm)))
					 (cond (dolod 
						((lambda (internal-macros) 
							 (liszt-form frm))
						 t))))
			  (cddr v-f)))))


;---- declare - the compiler version of the declare function
;	process the declare forms given. We evaluate each arg
;
(defun liszt-declare fexpr (forms)
   (cond ((status feature complr)
	  (do ((i forms (cdr i)))
	      ((null i))
	      (cond ((and (atom (caar i))
			  (getd (caar i)))
		     (eval (car i))) ; if this is a function
		    (t (comp-warn "Unknown declare attribute: " (car i))))))))

;---> handlers for declare forms
; declaration information for declarations which occur outside of
; functions is stored on the property list for rapid access.
; The indicator to look under is the value of one of the symbols:
;	g-functype, g-vartype, g-bindtype, or g-calltype
;  The value of the property is the declared function, declaration, binding
;	or call type for that variable.
; For local declarations, the information is kept on the g-decls stack.
; It is an assq list, the car of which is the name of the variable or
; function name, the cdr of which is the particular type.  To tell
; whether the particular type is a function type declaration, check the
; property list of the particular type for a 'functype' indicator.
; Likewise, to see if a particular type is a variable declaration, look
; for a 'vartype' indicator on the particular type's property list.
;
(defmacro declare-handler (args name type toplevind)
   `(mapc '(lambda (var)
	      (cond ((symbolp var)
		     (cond (g-compfcn	; if compiling a function
			      (Push g-decls (cons var ',name)))
			   (t          ; if at top level
			      (putprop var ',name ,toplevind))))))
	  ,args))

   
(defun *fexpr fexpr (args)
   (declare-handler args nlambda functype g-functype))

(defun nlambda fexpr (args)
   (declare-handler args nlambda functype g-functype))

(defun *expr fexpr (args)
   (declare-handler args lambda functype g-functype))

(defun lambda fexpr (args)
   (declare-handler args lambda functype g-functype))

(defun *lexpr fexpr (args)
   (declare-handler args lexpr functype g-functype))

(defun special fexpr (args)
   (declare-handler args special bindtype g-bindtype))

(defun unspecial fexpr (args)
   (declare-handler args unspecial bindtype g-bindtype))

(defun fixnum fexpr (args)
   (declare-handler args fixnum vartype g-vartype))

(defun flonum fexpr (args)
   (declare-handler args flonum vartype g-vartype))

(defun notype fexpr (args)
   (declare-handler args notype vartype g-vartype))



;--- special case, this is only allowed at top level.  It will
; be removed when vectors are fully supported
(def macarray 
  (nlambda (v-l)
	   (mapc '(lambda (x)
			  (if (dtpr x)
			      then (putprop (car x) (cdr x) g-arrayspecs)
				   (putprop (car x) 'array  g-functype)
			      else (comp-err "Bad macerror form" x)))
		 v-l)))


(def macros 
  (nlambda (args) (setq macros (car args))))

(def specials
  (nlambda (args) (setq special (car args))))

;--- *args
; form is (declare (*args minargs maxargs))
; this must occur within a function definition or it is an error
;
(def *args
   (nlambda (args)
	    (if (not g-compfcn)
	       then (comp-err
		       " *args declaration not given within a function definition "
		       args))
	    (let (min max)
	       (if (not (= (length args) 2))
		  then (comp-err " *args declaration must have two args: "
				 args))
	       (setq min (car args) max (cadr args))
	       (if (not (and (or (null min) (fixp min))
			     (or (null max) (fixp max))))
		  then (comp-err " *args declaration has illegal values: "
				 args))
	       (setq g-arginfo (cons min max))
	       (putprop g-fname (list g-arginfo) 'fcn-info))))

;--- *arginfo
; designed to be used at top level, but can be used within  function
; form: (declare (*arginfo (append 2 nil) (showstack 0 1)))
;
(def *arginfo
   (nlambda (args)
      (do ((xx args (cdr xx))
	   (name)
	   (min)
	   (max))
	  ((null xx))
	  (if (and (dtpr (car xx))
		   (eq (length (car xx)) 3))
	     then (setq name (caar xx)
			min  (cadar xx)
			max  (caddar xx))
		  (if (not (and (symbolp name)
				(or (null min) (fixp min))
				(or (null max) (fixp max))))
		     then (comp-err " *arginfo, illegal declaration "
				    (car xx))
		     else (putprop name (list (cons min max)) 'fcn-info))))))
						    
   
;--- another top level only.
;
(def localf
  (nlambda (args)
     (mapc '(lambda (ar)
	       (if (null (get ar g-localf))
		  then (putprop ar
				(cons (d-genlab) -1)
				g-localf))
	       (if (get ar g-stdref)
		  then (comp-err
			 "function " ar " is being declared local" N
		       " yet it has already been called in a non local way")))
	   args)))

; g-decls is a stack of forms like
;  ((foo . special) (bar . fixnum) (pp . nlambda))
; there are 4 types of cdr's:
;	function types (lambda, nlambda, lexpr)
;	variable types (fixnum, flonum, notype)
;	call types     (localf, <unspecified>)
;	bind types     (special, unspecial)
;
(mapc '(lambda (x) (putprop x t 'functype)) '(lambda nlambda lexpr))
(mapc '(lambda (x) (putprop x t 'vartype))  '(fixnum flonum notype))
(mapc '(lambda (x) (putprop x t 'calltype)) '(localf))
(mapc '(lambda (x) (putprop x t 'bindtype)) '(special unspecial))

;---> end declare form handlers






;--- d-makespec :: declare a variable to be special
;
(defun d-makespec (vrb)
  (putprop vrb 'special g-bindtype))
