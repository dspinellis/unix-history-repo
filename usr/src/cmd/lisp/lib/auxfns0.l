(setq SCCS-auxfns0 "@(#)auxfns0.l	1.29	7/9/81")
(setsyntax '\; 'splicing 'zapline)
; vi: set lisp :

;---------------- auxfns0 ---------------
; this file contains the definitions of the most common functions.
; It should only be loaded in Opus 30 Franz Lisp.
; These functions should be loaded into every lisp.
;
;------------------------------------------------
; preliminaries:

(eval-when (eval load)
  (cond ((null (getd 'back=quotify)) 
	 (cond ((atom (errset (fasl '/usr/lib/lisp/backquote)))
		(load 'backquote))))))

(eval-when (compile)
  (setq macros t))


;---- Table of Contents.
;
; I.  Functions required to be defined right away
;	declare		append		concatl		max
;	memq
;
; II. Macros
;	catch		throw		defmacro (defmacrosrch,defmcroopption)	
;	defun 		desetq		let (de-compose)
;	let*		listify		sassoc		unwind-protect
;
; III. Interrupt functions
;	FPEINT		INT
;
; IV.	garbage collection functions
;	prtpagesused	gcafter
;
; V.    the functions
;	append1		assoc		bigp
;	comment		copy		copysymbol	cvttomaclisp
;	defprop		delete		delq		evenp
;	ex (exvi)	exec		exl		explode
;	explodec	exploden	expt		ffasl
;	filepos		fixp
;	flatsize	floatp		getchar		getcharn
;	getl		help		hunk		
;	last		include		includef
;	length		macroexpand	makhunk		member		
;	min
;	nconc		nreverse	oddp		plusp
;	prog1
;	reverse		shell		sload		
;	sort (sorthelp,exchange2) 	sortcar	(sortcarhelp)
;	sublis (sublishelp)		subst		vi
;	vil		xcons
;
; VI.   array functions
;	array [macro]	arracfun	*array
;	arraycall [macro] (ev-arraycall)
;	arrcomputeint
;	store [macro] (storeintern)	arracfcnsimp
;	arraydims	fillarray
;
; VII.	equivalences
;	abs		add		chrct		diff
;	numbp		princ		remainder	terpri
;	typep		symeval		
;	<		=		>		-
;	/		+		-		*diff
;	\		1+		1+$		1-
;	1-$		*		*$		/$
;	+$		-$
;



;--- Section I - functions which must be declared immediately

;--- declare - ignore whatever is given, this is for the compiler
;
(def declare (nlambda (x) nil))


;--- append - x : list
;	    - y : list 
;
(declare (localf append2args))

(def append2args 
  (lambda (x y)
	  (prog (l l*)
		(cond ((null x) (return y))
		      ((atom x) (err (list '"Non-list to append:" x))))
		(setq l* (setq l (cons (car x) nil)))
	loop	(cond ((atom x) (err (list '"Non-list to append:" x)))
		      ((setq x (cdr x))
		       (setq l* (cdr (rplacd l* (cons (car x) nil))))
		       (go loop)))
		(rplacd l* y)
		(return l))))

(def append
  (lexpr (nargs)
	 (cond ((eq nargs 2) (append2args (arg 1) (arg 2)))
	       ((zerop nargs) nil)
	       (t (do ((i (1- nargs) (1- i))
		       (res (arg nargs)))
		      ((zerop i) res)
		      (setq res (append2args (arg i) res)))))))



;--- concatl - l : list of atoms
;	returns the list of atoms concatentated
;
(def concatl
 (lambda (x) (apply 'concat x)))


;--- max - arg1 arg2 ... : sequence of numbe
;	returns the maximum
;
(def max
  (lexpr (nargs)
	 (do ((i nargs (1- i))
	      (max (arg 1)))
	     ((< i 2) max)
	     (cond ((greaterp (arg i) max) (setq max (arg i)))))))


;--- memq - arg : (probably a symbol)
;	  - lis : list
; returns part of lis beginning with arg if arg is in lis
;	
(def memq
  (lambda ($a$ $l$)
	  (do ((ll $l$ (cdr ll)))
	      ((null ll) nil)
	      (cond ((eq $a$ (car ll)) (return ll))))))

; ---Section II - macros 
;	

;--- catch form [tag]  
;  catch is now a macro which translates to (*catch 'tag form)
;
(def catch
  (macro (l)
	 `(*catch ',(caddr l) ,(cadr l))))
;--- throw form [tag]
;  throw isnow a macro
;
(def throw
  (macro (l)
	 `(*throw ',(caddr l) ,(cadr l))))


; defmacro for franz, written 20sep79 by jkf

(declare (special defmacrooptlist protect-list protect-evform))

;--- defmacro - name - name of macro being defined
;	      - pattrn - formal arguments plus other fun stuff
;	      - body   - body of the macro
; This is an intellegent macro creator.  The pattern may contain
; symbols which are formal paramters, lists which show how the
; actual paramters will appear in the args, and these key words
;  &rest name  - the rest of the args (or nil if there are no other args)
;		 is bound to name
;  &optional name - bind the next arg to name if it exists, otherwise
;		    bind it to nil
;  &optional (name init) - bind the next arg to name if it exists, otherwise
;		    bind it to init evaluted. (the evaluation is done left
;		    to right for optional forms)
;  &optional (name init given) - bind the next arg to name and given to t
;		    if the arg exists, else bind name to the value of
;		    init and given to nil.
;  &aux name
;  &aux (name init)
;
; Method of operation:
;  the list returned from defmcrosrc has the form ((cxxr name) ...)
;	where cxxr is the loc of the macro arg and name is it formal name
;  defmcrooptlist has the form ((initv cxxr name) ...)
; which is use for &optional args with an initial value.
;  here cxxr looks like cdd..dr which will test of the arg exists.
;
; the variable defmacro-for-compiling determines if the defmacro forms
; will be compiled. If it is t, then we return (progn 'compile (def xx..))
; to insure that it is compiled
;
(declare (special defmacro-for-compiling))
(cond ((null (boundp 'defmacro-for-compiling))   ; insure it has a value
       (setq defmacro-for-compiling nil)))

(def defmacro
  (macro (args)
    ((lambda 
       (tmp tmp2 defmacrooptlist body protect-evform protect-list gutz)
       (setq tmp (defmcrosrch (caddr args) '(d r) nil)
	     body
	     `(def ,(cadr args)
		   (macro (defmacroarg)
		     ((lambda ,(mapcar 'cdr tmp)
			      ,@(mapcar 
				   '(lambda (arg)
				      `(cond ((setq ,(caddr arg)
						    (,(cadr arg) 
						      defmacroarg))
					      ,@(cond ((setq tmp2 (cadddr arg))
						       `((setq ,tmp2 t))))
					      (setq ,(caddr arg)
						    (car ,(caddr arg))))
					     (t (setq ,(caddr arg)
						      ,(car arg)))))
					defmacrooptlist)
			      ,@(cond (protect-evform 
				       (setq gutz 
					     (eval `((lambda ,(mapcar 'cdr tmp)
							     ,@(cdddr args))
						     ,@(mapcar
							'(lambda (x) `',(cdr x))
							tmp))))
				       (ncons 
					`(cond (,protect-evform
						      `((lambda ,',(mapcar 'cdr tmp)
						          ,',gutz)
						        ,,@(mapcar 'cdr tmp)))
					       (t ,@(cdddr args)))))
				      (t (cdddr args))))
		      ,@(mapcar '(lambda (arg) 
					 (cond ((dtpr (car arg))
						(caar arg))
					       ((car arg)
						`(,(car arg) defmacroarg))))
			       tmp)))))
      (cond (defmacro-for-compiling `(progn 'compile ,body))
	    (t body)))

     nil nil nil nil nil nil nil)))

(def defmcrosrch
  (lambda (pat form sofar)
	  (cond ((null pat) sofar)
		((atom pat) (cons (cons (concatl `(c ,@form)) pat)
				  sofar))
		((eq (car pat) '&rest)
		 (append (defmcrosrch (cadr pat) form nil)
		 	 (defmcrosrch (cddr pat) form sofar)))
		((eq (car pat) '&optional)
		 (defmcrooption (cdr pat) form sofar))
		((eq (car pat) '&protect)
		 (setq protect-list (cond ((atom (cadr pat))
					   (ncons (cadr pat)))
					  (t (cadr pat)))
		       protect-evform (cons 'or (mapcar '(lambda (x)
								 `(dtpr ,x))
							protect-list)))
		 (defmcrosrch (cddr pat) form sofar))
		((eq (car pat) '&aux)
		 (mapcar '(lambda (frm)
				  (cond ((atom frm) `((nil) . ,frm))
					(t `((,(cadr frm)) . ,(car frm)))))
			 (cdr pat)))
		(t (append (defmcrosrch (car pat) (cons 'a form) nil)
			   (defmcrosrch (cdr pat) (cons 'd form) sofar))))))

(def defmcrooption
  (lambda (pat form sofar)
    ((lambda (tmp tmp2)
	  (cond ((null pat) sofar)
		((eq (car pat) '&rest)
		 (defmcrosrch (cadr pat) form sofar))
		(t (cond ((atom (car pat))
			  (setq tmp (car pat)))
			 (t (setq tmp (caar pat))
			    (setq defmacrooptlist 
				  `((,(cadar pat) 
				        ,(concatl `(c ,@form))
				        ,tmp
				        ,(setq tmp2 (caddar pat)))
				    . ,defmacrooptlist))))
		   (defmcrooption 
			(cdr pat) 
			(cons 'd form) 
			`( (,(concatl `(ca ,@form)) . ,tmp)
			   ,@(cond (tmp2 `((nil . ,tmp2))))
			  . ,sofar)))))
     nil nil)))



;--- defun - standard maclisp function definition form.
;
(def defun 
       (macro (l)
	     (prog (name type arglist body specind specnam)
		   (setq name (cadr l) l (cddr l))
		   (cond ((dtpr name) 
			  (cond ((memq (cadr name) '(macro expr fexpr lexpr))
				 (setq l (cons (cadr name) l)
				       name (car name)))
				(t (setq specnam (car name)
					 specind (cadr name)
					 name (concat (gensym) "::" specnam))))))
		   (cond ((null (car l)) (setq type 'lambda))
			 ((eq 'fexpr (car l)) (setq type 'nlambda l (cdr l)))
			 ((eq 'expr (car l))  (setq type 'lambda l (cdr l)))
			 ((eq 'macro (car l)) (setq type 'macro l (cdr l)))
			 ((atom (car l)) (setq type 'lexpr 
					       l `((,(car l)) ,@(cdr l))))
			 (t (setq type 'lambda)))
		   (cond ((and (eq 'lambda type) (or (memq '&aux (car l))
						     (memq '&optional (car l))
						     (memq '&rest (car l))))
			  (setq l (lambdacvt l)
				type (car l)
				l (cdr l))))
		   (setq body `(def ,name 
				 (,type ,@l)))
		   (cond (specnam 
			  (return `(progn 'compile
					  ,body
					  (putprop ',specnam 
						   (getd ',name) 
						   ',specind))))
			 (t (return body))))))

	 


;--- lambdacvt
;	converts a lambda expression with &optional, &rest and &aux forms in
;	the argument list into a lexpr which will do the desired function.
; method of operation
; the argument list is examined and the following lists are made:
;  vbs - list of variables to be lambda bound
;  opl - list of optional forms
;  vals - list of values to be assigned to the vbs
;
(def lambdacvt
  (lambda (exp)
	  (prog (arg vbs vals opl rest opflg restflg narg narg2 narg3 auxflg
		 avbs)
		(do ((ll (car exp) (cdr ll))
		     (count 1 (1+ count)))
		    ((null ll))
		    (cond ((eq '&rest (car ll))
			   (setq restflg t opflg nil count (1- count)))
			  ((eq '&optional (car ll))
			   (setq opflg t   count (1- count)))
			  ((eq '&aux (car ll))
			   (setq auxflg t 
				 opflg nil 
				 restflg nil 
				 count (1- count)))
			  (opflg
			   (cond ((atom (setq arg (car ll))) 
				  (setq opl (cons (cons (ncons arg) count) opl)
				        vbs (cons arg vbs)
				        vals (cons nil vals)))
				 ((cddr arg)
				  (setq vbs (cons (car arg) 
						  (cons (caddr arg)
							vbs))
					vals (cons nil
						   (cons nil vals))
					opl (cons (cons arg count) opl)))
				 (t (setq vbs (cons (car arg) vbs)
					  vals (cons nil vals)
					  opl (cons (cons arg count) opl)))))
			  (restflg
			   (setq vbs (cons (car ll) vbs)
				 vals (cons nil vals)
				 rest (cons (car ll) count)))
			  (auxflg
			   (setq count (1- count))
			   (cond ((atom (setq arg (car ll)))
				  (setq avbs (cons (ncons arg) avbs)))
				 (t (setq avbs (cons arg avbs)))))
			  (t (setq vbs (cons (car ll) vbs)
				   vals (cons `(arg ,count) vals)))))
		(setq narg (gensym))
		
		(return
		 `(lexpr (,narg)
		     ((lambda ,(nreverse vbs)
			      ,@(mapcar 
				 '(lambda (arg)
					  `(cond ((greaterp ,(cdr arg)
							    ,narg)
						  ,@(cond ((cadar arg)
							   `((setq ,(caar arg) 
								   ,(cadar arg))))))
						 (t (setq ,(caar arg) (arg ,(cdr arg))) 
						    ,@(cond ((cddar arg)
							     `((setq ,(caddar arg)
								    t)))))))
				 (nreverse opl))
			      ,@(cond (rest (setq narg2 (gensym)
						  narg3 (gensym))
					    `((do ((,narg2 ,narg (1- ,narg2))
						  (,narg3 nil (cons (arg ,narg2)
								    ,narg3)))
						 ((lessp ,narg2 ,(cdr rest))
						  (setq ,(car rest) ,narg3))))))
			      ,@(cond (auxflg `((let* ,(nreverse avbs)
						      ,@(cdr exp))))
				      (t (cdr exp))))
		      ,@(nreverse vals)))))))

;--- desetq
;	- pattern - pattern containing vrbl names
;	- expr 	  - expression to be evaluated
;
(defmacro desetq (&rest forms &aux newgen destrs)
  (do ((xx forms (cddr xx))
       (res)
       (patt)
       (expr))
      ((null xx) (cond ((null (cdr res)) (car res))
		       (t (cons 'progn (nreverse res)))))
      (setq patt (car xx) expr (cadr xx))
      (setq res 
	    (cons (cond ((atom patt) `(setq ,patt ,expr))	;trivial case
			(t (setq newgen (gensym)
				 destrs (de-compose patt '(r)))
			   `((lambda (,newgen)
				     ,@(mapcar '(lambda (frm)
							`(setq  ,(cdr frm) 
								(,(car frm) ,newgen)))
					       destrs))
			     ,expr)))
		  res))))

; let for franz (with destructuring)
;--- let
;	- binds - binding forms
;	- . body - forms to execute
; the binding forms may have these forms
;   a	local variable a, initially nil
;  (a x)  local variable a, x is evaled and a gets its value initially
;  ((a . (b . c)) x)   three local variables, a,b and c which are given
;			values corresponding to the location in the value
;		        of x.  Any structure is allowed here. 
;
(defmacro let (binds &rest body &aux vrbls vals destrs newgen)
  (mapc '(lambda (form)
		(cond ((atom form)
		       (setq vrbls (cons form vrbls)
			     vals  (cons nil vals)))
		      ((atom (car form))
		       (setq vrbls (cons (car form) vrbls)
			     vals  (cons (cadr form) vals)))
		      (t (setq newgen (gensym)
			       destrs `((,newgen ,@(de-compose (car form) '(r)))
					,@destrs)
			       vrbls  (cons newgen vrbls)
			       vals   (cons (cadr form) vals)))))
       binds)

  (mapc '(lambda (frm)
		(do ((ll (cdr frm) (cdr ll)))
		    ((null ll))
		    (setq vrbls (cons (cdar ll) vrbls)
			  vals  (cons nil vals))))
       destrs)

  (setq vals (nreverse vals)
	vrbls (nreverse vrbls)
	destrs (nreverse destrs))
  `((lambda ,vrbls
	    ,@(mapcan '(lambda (frm)
			       (mapcar '(lambda (vrb)
						`(setq ,(cdr vrb) (,(car vrb)
								  ,(car frm))))
				       (cdr frm)))
		      destrs)
	    ,@body)
    ,@vals))

;--- de-compose
;		form - pattern to de-compose
;		sofar - the sequence of cxxr's needed to get to this part
;			of the pattern
;  de-compose returns a list of this form
;
;	((cxxr . a) (cyyr . b) ... )
; which tells how to get to the value for a and b ..etc..
;
(def de-compose 
  (lambda (form sofar)
	  (cond ((null form ) nil)
		((atom form) (ncons (cons (apply 'concat (cons 'c sofar))
					  form)))
		(t (nconc (de-compose (car form) (cons 'a sofar))
			  (de-compose (cdr form) (cons 'd sofar)))))))


;--- let*
;	- binds  - binding forms (like let)
;	- body   - forms to eval (like let)
; this is the same as let, except forms are done in a left to right manner
; in fact, all we do is generate nested lets
;
(defmacro let* (binds &rest body)
  (do ((ll (reverse binds) (cdr ll)))
      ((null ll) (car body))
      (setq body `((let (,(car ll)) ,@body)))))


		   
;--- listify : n  - integer
;	returns a list of the first n args to the enclosing lexpr if
; n is positive, else returns the last -n args to the lexpr if n is
; negative.
;
(def listify 
  (macro (lis)
	 `(let ((n ,(cadr lis)))
	       (cond ((minusp n)
		      (do ((i (arg nil)  (1- i))
			   (result nil (cons (arg i) result)))
			  ((< i (+ (arg nil) n  1)) result) ))
		     (t (do ((i n  (1- i))
			     (result nil (cons (arg i) result)))
			    ((< i 1) result) ))))))
;--- sassoc
;	- x : form
;	- y : assoc list
;	- fcn : function or lambda expression
; If (assoc x y) is non nil, then we apply the function fcn to nil.  
; This must be written as a macro if we expect to handle the case of
; a lambda expression as fcn in the compiler.  
;
(defmacro sassoc (x y fcn)
  (cond ((or (atom fcn) (not (eq 'quote (car fcn))))
	 `(or (assoc ,x ,y)
	      (apply ,fcn nil)))
	(t `(or (assoc ,x ,y)
		(,(cadr fcn) nil)))))

;--- sassq
;	- x : form
;	- y : assoc list
;	- fcn : function or lambda expression
; like sassoc above except it uses assq instead of assoc.
;
(defmacro sassq (x y fcn)
  (cond ((or (atom fcn) (not (eq 'quote (car fcn))))
	 `(or (assq ,x ,y)
	      (apply ,fcn nil)))
	(t `(or (assq ,x ,y)
		(,(cadr fcn) nil)))))

		    

;--- signp - test - unevaluated atom
;	   - value - evaluated value
; test can be l, le, e, n, ge or g   with the obvious meaning
; we return t if value compares to 0 by test
'(def signp
  (macro (l)
	 `(signphelpfcn ',(cadr l) ,(caddr l))))

(defmacro signp (tst val)
  (setq tst  (cond ((eq 'l tst)  `(minusp signp-arg))
		   ((eq 'le tst) `(not (greaterp signp-arg 0)))
		   ((eq 'e tst)  `(zerop signp-arg))
		   ((eq 'n tst)  `(not (zerop signp-arg)))
		   ((eq 'ge tst) `(not (minusp signp-arg)))
		   ((eq 'g tst)  `(greaterp signp-arg 0))
		   (t (error "bad arg to signp " tst))))
  (cond ((atom val) `(and (numberp ,val) ,(subst val 'signp-arg tst)))
	(t `((lambda (signp-arg) (and (numberp signp-arg) ,tst))
	     ,val))))
;-- signphelpfcn
; not needed for new code anymore
(def signphelpfcn
  (lambda (tst val)
	  (cond ((eq 'l tst) (minusp val))
		((eq 'le tst) (or (zerop val) (minusp val)))
		((eq 'e tst) (zerop val))
		((eq 'n tst) (not (zerop val)))
		((eq 'ge tst) (not (minusp val)))
		((eq 'g tst) (greaterp val 0)))))


;--- unwind-protect
;  The form of a call to unwind-protect is
;   (unwind-protect pform
;		    form1 form2 ...)
; and it works as follows:
;  pform is evaluated, if nothing unusual happens, form1 form2 etc are
;	then evaluated and unwind-protect returns the value of pform.
;  if while evaluating pform, a throw or error caught by an errset which
;   would cause control to pass through the unwind-protect, then
;   form1 form2 etc are evaluated and then the error or throw continues.
; Thus, no matter what happens, form1, form2 etc will be evaluated.
;
(defmacro unwind-protect (protected &rest conseq &aux (localv (gensym 'G)))
  `((lambda (,localv)
	    (setq ,localv (*catch 'ER%unwind-protect ,protected))
	    ,@conseq
	    (cond ((and (dtpr ,localv) (eq 'ER%unwind-protect (car ,localv)))
		   (I-throw-err (cdr ,localv)))
		  (t ,localv)))
    nil))





;----Section III -- Interrupt handlers 
; 

(def FPEINT 
      (lambda (x$) (error "Floating Exception ")))

(def INT 
      (lambda (dummy) (patom '"Interrupt:  ") (drain) (break)))


(signal 8 'FPEINT)
(signal 2 'INT)


;---- Section IV - interrupt handlers
;
(cond ((null (boundp '$gcprint))
       (setq $gcprint nil)))		; dont print gc stats by default

(cond ((null (boundp '$gccount$))
       (setq $gccount$ 0)))

;--- prtpagesused - [arg] : type of page allocated last time.
;	prints a summary of pages used for certain selected types
;	of pages.  If arg is given we put a star beside that type
;	of page.  This is normally called after a gc.
;
(def prtpagesused
  (lambda (space tottime gctime)
	  (patom "[")
	  (do ((curtypl (cond ((memq space '(list fixnum ))
			       '(list fixnum))
			      (t (cons space '(list fixnum))))
			(cdr curtypl))
	       (temp))
	      ((null curtypl) (print 'ut:)
	       (print (max 0 (quotient (times 100 (diff tottime gctime))
				       tottime)))
	       (patom "%]") (terpr))
	      (setq temp (car curtypl))
	      (cond ((greaterp (cadr (opval temp)) 0)
		     (cond ((eq space temp)
			    (patom '*)))
		     (patom temp)
		     (patom '":")
		     (print (cadr (opval temp)))
		     (patom '"{")
		     (print (fix (quotient 
				  (times 100.0
					 (car (opval temp)))
				  (* (cadr (opval temp))
					 (caddr (opval temp))))))
		     (patom '"%}")
		     (patom '"; "))))))

(declare (special gcafter-panic-mode $gccount$ $gc_midlim $gc_minalloc 
		  $gc_pct $gc_lowlim $gcprint ptimeatlastgc))

(setq gcafter-panic-mode nil)
(setq $gc_minalloc 10)
(setq $gc_lowlim 60)
(setq $gc_midlim 85)
(setq $gc_pct    .10)
(setq ptimeatlastgc (ptime))

;--- gcafter - [s] : type of item which ran out forcing garbage collection.
;	This is called after each gc.
; the form of an opval element is  (number_of_items_in_use
;				    number_of_pages_allocated
;				    number_of_items_per_page)
;
;
(def gcafter 
  (nlambda (s)
	   (prog (x pct amt-to-allocate thisptime diffptime difftottime
		    diffgctime)
		 (cond ((null s) (return)))  
		 (cond ((null (boundp '$gccount$)) (setq $gccount$ 0)))
		 (setq $gccount$ (1+ $gccount$))
		 (setq x (opval (car s)))
		 (setq thisptime (ptime)
		       difftottime (max  (diff (car thisptime) 
					 (car ptimeatlastgc)) 
					1)
		       diffgctime (diff (cadr thisptime)
					(cadr ptimeatlastgc))
		       ptimeatlastgc thisptime)
		 ; pct is the percentage of space used
		 (setq pct (quotient (times 100 (car x))
				     (max 1 (times (cadr x) (caddr x)))))
		 (setq amt-to-allocate
		       (cond (gcafter-panic-mode 
			      (cond ((greaterp pct 95) 
				     (patom "[Storage space totally exausted]")
				     (terpr)
				     (error "Space exausted when allocating "
					    (car s)))
				    (t 0)))
			     ((greaterp pct $gc_midlim)
			      (max $gc_minalloc (fix (times $gc_pct (cadr x)))))
			     ((greaterp pct $gc_lowlim)
			      $gc_minalloc)
			     ((lessp (cadr x) 100)
			      $gc_minalloc)
			     (t 0)))
		 (cond ((and (null gcafter-panic-mode) (greaterp amt-to-allocate
								 0))
			(cond ((atom (errset (allocate (car s) amt-to-allocate)))
			       (cond ($gcprint 
					(patom "[Now in storage allocation panic mode]")
					(terpr)))
			       (setq gcafter-panic-mode t)))))

		 (cond ($gcprint (prtpagesused (car s) difftottime diffgctime)
				 (comment (cond ((and (getd 'gcstat)
					     (eq $gcprint '$all))
					(print (gcstat))
					(terpr)))))))))

;----Section V - the functions
; 

;--- append - x : list
;	    - y : list 
; [ must appear at the beginning to allow backquote to work ]



;--- append1 - x : list
;	     - y : lispval
;	puts y at the end of list x
;
(def append1 (lambda (x y) (append x (list y))))


;--- assoc - x : lispval
;	   - l : list
;	l is a list of lists. The list is examined and the first
;	sublist whose car equals x is returned.
;
(def assoc
  (lambda (val alist)
	  (do ((al alist (cdr al)))
	      ((null al) nil)
	      (cond ((null (car al)))
		    ((not (dtpr (car al)))
		     (error "bad arg to assoc" al))
		    ((equal val (caar al)) (return (car al)))))))



;--- bigp - x : lispval
;	returns t if x is a bignum
;
(def bigp (lambda (arg) (equal (type arg) 'bignum)))

;--- comment - any
; 	ignores the rest of the things in the list
(def comment
  (nlambda (x) 'comment))

;--- concatl - l : list of atoms
;	returns the list of atoms concatentated
; [ must appear at top to allow defmacro to work ]



;--- copy - l : list (will work if atom but will have no effect)
;	makes a copy of the list.
; FIX THIS UP TO REMOVE RECURSION ON TAIL
;
(def copy 
  (lambda (l)
	  (cond ((atom l) l)
		(t (cons (copy (car l)) (copy (cdr l)))))))


;--- copysymbol - sym : symbol to copy
;		- flag : t or nil
;  generates an uninterned symbol with the same name as sym.  If flag is t
; then the value, function binding and property list of sym are placed
; in the uninterned symbol.
;
(def copysymbol 
  (lambda (sym flag)
	  ((lambda (newsym)
		   (cond (flag (cond ((boundp sym) (set newsym (eval sym))))
			       (putd newsym (getd sym))
			       (setplist newsym (plist sym))))

		   newsym)
	   (uconcat sym))))


;--- cvttointlisp -- convert reader syntax to conform to interlisp
;
(def cvttointlisp
  (lambda nil
	  (setsyntax '\% 143.)		; escape character
	  (setsyntax '\\ 2.)		; normal character
	  (setsyntax '\` 2.)		; normal character
	  (setsyntax '\, 2.)		; normal character
	  (sstatus uctolc t)		; one case
	  ))


;--- cvttomaclisp - converts the readtable to a maclisp character syntax
;
(def cvttomaclisp
  (lambda nil
	  (setsyntax '\| 138.)		; double quoting char
	  (setsyntax '\/ 143.)		; escape
	  (setsyntax '\\ 2)		; normal char
    ; not needed in new lisp   (setsyntax '\" 2)		; normal char
	  (setsyntax '\[ 2)		; normal char
	  (setsyntax '\] 2)		; normal char
	  (sstatus uctolc t)))


;--- cvttoucilisp - converts the readtable to a ucilisp character syntax
;
(def cvttoucilisp
  (lambda nil
	  (sstatus uctolc t)		; upper case to lower case
  					; change backquote character.
					; to ` and ! and !@ from ` , and ,@
  					; undo comma.
	  (cond ((eq (status syntax \,) 205)
		 (setsyntax '\! 'splicing (get '\, 'macro))))
	  (setsyntax '\, 2)
	  	; 
  		; ~ as comment character, not ; and / instead of \ for escape
  	  (setsyntax '\~ 'splicing 'zapline)
	  (setsyntax '\; 2)
	  (setsyntax '\/ 143)
	  (setsyntax '\\   2)))


;--- defprop - like putprop except args are not evaled
;
(def defprop 
    (nlambda (argl)
	(putprop (car argl) (cadr argl) (caddr argl) )))

;--- delete
;	- val - lispval
;	- lst - list
;	- n   - Optional arg, number of occurances to delete
; removes up to n occurances of val from the top level of lst.
; if n is not given, all occurances will be removed.
;
(def delete
  (lexpr (nargs)
	 (prog (val lst cur ret nmb)
	       (cond ((< nargs 2) (error " too few args to delete " nargs))
		     ((= nargs 3) 
		      (setq nmb (arg 3))
		      (cond ((not (greaterp nmb 0)) (return (arg 2))))))
	       (setq val (arg 1) lst (arg 2))
	       (cond ((and (atom lst) (not (null lst))) 	
		      (error " non list arg to delete " lst)))
	       (setq cur (cons nil lst)
		     ret cur)
	   loop
	       (cond ((null lst) (return (cdr ret)))
		     ((equal val (car lst))
		      (rplacd cur (cdr lst))
		      (cond ((and nmb (zerop (setq nmb (1- nmb))))
			     (return (cdr ret)))))
		     (t (setq cur (cdr cur))))
	       (setq lst (cdr lst))
	       (go loop))))


;--- delq 
;  same as delete except eq is used for testing.
;
(def delq
  (lexpr (nargs)
	 (prog (val lst cur ret nmb)
	       (cond ((< nargs 2) (error " too few args to delete " nargs))
		     ((= nargs 3) 
		      (setq nmb (arg 3))
		      (cond ((not (greaterp nmb 0)) (return (arg 2))))))
	       (setq val (arg 1) lst (arg 2))
	       (cond ((and (atom lst) (not (null lst))) 	
		      (error " non list arg to delete " lst)))
	       (setq cur (cons nil lst)
		     ret cur)
	   loop
	       (cond ((null lst) (return (cdr ret)))
		     ((eq val (car lst))
		      (rplacd cur (cdr lst))
		      (cond ((and nmb (zerop (setq nmb (1- nmb))))
			     (return (cdr ret)))))
		     (t (setq cur (cdr cur))))
	       (setq lst (cdr lst))
	       (go loop))))

;--- delete - val - s-expression
;	    - list - list to delete fromm
;	    -[n] optional count , if not specified, it is infinity
; delete removes every thing in the top level of list which equals val
; the list structure is modified
;
;(def delete
;  (lexpr (nargs)
;	 ((lambda (val list n)
;		  (cond ((or (null list) (zerop n)) list)
;			((atom list) (error "Bad arg to delete " list))
;			((equal val (car list)) 
;			 (delete val (cdr list) (1- n)))
;			(t (rplacd list (delete val (cdr list) n)))))
;	  (arg 1) 
;	  (arg 2) 
;	  (cond ((equal nargs 3) (arg 3))
;		(t 99999999)))))
;
;
;--- delq   - val - s-expression
;	    - list - list to delete fromm
;	    -[n] optional count , if not specified, it is infinity
; delq removes every thing in the top level of list which eq's val
; the list structure is modified
;
;(def delq
;  (lexpr (nargs)
;	 ((lambda (val list n)
;		  (cond ((or (atom list) (zerop n)) list)
;			((eq val (car list)) 
;			 (delq val (cdr list) (1- n)))
;			(t (rplacd list (delq val (cdr list) n)))))
;	  (arg 1) 
;	  (arg 2) 
;	  (cond ((equal nargs 3) (arg 3))
;		(t -1)))))
;
;--- evenp : num   -  return 
;
;
(def evenp
  (lambda (n)
	  (cond ((not (zerop (boole 4 1 n))) t))))

;--- ex [name] : unevaluated name of file to edit.
;	the ex editor is forked to edit the given file, if no
;	name is given the previous name is used
;
(def ex (nlambda (x) (exvi 'ex x nil)))

(declare (special edit_file))

(def exvi 
  (lambda (cmd x doload) 
	   (prog (handy handyport bigname)
		 (cond ((null x) (setq x (list edit_file)))
		       (t (setq edit_file (car x))))		 
		 (setq bigname (concat (car x) '".l"))
		 (cond ((setq handyport (car (errset (infile bigname) nil)))
			(close handyport)
			(setq handy bigname))
		       (t (setq handy (car x))))
		 (setq handy (concat cmd '" " handy))
		 (setq handy (list 'process handy))
		 (eval handy)
		 (cond (doload (load edit_file))))))

;--- exec - arg1 [arg2 [arg3 ...] ] unevaluated atoms
;	A string of all the args concatenated together seperated by 
;	blanks is forked as a process.
;
(def exec
 (nlambda ($list)
   (prog ($handy)
         (setq $handy (quote ""))
    loop (cond ((null $list)
                (return (eval (list (quote process) $handy))))
               (t (setq $handy
                        (concat (concat $handy (car $list))
                                (quote " ")))
                  (setq $list (cdr $list))
                  (go loop))))))


;--- exl - [name] : unevaluated name of file to edit and load.
;	If name is not given the last file edited will be used.
;	After the file is edited it will be `load'ed into lisp.
;
(def exl (nlambda (x) (exvi 'ex x t)))

;----- explode functions -------
; These functions, explode , explodec and exploden, implement the 
; maclisp explode functions completely.
; They have a similar structure and are written with efficiency, not
; beauty in mind (and as a result they are quite ugly)
; The basic idea in all of them is to keep a pointer to the last
; thing added to the list, and rplacd the last cons cell of it each time.
;
;--- explode - arg : lispval
;	explode returns a list of characters which print would use to
; print out arg.  Slashification is included.
;
(def explode
  (lambda (arg)
	  (cond ((atom arg) (aexplode arg))
		(t (do ((ll (cdr arg) (cdr ll))
			(sofar (setq arg (cons '"(" (explode (car arg)))))
			(xx))
		       ((cond ((null ll) (rplacd (last sofar) (ncons '")" )) 
			       t)
			      ((atom ll) (rplacd (last sofar)
						 `(" " "." " " ,@(explode ll) 
						     ,@(ncons '")")))
			       t))
			arg)
		       (setq xx (last sofar)
			     sofar (cons '" " (explode (car ll))))
		       (rplacd xx sofar))))))

;--- explodec - arg : lispval
; returns the list of character which would be use to print arg assuming that
; patom were used to print all atoms.
; that is, no slashification would be used.
;
(def explodec
  (lambda (arg)
	  (cond ((atom arg) (aexplodec arg))
		(t (do ((ll (cdr arg) (cdr ll))
			(sofar (setq arg (cons '"(" (explodec (car arg)))))
			(xx))
		       ((cond ((null ll) (rplacd (last sofar) (ncons '")" )) 
			       t)
			      ((atom ll) (rplacd (last sofar)
						 `(" " "." " " ,@(explodec ll) 
						     ,@(ncons '")")))
			       t))
			arg)
		       (setq xx (last sofar)
			     sofar (cons '" " (explodec (car ll))))
		       (rplacd xx sofar))))))

;--- exploden - arg : lispval
;	returns a list just like explodec, except we return fixnums instead
; of characters.
;
(def exploden
  (lambda (arg)
	  (cond ((atom arg) (aexploden arg))
		(t (do ((ll (cdr arg) (cdr ll))
			(sofar (setq arg (cons 40. (exploden (car arg)))))
			(xx))
		       ((cond ((null ll) (rplacd (last sofar) (ncons 41.)) 
			       t)
			      ((atom ll) (rplacd (last sofar)
						 `(32. 46. 32. ,@(exploden ll) 
						     ,@(ncons 41.)))
			       t))
			arg)
		       (setq xx (last sofar)
			     sofar (cons 32. (exploden (car ll))))
		       (rplacd xx sofar))))))

;-- expt  - x
;	  - y
;
;	   y
; returns x
;
(defun expt (x y)
  (cond ((equal x 1) x)
	((zerop x) x)   ; Maclisp does this 
	((lessp y 0) (quotient 1.0 (expt x (times -1 y))))
	((floatp y) 
	 (exp (times y (log x)))) ; bomb out for (-3)^4 or (-3)^4.0 or 0^y.
	(t ; y is integer, y>= 0
	   (prog (res)
		 (setq res 1)
		 loop
		 (cond ((equal y 0) (return res))
		       ((oddp y)(setq res (times  res x) y (1- y)))
		       (t (setq x (times x x) y (/ y 2))))
		 (go loop)))))


;--- expt
; old
'(defun expt(x y)
  (prog (res)
	(setq res 1)
 loop	(cond ((equal y 0) (return res))
	      (t (setq res (times x res)
		       y (1- y))))
	(go loop)))

;--- ffasl :: fasl in a fortran file
;  arg #
;   1	- fnam : file name
;   2	- entry : entry point name
;   3	- fcn  : entry name
;   4   - disc : optional discipline

(defun ffasl (fnam entry fcn &optional (disc 'subroutine))
  (cfasl fnam entry fcn disc "-lI77 -lF77 -lm"))


;
; filepos function (maclisp compatibility)
;
(defun filepos n
  (cond ((zerop n) nil)
	((onep n)
	 (fseek (arg 1) 0 1))
	((equal n 2)
	 (fseek (arg 1) (arg 2) 0))))

;--- fixp - l : lispval
;	returns t if l is a fixnum or bignum
;
(defun fixp (x) (or (equal (type x) 'fixnum)
		    (equal (type x) 'bignum)))



;--- flatsize - l : lispval
;	     the second arg should be:
;	      - n : limit for what we care about
;	     but we dont care about this at present, since we have
;	     to explode the whole thing anyway.
;	returns the number of characters which print would
;	use to print l
;
(defun flatsize n
  (length (explode (arg 1))))


;--- floatp - l : lispval
;	returns t if l is a flonum
;
(defun floatp (x) (equal 'flonum (type x)))


;--- getchar,getcharn   - x : atom
;	     		- n : fixnum
; returns the n'th character of x's pname (the first corresponds to n=1)
; if n is negative then it counts from the end of the pname
; if n is out of bounds, nil is returned

(def getchar
  (lambda (x n)
	  (concat (substring x n 1))))


(def getcharn
  (lambda (x n)
	  (substringn x n 0)))


(def getl 
  (lambda (atm lis)
	  (do ((ll (cond ((atom atm) (plist atm))
			 (t (cdr atm)))
		   (cddr ll)))
	      ((null ll) nil)
	      (cond ((memq (car ll) lis) (return ll))))))


;--- help
; retrive selected portions of the Franz Lisp manual.
; There are four types of help offered:
; (help) prints a description of the other three options
; (help tc) prints a table of contents.
; (help n) {where n is a number or b or c} prints the whole chapter.
; (help fcn) prints info on function fcn
;
; An index to the functions is kept in the documentation directory.
; The index has entries like (append ch2.r).  
; When asked to print info on a function, it locates the chapter
; using the index then asks more to locate the definition.
;
(declare (localf locatefunction))

(defun help fexpr (lis)
  (cond ((null lis) 
 (patom "type (help fnc) for info on function fnc")(terpr)
 (patom "type (help n) to see chapter n")(terpr)
 (patom "type (help tc) for a table of contents")(terpr))
	(t (do ((ll lis (cdr ll))
		(fcn))
	       ((null ll))
	       (cond ((not (atom (setq fcn (car ll))))
		      (patom "Bad option to help ")(print fcn)(terpr))
		     ((and (stringp fcn) (setq fcn (concat fcn)) nil))
		     ((eq fcn 'tc)
		      (patom "Table of contents")(terpr)
 (patom "1 - intro; 2 - data structure; 3 - arithmetic; 4 - special")(terpr)
 (patom "5 - i/o; 6 - system; 7 - reader; 8 - functions; 9 - arrays")(terpr)
 (patom "10 - exceptions; 11 - trace package; 12 - Liszt;")(terpr)
 (patom "14 - step package; 15 - fixit package") (terpr)
 (patom "b - special symbols; c - gc & debugging & top level ")(terpr))
		     ((or (and (numberp fcn) (lessp fcn 16) (greaterp fcn -1))
			  (memq fcn '(b c)))
		      (apply 'process 
			 (ncons (concat "/usr/ucb/ul /usr/lib/lisp/manual/ch"
				       fcn ".r | /usr/ucb/more -f" ))))
		     ((locatefunction fcn))
		     (t (patom "Unknown function: ")(print fcn)(terpr)))))))

(declare (special readtable))

(defun locatefunction (fc)
  (let (x inf )
       (cond ((null (get 'append 'helplocation)) 
	      (patom "[Reading help index]")(drain)
	      (setq inf (infile "/usr/lib/lisp/manual/helpindex"))
	      (do ((readtable (makereadtable t))
		   (x (read inf) (read inf)))
		  ((null x) (close inf) (terpr))
		  (cond ((null (cddr x))
			 (putprop (car x) (cadr x) 'helplocation))
			(t (putprop (concat (car x) " " (cadr x))
				    (caddr x)
				    'helplocation))))))
       (cond ((setq x (get fc 'helplocation))
	      (apply 'process (ncons (concat "/usr/ucb/ul /usr/lib/lisp/manual/"
					     x 
					     " | /usr/ucb/more -f \"+/(" 
					     fc 
					     "\"")))
	      t))))

;
; (hunk 'g_arg1 [...'g_argn])
;
; This function makes a hunk. The hunk is preinitialized to the
; arguments present. The size of the hunk is determined by the
; number of arguments present.
;

(defun hunk n
  (prog (size)
	(setq size -1)
	(cond ((> n 128) (error "hunk: size is too big" n))
	      ((eq n 1) (setq size 0))
	      ((eq n 0) (return nil))	; hunk of zero length
	      (t (setq size (1- (haulong (1- n))))))
	(setq size (*makhunk size))
	(do
	 ((argnum 0 (1+ argnum)))
	 ((eq argnum n))
	 (*rplacx argnum size (arg (1+ argnum))))
	(return size)))


;--- last - l : list
;	returns the last cons cell of the list, NOT the last element
;
(def last 
  (lambda (a)
	  (do ((ll a (cdr ll)))
	      ((null (cdr ll))  ll))))

;---- load 

(declare (localf load-a-file))
(declare (special gcdisable))

(defun load (filename &rest fasl-args)
  (cond ((not (or (symbolp filename) (stringp filename))) 
	 (error "load: illegal filename " filename)))
  (let ( load-only fasl-only no-ext len search-path name pred shortname explf)
       
       ; determine the length of the filename, ignoring the possible
       ; list of directories.  set explf to the reversed exploded filename
       (setq len (do ((xx (setq explf (nreverse (exploden filename))) (cdr xx))
		      (i 0 (1+ i)))
		     ((null xx) i)
		     (cond ((eq #// (car xx)) (return i)))))

       (cond ((> len 2)
	      (cond ((eq (cadr explf) #/.)
		     (cond ((eq (car explf) #/o)
			    (setq fasl-only t))
			   ((eq (car explf) #/l)
			    (setq load-only t))
			   (t (setq no-ext t))))
		    (t (setq no-ext t))))
	     (t (setq no-ext t)))

       ; a short name is less or equal 12 characters.  If a name is not
       ; short, then load will not try to append .l or .o
       (cond ((< len 12) (setq shortname t)))

       (cond ((and (> len 0) (eq (getchar filename 1) '/))
	      (setq search-path '(||)))
	     (t (setq search-path (status load-search-path))))
       (do ((xx search-path (cdr xx)))
	   ((null xx) (error "load: file not found " filename))
	   (setq pred (cond ((memq (car xx) '(|| |.|)) '||)
			    (t (concat (car xx) "/"))))
	   (cond (no-ext
		  (cond ((and shortname
			      (probef (setq name (concat pred filename ".o"))))
			 (return (fasl name (car fasl-args)
				       (cadr fasl-args))))
			((and shortname
			      (probef (setq name (concat pred filename ".l"))))
			 (return (load-a-file name)))
			((probef (setq name (concat pred filename)))
			 (cond (fasl-args (return (fasl name (car fasl-args)
							(cadr fasl-args))))
			       (t (return (load-a-file name)))))))
		 (fasl-only
		  (cond ((probef (setq name (concat  pred  filename)))
			 (return (fasl name (car fasl-args)
				       (cadr fasl-args))))))
		 (load-only
		  (cond ((probef (setq name (concat pred filename)))
			 (return (load-a-file name)))))))))

(defun load-a-file (name)
  (let ((piport (infile name))
	(gcdisable t)		; don't gc when loading, it slows things down
	(eof (list nil)))
       (do ((form (read piport eof) (read piport eof)))
	   ((eq eof form) (close piport) t)
	   (eval form))))

(sstatus load-search-path (|.| /usr/lib/lisp))
;--- include - read in the file name given, the name not evaluated
;
(def include (nlambda (l) (load (car l))))

;--- includef - read in the file name given and eval the first arg
;
(def includef (lambda (l) (load l)))

;--- length - l : list
;	returns the number of elements in the list.
;
(def length 
  (lambda ($l$)
	  (cond ((null $l$) 0)
		(t (do ((ll (cdr $l$)  (cdr ll))
			(i 1 (1+ i)))
		       ((null ll) i))))))

;--- list-to-bignum
;  convert a list of fixnums to a bignum.
; there is a function bignum-to-list but it is written in C
;
;(author: kls)
;
(def list-to-bignum
 (lambda (x) (cond (x (scons (car x) (list-to-bignum (cdr x))))
		   (t nil))))



;--- macroexpand - form 
;	expands out all macros it can
;
(def macroexpand
  (lambda (form)
    (prog nil
  top (cond ((atom form) (return form))
	    ((atom (car form))
	     (return
	      (let ((nam (car form)) def disc)
		   (setq def (getd nam))
		   (setq disc (cond ((bcdp def) (getdisc def))
				    ((arrayp def) 'array)
				    ((dtpr def) (car def))))
		   (cond ((memq disc '(array lambda lexpr nil))
			  (cons nam (mapcar 'macroexpand (cdr form))))
			 ((eq disc 'macro)
			  (setq form (apply nam form))
			  (go top))
			 (t form)))))
	    (t (return (cons (macroexpand (car form))
			     (mapcar 'macroexpand (cdr form)))))))))




;
; (makhunk 'n)
;
; This function is similar to hunk, except that:
;
; n can be a fixnum, which specifies the length of the hunk.
;	The hunk is preinitialized to nil's
; n can be a list which is used to preinitialize the hunk.
;
(defun makhunk (n)
  (prog (size Hunk)
	(setq size -1)
	(cond ((numberp n)
;
; If n is a number then build a nil hunk of the right size
;
	       (cond ((greaterp n 128) (error "makhunk: size is too big" n))
		     (t (setq size (1- (haulong n)))))
	       (cond ((minusp size) (return nil)))
	       (setq Hunk (*makhunk size))
	       (do ((i 0 (1+ i)))
		   ((= i n))
		   (*rplacx i Hunk nil))
	       (return Hunk))
;
; If it isn't a number, then try hunk on it
;
	      (t (return (apply 'hunk n))))))

;--- member - VAL : lispval
;	    - LIS : list
;	returns that portion of LIS beginning with the first occurance
;	of VAL  if  VAL is found at the top level of list LIS.
;	uses equal for comparisons.
;
(def member 
  (lambda ($a$ $l$)
	  (do ((ll $l$ (cdr ll)))
	      ((null ll) nil)
	      (cond ((equal $a$ (car ll)) (return ll))))))

;--- memq - arg : (probably a symbol)
;	  - lis : list
; returns part of lis beginning with arg if arg is in lis
;	
; [ defintion moved to top of file to allow backquote macro to work ]

;--- min - arg1 ... numbers 
;
; 	returns minimum of n numbers. 
;

(def min
  (lexpr (nargs)
	 (do ((i nargs (1- i))
	      (min (arg 1)))
	     ((lessp i 2) min)
	     (cond ((lessp (arg i) min) (setq min (arg i)))))))

;--- nconc - x1 x2 ...: lists
;	The cdr of the last cons cell of xi is set to xi+1.  This is the
;	structure modification version of append
;

(def nconc 
  (lexpr (nargs) 
	 (cond ((eq nargs '2) 
		(cond ((null (arg 1)) (arg 2))
		      (t (do ((tmp (arg 1) (cdr tmp)))
			     ((null (cdr tmp)) 
			      (rplacd tmp (arg 2))
			      (arg 1))))))
	       ((zerop nargs) nil)
	       (t (do ((i 1 nxt)
		       (nxt 2 (1+ nxt))
		       (res (cons nil (arg 1)))) 
		      ((eq i nargs) (cdr res))
		      (cond ((arg i) (rplacd (last (arg i)) (arg nxt)))
			    (t (rplacd (last res) (arg nxt)))))))))


(declare (localf nreverse1))	; quick fcn shared by nreverse and nreconc

;--- nreconc :: nreverse and nconc
; (nreconc list elemt) is equiv to (nconc (nreverse list) element)
;
(defun nreconc (list element)
  (cond ((null list) element)
	(t (nreverse1 list element))))

;--- nreverse - l : list
;	reverse the list in place
;

(defun nreverse (x)
  (cond ((null x) x)
	(t (nreverse1 x nil))))


;--- nreverse1
;  common local function to nreconc and nreverse.  [This can just be
; nreconc when I get local global functions allow in the compiler -jkf]
;
(defun nreverse1 (x ele)
  (prog (nxt)
  loop
	(setq nxt (cdr x))
	(rplacd x ele)
	(setq ele x)
	(cond (nxt (setq x nxt) (go loop)))
	(return x)))

;
(def oddp
  (lambda (n)
	  (cond ((not (zerop (boole 1 1 n))) t))))

;--- plusp : x - number
; returns t iff x is greater than zero

(def plusp
  (lambda (x)
	  (greaterp x 0)))


;--- princ : l - any s-expression
;	    [p] - port to write to
; prints using patom for atoms (unslashified)
;
(def princ
  (lexpr (n)
	 (prog (port val)
	       (cond ((eq n 2) (setq port (arg 2))))
	       (cond ((dtpr (setq val (arg 1)))
		      (cond ((and (eq 'quote (car val))
				  (dtpr (cdr val))
				  (null (cddr val)))
			     (patom "'" port)
			     (princ (cadr val) port))
			    (t 
			     (patom "(" port)
			     (do ((xx val))
				 ((null xx) (patom ")" port))
				 (princ (car xx) port)
				 (cond ((null (setq xx (cdr xx))))
				       ((not (dtpr xx))
					(patom " . " port)
					(princ xx port)
					(setq xx nil))
				       (t (patom " " port)))))))
		     (t (patom val port)))
	       (return t))))

;--- prog1 : return the first value computed in a list of forms
;
(def prog1
  (lexpr (n)
	 (arg 1)))

;--- reverse : l - list
;	returns the list reversed using cons to create new list cells.
;
(def reverse 
  (lambda (x)
	  (cond ((null x) nil)
		(t (do ((cur (cons (car x) nil) 
			     (cons (car res) cur))
			(res (cdr x) (cdr res)))
		       ((null res) cur))))))

;--- shell - invoke a new c shell
;
(def shell 
  (lambda nil 
	  ((lambda (shellname)
		   (cond ((lessp (flatc shellname) 1) (setq shellname 'csh)))
		   (apply 'process (ncons shellname)))
	   (getenv 'SHELL))))


;--- sload : fn - file name (must include the .l)
;	loads in the file printing each result as it is seen
;
(def sload
  (lambda (fn)
	  (prog (por eof)
		(cond ((setq por (infile fn)))
		      (t (patom "bad file name")(terpr)(return nil)))
		(setq eof (gensym))
		(do ((x (read por eof) (read por eof)))
		    ((eq eof x) (close por))
		    (cond ((dtpr x) (print (cadr x)))
			  (t (print x)))
		    (terpr)
		    (eval x)))))

; these is the old sort functions
(comment
(defun sort(a fun)
  (prog (n)
	(cond	((null a) (return nil)) ;no elements
		(t
		 (setq n (length a))
		 (do i 1  (1+ i) (greaterp i n)(sorthelp a fun))
		 (return a) ))))

(defun sorthelp (a fun)
  (do ((ii a (cdr ii)))
      ((null (cdr ii)))
      (cond ((funcall fun (cadr ii) (car ii))  
	     (exchange2 ii)))))

)

;---- bubble merge sort 
; it recursively splits the list to sort until the list is small.  At that
; point it uses a bubble sort.  Finally the sorted lists are merged.

(declare (special sort-function))

;--- sort :: sort a lisp list
; args: lst - list of items
;       fcn - function to compare two items.
; returns: the list with such that for each pair of adjacent elements,
;	   either the elements are equal, or fcn applied to the two 
; 	   args returns a non nil value.
;
(defun sort (lst fcn)
  (setq sort-function fcn)		; store function name in global cell
  ; (setq sort-compares 0)		; count number of comparisons
  (sortmerge lst (length lst)))


;--- sortmerge :: utility routine to sort
; args: lst - list of items to sort
;	nitems - a rough idea of how many items are in the list
;
; result - sorted list (see the result of sort above)
;
(defun sortmerge (lst nitems)
  (prog (tmp tmp2)
	(cond ((greaterp nitems 7)
	       ; do a split and merge
	       (setq tmp (splitlist lst (setq tmp2 (quotient nitems 2))))
	       (return (mergelists (sortmerge (car tmp) tmp2)
			      (sortmerge (cdr tmp) tmp2))))
	      (t ; do a bubble sort
		 (do ((l lst (cdr l))
		      (fin))
		     ((null l))
		     (do ((ll lst (cdr ll)))
			 ((eq fin (cdr ll)) (setq fin ll))
			 ;(setq sort-compares (1+ sort-compares))
			 (cond ((not (funcall sort-function (car ll) (cadr ll)))
				(rplaca ll (prog1 (cadr ll)
						 (rplaca (cdr ll)
							 (car ll))))))))
		 (return lst)))))

;--- splitlist :: utility routine to split a list
; args : lst - list to split
;        spliton - number of items to put in the first list
;
; returns: a cons cell whose car is the first part of the list
;	   and whose cdr is the second part.
;
(defun splitlist (lst spliton)
  (prog (second)
	(do ((i spliton (sub1 i))
	     (l lst))
	    ((or (null (cdr l)) (zerop i))
	     (setq second (cdr l))
	     (rplacd l nil))
	    (setq l (cdr l)))
	(return (cons lst second))))


;--- mergelists ::utility routine to merge two lists based on predicate function
; args: ls1 - lisp list
; 	ls2 - lisp list
;	sort-function (global) - compares items of the lists
;
; returns: a sorted list containing the elements of the two lists.
; 
(defun mergelists  (ls1 ls2)
  (prog (result current)
	; initialize
	(setq current (setq result (cons nil nil)))
loop	(cond ((null ls1)
	       (rplacd current ls2)
	       (return (cdr result)))
	      ((null ls2)
	       (rplacd current ls1)
	       (return (cdr result)))
	      ((funcall sort-function (car ls1) (car ls2))
	       ;(setq sort-compares (1+ sort-compares))
	       (rplacd current ls1)
	       (setq current ls1
		     ls1 (cdr ls1)))
	      (t ;(setq sort-compares (1+ sort-compares))
		 (rplacd current ls2)
		 (setq current ls2
		       ls2 (cdr ls2))))
	(go loop)))

;--- end bubble merge sort


(defun sortcar (a fun)
  (prog (n)
	(cond	((null a) (return nil)) ;no elements
		(t
		 (setq n (length a))
		 (do i 1  (1+ i) (greaterp i n)(sortcarhelp a fun))
		 (return a) ))))

(defun sortcarhelp (a fun)
  (cond ((null (cdr a)) a)
        ((funcall fun (caadr a) (caar a))  
	 (exchange2 a)
	 (sortcarhelp (cdr a) fun))
	(t (sortcarhelp (cdr a) fun))))


(defun exchange2 (a)
  (prog (temp)
	(setq temp (cadr a))
	(rplaca (cdr a) (car a))
	(rplaca a temp)))

;--- sublis: alst - assoc list ((a . val) (b . val2) ...)
;	     exp  - s-expression
; for each atom in exp which corresponds to a key in alst, the associated
; value from alst is substituted.  The substitution is done by adding
; list cells, no struture mangling is done.  Only the minimum number
; of list cells will be created.
;
(def sublis
  (lambda (alst exp)
     (let (tmp)
	  (cond ((atom exp) 
		 (cond ((setq tmp (assoc exp alst))
			(cdr tmp))
		       (t exp)))
		((setq tmp (sublishelp alst exp))
		 (car tmp))
		(t exp)))))

;--- sublishelp : alst - assoc list
;		  exp  - s-expression
; this function helps sublis work.  it is different from sublis in that
; it return nil if no change need be made to exp, or returns a list of
; one element which is the changed exp.
;
(def sublishelp
  (lambda (alst exp)
     (let (carp cdrp)
	  (cond ((atom exp)
		 (cond ((setq carp (assoc exp alst))
			(list (cdr carp)))
		       (t nil)))
		(t (setq carp (sublishelp alst (car exp))
			 cdrp (sublishelp alst (cdr exp)))
		   (cond ((not (or carp cdrp)) nil)		; no change
			 ((and carp (not cdrp))			; car change
			  (list (cons (car carp) (cdr exp))))	
			 ((and (not carp) cdrp)			; cdr change
			  (list (cons (car exp) (car cdrp))))	
			 (t					; both change 
			  (list (cons (car carp) (car cdrp))))))))))


;--- subst : new - sexp
;	     old - sexp
;	     pat - sexp
; substitutes in patrn all occurrences equal to old with new and returns the
; result
; MUST be put in the manual

(declare (special new old))

(def subst 
  (lambda (new old pat)
	  (cond ((symbolp old) (substeq pat))
		(t (substequal pat)))))

;use this function for substituting for symbols
(def substeq
  (lambda (pat)
	  (cond ((eq old pat) new)
		((atom pat) pat)
		(t (cons (substeq (car pat))(substeq (cdr pat)))))))

(def substequal
  (lambda (pat)
	  (cond ((equal old pat) new)
		((atom pat) pat)
		(t (cons (substequal (car pat))
			 ; in interlisp, the next line would be
			 ;(substeq (cdr pat))
			 ; for maclisp compatibility, we do this.
			 (substequal (cdr pat)))))))

(declare (unspecial new old))
;--- vi: arg is unevaluated name of function to run vi on
;
(def vi (nlambda (x) (exvi 'vi x nil)))

;--- vil : arg is unevaluated, edits file and then loads it
;
(def vil (nlambda (x) (exvi 'vi x t)))

;--- xcons : a - sexp
;	     b - sexp
; returns (b . a)   that is, it is an exchanged cons
;
(def xcons  (lambda (a b) (cons b a)))


;--- Section VI -  ARRAY functions .
;
(def array
  (macro ($lis$)
	 `(*array ',(cadr $lis$) ',(caddr $lis$) ,@(cdddr $lis$))))



;--OLD array stuff.  new stuff to follow
; array access function 
;
;  (def arracfun
;       (lambda (actlst ardisc)
;	       (prog (diml ind val)
;		     
;		     (setq actlst (mapcar 'eval actlst)
;			   diml   (getaux ardisc))
;		     
;		     (cond ((null (equal (length actlst)
;					 (length (cdr diml))))
;			    (break '"Wrong number of indexes to array ref"))
;			   
;			   (t (setq ind (arrcomputeind (cdr actlst)
;						       (cddr diml)
;						       (car actlst))
;				    val (arrayref ardisc ind))
;			      (cond ((or (eq (car diml) t)
;					 (eq (car diml) 'unmarked_array))
;				     (setq val (eval val)))
;				    (t (setq val (cpy1 val))))
;			      (return val))))))
;
;
;
;
;(def *array
;  (lexpr (nargs)
;	   (prog (name type rtype dims size tname)
;
;		 (setq name  (arg 1)
;		       type  (arg 2)
;		       rtype (cond ((null type) (setq type 'unmarked_array)
;				    'value)
;				   ((equal type t)
;				    'value)
;				   (t type))
;		       dims  (do ((i 3 (1+ i))
;				  (res nil (cons (arg i) res)))
;				 ((greaterp i nargs) (nreverse res)))
;		       size  (apply 'times dims))
;
;		 (setq tname (marray (segment rtype size)
;				     (getd 'arracfun)
;				     (cons type dims)
;				     size
;				     (sizeof rtype)))
;		 (cond (name (set name tname)
;			     (putd name tname)))
;		 '(fillarray tname (cond ((eq type 'fixnum) '(0))
;					((eq type 'flonum) '(0.0))
;					(t '(nil))))
;		 (return tname))))
;
;(defmacro arraycall (type name &rest indexs)
;  `(ev-arraycall ',type ,name (list ,@ indexs)))
;
;
;(def ev-arraycall
;  (lambda (type ardisc indexs)
;	  ; form (arraycall type name sub1 sub2 ... subn)
;	  (cond ((or (not (equal (car (getaux ardisc)) type))
;		     (and (eq (car (getaux ardisc)) 'unmarked_array)
;			        type))
;
;		 (patom '" Type given arraycall:")
;		 (patom type)
;		 (patom '" doesn't match array type:")
;		 (patom (car (getaux ardisc)))
;		 (break nil)))
;	  (apply (getaccess ardisc) 
;		 (list indexs ardisc))))
;				
;		       
;			
;
;; function to compute the raw array index
;
;(def arrcomputeind
;  (lambda (indl diml res)
;	  (cond ((null diml) res)
;		(t (arrcomputeind (cdr indl)
;				  (cdr diml)
;				  (+ (* res (car diml))
;					(car indl)))))))
;
;; store  
;;  we make store a macro to insure that all parts are evaluated at the
;; right time even if it is compiled.
;;  (store (foo 34 i) (plus r f))
;; gets translated to
;;  (storeintern foo (plus r f) (list 34 i))
;; and storeintern is a lambda, so when foo is evaluated it will pass the
;;	array descriptor to storeintern, so storeintern can look at the
;;	aux part to determine the type of array.
;;
;(defmacro store ( (arrname . indexes) value)
; (prog nil
;
;  loop
;   (cond 
;	((eq 'funcall arrname) 
;	 (setq arrname `(getd ,(car indexes))
;	       indexes (cdr indexes)))
;	((eq 'arraycall arrname)
;	 (setq arrname (cadr indexes)
;	       indexes (cddr indexes)))
;	((and ((lambda (fnd)
;		       (cond ((or (and (dtpr fnd) (eq 'macro (car fnd)))
;				  (and (bcdp fnd) (eq 'macro (getdisc fnd))))
;			      (setq fnd (apply arrname (cons arrname indexes)))
;			      (setq arrname (car fnd)
;				    indexes (cdr fnd)))))
;	       (getd arrname))
;	      (go loop)))
;        (t (setq arrname `(getd ',arrname))))
;  (return `(storeintern ,arrname ,value (list ,@indexes)))))
;
;;-- storeintern -  internal store function
;;	- arrnam : array descriptor
;;	- vl     : value to store
;;	- actlst : list of actual variable values
;;
;(def storeintern
;  (lambda (arrnam vl actlst)
;	   (prog (loc)
;		 (cond ((or (eq t (car (getaux arrnam)))
;			    (eq 'unmarked_array (car (getaux arrnam))))
;			(setq loc (arracfcnsimp actlst arrnam))
;			(set loc vl))
;
;		       (t (replace (arracfcnsimp actlst arrnam) vl)))
;		 (return vl))))
;
;
;(def arracfcnsimp
;  (lambda (indexes adisc)
;	  (prog (dims)
;		(setq dims (cdr (getaux adisc)))
;		(cond ((null (equal (length indexes)
;				    (length dims)))
;		       (break '"wrong number of indexes to array"))
;		      (t (setq dims (arrcomputeind (cdr indexes)
;						   (cdr dims)
;						   (car indexes)))))
;		(return (arrayref adisc dims)))))
;
;


;; new array package			(jkf 9/24/80)
; features of the new package:
; Most array will be notype arrays.  This is because they are the most 
; efficient in Franz.  What used to be fixnum and flonums arrays are
; now fixnum-block and flonum-block arrays.
; The array access functions are more specialized and much faster now.
; The array access functions have different semantics.  Now they are
; responsible for both accessing and storing in an array.
; When an access function is asked to access a value, it will be given
; the subscripts already evaluated and the array object.  These will
; be stacked, so the array access function should be a lexpr to read them.
; When an access function is asked to store a value that value will be
; the first argument, the subscripts will follow and finally there will
; be the array object.  
; It is up to the access function to determine if it is being asked to
; store or retrieve a value, and this determination will probably
; be made by looking at the number of arguments.

(declare (special gcdisable))

(def *array
  (lexpr (nargs)
	   (prog (name type rtype dims size tname numdims)

		 (cond ((lessp (setq numdims (- nargs 2)) 1)
			(error "no bounds to array declaration ")))

		 (setq name  (arg 1)
		       type  (arg 2)
		       rtype (cond ((memq type '(t nil fixnum flonum))
				    'value)
				   ((eq type 'fixnum-block)
				    'fixnum)
				   ((eq type 'flonum-block)
				    'flonum)
				   (t (error "array: bad type: " type)))
		       dims  (do ((i nargs (1- i))
				  (res nil (cons (arg i) res)))
				 ((eq i 2) res))

		       size  (apply 'times dims))

		 (cond ((null type) (setq type 'unmarked_array)))

		 ; we disable gc during the next calculation since
		 ; the data returned from small-segment is unprotected
		 ; and a gc would cause its data to be put on the 
		 ; free list.
		 (let ((gcdisable t))
		      (setq tname
			    (marray (small-segment rtype size)
				    (cond ((eq rtype 'value)
					   (cond ((eq numdims 1) 
						  (getd 'arrac-oneD))
						 ((eq numdims 2) 
						  (getd 'arrac-twoD))
						 (t (getd 'arrac-nD))))
					  (t (getd 'arrac-nD)))
				    (cons type dims)
				    size
				    (sizeof rtype))))
		 ; if type is fixnum or flonum
		 ; we must intialize to 0 or 0.0
		 (cond ((or (and (eq 'fixnum type)
				 (setq rtype 0))
			    (and (eq 'flonum type)
				 (setq rtype 0.0)))
			(do ((i size))
			    ((zerop i))
			    (set (arrayref tname (setq i (1- i))) rtype))))

		 (cond (name (putd name tname)))
		 (return tname))))

(defmacro arraycall (type array &rest indexes)
  `(funcall ,array ,@indexes))

; this is used by the old array scheme.  Keep this around until
; everything is recompiled

(defun ev-arraycall (type array indexes)
  (apply array indexes))


;;;---- array access functions.

; we first define a macro to evaluate a value cell.  In compiled code cdr
; is the fastest way to do this, in interpreted code the type checker 
; would not let us use cdr so we have to use eval.
(eval-when (compile)
  (defmacro value-eval (x) `(cdr ,x)))		; one level of indirection

(eval-when (eval)
  (defun value-eval (x) (eval x)))

;- one dimensional
(defun arrac-oneD n
  (cond ((eq n 2) (value-eval (arrayref (arg 2) (arg 1))))
	((eq n 3) (set (arrayref (arg 3) (arg 2)) (arg 1)))
	(t (error " wrong number of subscripts to array: " (arg n)))))

(defun arrac-twoD n
  (let ((aux (getaux (arg n))))
       (cond ((eq n 3)
	      (value-eval (arrayref (arg n) (+ (* (arg 1) (caddr aux)) (arg 2)))))
	     ((eq n 4)
	      (set (arrayref (arg n) (+ (* (arg 2) (caddr aux)) (arg 3))) 
		   (arg 1)))
	     (t (error " wrong number of subscripts to array: " (arg n))))))

;-- n dimensional array access function.  
(defun arrac-nD n
  (let ((aux (getaux (arg n)))
	firstsub subs
	store
	(index 0))

       (setq subs (length (cdr aux)))
       (cond ((eq n (1+ subs))
	      (setq firstsub 1))
	     ((eq n (+ 2 subs))
	      (setq firstsub 2 store t))
	     (t (error "wrong number of subscripts to array: " (arg n))))

       (setq index (arg firstsub))
       (do ((bounds (cddr aux) (cdr bounds))
	    (i firstsub))
	   ((null bounds))
	   (setq index (+ (* index (car bounds)) (arg (setq i (1+ i))))))

       (setq subs (arrayref (arg n) index))	; get cell requested
       (cond ((memq (car aux) '(fixnum-block flonum-block))
	      (cond (store (replace subs (arg 1)))
		    (t (cpy1 subs))))
	     (t (cond (store (set subs (arg 1)))
		      (t (value-eval subs)))))))


(defmacro store ( (arrname . indexes) value)
 (prog (defered)
  loop
   (cond 
	((eq 'funcall arrname) 
	 (setq arrname (car indexes)
	       indexes (cdr indexes)
	       defered t))
	((eq 'arraycall arrname)
	 (setq arrname (cadr indexes)
	       indexes (cddr indexes)
	       defered t))
	((arrayp arrname)
	 (setq arrname `',arrname
	       defered t))
	((and ((lambda (fnd)
		       (cond ((or (and (dtpr fnd) (eq 'macro (car fnd)))
				  (and (bcdp fnd) (eq 'macro (getdisc fnd))))
			      (setq fnd (apply arrname (cons arrname indexes)))
			      (setq arrname (car fnd)
				    indexes (cdr fnd)))))
	       (getd arrname))
	      (go loop))))
  (return (cond (defered `(funcall ,arrname ,value . ,indexes))
		(t `(,arrname ,value . ,indexes))))))

;-- storeintern  -  there may be residual calls to storeintern from 
; old code, we handle it here.  this routine can be eliminated when
; code is recompiled

(defun storeintern (arrnam value indexes)
  (apply arrnam (cons value indexes)))

;--- small segment storage allocators.

; this function allocates segments of storage and attempt to use the whole
; block instead of throwing away what isnt used
;

(declare (special gcdisable))

(defun small-segment (type n)
  (prog (lastseg retv elementsize itemsperpage-1 gcdisable tmp)
	(setq gcdisable t) 	; its not a good idea to gc while we are
				; handling pointers to things segment returns.
	(desetq (elementsize . itemsperpage-1) (get 'segment-sizes type))
	(cond ((null elementsize) (error "small-segment: bad type " type)))
	(setq lastseg (get 'segment-types type))
	(cond ((and lastseg (not (lessp (car lastseg) n))))
	      (t ; must allocate a block of storage, want to the least number of
		 ; pages which includes n elements
		 ; there are elementsize elements per page, and 
		 ; itemsperpage-1 is the number of elements on a page minus 1 
		 (setq retv (boole 4 
				   (+ n itemsperpage-1) 
				   itemsperpage-1))  ; 4 is x & ~y
		 (setq lastseg (cons retv (maknum (segment type retv))))))
	(setq retv (cdr lastseg))
	(rplaca lastseg (- (car lastseg) n))
	(rplacd lastseg (+ (cdr lastseg) (* elementsize n)))
	(cond ((greaterp (car lastseg) 0)
	       (putprop 'segment-types lastseg type)
	       (cond ((null (setq tmp (get 'segment-arrays type)))
		      (putprop 'segment-arrays 
			       (setq tmp (marray nil nil nil nil nil))
			       type)))
	       (putdata tmp (fake (cdr lastseg)))
	       (putlength tmp (car lastseg))
	       (putdelta tmp elementsize))
	      (t  ; remove all counters since we no longer have any space
		  ; left and we can't have a zero length array
		  (remprop 'segment-types type)
		  (remprop 'segment-arrays type)))
	(return (fake retv))))

; data base for small-segment
(putprop 'segment-sizes '( 4 . 127) 'value)
(putprop 'segment-sizes '( 4 . 127) 'fixnum)
(putprop 'segment-sizes '( 8 . 63)  'flonum)


(def arraydims (lambda (arg) (cond ((symbolp arg) (getaux (getd arg)))
				   ((arrayp arg) (getaux arg))
				   (t (break '"non array arg to arraydims")))))

; fill array from list or array

(def fillarray
  (lambda (arr lis)
	  (prog (maxv typept)
		(cond ((symbolp arr) (setq arr (getd arr))))

		(cond ((symbolp lis)
		       (setq lis (getd lis))
		       (return (fillarrayarray arr lis)))

		      ((arrayp lis) (return (fillarrayarray arr lis))))

		(setq maxv (1- (getlength arr))
		      typept (cond ((memq (car (getaux arr))
					  '(t fixnum flonum unmarked_array))
				    t)
				   (t nil)))
		(do ((ls lis)
		     (i 0 (1+ i)))
		    ((greaterp i maxv))

		    (cond (typept (set (arrayref arr i) (car ls)))
			  (t (replace (arrayref arr i) (car ls))))

		    (cond ((cdr ls) (setq ls (cdr ls))))))))

(def fillarrayarray
  (lambda (arrto arrfrom)
	  (prog (maxv)
		(setq maxv (1- (min (getlength arrto)
				      (getlength arrfrom))))
		(do ((i 0 (1+ i)))
		    ((greaterp i maxv))
		    (replace (arrayref arrto i) (arrayref arrfrom i))))))

(def listarray
  (lexpr (n)
	 (prog (arr size typ ret newv)
	       (setq arr (arg 1))
	       (cond ((and (symbolp arr) (arrayp (setq arr (getd arr)))))
		     (t (error "Non array to listarray " arr)))
	       (setq size (cond ((eq n 2) (arg 2))
				(t (apply '* (cdr (arraydims arr))))))
	       (setq typ (car (getaux arr)))
	       (cond ((memq typ '(t fixnum flonum unmarked_array))
		      (setq typ t))
		     (t (setq typ nil)))
	       (do ((i (1- size) (1- i)))
		   ((lessp i 0))
		   (setq newv (arrayref arr i))
		   (setq ret (cons (cond (typ (eval newv))
					 (t (cpy1 newv)))
				   ret)))
	       (return ret))))


;---Section VII - equivalances-------------------
; 

(putd 'abs (getd 'absval))
(putd 'add (getd 'sum))
(putd 'bcdcall (getd 'funcall))
(putd 'chrct (getd 'charcnt))
(putd 'diff (getd 'difference))
(putd 'numbp  (getd 'numberp))
(putd 'remainder (getd 'mod))
(putd 'terpri (getd 'terpr))
(putd 'typep (getd 'type))
(putd 'symeval (getd 'eval))
(putd '< (getd 'lessp))
(putd '= (getd 'equal))
(putd '> (getd 'greaterp))
(putd '*dif (getd 'difference))
(putd '\\ (getd 'mod)) 
(putd '\1+ (getd 'add1))
(putd '\1+$ (getd 'add1))
(putd '\1- (getd 'sub1))
(putd '\1-$ (getd 'sub1))
(putd '*$ (getd 'times))
(putd '/$ (getd 'quotient))
(putd '+$ (getd 'add))
(putd '-$ (getd 'difference))

