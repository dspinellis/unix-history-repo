(setq rcs-macros-
   "$Header: macros.l.l 1.2 83/06/09 01:42:51 jkf Exp $")

;; macros.l				-[Wed May 25 12:07:00 1983 by jkf]-
;;
;;  The file contains the common macros for Franz lisp.
;; contents:
;;	defmacro
;;	let
;;	let*
;;	caseq
;;	listify
;;	include-if
;;	includef-if
;;	defvar


(declare (macros t))

;; defmacro
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
		((memq (car pat) '(&rest &body))
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
		((memq (car pat) '(&rest &body))
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




;--- lambdacvt	:: new lambda converter.
;
; - input is  a lambda body beginning with the argument list.
;
; vrbls   :: list of (name n) where n is the arg number for name
; optlist :: list of (name n defval pred) where optional variable name is
;	     (arg n) [if it exists], initval is the value if it doesn't
;	     exist,  pred is set to non nil if the arg exists
; auxlist :: list of (name initial-value) for auxillary variables. (&aux)
; restform :: (name n) where args n to #args should be consed and assigned
;		to name.
;
;; strategy:
;  Until the compiler can compiler lexprs better, we try to avoid creating
; a lexpr.  A lexpr is only required if &optional or &rest forms
; appear.
;   Formal parameters which come after &aux are bound and evaluated in a let*
; surrounding the body.  The parameter after a &rest is put in the let*
; too, with an init form which is a complex do loop.  The parameters
; after &optional are put in the lambda expression just below the lexpr.
;
(defun lambdacvt (exp)
   (prog (vrbls optlist auxlist restform vbl fl-type optcode mainvar
	  minargs maxargs)
      (do ((reallist (car exp) (cdr reallist))
	   (count 1 (1+ count)))
	  ((null reallist))
	  (setq vbl (car reallist))
	  (cond ((memq vbl '(&rest &body))
		 (setq fl-type '&rest count (1- count)))
		((eq '&aux vbl)
		 (setq fl-type '&aux count (1- count)))
		((eq '&optional vbl)
		 (setq fl-type '&optional count (1- count)))
		((null fl-type)		 ; just a variable
		 (setq vrbls (cons (list vbl count) vrbls)))
		((eq fl-type '&rest)
		 (cond (restform (error "Too many &rest parameters " vbl)))
		 (setq restform (list vbl count)))
		((eq fl-type '&aux)
		 (cond ((atom vbl)
			(setq auxlist (cons (list vbl nil) auxlist)))
		       (t (setq auxlist (cons (list (car vbl) (cadr vbl))
					      auxlist)))))
		((eq fl-type '&optional)
		 (cond ((atom vbl)
			(setq optlist
			      (cons (list vbl count) optlist)))
		       (t (setq optlist
				(cons (cons (car vbl)
					    (cons count
						  (cdr vbl)))
				      optlist)))))))

      ;; arguments are collected in reverse order, but set them straight
      (setq vrbls (nreverse vrbls)
	    optlist (nreverse optlist)
	    auxlist (nreverse auxlist)
	    minargs (length vrbls)
	    maxargs (cond (restform nil)
			  (t (+ (length optlist) minargs))))

      ;; we must covert to a lexpr if there are &optional or &rest forms
      (cond ((or optlist restform) (setq mainvar (gensym))))
      
      ; generate optionals code
      (cond (optlist
	       (setq optcode
		     (mapcar '(lambda (x)
				 `(cond ((> ,(cadr x) ,mainvar)
					 (setq ,(car x) ,(caddr x)))
					(t (setq ,(car x)
						  (arg ,(cadr x)))
					   ,(cond ((cdddr x)
						   `(setq ,(cadddr x) t))))))
			     optlist))))

      ;; do the rest forms
      (cond (restform
	       (let ((dumind (gensym))
		     (dumcol (gensym)))
		  (setq restform
			`((,(car restform)
			    (do ((,dumind ,mainvar (1- ,dumind))
				 (,dumcol nil (cons (arg ,dumind) ,dumcol)))
				((< ,dumind ,(cadr restform)) ,dumcol))))))))
      
      ;; calculate body
      (let (body)
	 (setq body (cond ((or auxlist restform)
			     `((let* ,(append restform auxlist)
				  ,@(cdr exp))))
			  (t (cdr exp))))
	 (cond ((null mainvar)		; no &optional or &rest
		(return `(lambda ,(mapcar 'car vrbls)
			    (declare (*args ,minargs ,maxargs))
			    ,@body)))
	       (t (return
		     `(lexpr (,mainvar)
			 (declare (*args ,minargs ,maxargs))
			 ((lambda
			     ,(nconc
				 (mapcar 'car vrbls)
				 (mapcan '(lambda (x) 	; may be two vrbls
					     (cons (car x)
						   (cond ((cdddr x) ;pred?
							  (ncons
							     (cadddr x))))))
					 optlist))
			     ,@optcode ,@body)
			  ,@(nconc (mapcar '(lambda (x) `(arg ,(cadr x)))
					   vrbls)
				   (mapcan '(lambda (x)
					       (cond ((cdddr x)
						      (list nil nil))
						     (t (list nil))))
					   optlist))))))))))

;--- defcmacro :: like defmacro but result ends up under cmacro ind
;
(def defcmacro
   (macro (args)
	(let ((name (concat (cadr args) "::cmacro:" (gensym))))
	   `(eval-when (compile load eval)
		    (defmacro ,name ,@(cddr args))
		    (putprop ',(cadr args) (getd ',name) 'cmacro)
		    (remob ',name)))))

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

;--- caseq
; use is 
;    (caseq expr
;	    (match1 do1)
;	    (match2 do2)
;	    (t  doifallelsefails))
; the matchi can be atoms in which case an 'eq' test is done, or they
; can be lists in which case a 'memq' test is done.
;

(defmacro caseq (switch &body clauses &aux var code)
   (setq var (cond ((symbolp switch) switch) ((gensym 'Z))))
   (setq code
	 `(cond . ,(mapcar '(lambda (clause)
			       (cons
				  (let ((test (car clause)))
				     (cond ((eq test t) t)
					   ((dtpr test)
					    `(memq ,var ',test))
					   (t `(eq ,var ',test))))
				  (cdr clause)))
			   clauses)))
   (cond ((symbolp switch) code)
	 (`((lambda (,var) ,code) ,switch))))

;--- selectq :: just like caseq
; except 'otherwise' is recogized as equivalent to 't' as a key
;
(defmacro selectq (key . forms)
	  (setq forms
		(mapcar '(lambda (form) (if (eq (car form) 'otherwise)
					     (cons t (cdr form)) form))
			forms))
	  `(caseq ,key . ,forms))

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
; returns nil if n is 0
;
(def listify 
  (macro (lis)
	 `(let ((n ,(cadr lis)))
	       (cond ((eq n 0) nil)
		     ((minusp n)
		      (do ((i (arg nil)  (1- i))
			   (result nil (cons (arg i) result)))
			  ((<& i (+ (arg nil) n  1)) result) ))
		     (t (do ((i n  (1- i))
			     (result nil (cons (arg i) result)))
			    ((<& i 1) result) ))))))

;--- include-if
; form: (include-if <predicate> <filename>)
;  will return (include <filename>) if <predicate> is non-nil
;  This is useful at the beginning of a file to conditionally
;  include a file based on whether it has already been included.
;
(defmacro include-if (pred filename)
   (cond ((eval pred) `(include ,filename))))

;--- includef-if
; form: (includef-if <predicate> '<filename>)
;  like the above except it includef's the file.
;
(defmacro includef-if (pred filenameexpr)
   (cond ((eval pred) `(includef ,filenameexpr))))

;--- if :: macro for doing conditionalization
;
;  This macro is compatible with both the crufty mit-version and
; the keyword version at ucb.
;
;  simple summary:
;   non-keyword use:
;	(if a b) ==> (cond (a b))
;	(if a b c d e ...) ==> (cond (a b) (t c d e ...))
;   with keywords:
;	(if a then b) ==> (cond (a b))
;	(if a thenret) ==> (cond (a))
;	(if a then b c d e) ==> (cond (a b c d e))
;	(if a then b c  else d) ==> (cond (a b c) (t d))
;	(if a then b c  elseif d  thenret  else g)
;		==> (cond (a b c) (d) (t g))
;
;   
;
;
; In the syntax description below,
;    optional parts are surrounded by [ and ],
;    + means one or more instances.
;    | means 'or'
;    <expr> is an lisp expression which isn't a keyword
;       The keywords are:  then, thenret, else, elseif.
;    <pred> is also a lisp expression which isn't a keyword.
; 
; <if-stmt> ::=  <simple-if-stmt>
; 	       | <keyword-if-stmt>
; 
; <simple-if-stmt> ::=  (if <pred> <expr>)
; 		      | (if <pred> <expr> <expr>)
; 
; <keyword-if-stmt> ::= (if <pred> <then-clause> [ <else-clause> ] )
; 
; <then-clause> ::=  then <expr>+
; 		   | thenret
; 
; <else-clause> ::=  else <expr>+
; 		   | elseif <pred> <then-clause> [ <else-clause> ]
;

(declare (special if-keyword-list))

(eval-when (compile load eval)
   (setq if-keyword-list '(then thenret elseif else)))

;--- if
;
;  the keyword if expression is parsed using a simple four state
; automaton.  The expression is parsed in reverse.
; States:
;	init - have parsed a complete predicate,  then clause
;	col  - have collected at least one non keyword in col
;	then - have just seen a then, looking for a predicate
;	compl - have just seen a predicate after an then, looking
;		for elseif or if (i.e. end of forms).
;
(defmacro if (&rest args)
   (let ((len (length args)))
      ;; first eliminate the non-keyword if macro cases
      (cond ((<& len 2)
	     (error "if: not enough arguments " args))
	    ((and (=& len 2)
		  (not (memq (cadr args) if-keyword-list)))
	     `(cond (,(car args) ,(cadr args))))
	    ; clause if there are not keywords (and len > 2)
	    ((do ((xx args (cdr xx)))
		 ((null xx) t)
		 (cond ((memq (car xx) if-keyword-list)
			(return nil))))
	     `(cond (,(car args) ,(cadr args))
		    (t ,@(cddr args))))
	    
	    ;; must be an instance of a keyword if macro
	    
	    (t (do ((xx (reverse args) (cdr xx))
		    (state 'init)
		    (elseseen nil)
		    (totalcol nil)
		    (col nil))
		   ((null xx)
		    (cond ((eq state 'compl)
			   `(cond ,@totalcol))
			  (t (error "if: illegal form " args))))
		   (cond ((eq state 'init)
			  (cond ((memq (car xx) if-keyword-list)
				 (cond ((eq (car xx) 'thenret)
					(setq col nil
					      state 'then))
				       (t (error "if: bad keyword "
						 (car xx) args))))
				(t (setq state 'col
					 col nil)
				   (push (car xx) col))))
			 ((eq state 'col)
			  (cond ((memq (car xx) if-keyword-list)
				 (cond ((eq (car xx) 'else)
					(cond (elseseen
						 (error
						    "if: multiples elses "
						    args)))
					(setq elseseen t)
					(setq state 'init)
					(push `(t ,@col) totalcol))
				       ((eq (car xx) 'then)
					(setq state 'then))
				       (t (error "if: bad keyword "
						 (car xx) args))))
				(t (push (car xx) col))))
			 ((eq state 'then)
			  (cond ((memq (car xx) if-keyword-list)
				 (error "if: keyword at the wrong place "
					(car xx) args))
				(t (setq state 'compl)
				   (push `(,(car xx) ,@col) totalcol))))
			 ((eq state 'compl)
			  (cond ((not (eq (car xx) 'elseif))
				 (error "if: missing elseif clause " args)))
			  (setq state 'init))))))))

;--- If :: the same as 'if' but defined for those programs that still
;	use it.
;
(putd 'If (getd 'if))

;--- defvar :: a macro for declaring a variable special
;  a variable declared special with defvar will be special when the
; file containing the variable is compiled and also when the file
; containing the defvar is loaded in.  Furthermore, you can specify
; an default value for the variable. It will be set to that value
; iff it is unbound
;
(defmacro defvar (variable &optional (initial-value nil iv-p) documentation)
  (if iv-p
     then `(progn 'compile
		   (eval-when (eval compile load)
			  (eval '(liszt-declare (special ,variable))))
		   (or (boundp ',variable) (setq ,variable ,initial-value)))
     else `(eval-when (eval compile load)
		  (eval '(liszt-declare (special ,variable))))))
