;; macros.l				-[Wed May 12 13:32:53 1982 by jkf]-
;;
;;  The file contains the common macros for Franz lisp.
;; contents:
;;	defmacro
;;	let
;;	let*
;;	caseq
;;	listify
;;

(setq SCCS-macros "@(#)macros.l	1.3	5/18/82")

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
   (prog (vrbls optlist auxlist restform vbl fl-type optcode mainvar)
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
	    auxlist (nreverse auxlist))

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
			    ,@body)))
	       (t (return
		     `(lexpr (,mainvar)
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
(def caseq
  (macro (form)
     ((lambda (x temp)
	 `((lambda (,x)
		   (cond
		      ,@(mapcar
			   '(lambda (ff)
				    (setq temp
					  (cond ((cddr ff)
						 `(progn ,@(cdr ff)))
						(t (cadr ff))))

				    (cond ((eq (car ff) 't)
					   `(t ,temp))
					  ((dtpr (car ff))
					   `((memq ,x ',(car ff))
					     ,temp))
					  (t `((eq ,x ',(car ff))
					       ,temp))))
				(cddr form))))
	   ,(cadr form)))
      (gensym 'Z) nil)))

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
