(setq rcs-ucifnc-
   "$Header: /usr/lib/lisp/ucifnc.l,v 1.1 83/01/29 18:41:16 jkf Exp $")

;
; There is problems with the ucilisp do being
;	incompatible with maclisp/franz do,
;	The problems with compiling do are gone, but
;	due to these possible problems, the ucilisp do function
;	is in a seperate file ucido.l and users of it
;	should also load that file in at compile time before
;	any call to do (since do is a macro) (and
;	at runtime if do is to be interpreted).
;
; This file is meant to be fasl'd or used with liszt -u
;	not to be read in interpretively (the syntax changes
;	will not work in that case.
;
;	to compile this file do liszt ucifnc.l
;
;	one who wants to use these functions or compile and run
;	a ucilisp program should do both
;	liszt -u file.l 	when compiling.
;	and
;	(fasl '/usr/lib/lisp/ucifnc)
;		before loading in and running them
;		programs in lisp.
;	This is because some functions are macros and others are too
;		complicated and need other functions around.
;	Note this file will not load in directly and when fasl'd in will
;		cause the syntax of lisp to change to ucilisp syntax.
;
(declare (macros t))

;
; ucilisp (de df dm) declare function macros.
;
; (de name args body) -> declare exprs and lexprs.
;
(defun de macro (l) 
  `(defun ,@(cdr l)))
  
;
; (df name args body) -> declare fexprs.
;
(defun df macro (l) 
  `(defun ,(cadr l)
	  fexpr
	  ,@(cddr l)))

;
; macro's are not compiled except under the same
;	conditions as in franz lisp.
;	(usually just do (declare (macros t))
;		to have macros also compiled).
;
;
; (dm name args body) -> declare macros. same as (defun name 'macro body)
;
(defun dm macro (l) 
  `(defun ,(cadr l)
	  macro
	  ,@(cddr l)))
  
;
; ucilisp let macro.
;
(eval-when (compile load eval)
  (defun let1 (l vars vals body)
	 (cond ((null l) 
		(cons (cons 'lambda (cons vars body)) vals))
	       (t 
		(let1 (cddr l) 
		      (cons (car l) vars) 
		      (cons (cadr l) vals) body)))))
  
(defun let macro (l)
  (let1 (cadr l) nil nil (cddr l)))
  
(defun nconc1 macro (l) 
  `(nconc ,(cadr l) (list ,(caddr l))))
  
(putd 'expandmacro (getd 'macroexpand))
  
;
; ucilisp selectq function. (written by jkf)
;
(def selectq
  (macro (form)
	 ((lambda (x)
		  `((lambda (,x)
			    (cond 
			     ,@(maplist 
				'(lambda (ff)
					 (cond ((null (cdr ff))
						`(t  ,(car ff)))
					       ((atom (caar ff))
						`((eq ,x ',(caar ff))
						  . ,(cdar ff)))
					       (t
						`((memq ,x ',(caar ff))
						  . ,(cdar ff)))))
				(cddr form))))
		    ,(cadr form)))
	  (gensym 'Z))))

;
; ucilisp functions which declare read macros.
;
; dsm - declare splicing read macro.
;
(defun dsm macro (l) 
  `(eval-when (compile load eval)
	      (setsyntax ',(cadr l) 'splicing ',(caddr l))))

;
; drm - declare read macro.
;
(defun drm macro (l) 
  `(eval-when (compile load eval)
	      (setsyntax ',(cadr l) 'macro ',(caddr l))))

;
;(:= a b) -> ucilisp assignment macro.
;
(defun := macro (expression)
      (let (lft (macroexpand (cadr expression)) rgt (caddr expression))
	   (cond ((atom lft) 
		  `(setq ,lft ,(subst lft '*-* rgt)))
		 ((get (car lft) 'set-program)
		  (cons (get (car lft) 'set-program)
			(append (cdr lft) (list (subst lft '*-* rgt))))))))
  
(defprop car rplaca set-program)
(defprop cdr rplacd set-program)
(defprop cadr rplacad set-program)
(defprop cddr rplacdd set-program)
(defprop caddr rplacadd set-program)
(defprop cadddr rplacaddd set-program)
(defprop get get-set-program set-program)

(defun get-set-program (atm prop val) 
  (putprop atm val prop))

(defun rplacad (exp1 exp2) 
  (rplaca (cdr exp1) exp2))

(defun rplacdd (exp1 exp2) 
  (rplacd (cdr exp1) exp2))

(defun rplacadd (exp1 exp2) 
  (rplaca (cddr exp1) exp2))

(defun rplacaddd (exp1 exp2) 
  (rplaca (cdddr exp1) exp2))

;
; ucilisp record-type package to declare records and field extraction
;	macros.
;

(declare (special *type*))

(defun record-type macro (l)
  (let (*type* (cadr l) *flag* (caddr l) slots (car (last l)))
       `(progn 'compile
	       (defun ,*type*
		      ,(slot-funs-extract slots (and *flag* '(d)))
		      ,(cond ((null *flag*) (struc-cons-form slots))
			     (t (append `(cons ',*flag*)
					(list (struc-cons-form slots))))))
	       ,(cond (*flag*
		       (cond ((dtpr *flag*) (setq *flag* *type*)))
		       `(defun ,(concat 'is- *type*)
			       macro
			       (l)
			       (list 'and (list 'dtpr (cadr l))
				     (list 'eq (list 'car (cadr l))
					   '',*flag*))))))))
  
(defun slot-funs-extract (slots path)
  (cond ((null slots) nil)
	((atom slots)
	 (eval `(defun ,(concat slots ': *type*)
		       macro
		       (l)
		       (list ',(readlist `(c ,@path r))
			     (cadr l))))
	 (list slots))
	((nconc (slot-funs-extract (car slots) (cons 'a path))
		(slot-funs-extract (cdr slots) (cons 'd path))))))
  
(defun struc-cons-form (struc)
  (cond ((null struc) nil)
	((atom struc) struc)
	(t `(cons ,(struc-cons-form (car struc))
		  ,(struc-cons-form (cdr struc))))))

(defun some macro (l)
  `((lambda (f a)
	    (prog ()
		  loop
		  (cond ((null a) (return nil))
			((funcall f (car a))
			 (return a))
			(t (setq a (cdr a))
			   (go loop)))))
    ,(cadr l)
    ,(caddr l)))

(declare (special vars))
  
(defun for macro (*l*)
  (let (vars (vars:for *l*)
	     args (args:for *l*)
	     test (test:for *l*)
	     type (type:for *l*)
	     body (body:for *l*))
       (cons (make-mapfn vars test type body)
	     (cons (list 'quote
			 (make-lambda 
			  vars (add-test test
					 (make-body vars test type body))))
		   args))))
  
(defun type:for (*l*)
  (let (item (item:for '(do save splice filter) *l*))
       (cond (item (car item))
	     ((error '"No body in for loop")))))
  
(defun error (l &optional x)
  (cond (x (terpri) (patom l) (terpri) (drain) (break) l)
	(t l)))
  
(defun vars:for (*m*)
  (mapcan '(lambda (x) (cond ((is-var-form x) (list (var:var-form x))))) *m*))

(defun args:for (*n*)
  (mapcan '(lambda (x) 
		   (cond ((is-var-form x) (list (args:var-form x)))))
	  *n*))

(defun is-var-form (x) (and (eq (length x) 3) (eq (cadr x) 'in)))
  
(defun var:var-form (x) (car x))
(defun args:var-form (x) (caddr x))
  
(defun test:for (*o*)
  (let (item (item:for '(when) *o*))
       (cond (item (cadr item)))))
  
(defun body:for (*p*)
  (let (item (item:for '(do save splice filter) *p*))
       (cond ((not item) (error '"NO body in for loop"))
	     ((eq (length (cdr item)) 1) (cadr item))
	     ((cons 'progn (cdr item))))))

(declare (special *l* item))

(defun item:for (keywords *l*)
  (let (item nil)
       (some '(lambda (key) (setq item (assoc key (cdr *l*))))
	     keywords)
       item))

(defun make-mapfn (vars test type body)
  (cond ((equal type 'do) 'mapc)
	((not (equal type 'save)) 'mapcan)
	((null test) 'mapcar)
	((subset-test vars body) 'subset)
	('mapcan)))
  
(defun subset-test (vars body)
  (and (equal (length vars) 1) (equal (car vars) body)))
  
(defun make-body (vars test type body)
  (cond ((equal type 'filter)
	 (list 'let (list 'x body) '(cond (x (list x)))))
	((or (not (equal type 'save)) (null test)) body)
	((subset-test vars body) nil)
	((list 'list body))))
  
(defun add-test (test body)
  (cond ((null test) body)
	((null body) test)
	(t (list 'cond (cond ((eq (car body) 'progn) (cons test (cdr body)))
			     ((list test body)))))))
  
(defun make-lambda (var body)
  (cond ((equal var (cdr body)) (car body))
	((eq (car body) 'progn) (cons 'lambda (cons vars (cdr body))))
	((list 'lambda vars body))))
  
(defun pop macro (q)
  `(prog (*q*)
	 (setq *q* (car ,(cadr q)))
	 (setq ,(cadr q) (cdr ,(cadr q)))
	 (return *q*)))
  
(defun length (*u*)
  (cond ((null *u*) 0)
	((atom *u*) 0)
	((add1 (length (cdr *u*))))))
  
(declare (special l))
  
(defun every macro (l)
  `(prog ($$k $v)
	 (setq $$k ,(caddr l))
	 loop
	 (cond ((null $$k)
		(return t))
	       ((apply ,(cadr l) (list (car $$k)))
		(setq $$k (cdr $$k))
		(go loop)))
	 (return nil)))

(defun timer fexpr (request)
  (prog (timein timeout result cpu garbage)
	(setq timein (ptime))
	(prog ()
	      loop (setq result (eval (car request)))
	      (setq request (cdr request))
	      (cond ((null request) (return result))
		    ((go loop))))
	(setq timeout (ptime))
	(setq cpu (quotient (times 1000.0
				   (quotient (difference (car timeout) 
							 (car timein))
					     60.0))
			    1000.0))
	(setq garbage (quotient (times 1000.0
				       (quotient (difference (cadr timeout) 
							     (cadr timein)) 
						 60.0))
				1000.0))
	(print (cons cpu garbage))
	(terpri)
	(return result)))
  
(defun addprop (id value prop)
  (putprop id (enter value (get id prop)) prop))
  
(defun enter (v l)
  (cond ((member v l) l)
	(t (cons v l))))
  
(defmacro subset (fun lis)
  `(mapcan '(lambda (ele)
		    (cond ((funcall ,fun ele) (ncons ele))))
	   ,lis))
  
(defun push macro (varval)
  `(setq ,(cadr varval)
	 (cons ,(caddr varval)
	       ,(cadr varval))))
  
(putd 'consp (getd 'dtpr))
  
(defun prelist (a b)
  (cond ((null a) nil)
	((eq b 0) nil)
	((cons (car a) (prelist (cdr a) (sub1 b))))))
  
(defun suflist (a b)
  (cond ((null a) nil)
	((eq b 0) a)
	((suflist (cdr a) (sub1 b)))))
  
(defun loop macro (l)
  `(prog ,(var-list (get-keyword 'initial l))
	 ,@(subset (function caddr)
		   (setq-steps (get-keyword 'initial l)))
	 loop
	 ,@(apply (function append) (mapcar (function do-clause) (cdr l)))
	 (go loop)
	 exit
	 (return ,@(get-keyword 'result l))))
  
(defun do-clause (clause)
  (cond ((memq (car clause) '(initial result)) nil)
	((eq (car clause) 'while)
	 (list (list 'or (cadr clause) '(go exit))))
	((eq (car clause) 'do) (cdr clause))
	((eq (car clause) 'next) (setq-steps (cdr clause)))
	((eq (car clause) 'until)
	 (list (list 'and (cadr clause) '(go exit))))
	(t (terpri) (patom '"unknown keyword clause")
	   (patom (car clause))
	   (terpri))))
  
(defun get-keyword (key l)
  (cdr (assoc key (cdr l))))
  
(defun var-list (r)
  (and r (cons (car r) (var-list (cddr r)))))
  
(defun setq-steps (s)
  (and s (cons (list 'setq (car s) (cadr s))
	       (setq-steps (cddr s)))))

(putd 'readch (getd 'readc))


;
; ucilisp msg function. (written by jkf)
;
(defmacro msg ( &rest body)
  `(progn ,@(mapcar 
	     '(lambda (form)
		      (cond ((eq form t) '(line-feed 1))
			    ((numberp form)
			     (cond ((greaterp form 0) 
				    `(msg-space ,form))
				   (t `(line-feed ,(minus form)))))
			    ((atom form) `(patom ,form))
			    ((eq (car form) t) '(patom '/	))
			    ((eq (car form) 'e) 
			     `(patom ,(cadr form)))
			    (t `(patom ,form))))
	     body)))
  
;
; this must be fixed to not use do.
;
(defmacro msg-space (n)
  (cond ((eq 1 n) '(patom '" "))
	(t `(do i ,n (sub1 i) (lessp i 1) (patom '/ ))))) 

(defmacro line-feed (n)
  (cond ((eq 1 n) '(terpr))
	(t `(do i ,n (sub1 i) (lessp i 1) (terpr)))))

(defmacro prog1 ( first &rest rest &aux (foo (gensym)))
  `((lambda (,foo) ,@rest ,foo) ,first))

(defun append1 (l x) (append l (list x)))

; compatability functions: functions required by uci lisp but not
;	present in franz
;
; union uses the franz do loop (not the ucilisp one defined in this file).
;

(def union 
  (lexpr (n)
	 (do ((res (arg n))
	      (i (sub1 n) (sub1 i)))
	     ((zerop i) res)
	     (mapc '(lambda (arg)
			    (cond ((not (member arg res)) 
				   (setq res (cons arg res)))))
		   (arg i)))))


(putd 'newsym (getd 'gensym))	; this is not exactly correct.
				; it only uses the first letter of the arg.
(putd 'remove (getd 'delete))

; ignore column count
(def sprint
  (lambda (form column)
	  ($prpr form)))

(def save  (lambda (f) (putprop f (getd f) 'olddef)))

(def unsave 
  (lambda (f) 
	  (putd f (get f 'olddef))))

(putd 'atcat (getd 'concat))
(putd 'consp (getd 'dtpr))

(defun neq macro (x)
  `(not (eq ,@(cdr x))))

(putd 'gt (getd '>))
(putd 'lt (getd '<))

(defun le macro (x)
  `(not (> ,@(cdr x))))

(defun ge macro (x)
  `(not (< ,@(cdr x))))

(defun litatom macro (x)
  `(and (atom ,@(cdr x))
	(not (numberp ,@(cdr x)))))

(putd 'apply\# (getd 'apply))

(defun tconc (ptr x)
  (cond ((null ptr)
	 (prog (temp)
	       (setq temp (list x))
	       (return (setq ptr (cons temp (last temp))))))
	((null (car ptr))
	 (rplaca ptr (list x))
	 (rplacd ptr (last (car ptr)))
	 ptr)
	(t (prog (temp)
		 (setq temp (cdr ptr))
		 (rplacd (cdr ptr) (list x))
		 (rplacd ptr (cdr temp))
		 (return ptr)))))

;
;	unbound - (setq x (unbound)) will unbind x.
; "this [code] is sick" - jkf.
;
(defun unbound macro (l)
  `(fake -4))

;
;
;	due to problems with franz do in the compiler, this
;		has been commented out and is left in a seperate
;		file called /usr/lib/lisp/ucido.l
;
;(defun do macro (l)
;  ((lambda (dotype alist)
;	   (selectq dotype 
;		    (while (dowhile (car alist) (cdr alist)))
;		    (until (dowhile (list 'not (car alist))
;				    (cdr alist)))
;		    (for (dofor (car alist) 
;				(cadr alist)
;				(caddr alist)
;				(cdddr alist)))
;		    `((lambda ()
;			      ,@alist))))
;   (cadr l)
;   (cddr l)))
;
;(defun dowhile (expr alist)
;  `(prog (returnvar)
;	 loop
;	 (cond (,expr
;		(setq returnvar ((lambda ()
;					 ,@alist)))
;		(go loop))
;	       (t (return returnvar)))))
;
;(defun dofor (var fortype varlist stmlist)
;  (selectq fortype 
;	   (in `(prog (returnvar l1 l2)
;		      (setq l2 ',varlist)
;		      loop
;		      (setq l1 (car l2))
;		      (setq l2 (cdr l2))
;		      (cond ((null l1) 
;			     (return returnvar)))
;		      (setq returnvar
;			    ((lambda (,var)
;				     ,@stmlist)
;			     (l1)))
;		      (go loop)))
;	   (on `(prog (returnvar l1 l2)
;		      (setq l2 ',varlist)
;		      loop
;		      (cond ((null l2) 
;			     (return returnvar)))
;		      (setq returnvar
;			    ((lambda (,var)
;				     ,@stmlist)
;			     (l2)))
;		      (setq l2 (cdr l2))
;		      (go loop)))
;	   (rpt `(prog (returnvar ,var)
;		       (setq ,var 1)
;		       loop
;		       (cond ((not (> ,var ,varlist))
;			      (setq returnvar ((lambda ()
;						       ,@stmlist)))
;			      (setq ,var (1+ ,var))
;			      (go loop))
;			     (t (return returnvar)))))
;	   nil))
;
(putd 'dddd* (getd 'boundp))
(defun boundp (l)
  (cond ((arrayp l))
	((dddd* l))))

;
; now change to ucilisp syntax.
;
(sstatus uctolc t)
;
;	Leave backquote macro in for now.
;		These characters should be declared as follows for real
;		ucilisp syntax though.
;(setsyntax '\` 2)
;(setsyntax '\, 2)
;(setsyntax '\@ 201)
;(setsyntax '\@ 'macro '(lambda () (list 'quote (read))))
; 
; ~ as comment character, not ; and / instead of \ for escape
(setsyntax '\~ 'splicing 'zapline)
(setsyntax '\; 2)
(setsyntax '\# 2)
(setsyntax '\/ 143)
(setsyntax '\\   2)
(setsyntax '\! 2)
