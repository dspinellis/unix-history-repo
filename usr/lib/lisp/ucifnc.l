(setq SCCS-ucifnc "@(#)ucifnc.l	1.2	10/22/80")

(eval-when (eval compile load) 
  
  (defun de macro (l) 
	 `(eval-when (load eval compile)
		     ,(append (list
			       'defun (cadr l) (caddr l))
			      (cdddr l))))
  
  (defun df macro (l) 
	 `(eval-when (load eval compile)
		     ,(append (list
			       'defun (cadr l) 'fexpr (caddr l))
			      (cdddr l))))
  
  (defun dm macro (l) 
	 `(eval-when (load eval compile)
		     ,(append (list
			       'defun (cadr l) 'macro (caddr l))
			      (cdddr l))))
  
  
  (defun let1 (*l* *vars* *vals* *body*)
	 (cond ((null *l*) 
		(cons (cons 'lambda (cons *vars* *body*)) *vals*))
	       (t 
		(let1 (cddr *l*) 
		      (cons (car *l*) *vars*) 
		      (cons (cadr *l*) *vals*) *body*))))
  
  (defun let macro (l)
	 (let1 (cadr l) nil nil (cddr l)))
  
  (dm nconc1 (l) (list 'nconc (cadr l) (list 'list (caddr l))))
  
  (declare (special vars))
  (declare (special *vars*))
  (declare (special *l*))
  
  (putd 'expandmacro (getd 'macroexpand))
  
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

  (dm dsm (l) 
      `(eval-when (compile load eval)
		  ,(list 'setsyntax (list 'quote (cadr l))
			 (list 'quote 'splicing)
			 (list 'quote (caddr l)))))
  
  (dm drm (l) 
      `(eval-when (compile load eval)
		  ,(list 'setsyntax (list 'quote (cadr l))
			 (list 'quote 'macro)
			 (list 'quote (caddr l)))))
  
  (dm := (expression)
      (let (lft (macroexpand (cadr expression)) rgt (caddr expression))
	   (cond ((atom lft) (list 'setq lft (subst lft '*-* rgt)))
		 ((get (car lft) 'set-program)
		  (cons (get (car lft) 'set-program)
			(append (cdr lft) (list (subst lft '*-* rgt))))))))
  
  (declare (special car))
  (declare (special cdr))
  (declare (special cadr))
  (declare (special cddr))
  (declare (special caddr))
  (declare (special cadddr))
  (declare (special get))
  
  (defprop car rplaca set-program)
  (defprop cdr rplacd set-program)
  (defprop cadr rplacad set-program)
  (defprop cddr rplacdd set-program)
  (defprop caddr rplacadd set-program)
  (defprop cadddr rplacaddd set-program)
  (defprop get get-set-program set-program)
  (de get-set-program (atm prop val) (putprop atm val prop))
  (de rplacad (exp1 exp2) (rplaca (cdr exp1) exp2))
  (de rplacdd (exp1 exp2) (rplacd (cdr exp1) exp2))
  (de rplacadd (exp1 exp2) (rplaca (cddr exp1) exp2))
  (de rplacaddd (exp1 exp2) (rplaca (cdddr exp1) exp2))

  (declare (special *type*))
  
  (dm record-type (l)
      (let (*type* (cadr l) *flag* (caddr l) slots (car (last l)))
	   (list 'progn ''compile
		 (list 'de *type* (slot-funs-extract slots (and *flag* '(d)))
		       (cond ((null *flag*) (struc-cons-form slots))
			     (t (append (list 'cons (list 'quote *flag*))
					(list (struc-cons-form slots))))))
		 (cond (*flag*
			(cond ((dtpr *flag*) (setq *flag* *type*)))
			(list 'dm 
			      (readlist 
			       (cons 'i
				     (cons 's
					   (cons '-
						 (append (explode *type*) nil)))))
			      '(l)
			      (list 'list ''and '(list 'dtpr (cadr l))
				    (list 'list ''eq '(list 'car (cadr l))
					  (list 'quote (list 'quote *flag*))))))))))
  
  (de slot-funs-extract (slots path)
      (cond ((null slots) nil)
	    ((atom slots)
	     (eval (list 'dm (readlist (append (explode slots)
					       (cons ':
						     (append 
						      (explode *type*) nil))))
			 '(l)
			 (cons 'list
			       (cons (list 'quote
					   (readlist
					    (cons 'c (append path '(r)))))
				     '((cadr l))))))
	     (list slots))
	    ((nconc (slot-funs-extract (car slots) (cons 'a path))
		    (slot-funs-extract (cdr slots) (cons 'd path))))))
  
  (de struc-cons-form (struc)
      (cond ((null struc) nil)
	    ((atom struc) struc)
	    ((list 'cons
		   (struc-cons-form (car struc))
		   (struc-cons-form (cdr struc))))))
  
  (dm for (*l*)
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
  
  (de type:for (*l*)
      (let (item (item:for '(do save splice filter) *l*))
	   (cond (item (car item))
		 ((error '"No body in for loop")))))
  
  (de error (l x)
      (cond (x (terpri) (patom l) (terpri) (drain) (break) l)
	    (t l)))
  
  (de vars:for (*m*)
      (mapcan '(lambda (x) (cond ((is-var-form x) (list (var:var-form x))))) *m*))
  
  (de args:for (*n*)
      (mapcan '(lambda (x) (cond ((is-var-form x) (list (args:var-form x))))) *n*))
  (de is-var-form (x) (and (eq (length x) 3) (eq (cadr x) 'in)))
  
  (de var:var-form (x) (car x))
  (de args:var-form (x) (caddr x))
  
  (de test:for (*o*)
      (let (item (item:for '(when) *o*))
	   (cond (item (cadr item)))))
  
  (de body:for (*p*)
      (let (item (item:for '(do save splice filter) *p*))
	   (cond ((not item) (error '"NO body in for loop"))
		 ((eq (length (cdr item)) 1) (cadr item))
		 ((cons 'progn (cdr item))))))
  
  (declare (special *l*))
  (declare (special keywords))
  (declare (special item))
  
  (de item:for (keywords *l*)
      (let (item nil)
	   (some '(lambda (key) (setq item (assoc key (cdr *l*))))
		 keywords)
	   item))
  
  (de make-mapfn (vars test type body)
      (cond ((equal type 'do) 'mapc)
	    ((not (equal type 'save)) 'mapcan)
	    ((null test) 'mapcar)
	    ((subset-test vars body) 'subset)
	    ('mapcan)))
  
  (de subset-test (vars body)
      (and (equal (length vars) 1) (equal (car vars) body)))
  
  (de make-body (vars test type body)
      (cond ((equal type 'filter)
	     (list 'let (list 'x body) '(cond (x (list x)))))
	    ((or (not (equal type 'save)) (null test)) body)
	    ((subset-test vars body) nil)
	    ((list 'list body))))
  
  (de add-test (test body)
      (cond ((null test) body)
	    ((null body) test)
	    (t (list 'cond (cond ((eq (car body) 'progn) (cons test (cdr body)))
				 ((list test body)))))))
  
  (de make-lambda (var body)
      (cond ((equal var (cdr body)) (car body))
	    ((eq (car body) 'progn) (cons 'lambda (cons vars (cdr body))))
	    ((list 'lambda vars body))))
  
  (dm pop (*q*)
      (list 'prog '(*q*)
	    (list 'setq '*q* (list 'car (cadr *q*)))
	    (list 'setq (cadr *q*) (list 'cdr (cadr *q*)))
	    '(return *q*)))
  
  (de length (*u*)
      (cond ((null *u*) 0)
	    ((atom *u*) 0)
	    ((add1 (length (cdr *u*))))))
  
  (declare (special $f))
  (declare (special $l))
  
  (de some ($f $l)
      (cond ((null $l) nil)
	    ((funcall $f (car $l))
	     $l)
	    ((some $f (cdr $l)))))
  
  (declare (special l))
  
  (dm every (l)
      (list 'prog '($$k)
	    (list 'setq '$$k (cons 'list (cddr l)))
	    'loop
	    (append (list 'and
			  (list 'apply (cadr l) '(mapcar (quote car) $$k)))
		    '((setq $$k (mapcar (quote cdr) $$k))
		      (cond ((memq nil $$k) (setq $$k nil))
			    (t))
		      (go loop)))
	    '(or $$k (return t))))

  (df timer (request)
      (prog (timein timeout result cpu garbage)
	    (setq timein (ptime))
	    (prog ()
		  loop (setq result (eval (car request)))
		  (setq request (cdr request))
		  (cond ((null request) (return result))
			((go loop))))
	    (setq timeout (ptime))
	    (setq cpu (/ (- (car timeout) (car timein)) 60.0))
	    (setq cpu (/ (float (fix (* 1000.0 cpu))) 1000.0))
	    (setq garbage (/ (- (cadr timeout) (cadr timein)) 60.0))
	    (setq garbage (/ (float (fix (* 1000.0 garbage))) 1000.0))
	    (terpri)
	    ($prpr (cons cpu garbage))
	    (terpri)
	    (return result)))
  
  (de addprop (id value prop)
      (putprop id (enter value (get id prop)) prop))
  
  (de enter (v l)
      (cond ((member v l) l)
	    (t (cons v l))))
  
  (declare (special fun))
  (declare (special lis))
  
  (defmacro subset (fun lis)
	    `(mapcan '(lambda (ele)
			      (cond ((funcall ,fun ele) (ncons ele))))
		     ,lis))
  
  (dm push (varval)
      (list 'setq (caddr varval) (list 'cons (cadr varval) (caddr varval))))
  
  (putd 'consp (getd 'dtpr))
  
  (de prelist (a b)
      (cond ((null a) nil)
	    ((eq b 0 ) nil)
	    ((cons (car a) (prelist (cdr a) (sub1 b))))))
  
  (de suflist (a b)
      (cond ((null a) nil)
	    ((eq b 0) a)
	    ((suflist (cdr a) (sub1 b)))))
  
  (declare (special **l$))
  
  (defun loop macro (**l$)
	 (append (list 'prog (var-list (get-keyword 'initial **l$)))
		 (subset (function caddr) (setq-steps (get-keyword 'initial **l$)))
		 '(loop)
		 (apply (function append) (mapcar (function do-clause) (cdr **l$)))
		 (list '(go loop)
		       'exit (cons 'return (get-keyword 'result **l$)))))
  
  (defun do-clause (clause)
	 (cond ((memq (car clause) '(initial result)) nil)
	       ((eq (car clause) 'while)
		(list (list 'or (cadr clause) '(go exit))))
	       ((eq (car clause) 'do) (cdr clause))
	       ((eq (car clause) 'next) (setq-steps (cdr clause)))
	       ((eq (car clause) 'until)
		(list (list 'and (cadr clause) '(go exit))))
	       (t (terpri) (patom '"unknown keyword clause"))))
  
  
  (defun get-keyword (key l)
	 (cdr (assoc key (cdr l))))
  
  (defun var-list (*r*)
	 (and *r* (cons (car *r*) (var-list (cddr *r*)))))
  
  (defun setq-steps (*s*)
	 (and *s* (cons (list 'setq (car *s*) (cadr *s*))
			(setq-steps (cddr *s*)))))

  (defun gtblk macro (p)
	 `(*array nil t (cadr p)))
  ; here comes syntax changes to ucilisp
  ;
  ; upper case to lower case
  (putd 'readch (getd 'readc))

  (declare (macros t))

  (defmacro msg ( &rest body)
	    `(progn ,@(mapcar '(lambda (form)
				       (cond ((eq form t) '(line-feed 1))
					     ((numberp form)
					      (cond ((greaterp form 0) `(msg-space ,form))
						    (t `(line-feed ,(minus form)))))
					     ((atom form) `(patom ',form))
					     ((eq (car form) t) '(patom '/	))
					     ((eq (car form) 'e) 
					      `(patom ,(cadr form)))
					     (t `(patom ,form))))
			      body)))
  
  (defmacro msg-space (n)
	    (cond ((eq 1 n) '(patom '" "))
		  (t `(do i ,n (sub1 i) (lessp i 1) (patom '/ ))))) 

  (defmacro line-feed (n)
	    (cond ((eq 1 n) '(terpr))
		  (t `(do i ,n (sub1 i) (lessp i 1) (terpr)))))

  (defmacro prog1 ( first &rest rest &aux (foo (gensym)))
	    `((lambda (,foo) ,@rest ,foo) ,first))

  (de append1 (l x) (append l (list x)))

  ; compatability functions: functions required by uci lisp but not
  ;	present in franz

  (def union 
       (lexpr (n)
	      (do ((res (arg n))
		   (i (sub1 n) (sub1 i)))
		  ((zerop i) res)
		  (mapc '(lambda (arg)
				 (cond ((not (member arg res)) 
					(setq res (cons arg res)))))
			(arg i)))))


  (putd 'newsym (getd 'gensym))	; this is probably correct
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

  (sstatus uctolc t)
  (setsyntax '\, 2)
  (setsyntax '\! 2)
  (setsyntax '\` 2)
  ; 
  ; ~ as comment character, not ; and / instead of \ for escape
  (setsyntax '\~ 'splicing (get '\; 'macro))
  (setsyntax '\; 2)
  (setsyntax '\/ 143)
  (setsyntax '\\   2)
  (setsyntax '\@ 201)
  
  )
