;--- file : complrc.l
(include "compmacs.l")

(declare (special w-vars w-labs w-ret w-name w-bv w-atmt cm-alv v-cnt))
(def $pr$ (macro (x) `(patom ,(cadr x) compout)))

(def put 
  (macro (x)
	 ((lambda (atm prp arg)
		  `(progn (putprop ,atm ,arg ,prp) ,atm))
	  (cadr x) (caddr x) (cadddr x))))

(def f-if 
  (lambda (v-l v-r v-j v-t) 
	  (cond ((eq (caar v-l) 't) 
		 (cond ((null (cdar v-l)) (f-exp t v-r v-t)) 
		       (t (f-seq (cdar v-l) v-r v-t)))) 
		(t (prog (v-tr v-i v-dv) 
			 (setq v-tr (f-reg nil))
			 (setq v-dv 'amb)
			 (cond ((null (cdr v-l)) 
				(setq v-tr v-r) 
				(cond ((null (cdar v-l)) (go loop2))) 
				(setq v-dv nil) 
				(setq v-i (cadr v-j))) 
			       ((null (cdar v-l)) 
				(setq v-tr v-r) 
				(setq v-t (f-if (cdr v-l) v-r v-j v-t)) 
				(setq v-t (f-addi (list 'true (cadr v-j) t)
						  v-t))
				(go loop1)) 
			       (t (setq v-t (f-leap (f-if (cdr v-l)
							  v-r 
							  v-j 
							  v-t))) 
				  (setq v-t (f-addi v-j v-t)) 
				  (setq v-i (cadr s-inst)))) 
			 (setq v-t (f-seq (cdar v-l) v-r v-t)) 
			 (setq v-t (f-addi (list 'false v-i v-dv) v-t)) 
		 loop1 
			 (setq v-t (f-addi (list 'minus (f-use v-tr) nil) v-t)) 
		 loop2 
			 (return (f-exp (caar v-l) v-tr v-t))))))) 
;--- f-seqp - v-l : sequence of s-expressions and labels to evaluate
;	    - v-r : psreg in which to store the final result
;	    - v-t : tail.
;	This will do the top level of prog bodies 
;
(def f-seqp 
  (lambda (v-l v-r v-t) 
	  (do ((l (reverse v-l) (cdr l))
	       (newreg v-r)
	       (reg v-r newreg))
	      ((null l) v-t)
	      (cond ((symbolp (car l))
		     (setq v-t (f-labl v-t (car l))))
		    (t (setq v-t (f-exp (car l) reg v-t))
		       (setq newreg (Gensym nil)))))))

;--- f-seq - v-l : sequence of s-expressions to evaluate
;	   - v-r : psreg in which to store the final result
;	   - v-t : tail
;
;	This generates intermediate codes to calculate the s-expressions
;	in v-l.  This does not look for labels.
;
(def f-seq
  (lambda (v-l v-r v-t)
	  (do ((l (reverse v-l) (cdr l))
	       (reg v-r (Gensym nil)))
	      ((null l) v-t)
	      (setq v-t (f-exp (car l) reg v-t)))))

;--- f-pusha - v-l : list of forms to evaluate and push on stack
;	     - v-r : register to place result of last expr in 
;	     - v-t : tail
;	emits code to to evaluate and push forms on the stack.
(def f-pusha
  (lambda (v-l v-r v-t)
	  (cond ((null v-l) v-t) 
		(t (do ((ll (reverse v-l) (cdr ll)) 
			(reg v-r (Gensym nil))
			(res v-t
			     (f-exp (car ll) 
				    reg
				    (f-addi `(push ,(f-use reg)) res))))
		       ((null ll) res))))))

;--- f-iter - v-e : list of expression to evaluate
;	    - v-v : list of variables those expressions will be bound to
;	This checks of the given expressions can be bound to the given
;	variables with no conflicts.  This is determining if tail
;	merging is possible were we replace recursion by iteration.
;
(def f-iter
  (lambda (v-e v-v) 
	  (prog (v-y w-vars) 

	  loop 
		(cond ((null v-e) (return t)) 
		      ((null v-v) (go bad)) 
		      ((ifflag (setq v-y (car v-v)) x-spec) (go bad)) 
		      ((equal (car v-e) v-y) (go usable))
		      (t (go check)))
	  next 
		(setq w-vars (cons v-y w-vars)) 
	  usable 
		(setq v-e (cdr v-e)) 
		(setq v-v (cdr v-v)) 
		(go loop) 
	  check 
	        (cond ((f-nice (car v-e)) (go next))) 
	  bad 
		(return nil)))) 

(def f-nice 
  (lambda (v-e)
	  (cond ((atom v-e) (not (member v-e w-vars))) 
		((atom (car v-e)) 
		 (cond ((eq (car v-e) 'quote) t) 
		       ((ifflag (car v-e) x-dont) nil) 
		       (t (f-all v-e 'f-nice)))) 
		(t (f-all v-e 'f-nice))))) 

;--- f-all - v-l : list
;	   - v-f : function
;	mapc function v-f over v-l as long as the result is non nil
;
(def f-all 
  (lambda (v-l v-f) 
	  (cond ((null v-l) t) 
		((funcall v-f (car v-l)) (f-all (cdr v-l) v-f)) 
		(t nil)))) 

(def f-make 
  (lambda (v-r v-v) 
	  (put v-r x-reg v-v))) 

;--- f-leap - v-t : tail
;	We generate and place in global variable s-inst an itermediate
;	instructin which will jump to the current top location in v-t.
;	If there is not a label on top of v-t, one is added.
;
(def f-leap 
  (lambda (v-t) 
	  (cond ((not (setq s-inst (get (caar v-t) x-leap))) 
		 (setq v-t (f-labl v-t nil)) 
		 (setq s-inst 'go))) 
	  (setq s-inst (list s-inst (cadar v-t))) 
	  v-t)) 

;--- f-labl - v-t : tail
;	    - v-l : real label or nil
;	We insure that there is a label on top of v-t. If not we
;	create one. If we are given a label, we associate it with
;	a created label. 
;	Labels in v-t are all gensymed and the association is all
;	on the property list of the value of w-labs.
; Errors: duplicate labels
;
(def f-labl 
  (lambda (v-t v-l) 
	  (prog (v-i) 
		(cond ((eq (caar v-t) 'label) 
		       (cond (v-l (cond ((setq v-i (get w-labs v-l)))
					(t (put w-labs v-l (cadar v-t))
					   (return v-t))))
			     (t (return v-t)))) 
		      
		      ((null v-l) (setq v-i (Gensym nil))) 
		      ((setq v-i (get w-labs v-l))) 
		      (t (put w-labs v-l (setq v-i (Gensym nil))))) 
		(return (f-addi (list 'label v-i) v-t))))) 

(def f-test 
  (lambda (v-t) 
	  (and (eq (caar v-t) 'minus) 
	       (null (caddar v-t))))) 

(def f-vble 
  (lambda (v-v v-r) 
	  (f-use v-r) 
	  (cond ((not (symbolp v-v)) v-v) 
		((null v-v) nil)
		((f-con v-v) v-v) 
		((ifflag v-v x-spec) v-v) 
		((member v-v w-vars) v-v) 
		(t (setq k-free (cons v-v k-free)) 
		   (flag v-v x-spec))))) 

(def f-addi 
  (lambda (v-i v-t) 
	  (prog (v-o) 
		(cond ((not (setq v-o (get (car v-i) x-opt))) (go normal)) 
		      ((setq v-o (funcall v-o  v-i v-t)) (return v-o)))
	   normal 
		(return (cons v-i v-t))))) 

(def f-reg 
  (lambda (v-f) 
	  (cond ((numberp v-f) (put (Gensym nil) x-reg v-f)) 
		(v-f (flag (Gensym nil) v-f)) 
		(t (Gensym nil))))) 

(def f-con 
  (lambda (v-v) 
	  (cond ((ifflag v-v x-spec) nil)
		(t (ifflag v-v x-con)))))

(def f-one 
  (lambda (v-e) 
	  (or (atom v-e) 
	      (eq (car v-e) 'quote)))) 

(def f-swap 
  (lambda (v-t) 
	  (cond ((eq (caar v-t) 'get) (f-swap (cdr v-t))) 
		(t (rplaca (car v-t) 
			   (cond ((eq (caar v-t) 'true) 'false) 
				 (t 'true))))) 
	  v-t)) 

(def f-xval 
  (lambda (v-t v-r) 
	  (cond ((or (eq (caar v-t) 'get) 
		     (eq (caddar v-t) 'amb)) v-t) 
		(t (f-addi (list 'get (f-use v-r) (caddar v-t)) v-t))))) 

;--- f-use - v-r :  psreg whose value is being used
;	we keep track of the number of times the value of a register is
;	used,  the count is kept under the indicator x-count in the
;	psreg's property list.  the count starts at nil, goes to `used'
;	and then to `force'.  Once the count goes to `force' all gets
;	must be done. when the count is used get should look to see
;	if the following intermediate code instruction is the one
;	using the register and in that case it can merge with that
;	instruction
;
(def f-use
  (lambda (v-r)
	  ((lambda (curv)
		   (cond (curv (cond ((not (eq curv 'force)) 
				      (putprop v-r 'force 'x-count))))
			 (t (putprop v-r 'used 'x-count)))
		   v-r)
	   (get v-r 'x-count))))


(def f-chop 
  (lambda (v-t) 
	  (cond ((or (eq (caar v-t) 'label) 
		     (eq (caar v-t) 'end)) v-t) 
		(t (f-chop (cdr v-t)))))) 

(def f-tfo 
  (lambda (v-i v-t) 
	  (cond ((not (f-like v-t '(go label))) nil) 
		((not (equal (cadr v-i) (cadadr v-t))) nil) 
		(t (rplaca (cdr v-i) (cadar v-t)) 
		   (f-swap (rplaca v-t v-i)))))) 

(def f-like 
  (lambda (v-t v-p) 
	  (cond ((null v-p) t) 
		((null v-t) nil) 
		((equal (caar v-t) (car v-p)) (f-like (cdr v-t) (cdr v-p))) 
		(t nil)))) 

(def f-aor 
  (lambda (v-l v-e v-r v-t) 
	  (cond ((null v-l) 
		 (f-addi (list 'get (f-use v-r) (eq v-e 'and)) v-t)) 
		(t (prog (v-j v-dv v-tr v-tr2) 
			 (setq v-dv (eq v-e 'or))
			 (setq v-tr v-r)
			 (setq v-tr2 v-r)
			 (setq v-e 
			       (cond ((eq v-e 'and) 'false) 
				     (t 'true))) 
			 (setq v-l (reverse v-l)) 
			 (cond ((null (cdr v-l)) (go loop)) 
			       ((and (f-test v-t) 
				     (not (eq (caadr v-t) 'get))) 
				(cond ((eq (caddadr v-t) 'amb) 
				       (setq v-dv 'amb) 
				       (setq v-tr2 (f-reg nil))) 
				      ((not (equal (caddadr v-t) v-dv)) 
				       (setq v-dv 'amb))) 
				(cond ((equal (caadr v-t) v-e) 
				       (setq v-j (cadadr v-t)) 
				       (go loop))) 
				(rplacd (cdr v-t) (f-leap (cddr v-t)))) 
			       (t (setq v-t (f-leap v-t)))) 
			 (setq v-j (cadr s-inst)) 
		  loop 
			 (setq v-t (f-exp (car v-l) v-tr v-t)) 
			 (setq v-tr v-tr2) 
			 (cond ((null (setq v-l (cdr v-l))) (return v-t))) 
			 (setq v-t (f-addi (list v-e v-j v-dv) v-t)) 
			 (setq v-t (f-addi (list 'minus (f-use v-tr) nil) v-t)) 
			 (go loop)))))) 

(def f-repl 
  (lambda (v-e) 
	  (cons (ucar (car v-e)) (cdr v-e)))) 

;this seems out of date, must change to mapconvert
(def f-domap
  (lambda (v-e) 
	  (prog (v-x) 
		(cond ((setq v-x (f-chkf (cadr v-e) 4)) 
		       (return (list (car v-e) 
				     (list 'quote v-x)
				     (caddr v-e)))) 
		      (t (return v-e))))))


;--- mapconvert - access : function to access parts of lists
;		- join	 : function to join results
;		- resu	 : function to apply to result
;		- form	 : mapping form
;	This function converts maps to an equivalent do form.
;
(def mapconvert
  (lambda (access join resu form )
	  (prog (vrbls finvar acc accform compform tmp)

		(setq finvar (Gensym 'X)   ; holds result

		      vrbls (maplist '(lambda (arg)
					((lambda (temp)
					    (cond ((or resu (cdr arg))
						   `(,temp ,(car arg)
							   (cdr ,temp)))
						  (t `(,temp 
						       (setq ,finvar ,(car arg))
						       (cdr ,temp)))))
					 (Gensym 'X)))
				    (cdr form))


		      acc (mapcar '(lambda (tem)
					   (cond (access `(,access ,(car tem)))
						 (t (car tem))))
				  vrbls)

		      accform (cond ((or (atom (setq tmp (car form)))
					 (null (setq tmp (cmacroexpand tmp)))
					 (not (member (car tmp) '(quote function))))
				     `(funcall ,tmp ,@acc))
				    (t `(,(cadr tmp) ,@acc))))
		(return
		 `((lambda (,finvar)
		    (do ( ,@vrbls)
			((null ,(caar vrbls)))
			,(cond (join `(setq ,finvar (,join ,accform ,finvar)))
			       (t accform)))
		    ,(cond (resu `(,resu ,finvar))
			   (t finvar)))
		   nil )))))
(putprop 'mapc 'f-mapc 'x-spfm)
(def f-mapc
  (lambda (v-e)
	  (mapconvert 'car nil nil (cdr v-e))))

(putprop 'mapcar 'f-mapcar 'x-spfm)
(def f-mapcar
  (lambda (v-e)
	  (mapconvert 'car 'cons 'reverse (cdr v-e))))

(putprop 'map 'f-map 'x-spfm)
(def f-map
  (lambda (v-e)
	  (mapconvert nil nil nil (cdr v-e))))


(putprop 'maplist 'f-maplist 'x-spfm)
(def f-maplist
  (lambda (v-e)
	  (mapconvert nil 'cons 'reverse (cdr v-e))))




(def f-initv
  (lambda (v-l)
	  (mapcar 'car (car v-l))))

(def f-inits
  (lambda (v-l)
	  (mapcar 'cadr (car v-l))))

(def f-repv
  (lambda (v-l)
	  (prog (v-x)
		(setq v-l (car v-l))
	   lp 
		(cond ((null v-l) (return (reverse v-x))))
		(cond ((cddar v-l) (setq v-x (cons (caar v-l) v-x))))
		(setq v-l (cdr v-l))
		(go lp))))

(def f-reps
  (lambda (v-l)
	  (prog (v-x v-y)
		(setq v-l (car v-l))
	   lp 
		(cond ((null v-l) (return (reverse v-x))))
		(cond ((cddar v-l) 
		       (setq v-y (caddar v-l)) (setq v-x (cons v-y v-x))))
		(setq v-l (cdr v-l))
		(go lp))))

(def f-endtest
  (lambda (v-l)
	  (caadr v-l)))

(def f-endbody
  (lambda (v-l)
	  (cdadr v-l)))

(def f-dobody
  (lambda (v-l)
	  (cddr v-l)))


(putprop 'do 'f-do 'x-spf)

(def f-do
  (lambda (v-l v-r v-t)
    (prog (v-init v-initv v-rep v-repv v-loop v-outl v-retl)
	(cond ((and (car v-l) (atom (car v-l)))	; look for old do
	       (setq v-l (olddo-to-newdo v-l))))
	(setq v-initv (f-initv v-l)
	      v-init (f-inits v-l)
	      v-repv (f-repv v-l)
	      v-rep (f-reps v-l)
	      v-retl (Gensym nil)
	      v-loop (Gensym nil)
	      v-outl (Gensym nil))
	(w-save)
	(return
	 (f-pusha v-init v-r
	    (prog (w-ret w-labs tmp)
		  (setq w-ret `(,v-r . (go ,v-retl)))
		  (setq w-labs (Gensym nil))
		  (setq tmp 
		   `((begin ,(length v-initv))
		     ,@(mapcar '(lambda (arg) (setq w-locs
							   (cons arg w-locs))
					     `(bind ,arg))
			       v-initv)
		     (label ,v-loop)
		     ,@(f-exp (f-endtest v-l) v-r
			      `((minus ,v-r nil)
				(true ,v-outl nil)
				,@(f-seqp (f-dobody v-l) v-r
					  (f-pusha v-rep v-r
					      `((dopop ,v-repv)
						(go ,v-loop)
						(label ,v-outl)
						,@(f-seq (f-endbody v-l) v-r
							 `((end ,v-retl)
							   ,@v-t)))))))))
		  (w-unsave)
		  (return tmp)))))))

(def olddo-to-newdo
  (lambda (v-l)
	  `(((,(car v-l) ,(cadr v-l) ,(caddr v-l)))
	    (,(cadddr v-l) nil)
	    ,@(cddddr v-l))))

(putprop 'cond 'f-cond 'x-spf)

(def f-cond	
  (lambda (v-l v-r v-t) 
	  (setq v-t (f-leap v-t)) 
	  (f-if v-l v-r s-inst v-t))) 

(putprop 'quote 'f-quote 'x-spf)

(def f-quote 
  (lambda (v-l v-r v-t) 
        (f-addi (list 'get v-r (cons 'quote v-l)) v-t))) 

(putprop 'prog 'f-prog 'x-spf)




(putprop 'setq 'f-setq 'x-spf)

(def f-setq 
  (lambda (v-l v-r v-t)
	  (cond ((null (car v-l)) v-t))
	  (do ((ll (reverse v-l) (cddr ll))
	       (reg v-r (Gensym nil)))
	      ((null ll) v-t)
	      (setq v-t (f-exp (car ll)
			       reg
			       `((set ,(f-use reg) ,(g-specialchk (cadr ll)))
				 ,@v-t))))))


(putprop 'rplaca 'f-rplaca 'x-spf)


(def f-rplaca 
  (lambda (v-l v-r v-t)
	  (cond ((f-one (cadr v-l))
		 (f-exp (car v-l) 
			v-r
			(f-exp (cadr v-l) 
			       (setq v-l (Gensym nil))
			       (f-addi (list 'seta (f-use v-r) (f-use v-l))
				       v-t))))
		(t (f-pusha v-l 
			    (Gensym nil)
			    (f-addi (list 'setas v-r) v-t))))))

(putprop 'rplacd 'f-rplacd 'x-spf)


(def f-rplacd 
  (lambda (v-l v-r v-t)
	  (cond ((f-one (cadr v-l))
		 (f-exp (car v-l)
			v-r
			(f-exp (cadr v-l)
			       (setq v-l (Gensym nil))
			       (f-addi (list 'setd (f-use v-r) (f-use v-l)) v-t))))
		(t (f-pusha v-l 
			    (Gensym nil)
			    (f-addi (list 'setds (f-use v-r)) v-t))))))

(putprop 'go 'f-go 'x-spf)

;--- f-go - v-l : label to go to
;	  - v-r : not used
;	  - v-t : tail
; We allow non local go to's, however the goto must go no further than the
; first inclosing prog.
; f-go works by finding the w-labs associated with the first enclosing prog,
; and keeping track of the number of binding levels which must be traversed
; to get to that prog.o
; when it finds the correct w-labs, it checks if this label has been seen yet,
; if not iit assigns it a gensymed symbol.  
; if a binding level must be traversed, we eimit
;	(unbind n)	n is number of binding levels to traverse, 
;			0 means current level only.
;	(go gensymedlabl)
;
; if this is a local goto only the (go gensymedlabl) will be emitted.
;
(def f-go 
  (lambda (v-l v-r v-t) 
	  (prog (use-labs levels)
		(setq v-l (car v-l)) 
		(setq use-labs
		      (cond (w-ret w-labs)
			    (t (do ((ll w-save (cdr ll))
				    (count 0 (add1 count)))
				   ((null ll)
				    (comp-err " go not within prog"))
				   (cond ((caar ll)
					  (setq levels count)
					  (comp-warn " non-local go used")
					  (return (cadar ll))))))))
		
		(cond ((not (setq v-r (get use-labs v-l))) 
		       (put use-labs v-l (setq v-r (Gensym nil))))) 
		(setq v-t (f-addi (list 'go v-r) v-t))
		(cond (levels (setq v-t (f-addi `(unbind ,levels) v-t))))
		(return v-t)))) 

(putprop 'lambda 'f-lambda 'x-spf)

;--- f-lambda - ?? how is this routine called, certainly this isnt the
;		same as ((lambda (n) form)  arg)
;

(putprop 'and 'f-and 'x-spf)

(def f-and 
  (lambda (v-l v-r v-t) 
        (f-aor v-l 'and v-r v-t))) 

(putprop 'or 'f-or 'x-spf)

(def f-or 
  (lambda (v-l v-r v-t) 
        (f-aor v-l 'or v-r v-t))) 



(putprop 'prog2 'prog2toprog 'x-spfm)


;--- prog2toprog - v-e : prog2 expression
; we convert this (prog2 a b c d e f) to
;	(progn a ((lambda (newsim) c d e f newsim) b))
; simple enough.
;
(def prog2toprog
  (lambda (v-e)
	  ((lambda (newsim)
		   `(progn ,(cadr v-e)
			   ((lambda (,newsim)
				    ,@(cdddr v-e)
				    ,newsim)
			    ,(caddr v-e))))
	   (Gensym nil))))


(putprop 'progn 'f-seq 'x-spf)

(putprop 'return 'f-return 'x-spfn)

;--- f-return - v-l : arg to return, may be nil meaning return nil
;	      - v-r : psreg in which to store result
;	      - v-t : tail
;	this handles the return statement.  While returns should
;	occur in progs, this allows for a return inside a context
;	which is inside a prog (or do).  If this is a simple return
;	from prog or do, we have:
;		... code to place to be returned val in v-r
;	      (go retlb)	jump to label at end of prog body
;				but before special unbinding
;	for non local cases we have
;	 	...   code to place value to be returned into v-r
;	      (unwind levels)  where is levels is the number of enclosing
;		      contexts (which begin with a (begin xx)) to return
;		      from.
;	      (go retlb)	then go to the return spot.
;
(def f-return
  (lambda (v-l v-r v-t) 
	  (prog (use-ret levels)
		(setq use-ret 
		      (cond (w-ret)
			    (t (do ((ll w-save (cdr ll))
				    (count 0 (add1 count)))
				   ((null ll) 
				    (comp-err " return not within a prog"))
				   (cond ((caar ll)
					  (setq levels count)
					  (comp-warn " non local return used")
					  (return (caar ll))))))))
		
		(setq v-t (f-addi (cdr use-ret) v-t)) 
		(cond (levels (setq v-t (f-addi `(unbind ,levels) v-t))))
		(return (f-exp (and v-l (car v-l)) (f-use (car use-ret)) v-t))))) 

(putprop 'null 'f-null 'x-spfn)

(def f-null 
  (lambda (v-l v-r v-t) 
	  (cond ((f-test v-t) 
		 (rplaca (cdar (rplacd v-t (f-xval (f-swap (cdr v-t)) v-r)))
			 (f-use (setq v-r (Gensym nil))))
		 (f-exp (car v-l) v-r v-t)))))

(putprop 'not 'f-null 'x-spfn)


(def f-type 
  (lambda (v-l v-r v-t v-bits) 
	  (cond ((f-test v-t) 
		 (setq v-t (f-xval (cdr v-t) v-r))
		 (f-exp (car v-l) 
			(setq v-r (Gensym nil))
			(f-addi (list 'getype (f-use v-r) v-bits) v-t)))))) 

(putprop 'atom 'f-atom 'x-spfn)

(def f-atom 
  (lambda (v-l v-r v-t)
	  (f-type v-l v-r v-t '(0 1 2 4 5 6 7 9 10))))

(putprop 'numberp 'f-numberp 'x-spfn)

(def f-numberp 
  (lambda (v-l v-r v-t) 
	  (f-type v-l v-r v-t '(2 4 9))))

(putprop 'symbolp 'f-symbolp 'x-spfn)

(def f-symbolp 
  (lambda (v-l v-r v-t)
	(f-type v-l v-r v-t 1)))

(putprop 'dtpr 'f-dtpr 'x-spfn)

(def f-dtpr 
  (lambda (v-l v-r v-t)
	  (f-type v-l v-r v-t 3)))

(putprop 'bcdp 'f-bcdp 'x-spfn)

(def f-bcdp 
  (lambda (v-l v-r v-t)
	  (f-type v-l v-r v-t 5)))

(putprop 'stringp 'f-stringp 'x-spfn)

(def f-stringp 
  (lambda (v-l v-r v-t)
	  (f-type v-l v-r v-t 0)))

(putprop 'type 'f-ty 'x-spfn)

(def f-ty 
  (lambda (v-l v-r v-t)
	  (f-exp (car v-l) 
		 (setq v-r (Gensym nil))
		 (f-addi (list 'getype (f-use v-r) 'name) v-t))))

(putprop 'eq 'f-eq 'x-spfn)

(def f-eq 
  (lambda (v-l v-r v-t)
	  (prog (v-r1)
		(cond ((f-test v-t)
		       (setq v-t (f-xval (cdr v-t) v-r))
		       (cond ((and (f-one (car v-l)) (f-one (cadr v-l)))
			      (return (f-addi (list 'eqv (car v-l) (cadr v-l))
					      v-t))))
		       (return (f-pusha v-l 
					(Gensym nil)
					(f-addi '(eqs) v-t))))))))

(putprop 'cons 'f-repl 'x-spfh)

'(putprop 'map 'f-domap 'x-spfh)

'(putprop 'mapc 'f-domap 'x-spfh)

'(putprop 'mapcar 'f-domap 'x-spfh)

'(putprop 'maplist 'f-domap 'x-spfh)

(putprop 'zerop 'f-zerop 'x-spfm)

(def f-zerop 
  (lambda (v-e)
	  (list 'equal 0 (cadr v-e))))

(putprop 'plist 'f-plist 'x-spfm)

(def f-plist 
  (lambda (v-e)
	  (list 'car (cadr v-e))))

(putprop 'go 'f-xgo 'x-opt)

(def f-xgo 
  (lambda (v-i v-t) 
	  (setq v-t (f-chop v-t)) 
	  (cond ((equal (cadr v-i) (cadar v-t)) v-t) 
		(t (cons v-i v-t))))) 

(putprop 'return 'f-xreturn 'x-opt)

(def f-xreturn 
  (lambda (v-i v-t) 
	  (cons v-i (f-chop v-t)))) 

(putprop 'repeat 'f-xreturn 'x-opt)

(putprop 'false 'f-tfo 'x-opt) 

(putprop 'true 'f-tfo 'x-opt) 


(putprop '*catch 'f-*catch 'x-spf)


;--- f-*catch - v-l : list of (tag exp) , tag is evaled, exp is to be run
;	     - v-r : result register
;	     - v-t : tail
;	This compiles a catch by emiting these intermediate codes:
;	..calculate tag..
;	(catchent <gensym> <tag> nil)
;	 .. code to eval (car v-l) ..
;	(catchexit)
;	(label <gensym>)
;
;	The catchent sets up a catch frame on the c-runtime stack.
;	The (car v-l) is evaluated and the result placed in r0 (it must
;	be since that is where the value would be thrown). If no throw
;	is done, it enters the catchexit which pops our catchframe off
;	the stack. If a throw is done it ends up at the label <gensym>
;	with the catch frame already popped off.
;
(def f-*catch
  (lambda (v-l v-r v-t)
	  (prog (v-loop v-tag x y z v-nr)
		(setq v-tag (car v-l))
		; we check to make sure we can force v-r to be r0, else
		; we must give up.
		(cond ((and  (get v-r 'x-reg) 
			     (not (equal (get v-r 'x-reg) 0)))
		       (err '"Can't compile catch correctly"))
		      (t (f-make v-r 0)))

		(return
		 (f-exp v-tag
			(setq v-nr (Gensym nil))
			(f-addi `(catchent ,(setq v-loop (Gensym nil)) 
					   ,(f-use v-nr)
					   nil)
				(f-exp (cadr v-l) (f-use v-r)
				       (f-addi `(catchexit)
					       (f-addi `(label ,v-loop) v-t)))))))))

(putprop 'errset 'f-errset 'x-spf)
;--- f-errset - v-l : list of (errset form [flag])
;	      - v-r : place to put result.
;	      - v-t : tail
;
;	This sets up an errset frame.  It is different than a catch in
;	that the tag is always (ER%all) and the result returned upon
;	a regular exit is listified.
;	again, we must insure that v-r can be forced to be r0 since
;	an err or error will place the result there.
;
(def f-errset
  (lambda (v-l v-r v-t)
	  (prog (v-loop v-tag v-flag v-nr)
		(cond ((and (get v-r 'x-reg) (not (equal (get v-r 'x-reg) 0)))
		       (err '"Can't compile errset  correctly"))
		      (t (f-make v-r 0)))

		; flag tells if error message will be reported, t if so.
		; t is the default
		(cond ((cdr v-l) (setq v-flag (cadr v-l)))
		      (t (setq v-flag t)))

		(return
		  (f-exp v-flag
			 (setq v-nr (Gensym nil))
			 (f-addi `(catchent ,(setq v-loop (Gensym nil)) 
					    '(ER%all)
					    ,(f-use v-nr))
				 (f-exp (car v-l)
					v-r
					`((catchexit)
					  (push ,v-r)
					  (call ,v-r _Lncons 1)
					  (label ,v-loop)
					  ,@v-t))))))))




(putprop '*throw 'f-*throw 'x-spf)

;--- f-*throw - v-l : list of (tag exp)
;	     - v-r : loc to eval exp to
;	     - v-t : tail
;
(def f-*throw
  (lambda (v-l v-r v-t)
	  (let ((v-nr (Gensym nil)))
	       (f-exp (car v-l) 
		      v-nr
		       (f-exp (cadr v-l) v-r
			      (f-addi `(*throw ,(f-use v-r) ,(f-use v-nr)) v-t))))))


(putprop 'arg 'f-arg 'x-spf)

;--- f-arg - v-l : list of arg to evaluate
;	   - v-r : place to store value
;	   - v-t : tail
(def f-arg
  (lambda (v-l v-r v-t)
	  (f-exp (car v-l) v-r
		 (f-addi `(arg ,(f-use v-r))
			 v-t))))
