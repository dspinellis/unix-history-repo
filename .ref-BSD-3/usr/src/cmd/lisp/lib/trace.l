;---- The Joseph Lister Trace Package, v1
;         John Foderaro, Sept 1979
;------------------------------------------------------------------;
; Copyright (c) 1979 The Regents of the University of California   ;
;	All rights reserved.					   ;
;------------------------------------------------------------------;
(eval-when (eval)
  (setq old-read-table-trace readtable)
  (setq readtable (makereadtable t))
  (load 'backquote))
(cond ((null (boundp '$$traced-functions$$)) (setq $$traced-functions$$ nil)))
(setq $$trace-indent$$ 0)

;----> It is important that the trace package not use traced functions
;	thus we give the functions the trace package uses different
;	names and make them equivalent at this time to their 
;	traceable counterparts.  

(do ((i '( (add1 T-add1)(append T-append)
	   (apply T-apply)(atom T-atom)(bcdp T-bcdp)
	   (car T-car)(cadr T-cadr)(cdr T-cdr)(cons T-cons) (delq T-delq)
	   (drain T-drain)
	   (dtpr T-dtpr) (eq T-eq) (eval T-eval)(funcall T-funcall)
	   (get T-get) (getd T-getd)(getdisc T-getdisc)
   	   (greaterp T-greaterp)(memq T-memq)(not T-not)
	   (null T-null) (patom T-patom) (print T-print) 
	   (prog T-prog)
	   (or T-or) (patom T-patom)(putd T-putd) 
	   (putprop T-putprop)
	   (read T-read)(remprop T-remprop) (reverse T-reverse)
	   (set T-set)
	   (setq T-setq) (sub1 T-sub1) (terpr T-terpr)
	   (zerop T-zerop))
	(cdr i)))
    ((null i))
    (putd (cadar i) (getd (caar i)))
    (putprop (cadar i) t 'Untraceable))

;--- trace - arg1,arg2, ... names of functions to trace
;	This is the main user callable trace routine. 
; work in progress, documentation incomplete since im not sure exactly
; where this is going.	
;
(def trace
  (nlambda (argl)
   (prog (if ifnot evalin evalout funnm funcd did break)

    ; process each argument	 

    (do ((ll argl (cdr ll))
	 (funnm) 
	 (funcd))
	((T-null ll))
	(T-setq funnm (car ll)
		if t
		break nil
		ifnot nil
		evalin nil
		evalout nil)

	; a list as an argument means that the user is specifying
	; conditions on the trace
	(cond ((T-not (atom funnm))
	       (cond ((T-not (T-atom (T-setq funnm (car funnm))))
		      (T-print (car funnm))
		      (T-patom '" is non an function name")
		      (go botloop)))
	       (do ((rr (cdar ll) (cdr rr)))
		   ((T-null rr))
		   (cond ((T-memq (T-car rr) '(if ifnot evalin evalout))
			  (T-set (T-car rr) (T-cadr rr))
			  (T-setq rr    (T-cdr rr)))
			 ((T-eq (T-car rr) 'evalinout)
			  (T-setq evalin (T-setq evalout (T-cadr rr))
				rr (T-cdr rr)))
			 ((T-eq (T-car rr) 'break)
			  (T-setq break t))
			 (t (T-patom '"bad request ")
			    (T-print (T-car rr)))))))


	 ; if function is already traced, untrace it first
	 (cond ((get funnm 'T-original) 
		(apply 'untrace `(,funnm))
		(T-setq did (T-cons `(,funnm untraced) did))))

	 ; we must determine the type of function being traced
	 ; in order to create the correct replacement function	

	 (cond ((T-setq funcd (T-getd funnm))
	       (cond ((T-bcdp funcd)		; machine code
		      (cond ((T-eq 'lambda (T-getdisc funcd))
			     (T-setq typ 'lambda))
			    ((T-eq 'nlambda (T-getdisc funcd))
			     (T-setq typ 'nlambda))
			    (t (T-patom '"Unknown type of compiled function")
			       (T-print funnm)
			       (T-setq typ nil))))

		     ((T-dtpr funcd)		; lisp coded
		      (cond ((T-or (T-eq 'lambda (T-car funcd))
				 (T-eq 'lexpr (T-car funcd)))
			     (T-setq typ 'lambda))
			    ((T-or (T-eq 'nlambda (T-car funcd))
				   (T-eq 'macro (T-car funcd)))
			     (T-setq typ 'nlambda))
			    (t (T-patom '"Bad function definition: ")
			       (T-print funnm)
			       (T-setq typ nil))))
		     (t (T-patom '"Bad function defintion: ")
			(T-print funnm)))
	       
 	       ; now that the arguments have been examined for this
	       ; function, do the tracing stuff.
	       ; First save the old function on the property list

	       (T-putprop funnm funcd 'T-original)

	       ; now build a replacement

	       (cond ((T-eq typ 'lambda)
		      (T-eval 
			   `(def ,funnm
			      (lexpr (nargs)
				(prog (T-arglst T-res T-rslt)	 
				      (do ((i nargs (T-sub1 i)))
					  ((T-zerop i))
					  (T-setq T-arglst 
						  (T-cons (arg i) T-arglst)))
				      (cond ((T-setq T-res 
						   (and ,if
							(not ,ifnot)))
					     (T-traceenter ,funnm)
					     (T-print T-arglst)
					     (T-terpr)
					     ,evalin
					     (cond (,break (trace-break)))))
				      (T-setq T-rslt 
					    (T-apply ',funcd T-arglst))
				      (cond (T-res 
					     ,evalout 
					     (T-traceexit ',funnm T-rslt)
					     (T-terpr)))
				      (return T-rslt)))))
		      (T-setq did (T-cons funnm did)
			      $$traced-functions$$ (cons funnm
							 $$traced-functions$$)))

		     ((T-eq typ 'nlambda)
		      (T-eval 
		       `(def ,funnm
			    (nlambda (T-arglst)
				     (prog ( T-res T-rslt)	 
					   (cond ((T-setq T-res 
							  (and ,if
							       (not ,ifnot)))
						  (T-traceenter ,funnm)
						  (T-print T-arglst)
						  (T-terpr)
						  ,evalin
						  (cond (,break (trace-break)))))
					   (T-setq T-rslt 
						   (T-apply ',funcd T-arglst))
					   (cond (T-res 
						  ,evalout 
						  (T-traceexit ',funnm T-rslt)
						  (T-terpr)))
					   (return T-rslt)))))
		      (T-setq did (T-cons funnm did)
			      $$traced-functions$$ (T-cons funnm
							   $$traced-functions$$)))
		     
		     (t (T-patom '"No such function as: ")
			(T-print funnm)
			(T-terpr))))))
	 (return (nreverse did)))))



(def untrace
  (nlambda (argl)
	   (cond ((T-null argl) (T-setq argl $$traced-functions$$)))

	   (do ((i argl (T-cdr i))
		(curf)
		(res))
	       ((T-null i)  
		(cond ((T-null $$traced-functions$$)
		       (T-setq $$trace-indent$$ 0)))
		res)
	       (cond ((T-setq tmp (T-get (T-setq curf (T-car i)) 'T-original))
		      (T-putd curf tmp)
		      (T-remprop curf 'T-original)
		      (T-remprop curf 'entercount)
		      (T-setq $$traced-functions$$ 
			      (T-delq curf $$traced-functions$$))
		      (T-setq res (T-cons curf res)))
		     (t (T-setq res (T-cons `(,curf not traced) res)))))))


;--- T-traceenter - funnm : name of function just entered
;		  - count : count to print out
;	This routine is called to print the entry banner for a
;	traced function.
;
(def T-traceenter
  (nlambda (nm)
	   (T-prog (name count)
		 (T-setq name (T-car nm))
		 (cond ((T-null (T-setq count (T-get name 'entercount)))
			(T-setq count 1)))
		 (T-putprop name (add1 count) 'entercount)

		 (do ((i 1 (T-add1 i)))
		     ((T-greaterp i $$trace-indent$$))
		     (T-patom '" "))
		 (T-setq $$trace-indent$$ (T-add1 $$trace-indent$$))
		 (T-print count)
		 (T-patom '" <Enter> ")
		 (T-print name)
		 (T-patom '" "))))

(def T-traceexit
  (lambda (name retval)
	  (T-prog (count)
		(T-putprop name 
			 (T-setq count (T-sub1 (T-get name 'entercount)))
			 'entercount)
		(do ((i 1 (T-add1 i))
		     (over (T-setq $$trace-indent$$ (T-sub1 $$trace-indent$$))))
		    ((T-greaterp i over))
		    (T-patom '" "))

		(T-print count)
		(T-patom '" <EXIT>  ")
		(T-print name)
		(T-patom '"  ")
		(T-print retval)
		(return retval))))

; trace-break  - this is the trace break loop
(def trace-break
  (lambda nil
	 (prog (tracevalread)
	       (T-terpr) (T-patom '"[tracebreak]")
	loop   (T-terpr)
	       (T-patom '"T>")
	       (T-drain)
	       (cond ((eq '<EOF> (T-setq tracevalread
					 (car
					  (errset (T-read nil '<EOF>)))))
		      (return nil)))
	       (T-print (car (errset (T-eval tracevalread))))
	       (go loop))))

(eval-when (eval)
  (setq readtable old-read-table-trace))
