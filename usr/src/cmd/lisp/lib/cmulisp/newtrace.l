(setq SCCS-trace "@(#)trace.l	1.5	11/7/80")
; LWE 11/25/80: Maybe this will make it behave like interpreted:
(declare (special tmp typ $$traced-functions$$))
;---- The Joseph Lister Trace Package, v1
;         John Foderaro, Sept 1979
;------------------------------------------------------------------;
; Copyright (c) 1979 The Regents of the University of California   ;
;	All rights reserved.					   ;
;------------------------------------------------------------------;
(eval-when (eval)
  (setq old-read-table-trace readtable)
; LWE 1/11/81 DNC didn't like this, so I don't either.
; He says: "because we LIKE our readtable -- it allows us to read this file."
;  (setq readtable (makereadtable t))
  (setq old-uctolc-value (status uctolc))
  (sstatus uctolc nil)		; turn off case conversion
; LWE 1/11/81 DNC didn't like this one, either.
; He says: "we have our own backquote."
;  (load 'backquote)
  )

(declare (nlambda T-status T-sstatus)
  (special piport
	   if ifnot evalin evalout 
	   printargs printres evfcn
	   traceenter traceexit
	   prinlevel prinlength
	   $$functions-in-trace$$	; active functions 
	   $$funcargs-in-trace$$	; arguments to active functions.
	   ))


(cond ((null (boundp '$$traced-functions$$)) (setq $$traced-functions$$ nil)))
(cond ((null (boundp '$$functions-in-trace$$)) (setq $$functions-in-trace$$ nil)))
(cond ((null (boundp '$$funcargs-in-trace$$)) (setq $$funcargs-in-trace$$ nil)))

;----> It is important that the trace package not use traced functions
;	thus we give the functions the trace package uses different
;	names and make them equivalent at this time to their 
;	traceable counterparts.  

(do ((i '( (add1 T-add1)(append T-append)
	   (and T-and)  (apply T-apply)
	   (cond T-cond) (cons T-cons) (delq T-delq)
	   (def T-def) (do T-do) (drain T-drain)
	   (dtpr T-dtpr)  (eval T-eval)(funcall T-funcall)
	   (get T-get) (getd T-getd)(getdisc T-getdisc)
   	   (greaterp T-greaterp)(lessp T-lessp)
	   (mapc T-mapc) (not T-not)
	    (patom T-patom) (print T-print) (prog T-prog)
	    (patom T-patom)(putd T-putd) 
	   (putprop T-putprop)
	   (read T-read)(remprop T-remprop) (reverse T-reverse)
	   (return T-return)
	   (set T-set) (setq T-setq)
	    (status T-status) (sstatus T-sstatus)
	   (sub1 T-sub1) (terpr T-terpr) 
	   (zerop T-zerop))
	(cdr i)))
    ((null i))
    (putd (cadar i) (getd (caar i)))
    (putprop (cadar i) t 'Untraceable))

(putprop 'quote t 'Untraceable)		; this prevents the common error
					; of (trace 'foo) from causing big
					; problems.

;--- trace - arg1,arg2, ... names of functions to trace
;	This is the main user callable trace routine. 
; work in progress, documentation incomplete since im not sure exactly
; where this is going.	
;
(def trace
  (nlambda (argl)
   (prog (if ifnot evalin evalout funnm 
	  funcd did break printargs printres evfcn traceenter traceexit)

    ; turn off transfer table linkages if they are on
    (cond ((T-status translink) (T-sstatus translink nil)))

    ; process each argument	 

    (do ((ll argl (cdr ll))
	 (funnm) 
	 (funcd))
	((null ll))
	(setq funnm (car ll)
		if t
		break nil
		ifnot nil
		evalin nil
		evalout nil
		printargs nil
		printres nil
		evfcn nil
		traceenter 'T-traceenter
		traceexit  'T-traceexit)

	; a list as an argument means that the user is specifying
	; conditions on the trace
	(cond ((not (atom funnm))
	       (cond ((not (atom (setq funnm (car funnm))))
		      (T-print (car funnm))
		      (T-patom '" is non an function name")
		      (go botloop)))
	       (do ((rr (cdar ll) (cdr rr)))
		   ((null rr))
		   (cond ((memq (car rr) '(if ifnot evalin evalout 
						 printargs printres evfcn
						 traceenter traceexit))
			  (T-set (car rr) (cadr rr))
			  (setq rr    (cdr rr)))
			 ((eq (car rr) 'evalinout)
			  (setq evalin (setq evalout (cadr rr))
				rr (cdr rr)))
			 ((eq (car rr) 'break)
			  (setq break t))
			 ((eq (car rr) 'lprint)
			  (setq printargs 'T-levprint
				  printres  'T-levprint))
			 (t (T-patom '"bad request: ")
			    (T-print (car rr))
			    (T-terpr))))))

	 ; if function is untraceable, print error message and skip
	 (cond ((get funnm 'Untraceable)
		(setq did (cons `(,funnm untraceable) did))
		(go botloop)))


	 ; if function is already traced, untrace it first
	 (cond ((get funnm 'original) 
		(apply 'untrace `(,funnm))
		(setq did (cons `(,funnm untraced) did))))

	; store the names of the arg printing routines if they are
	; different than print

	(cond (printargs (T-putprop funnm printargs 'trace-printargs)))
	(cond (printres  (T-putprop funnm printres 'trace-printres)))

	 ; we must determine the type of function being traced
	 ; in order to create the correct replacement function	

	 (cond ((setq funcd (T-getd funnm))
	       (cond ((bcdp funcd)		; machine code
		      (cond ((or (eq 'lambda (T-getdisc funcd))
				   (eq 'nlambda (T-getdisc funcd))
				   (eq 'macro (T-getdisc funcd)))
			     (setq typ (T-getdisc funcd)))
			    ((stringp (T-getdisc funcd))	; foreign func
			     (setq typ 'lambda))		; close enough
			    (t (T-patom '"Unknown type of compiled function")
			       (T-print funnm)
			       (setq typ nil))))

		     ((dtpr funcd)		; lisp coded
		      (cond ((or (eq 'lambda (car funcd))
				 (eq 'lexpr (car funcd)))
			     (setq typ 'lambda))
			    ((or (eq 'nlambda (car funcd))
				 (eq 'macro (car funcd)))
			     (setq typ (car funcd)))
			    (t (T-patom '"Bad function definition: ")
			       (T-print funnm)
			       (setq typ nil))))
		     ((arrayp funcd)		; array 
		      (setq typ 'lambda))
		     (t (T-patom '"Bad function defintion: ")
			(T-print funnm)))
	       
 	       ; now that the arguments have been examined for this
	       ; function, do the tracing stuff.
	       ; First save the old function on the property list

	       (T-putprop funnm funcd 'original)

	       ; now build a replacement

	       (cond ((eq typ 'lambda)
		      (T-eval 
			`(T-def ,funnm
			   (lexpr (T-nargs)
			      ((lambda (T-arglst T-res T-rslt 
						 $$functions-in-trace$$
						 $$funcargs-in-trace$$)	 
				       (T-do ((i T-nargs (T-sub1 i)))
					     ((T-zerop i))
					     (T-setq T-arglst 
						     (T-cons (arg i) T-arglst)))
				       (T-setq $$funcargs-in-trace$$
					       (T-cons T-arglst 
						       $$funcargs-in-trace$$))
				       (T-cond ((T-setq T-res 
							(T-and ,if
							       (T-not ,ifnot)))
						(,traceenter ',funnm T-arglst)
						,evalin
						(T-cond (,break (trace-break)))))
				       (T-setq T-rslt 
					       ,(cond 
						 (evfcn)
						 (t `(T-apply ',funcd T-arglst))))
				       (T-cond (T-res 
						,evalout 
						(,traceexit ',funnm T-rslt)))
				       T-rslt)
			       nil nil nil
			       (cons ',funnm $$functions-in-trace$$)
			       $$funcargs-in-trace$$))))
		      (setq did (cons funnm did)
			      $$traced-functions$$ (cons funnm
							 $$traced-functions$$)))

		     ((or (eq typ 'nlambda)
			    (eq typ 'macro))
		      (T-eval 
		       `(T-def ,funnm
			  (,typ (T-arglst)
			     ((lambda (T-res T-rslt $$functions-in-trace$$
					     $$funcargs-in-trace$$)	 
				      (T-setq $$funcargs-in-trace$$
					      (T-cons T-arglst
						    $$funcargs-in-trace$$))
				      (T-cond ((T-setq T-res 
						       (T-and ,if
							      (not ,ifnot)))
					       (,traceenter ',funnm T-arglst)
					       ,evalin
					       (T-cond (,break (trace-break)))))
				      (T-setq T-rslt 
					      ,(cond 
						(evfcn `(,evfcn ',funcd T-arglst))
						(t `(T-apply ',funcd T-arglst))))
				      (T-cond (T-res 
					       ,evalout 
					       (,traceexit ',funnm T-rslt)))
				      T-rslt)
			      nil nil
			      (cons ',funnm $$functions-in-trace$$)
			      $$funcargs-in-trace$$))))
		      (setq did (cons funnm did)
			      $$traced-functions$$ (cons funnm
							   $$traced-functions$$)))
		     
		     (t (T-patom '"No such function as: ")
			(T-print funnm)
			(T-terpr)))))
	botloop )
	 (return (nreverse did)))))

;--- untrace
; (untrace foo bar baz)
;    untraces foo, bar and baz.
; (untrace)
;    untraces all functions being traced.
;

(def untrace
  (nlambda (argl)
	   (cond ((null argl) (setq argl $$traced-functions$$)))

	   (do ((i argl (cdr i))
		(curf)
		(res))
	       ((null i)  
		(cond ((null $$traced-functions$$)
		       (setq $$functions-in-trace$$ nil)
		       (setq $$funcargs-in-trace$$ nil)))
		res)
	       (cond ((setq tmp (T-get (setq curf (car i)) 'original))
		      ; we only want to restore the original definition
		      ; if this function has not been redefined!
		      ; we can check if it has been redefined by seeing
		      ; if its current definition is one the trace package
		      ; would generate.
		      (let ((funcdef (T-getd curf)))
			   (cond ((and (dtpr funcdef)
				       (dtpr (cadr funcdef))
				       (memq (caadr funcdef)
					     '(T-nargs T-arglst)))
				  (T-putd curf tmp))))
		      (T-remprop curf 'original)
		      (T-remprop curf 'entercount)
		      (setq $$traced-functions$$ 
			      (T-delq curf $$traced-functions$$))
		      (setq res (cons curf res)))
		     (t (setq res (cons `(,curf not traced) res)))))))

;--- tracedump :: dump the currently active trace frames
;
(def tracedump
  (lambda nil
	  (T-tracedump-recursive $$functions-in-trace$$ $$funcargs-in-trace$$)))


;--- T-tracedump-recursive
; since the lists of functions being traced and arguments are in the reverse
; of the order we want to print them, we recurse down the lists and on the
; way back we print the information.
;
(def T-tracedump-recursive
  (lambda ($$functions-in-trace$$ $$funcargs-in-trace$$)
	  (cond ((null $$functions-in-trace$$))
		(t (T-tracedump-recursive (cdr $$functions-in-trace$$)
					  (cdr $$funcargs-in-trace$$))
		   (T-traceenter (car $$functions-in-trace$$)
				 (car $$funcargs-in-trace$$))))))



;--- T-traceenter - funnm : name of function just entered
;		  - count : count to print out
;	This routine is called to print the entry banner for a
;	traced function.
;
(def T-traceenter
  (lambda (name args)
	   (prog (count indent)
		 (setq count 0 indent 0)
		 (do ((ll $$functions-in-trace$$ (cdr ll)))
		     ((null ll))
		     (cond ((eq (car ll) name) (setq count (1+ count))))
		     (setq indent (1+ indent)))

		 (T-traceindent indent)
		 (T-print count)
		 (T-patom '" <Enter> ")
		 (T-print name)
		 (T-patom '" ")
		 (cond ((setq count (T-get name 'trace-printargs))
			(T-funcall count args))
		       (t (T-print args)))
		 (T-terpr))))

(def T-traceexit
  (lambda (name res)
	  (prog (count indent)
		(setq count 0 indent 0)
		(do ((ll $$functions-in-trace$$ (cdr ll)))
		    ((null ll))
		    (cond ((eq (car ll) name) (setq count (1+ count))))
		    (setq indent (1+ indent)))
		
		
		(T-traceindent indent)
		(T-print count)
		(T-patom " <EXIT>  ")
		(T-print name)
		(T-patom "  ")
		
		(cond ((setq count (T-get name 'trace-printres))
		       (T-funcall count res))
		      (t (T-print res)))
		
		(T-terpr))))



; T-traceindent
; - n   :  indent to column n

(def T-traceindent
  (lambda (col)
	  (do ((i col (1- i))
	       (char '| |))
	      ((< i 2))
	      (T-patom (cond ((eq char '| |) (setq char '\|))
			     (t (setq char '| |)))))))



; trace-break  - this is the trace break loop
(def trace-break
  (lambda nil
	 (prog (tracevalread piport)
	       (T-terpr) (T-patom '"[tracebreak]")
	loop   (T-terpr)
	       (T-patom '"T>")
	       (T-drain)
	       (cond ((or (eq '<EOF> (setq tracevalread
					 (car
					  (errset (T-read nil '<EOF>)))))
			  (and (dtpr tracevalread)
			       (eq 'tracereturn (car tracevalread))))
			(T-terpr)
		        (return nil)))
	       (T-print (car (errset (T-eval tracevalread))))
	       (go loop))))

(def T-levprint
  (lambda (x)
	  ((lambda (prinlevel prinlength)
		  (print x))
	   3 10)))

		       
(eval-when (eval)
  (apply 'sstatus `(uctolc ,old-uctolc-value))
  (setq readtable old-read-table-trace)
  )
