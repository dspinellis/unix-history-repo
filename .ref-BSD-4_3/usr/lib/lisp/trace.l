(setq rcs-trace-
   "$Header: /usr/lib/lisp/RCS/trace.l,v 1.2 83/08/15 22:30:36 jkf Exp $")

;---- The Joseph Lister Trace Package, v1
;         John Foderaro, Sept 1979
;------------------------------------------------------------------;
; Copyright (c) 1979 The Regents of the University of California   ;
;	All rights reserved.					   ;
;------------------------------------------------------------------;
(eval-when (eval)
  (setq old-read-table-trace readtable)
  (setq readtable (makereadtable t))
  (setq old-uctolc-value (status uctolc))
  (sstatus uctolc nil)		; turn off case conversion
  (load 'charmac)
  (setsyntax '\; 'macro 'zapline)
  )



;----
; trace uses these properties on the property list:
;    trace-orig-fcn: original occupant of the function cell
;    trace-trace-fcn: the value trace puts in the  function cell
;	(used to check if the trace function has be overwritten).
;    trace-trace-args: the arguments when function was traced.
;    trace-printargs: function to print argument to function
;    trace-printres: function to print result of function

(declare (nlambda T-status T-sstatus)
  (special piport
	   if ifnot evalin evalout 
	   printargs printres evfcn
	   traceenter traceexit
	   prinlevel prinlength
	   $$traced-functions$$		; all functions being traced
	   $$functions-in-trace$$	; active functions 
	   $$funcargs-in-trace$$	; arguments to active functions.
	   $tracemute			; if t, then enters and exits
					; are quiet, but info is still
					; kept so (tracedump) will work
	   trace-prinlevel		; default values
	   trace-prinlength
	   trace-printer		; function trace uses to print
	   ))



(cond ((null (boundp '$$traced-functions$$)) (setq $$traced-functions$$ nil)))
(cond ((null (boundp '$$functions-in-trace$$)) (setq $$functions-in-trace$$ nil)))
(cond ((null (boundp '$$funcargs-in-trace$$)) (setq $$funcargs-in-trace$$ nil)))
(cond ((null (boundp '$tracemute)) (setq $tracemute nil)))
(cond ((null (boundp 'trace-prinlevel)) (setq trace-prinlevel 4)))
(cond ((null (boundp 'trace-prinlength)) (setq trace-prinlength 5)))
(cond ((null (boundp 'trace-printer)) (setq trace-printer 'Trace-print)))

;----> It is important that the trace package not use traced functions
;	thus we give the functions the trace package uses different
;	names and make them equivalent at this time to their 
;	traceable counterparts.  
(defun trace-startup-func nil
  (do ((i '( (add1 T-add1)(append T-append)
	     (and T-and)  (apply T-apply)
	     (cond T-cond) (cons T-cons) (delq T-delq)
	     (def T-def) (do T-do) (drain T-drain)
	     (dtpr T-dtpr)  (eval T-eval)(funcall T-funcall)
	     (get T-get) (getd T-getd)(getdisc T-getdisc)
	     (greaterp T-greaterp)(lessp T-lessp)
	     (mapc T-mapc) (not T-not)(nreverse T-nreverse)
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
      (putprop (cadar i) t 'Untraceable)))

(trace-startup-func)


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
   (prog (if ifnot evalin evalout funnm  typ
	  funcd did break printargs printres evfcn traceenter traceexit
	  traceargs)

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
		traceexit  'T-traceexit
		traceargs  nil)

	; a list as an argument means that the user is specifying
	; conditions on the trace
      (cond ((not (atom funnm))
	     (cond ((not (atom (setq funnm (car funnm))))
		    (T-print (car funnm))
		    (T-patom '" is non an function name")
		    (go botloop)))
	     ; remember the arguments in case a retrace is requested
	     (setq traceargs (cdar ll))
	     ; scan the arguments
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
			  (T-terpr)))))
	    (t (setq traceargs nil)  ;no args given
	       ))

	    ; if function is untraceable, print error message and skip
       (cond ((get funnm 'Untraceable)
	      (setq did (cons `(,funnm untraceable) did))
	      (go botloop)))


       ; Untrace before tracing
       (let ((res (funcall 'untrace (list funnm))))
	  (cond (res (setq did (cons `(,funnm untraced) did)))))

       ; store the names of the arg printing routines if they are
       ; different than print

       (cond (printargs (T-putprop funnm printargs 'trace-printargs)))
       (cond (printres  (T-putprop funnm printres 'trace-printres)))
       (T-putprop funnm traceargs 'trace-trace-args)

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

	      (T-putprop funnm funcd 'trace-orig-fcn)

	      ; now build a replacement

	      (cond
		 ((eq typ 'lambda)
		  (T-eval
		     `(T-def
			 ,funnm
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
					     ,@(cond (evalin
							`((T-patom ,'":in: ")
							  ,evalin
							  (T-terpr))))
					     (T-cond (,break
						       (trace-break)))))
				    (T-setq T-rslt
					    ,(cond
						(evfcn)
						(t `(T-apply
						       ',funcd
						       T-arglst))))
				    (T-cond (T-res
					       ,@(cond (evalout
							  `((T-patom ,'":out: ")
							    ,evalout
							    (T-terpr))))
					       (,traceexit ',funnm T-rslt)))
				    T-rslt)
				 nil nil nil
				 (T-cons ',funnm $$functions-in-trace$$)
				 $$funcargs-in-trace$$))))
		  (T-putprop funnm (T-getd funnm) 'trace-trace-fcn)
		  (setq did (cons funnm did)
			$$traced-functions$$ (cons funnm
						   $$traced-functions$$)))

		 ((or (eq typ 'nlambda)
		      (eq typ 'macro))
		  (T-eval
		     `(T-def ,funnm
			      (,typ (T-arglst)
				((lambda (T-res T-rslt
						$$functions-in-trace$$
						$$funcargs-in-trace$$)
				    (T-setq $$funcargs-in-trace$$
					    (T-cons
					       T-arglst
					       $$funcargs-in-trace$$))
				    (T-cond ((T-setq
						T-res
						(T-and ,if
							(not ,ifnot)))
					     (,traceenter
					       ',funnm
					       T-arglst)
					     ,evalin
					     (T-cond (,break
						       (trace-break)))))
				    (T-setq T-rslt
					    ,(cond
						(evfcn `(,evfcn
							  ',funcd
							  T-arglst))
						(t `(T-apply ',funcd
							     T-arglst))))
				    (T-cond (T-res
					       ,evalout
					       (,traceexit ',funnm T-rslt)))
				    T-rslt)
				 nil nil
				 (cons ',funnm $$functions-in-trace$$)
				 $$funcargs-in-trace$$))))
		  (T-putprop funnm (T-getd funnm) 'trace-trace-fcn)
		  (setq did (cons funnm did)
			$$traced-functions$$ (cons funnm
						   $$traced-functions$$)))

		 (t (T-patom '"No such function as: ")
		    (T-print funnm)
		    (T-terpr)))))
	    botloop )
	 ; if given no args, just return the function currently being traced
	 (return (cond ((null argl) $$traced-functions$$)
		       (t (T-nreverse did)))))))

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
		(tmp)
		(curf)
		(res))
	       ((null i)  
		(cond ((null $$traced-functions$$)
		       (setq $$functions-in-trace$$ nil)
		       (setq $$funcargs-in-trace$$ nil)))
		res)
	       (cond ((and (T-getd (setq curf (car i)))
			   (eq (T-getd (car i))
			       (get (car i) 'trace-trace-fcn)))
		      ; we only want to restore the original definition
		      ; if this function has not been redefined!
		      ; we test this by checking to be sure that the
		      ; trace-trace-property is the same as the function
		      ; definition.
		      (T-putd curf (get curf 'trace-orig-fcn))
		      (T-remprop curf 'trace-orig-fcn)
		      (T-remprop curf 'trace-trace-fcn)
		      (T-remprop curf 'trace-trace-args)
		      (T-remprop curf 'entercount)
		      (setq $$traced-functions$$ 
			      (T-delq curf $$traced-functions$$))
		      (setq res (cons curf res)))))))


;--- retrace :: trace again all function thought to be traced.
;
(def retrace
   (nlambda (args)
	(cond ((null args) (setq args $$traced-functions$$)))
	(mapcan '(lambda (fcn)
		    (cond ((and (symbolp fcn)
				(not (eq (T-getd fcn)
					 (get fcn 'trace-trace-fcn))))
			   
			   (funcall 'trace
				    `((,fcn ,@(get fcn 'trace-trace-args)))))))
		args)))

;--- tracedump :: dump the currently active trace frames
;
(def tracedump
  (lambda nil
	  (let (($tracemute nil))
	       (T-tracedump-recursive $$functions-in-trace$$ 
				      $$funcargs-in-trace$$))))


;--- traceargs :: return list of args to currently entered traced functions
;  call is:
;	(traceargs foo)  returns first call to foo starting at most current
;       (traceargs foo 3) returns args to third call to foo, starting at
;			  most current
;
(def traceargs
  (nlambda (args)
	   (cond ((and args $$functions-in-trace$$)
		  (let ((name (car args))
			(amt (cond ((numberp (cadr args)) (cadr args))
				   (t 1))))
		       (do ((fit $$functions-in-trace$$ (cdr fit))
			    (fat $$funcargs-in-trace$$ (cdr fat)))
			   ((null fit))
			   (cond ((eq name (car fit))
				  (cond ((zerop (setq amt (1- amt)))
					 (return (car fat))))))))))))

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
		(cond ((not $tracemute)
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
			      (funcall count args))
			     (t (funcall trace-printer args)))
		       (T-terpr))))))

(def T-traceexit
  (lambda (name res)
	  (prog (count indent)
		(cond ((not $tracemute)
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
			      (funcall count res))
			     (t (funcall trace-printer res)))
		       
		       (T-terpr))))))


;--- Trace-printer
;  this is the default value of trace-printer.  It prints a form after
; binding prinlevel and prinlength.
;
(def Trace-print
   (lambda (form)
      (let ((prinlevel trace-prinlevel)
	    (prinlength trace-prinlength))
	 (T-print form))))

; T-traceindent
; - n   :  indent to column n

(def T-traceindent
  (lambda (col)
	  (do ((i col (1- i))
	       (char '| |))
	      ((< i 2))
	      (T-patom (cond ((eq char '| |) (setq char '\|))
			     (t (setq char '| |)))))))
; from toplevel.l:
;
;--- read and print functions are user-selectable by just
; assigning another value to top-level-print and top-level-read
;
(declare (special top-level-read top-level-print))

(defmacro top-print (&rest args)
   `(cond (top-level-print (funcall top-level-print ,@args))
	  (t (T-print ,@args))))

(defmacro top-read (&rest args)
   `(cond ((and top-level-read
		(T-getd top-level-read))
	   (funcall top-level-read ,@args))
	  (t (T-read ,@args))))


; trace-break  - this is the trace break loop
(def trace-break
  (lambda nil
	 (prog (tracevalread piport)
	       (T-terpr) (T-patom '"[tracebreak]")
	loop   (T-terpr)
	       (T-patom '"T>")
	       (T-drain)
	       (cond ((or (eq nil (setq tracevalread
					 (car
					  (errset (top-read nil nil)))))
			  (and (dtpr tracevalread)
			       (eq 'tracereturn (car tracevalread))))
			(T-terpr)
		        (return nil)))
	       (top-print (car (errset (T-eval tracevalread))))
	       (go loop))))


(def T-levprint
  (lambda (x)
	  ((lambda (prinlevel prinlength)
		  (T-print x))
	   3 10)))

		       
(eval-when (eval)
  (apply 'sstatus `(uctolc ,old-uctolc-value))
  (setq readtable old-read-table-trace)
  )
