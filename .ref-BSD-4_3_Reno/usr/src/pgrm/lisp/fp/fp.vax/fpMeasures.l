;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;  Dynamics Statistics by Dorab Patel (UCLA)
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-fpMeasures.l "@(#)fpMeasures.l	5.1 (Berkeley) 5/31/85")

; Initialize and update the 'Measures' plist  with
; the run-time measurement data
;
; Special symbol 'TracedFns' also manipulated
; It contains the list of currently traced user defined functions.
; The attributes for each functional form and function are:

; times: the total number of times it has been called
; size: the sum of the top-level sizes of the arguments given to it
; funargno: the number of functional arguments to this form
;		(in general this is only for construct)
; funargtype: the type and total number of functions of that type
;		supplied to this functional form.
;		This is an alist ((fntype.times) ...)



(include specials.l)
(declare (special statport dummy))
(declare (localf InitSize InitFunArgTyp 
		 InitFunArgNo trace1
		 extractName goodStats
		 untrace1 SendMeasures))


; The following functions are global. i.e. used externally
; startDynStats	clrDynStats	IncrTimes	IncrSize
; IncrFunArgNo	IncrFunArgTyp	size		Trace
; PrintMeasures	IncrUDF		Untrace		stopDynStats

; This is called by the main routine to initialize all the
; measurement stuff


(defun clrDynStats nil
  (dontLoseStats)
  (initStats))


(defun dontLoseStats nil
  (cond ((goodStats) ; check to see if there are stats to report
	 (patom "output dynamic statistics? ")
	 (let ((response (car (explodec (ratom)))))
	      (If ptport then (msg (P ptport) response))
	      
	      (Tyi)
	      (cond ((eq response 'y)
		     (patom "File: ")
		     (let ((statFile 
			    (cond ((eq (tyipeek) #.CR) nil)
				  (t 
				   (let ((fl (ratom)))
					(If ptport then (msg (P ptport) fl))
					fl)))))
			  (Tyi)
			  (PrintMeasures statFile))))))))

(defun initStats nil
  
  (InitMeasures 
   `(,@#.dyadFns
     ,@#.miscFns
     ,@#.multiAdicFns
     ,@#.libFns
     ,@#.funcForms))
  
  (InitSize #.multiAdicFns)
  (InitSize #.funcForms)
  (InitFunArgNo '(constr$fp))
  
  ; included here even though it's not  a functional form
  (InitFunArgTyp '(select$fp))
  
  (InitFunArgTyp #.funcForms))


; Makes the symbol 'Measures'  have the property indicators
; corresponding to  the function names in 'ListOfFns' and the values
; to be ((times.0)).

(defun InitMeasures (ListOfFns)
  (setplist 'Measures
            (apply 'append 
		   (mapcar '(lambda (x) (list  x (list (cons 'times 0)))) 
			   ListOfFns))))

(defun goodStats nil
  (do ((M (plist 'Measures) (cddr M)))
      ((null M) nil)
      (cond ((not (zerop (cdr (assoc 'times (cadr M)))))
	     (return t)))))


; This is used to stop the collection of dynamic statistics
; needs to untrace functions if they still are. i.e. do the traced-expr stuff
; note that rds which calls this, also calls PrintMeasures, though
; this may change.

(defun stopDynStats nil
  (cond (TracedFns		; if any fns still being traced
	 (Untrace TracedFns)))  ; untrace them
  (setq DynTraceFlg nil))

(defun extractName (fnName)
  (patom 
   (implode (reverse (cons "'" (cdddr (reverse (explodec (concat "'" fnName)))))))))

; this is the function called by the system function trace to
; enable the tracing of the User Defined Functions specified
; NOTE: successive calls will add to the UDFs to be traced.

(defun Trace (arglist)
  (setq traceport poport)
  (mapc '(lambda (x)
		 (cond ((memq x TracedFns) ; if already traced
			(setq arglist 
			      (delq x arglist 1)) ; delete from arglist
			(extractName x) 	  ; and tell the user
			(patom " is already being traced")
			(terpr))))
	arglist)
  (mapc 'trace1 arglist)) ; set up traced-expr stuff

; This is called by the system function untrace to disable the tracing
; of user defined functions.
; This removes the named user defined function from the list
; of traced functions

(defun Untrace (arglist)
  (mapc '(lambda (x)
		 (cond ((memq x TracedFns) ; if being traced
			(setq TracedFns (delq x TracedFns)) ; remove
			(untrace1 x)) ; restore stuff
		       (t (extractName x) ; else complain
			  (patom " is not being traced")
			  (terpr))))
	arglist))

; This is called by Trace on each individual function that is to
; be traced. It does the manipulation of the traced-expr property

(defun trace1 (name)
  ; actually you should check for getd name returning something decent
  (let ((zExpr (getd name)))
       (cond ((null zExpr)
	      (patom "Can't trace the undefined fn ")
	      (extractName name)
	      (patom ".")
	      (terpr))
	     
	     (t
	      (putprop name zExpr 'traced-expr) ; put fn def on traced-expr
	      (setq TracedFns (append1 TracedFns name)) ; update TracedFns
	      (InitUDF name) 			; set up the measurement stuff
	      (putd name  ; make a new function def
		    `(lambda (x)
			     (prog (tmp)
				   (setq level (1+ level)) ; increment level counter
				   (printLevel)
				   (patom " >Enter> " traceport)
				   (patom (extName ',name) traceport)
				   (patom " [" traceport)
				   (d_isplay x traceport)
				   (patom "]" traceport)
				   (terpri traceport)
				   ; now call the actual function
				   (setq tmp (funcall (get ',name 'traced-expr) x))
				   (printLevel)
				   (patom " <EXIT<  " traceport) ; now print epilog
				   (patom (extName ',name) traceport)
				   (patom "  " traceport)
				   (d_isplay tmp traceport)
				   (terpri traceport)
				   (return tmp)))))))) ; return the return value



(defun extName (fnName)
  (let ((zzName (reverse (explodec fnName))))
       (cond ((memq '$ zzName) (implode (reverse (cdr (memq '$ zzName)))))
	     (t (implode (reverse (cdr (memq '_ zzName))))))))


(defun printLevel nil
  (do ((counter 1 (1+ counter)))
      ((eq counter level) (patom level traceport))
      (cond ((oddp counter) (patom "|" traceport))
	    (t (patom " " traceport)))))

; This is called by Untrace for each individaul function to be untraced.
; It handles the traced-expr property hassles.

(defun untrace1 (name)
  (let ((tmp (get name 'traced-expr)))
       (cond ((null tmp) ; if the traced-expr property is unreasonable
	      ; a better check for unreasonableness is needed
	      (extractName name) ; complain
	      (patom " was not traced properly - cant restore")
	      (terpr))
	     (t (putd name tmp) ; else restore and remove the traced-expr
		(remprop name 'traced-expr)))))

; sz is a function that returns the total number of atoms in its argument

(defun sz (x)
  (cond ((null x) 0)
	((atom x) 1)
	(t (add (size (car x))
	        (size (cdr x))))))

; inc is a macro used by the increment functions

(defmacro inc (x)
  `(rplacd ,x (1+ (cdr ,x))))

; inctimes is a macro used by IncrFunArgNo

(defmacro inctimes (x times)
  `(rplacd ,x (add times (cdr ,x))))

; increment the 'funargno' attribute of the functional form 

(defun IncrFunArgNo (fform times)
  (inctimes (sassq 'funargno
	           (get 'Measures fform)
	           '(lambda ()
		            (cprintf "error: %s has no funargno"
				     fform)
		            (terpr)
		            (break)))
	    times))

; increment the 'funargtyp' information of the functional form
; if the particular function/form has never yet been used with his
; functional form, create the entry

(defun IncrFunArgTyp (fform funct)
  (inc (sassoc funct ; get (fn.#oftimes). This has to be sassoc NOT sassq.
	       (cadr (sassq 'funargtyp	; get (funargtyp ...)
			    (get 'Measures fform)
			    '(lambda ()
				     (cprintf "error: %s has no funargtyp"
					      fform)
				     (terpr)
				     (break))))
	       ; 'funargtyp' was there but not the funct
	       ; should return (fn.#oftimes)
	       '(lambda ()
			(cond ((setq dummy (cadr (assq 'funargtyp
						       (get 'Measures fform))))
			       ; the alist is not empty and we
			       ; know that funct was not there
			       (assq funct
				     (nconc dummy
					    (list (cons funct 0)))))
			      ; the alist is empty, so add the element
			      (t (assq funct
				       (cadr (nconc (assq 'funargtyp
							  (get 'Measures fform))
						    (list (list (cons funct 0))))))))))))
; increment the 'times' attribute of the function

(defun IncrTimes (funct)
  (inc (assq 'times (get 'Measures funct))))

; update the 'avg arg size' attribute of the function
; actually it is the total size. it should be divided by the 'times'
; attribute to get the avg size.

(defun IncrSize (funct size)
  (rplacd (assq 'size (get 'Measures funct))
	  (add (cdr (assq 'size (get 'Measures funct)))
	       size)))

; This adds the given function as a property of Measures and
; initializes it to have the 'times' and 'size' attributes.

(defun InitUDF (UDF)
  (putprop 'Measures '((times . 0) (size . 0)) UDF))


; This increments the times and the size atribute of a UDF, if it exists 
; Otherwise, it does nothing.

(defun IncrUDF (UDF seq)
  (cond 
   ((and (memq UDF TracedFns) (get 'Measures UDF)) ;if the UDF is traceable
    (IncrTimes UDF)
    (IncrSize UDF (size seq)))))

; This adds the 'size' attribute to the alist corresponding to each
; function in 'FnList' and initializes the value to 0.

(defun InitSize (FnList)
  (mapcar '(lambda (funct) (nconc (get 'Measures funct) (list (cons 'size 0))))
	  FnList))

; This adds the 'funargtyp' (functional argument type) attribute to
; the alist corresponding to each functional form in 'FnFormList' and
; initializes the value to nil.

(defun InitFunArgTyp (FnFormList)
  (mapcar '(lambda (fform)
		   (nconc (get 'Measures fform)
			  (list (list 'funargtyp))))
	  FnFormList))

; This adds the 'funargno' (number of functional args) attribute to
; the alist correphsponding to each functional form in 'FnFormList'
; and initializes the value to 0.

(defun InitFunArgNo (FnFormList)
  (mapcar '(lambda (fform)
		   (nconc (get 'Measures fform)
			  (list (cons 'funargno 0))))
	  FnFormList))

; Prints out the stats to a file

(defun PrintMeasures (sFileName)
  (cond (sFileName
	 (let ((statPort nil))
	      (cond ((setq statPort (outfile sFileName 'append))
		     (SendMeasures statPort) ; write the stuff
		     (terpri statPort)
		     (close statPort))
		    (t (terpr)
		       (patom "Cannot open statFile")
		       (terpr)))))
	(t (SendMeasures nil))))


; Traverses the Measures structure and prints out the
; info onto 'port'.
; Also removes the attributes from Measures (during traversal)

(defun SendMeasures (port)
  (do ((functlist (plist 'Measures) 
		  (cddr functlist)));for each alternate elem in functlist
      ((null functlist)) ; end when all done
      (let ((fnStats (cadr functlist)))
	   (cond ((and fnStats (not (zerop (cdr (assoc 'times fnStats)))))
		  (cprintf "%s:" (printName (car functlist)) port)
		  (do ((proplist fnStats (cdr proplist)))
		      ((null proplist))
		      (let ((prop (car proplist))) ; for each prop in proplist
			   (cond ((eq (car prop) 'funargtyp) ; if it is funargtyp
				  (doFuncArg port prop))
				 (t (cprintf "	%s" (car prop) port);if not funargtyp
				    (cprintf "	%d" (cdr prop) port)))))
		  ; end of function
		  (terpri port)
		  (terpri port)))))); a newline separates functions

(defun doFuncArg (port prop)
  (terpri port)
  (terpri port)
  (cprintf "			Functional Args" nil port)
  (terpri port)
  (cprintf "		Name			Times" nil port)
  (terpri port)
  (do ((funclist (cadr prop) (cdr funclist)))
      ((null funclist))
      (cprintf "		" nil port)
      (patom (printName (caar funclist)) port)
      (cprintf "			%d" (cdar funclist) port)
      (terpri port)))

(defun printName (fnName)
  (let ((zzName (reverse (explodec fnName)))
	(tName nil))
       (setq tName (memq '$ zzName))
       (cond (tName (implode (reverse (cdr tName))))
	     (t 
	      (setq tName (memq '_ zzName))
	      (cond (tName (implode (reverse (cdr tName))))
		    ((stringp fnName) (concat '|"| fnName '|"|))
		    (t (put_obj fnName)))))))

; this is the same as the function in fp_main.l except that it takes
; an extra argument which is the port name. it is used for printing
; out a lisp object in the FP form

(defun d_isplay (obj port)
  (cond ((null obj) (patom "<>" port))
	((atom obj) (patom obj port))
	((listp obj)
	 (patom "<" port)
	 (maplist 
	  '(lambda (x) 
		   (d_isplay (car x) port)
		   (cond ((not (onep (length x))) (patom " " port)))) obj)
	 (patom ">" port))))


(defun measAlph (al seq)
  (IncrFunArgTyp 'alpha$fp al)
  (IncrTimes 'alpha$fp)
  (IncrSize 'alpha$fp (size seq)))

(defun measIns (ins seq)
  (IncrFunArgTyp 'insert$fp ins)
  (IncrTimes 'insert$fp)
  (IncrSize 'insert$fp (size seq)))

(defun measTi (ains seq)
  (IncrFunArgTyp 'ti$fp ains)
  (IncrTimes 'ti$fp)
  (IncrSize 'ti$fp (size seq)))

(defun measSel (sel seq)
  (IncrFunArgTyp 'select$fp sel)
  (IncrTimes 'select$fp)
  (IncrSize 'select$fp (size seq)))

(defun measCons (cons seq)
  (IncrFunArgTyp 'constant$fp cons)
  (IncrTimes 'constant$fp)
  (IncrSize 'constant$fp (size seq)))

(defun measCond (c1 c2 c3 seq)
  (IncrFunArgTyp 'condit$fp c1)
  (IncrFunArgTyp 'condit$fp c2)
  (IncrFunArgTyp 'condit$fp c3)
  (IncrTimes 'condit$fp)
  (IncrSize 'condit$fp (size seq)))

(defun measWhile (w1 w2 seq)
  (IncrFunArgTyp 'while$fp  w1)
  (IncrFunArgTyp 'while$fp  w2)
  (IncrTimes 'while$fp)
  (IncrSize 'while$fp (size seq)))

(defun measComp (cm1 cm2 seq)
  (IncrFunArgTyp 'compos$fp cm1)
  (IncrFunArgTyp 'compos$fp cm2)
  (IncrTimes 'compos$fp)
  (IncrSize 'compos$fp (size seq)))

(defun measConstr (fns seq)
  (mapcar '(lambda (x) (IncrFunArgTyp 'constr$fp x)) fns)
  (IncrFunArgNo 'constr$fp (length fns))
  (IncrTimes 'constr$fp)
  (IncrSize 'constr$fp (size seq)))

; get the corect name of the functional form

(defmacro getFform (xx)
  `(implode (nreverse `(p f ,@(cdr (nreverse (explodec (cxr 0 ,xx))))))))

