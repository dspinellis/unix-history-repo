; 			l i s z t   v 4    
;
;
;
; 		    A compiler for Franz lisp
;
; Copyright (c) 1980 ,  The Regents of the University of California.
; All rights reserved.  
; author: j. foderaro
;
; Section INIT -- initialization and macros

(include "caspecs.l")

(eval-when (compile eval)
  (or (get 'camacs 'version) (load 'camacs))
  (or (get 'syscall 'version) (load 'syscall)))

;the version number is maintained by hand, and is written twice
; once for the benefit of the user
(setq compiler-name "Lisp Compiler 6.0")
; and the other time for SCCS's what command
(setq sccs-compiler-name "@(#)Liszt version 6.0")

(setq sectioncarid "@(#)car.l	5.22	7/9/81")  ; id for SCCS

(setq original-readtable readtable)
(setq raw-readtable (makereadtable t))

;--- special handlers
(putprop 'and	'cc-and		'fl-exprcc)
(putprop 'arg  	'cc-arg		'fl-exprcc)
(putprop 'assq 	'cm-assq	'fl-exprm)
(putprop 'atom 	'cc-atom	'fl-exprcc)
(putprop 'bigp  'cc-bigp	'fl-exprcc)
(putprop 'bcdcall 'c-bcdcall	'fl-expr)
(putprop 'bcdp	'cc-bcdp	'fl-exprcc)
(putprop '*catch 'c-*catch 	'fl-expr)
(putprop 'comment 'cc-ignore	'fl-exprcc)
(putprop 'cond	'c-cond  	'fl-expr)
(putprop 'cons	'c-cons		'fl-expr)
(putprop 'cxr 	'cc-cxr		'fl-exprcc)
(putprop 'declare 'c-declare	'fl-expr)
(putprop 'do	'c-do     	'fl-expr)
(putprop 'liszt-internal-do 	'c-do     	'fl-expr)
(putprop 'dtpr	'cc-dtpr	'fl-exprcc)
(putprop 'eq	'cc-eq    	'fl-exprcc)
(putprop 'equal 'cc-equal	'fl-exprcc)
(putprop '= 	'cc-equal	'fl-exprcc)
(putprop 'errset 'c-errset 	'fl-expr)
(putprop 'fixp	 'cc-fixp	'fl-exprcc)
(putprop 'floatp 'cc-floatp	'fl-exprcc)
(putprop 'get	 'c-get		'fl-expr)
(putprop 'getd 	'cm-getd 	'fl-exprm)
(putprop 'getdata 'cm-getdata 	'fl-exprm)
(putprop 'go	'c-go	  	'fl-expr)
(putprop 'list	'c-list	  	'fl-expr)
(putprop 'map	'cm-map	  	'fl-exprm)
(putprop 'mapc	'cm-mapc  	'fl-exprm)
(putprop 'mapcan 'cm-mapcan 	'fl-exprm)
(putprop 'mapcar 'cm-mapcar 	'fl-exprm)
(putprop 'mapcon 'cm-mapcon 	'fl-exprm)
(putprop 'maplist 'cm-maplist 	'fl-exprm)
(putprop 'memq	  'cc-memq	'fl-exprcc)
(putprop 'ncons	'cm-ncons	'fl-exprm)
(putprop 'not	'cc-not   	'fl-exprcc)
(putprop 'null	'cc-not   	'fl-exprcc)
(putprop 'numberp 'cc-numberp	'fl-exprcc)
(putprop 'or	'cc-or    	'fl-exprcc)
(putprop 'prog	'c-prog   	'fl-expr)
(putprop 'progn	'cm-progn 	'fl-exprm)
(putprop 'prog1 'cm-prog1	'fl-exprm)
(putprop 'prog2	'cm-prog2 	'fl-exprm)
(putprop 'quote	'cc-quote 	'fl-exprcc)
(putprop 'return 'c-return 	'fl-expr)
(putprop 'rplaca 'c-rplaca 	'fl-expr)
(putprop 'rplacd 'c-rplacd 	'fl-expr)
(putprop 'rplacx 'c-rplacx 	'fl-expr)
(putprop '*rplacx 'c-rplacx 	'fl-expr)
(putprop 'setarg 'c-setarg	'fl-expr)
(putprop 'setq	'cc-setq  	'fl-exprcc)
(putprop 'stringp 'cc-stringp 	'fl-exprcc)
(putprop 'symbolp 'cc-symbolp	'fl-exprcc)
(putprop 'symeval 'cm-symeval	'fl-exprm)
(putprop '*throw 'c-*throw 	'fl-expr)
(putprop 'typep   'cc-typep	'fl-exprcc)
(putprop 'zerop   'cm-zerop	'fl-exprm)

; functions which expect fixnum operands 


(putprop '+ 'c-fixnumop  'fl-expr)
(putprop '+ 'addl3 'fixop)

(putprop '- 'c-fixnumop 'fl-expr)
(putprop '- 'subl3 'fixop)

(putprop '* 'c-fixnumop 'fl-expr)
(putprop '* 'mull3 'fixop)

(putprop '/ 'c-fixnumop 'fl-expr)
(putprop '/ 'divl3 'fixop)


(putprop '1+ 'cc-oneplus 'fl-exprcc)
(putprop '1- 'cc-oneminus 'fl-exprcc)

(putprop '\\	'c-\\	'fl-expr)   ; done in the old way, should be modified

; these have typically fixnum operands, but not always 


; there should be some checking here for flonums.
;
(putprop '< 'cc-< 'fl-exprcc)
(putprop '> 'cc-> 'fl-exprcc)		


; functions which can only be compiled
(putprop 'assembler-code 'c-assembler-code 'fl-expr)
(putprop 'fixnum-cxr 'cm-fixnum-cxr 'fl-exprm)
(putprop 'internal-fixnum-box 'c-internal-fixnum-box 'fl-expr)
(putprop 'offset-cxr 'cc-offset-cxr 'fl-exprcc)


; Section INTERF -- user interface


;--- lisztinit : called upon compiler startup. If there are any args
;	       on the command line, we build up a call to liszt, which
;	       will do the compile. Afterwards we exit.
;
(def lisztinit
  (lambda nil
	  (cond ((greaterp (argv -1) 1)      ; build up list of args
		 (do ((i (1- (argv -1)) (1- i)) (arglis))
		     ((lessp i 1) 
		      (setq user-top-level nil)
		      (exit (apply 'liszt arglis)))
		     (setq arglis (cons (argv i) arglis))))
		(t (patom compiler-name)
		   (terpr poport)
		   (setq user-top-level nil)))))

(setq user-top-level 'lisztinit)



;--- liszt - v-x : list containing file name to compile and optionaly
;		 and output file name for the assembler source.
;
(def liszt
  (nlambda (v-x)
	   (prog (piport v-root v-ifile v-sfile v-ofile 
			 vp-ifile vp-sfile vps-crap
			 vps-include vns-include
			 tmp ntem temgc temcp rootreal g-arrayspecs
			 g-fname g-trueop g-falseop
			 tem temr starttime startptime startgccount
			 fl-asm fl-warn fl-verb fl-inter fl-xref fl-uci
			 fl-run fl-case
			 g-skipcode g-dropnpcnt)

		 ; turn on monitoring if it exists
		 #+monitoring
		 (errset (progn (monitor t)	; turn it on
				(print 'monitor-on)
				(terpr))
			 nil)
		 (setq starttime (sys_time)   ; real time in seconds
		       startptime (ptime)
		       startgccount $gccount$)
		 (cond ((null (boundp 'internal-macros))
			(setq internal-macros nil)))
		 (cond ((null (boundp 'macros))
			(setq macros nil)))
		 (setq er-fatal 0)
		 (setq vps-include nil  
		       vns-include nil)  ;stack of ports and names
		 (setq twa-list nil)
		 (setq liszt-eof-forms nil)

		 ; set up once only g variables
		 (setq g-comments nil
		       g-current nil		; current function name
		       g-funcs nil
		       g-lits nil
		       g-trueloc nil
		       g-tran nil
		       g-allf nil		; used in xrefs
		       g-reguse '((r4 0 . nil) (r3 0 . nil) 
				  (r2 0 . nil) (r7 0 . nil) (r1 0 . nil))
		       g-trancnt 0
		       g-ignorereg nil
		       g-trueop  'jneq		; used in e-gotot
		       g-falseop 'jeql		; used in e-gotonil
		       g-litcnt 0)
		 (setq g-spec (gensym 'S))	; flag for special atom
		 (setq g-fname "")		; no function yet
		 (setq special nil)		; t if all vrbs are special
		 (setq g-functype (gensym)
		       g-bindloc  (gensym)
		       g-localf   (gensym)
		       g-arrayspecs (gensym)
		       g-tranloc  (gensym))

		 ; declare these special

		 (sstatus feature complr)
		 (d-makespec 't)		; always special

		 ; process input form
		 (setq fl-asm t		; assembler file assembled
		       fl-warn t	; print warnings
		       fl-verb t	; be verbose
		       fl-macl nil	; compile maclisp file
		       fl-inter nil	; do interlisp compatablity
		       fl-tty nil	; put .s on tty
		       fl-comments nil    ; put in comments
		       fl-profile nil	; profiling
		       fl-tran	  t	; use transfer tables
		       fl-vms	nil	; vms hacks
		       fl-case  nil	; trans uc to lc
		       fl-xref	nil	; xrefs
		       fl-run	nil	; autorun capability
		       fl-uci   nil	; uci lisp compatibility
		       )

		 ; look in the environment for a LISZT variable
		 ; if it exists, make it the first argument 
		 (If (not (eq '|| (setq tem (getenv 'LISZT))))
		     then (setq v-x (cons (concat "-" tem) v-x)))

		 (do ((i v-x (cdr i)))	; for each argument
		     ((null i))
		     (setq tem (aexplodec (car i)))

		     (cond ((eq '- (car tem))	; if switch
			    (do ((j (cdr tem) (cdr j)))
				((null j))
				(cond ((eq 'S (car j)) (setq fl-asm nil))
				      ((eq 'C (car j)) (setq fl-comments t))
				      ((eq 'm (car j)) (setq fl-macl t))
				      ((eq 'o (car j)) (setq v-ofile (cadr i)
							     i (cdr i)))
				      ((eq 'w (car j)) (setq fl-warn nil))
				      ((eq 'q (car j)) (setq fl-verb nil))
				      ((eq 'Q (car j)) (setq fl-verb t))
				      ((eq 'T (car j)) (setq fl-tty t))
				      ((eq 'i (car j)) (setq fl-inter t))
				      ((eq 'p (car j)) (setq fl-profile t))
				      ((eq 'F (car j)) (setq fl-tran nil))
				      ((eq 'v (car j)) (setq fl-vms t))
				      ((eq 'r (car j)) (setq fl-run t))
				      ((eq 'x (car j)) (setq fl-xref t))
				      ((eq 'c (car j)) (setq fl-case t))
				      ((eq 'u (car j)) (setq fl-uci  t))
				      ((eq '- (car j)))  ; ignore extra -'s
				      (t (comp-gerr "Unknown switch: "
						    (car j))))))
			   ((null v-root)
			    (setq temr (reverse tem))
			    (cond ((and (eq 'l (car temr))
					(eq '\. (cadr temr)))
				   (setq rootreal nil)
				   (setq v-root (apply 'concat (reverse (cddr temr)))))
				  (t (setq v-root (car i)
					   rootreal t))))

			   (t (comp-gerr "Extra input file name: " (car i)))))


		 (cond (fl-vms (setq fl-tran nil)))  ; no transfer tables in vms

		 ; if verbose mode, print out the gc messages and
		 ; fasl messages, else turn them off.

		 (cond (fl-verb (setq $gcprint t  $ldprint t))
		       (t  (setq $gcprint nil   $ldprint nil)))

		 ; -c says set reader to xlate uc to lc
		 (cond (fl-case (sstatus uctolc t)))

		 ; now see what the arguments have left us

		 (cond ((null v-root)
			(comp-gerr "No file for input"))
		       ((or (portp 
			     (setq vp-ifile 
				   (car (errset (infile 
						   (setq v-ifile 
							 (concat v-root '".l"))) 
						nil))))
			    (and rootreal
				 (portp
				  (setq vp-ifile
					(car (errset 
					         (infile (setq v-ifile v-root))
					         nil)))))))
		       (t (comp-gerr "Couldn't open the source file :"
				     (or v-ifile))))


		 ; determine the name of the .s file
		 ; strategy: if fl-asm is t (only assemble) use (v-root).s
		 ;	     else use /tmp/(PID).s
		 ;  
		 ; direct asm to tty temporarily
		 (setq v-sfile '"tty")
		 (setq vp-sfile nil)
		 (If (null fl-tty) then
		    (cond (fl-asm (setq v-sfile (concat '"/tmp/jkf" 
						          (boole 1 65535
								 (sys_getpid))
						           '".s")))
			  (t (setq v-sfile (concat v-root '".s"))))

		   (cond ((not (portp (setq vp-sfile 
					  (car (errset (outfile v-sfile) 
						       nil)))))
			(comp-gerr "Couldn't open the .s file: "
				   (or v-sfile)))))
				     
		 
		 ; determine the name of the .o file (object file)
		 ; strategy: if we aren't supposed to assemble the .s file
		 ;	      don't worry about a name
		 ; 	     else if a name is given, use it
		 ;	     else if use (v-root).o
		 ;  if profiling, use .o
		 (cond ((or v-ofile (null fl-asm)))		;ignore
		       ((null fl-profile) (setq v-ofile (concat v-root '".o")))
		       (t (setq v-ofile (concat v-root ".o"))))

		 ; determine the name of the .x file (xref file)
		 ; strategy: if fl-xref is true, then use (v-root).x
		 ;
		 (cond (fl-xref 
			(cond ((not 
				(portp 
				 (setq vp-xfile 
				      (car (errset (outfile (setq v-xfile 
						     (concat v-root ".x"))))))))
			       (comp-gerr "Can't open the .x file" (or v-xfile))))))
		 (cond ((checkfatal) (return 1)))

		 (setq readtable (makereadtable nil))	; use new readtable


		 ; if the macsyma flag is set, change the syntax to the
		 ; maclisp standard syntax.  We must be careful that we
		 ; dont clobber any syntax changes made by files preloaded
		 ; into the compiler.

		 (cond (fl-macl (setsyntax '\/ 143) 	;  143 = vesc

				(cond ((equal 143 (status syntax \\))
				       (setsyntax '\\ 2)))

				(setsyntax '\| 138)	;  138 = vdq
				(cond ((equal 198 (status syntax \[))
				       (setsyntax '\[ 2)
				       (setsyntax '\] 2)))
				(setq ibase  8.)
				(sstatus uctolc t)
				
				(d-makespec 'ibase)	; to be special
				(d-makespec 'base)
				(d-makespec 'tty)

				(errset (cond ((null (getd 'macsyma-env))
					       (fasl '/usr/lib/lisp/machacks)))
					nil))
		       (fl-uci (load "/usr/lib/lisp/ucifnc")
			       (cvttoucilisp)))

		 (cond (fl-inter (putprop '* 'cc-ignore 'fl-exprcc) ;comment
				 (remprop '* 'fl-expr)
				 ))

		 (cond ((checkfatal) (return 1)))  ; leave if fatal errors	

		 (If fl-verb 
		     then (comp-msg "Compilation begins with " compiler-name )
		          (comp-msg "source: "  v-ifile ", result: "
				    (cond (fl-asm v-ofile) (t v-sfile))))

		 (setq piport vp-ifile)		; set to standard input
		 (setq liszt-root-name v-root
		       liszt-file-name v-ifile)


		 (If fl-run then (d-printautorun))
	
		 (If fl-profile then (e-write1 '".globl mcount"))
	loop

	        (cond ((atom (errset 		; list for debugging,
						; errset for production.
			      (do ((i (read piport '<<end-of-file>>) 
				      (read piport '<<end-of-file>>))) 
				  ((eq i '<<end-of-file>>) nil)
				  (catch (liszt-form i) Comp-error))))
		       (catch (comp-err "Lisp error during compilation")
			      Comp-error)
		       (setq piport nil)
		       (setq er-fatal (1+ er-fatal))
		       (return 1)))

		 (close piport)

		 ; if doing special character stuff (maclisp) reassert
		 ; the state

		 (cond (vps-include
			(comp-note  " done include")
			(setq piport (car vps-include)
			      vps-include (cdr vps-include)
			      v-ifile (car vns-include)
			      vns-include (cdr vns-include))
			(go loop)))

		 (cond (liszt-eof-forms
			(do ((ll liszt-eof-forms (cdr ll)))
			    ((null ll))
			    (cond ((atom (errset (liszt-form (car ll))))
				   (catch
				    (comp-note "Lisp error during eof forms")
				    Comp-error)
				   (setq piport nil)
				   (return 1))))))

		 ; reset input base
		 (setq ibase 10.)
		 (setq readtable (makereadtable t))
		 (d-bindtab)


		 (close vp-sfile)		; close assembler language file

		 ; check for fatal errors and don't leave if so
		 (cond ((checkfatal) 
			(If fl-asm   			; unlink .s file
			    then (sys_unlink v-sfile))  ; if it is a tmp
			(return 1)))		; and ret with error status

		 (comp-note "Compilation complete")

		 (setq tem (Divide (difference (sys_time) starttime) 60))
		 (setq ntem (ptime))

		 (setq temcp (Divide (difference (car ntem) (car startptime))
				    3600))

		 (setq temgc (Divide (difference (cadr ntem) (cadr startptime))
				    3600))

		 (comp-note " Time: Real: " (car tem) ":" (cadr tem)
		        ", CPU: " (car temcp) ":" (quotient (cadr temcp) 60.0) 
			 ", GC: " (car temgc) ":" (quotient (cadr temgc) 60.0) 
			    " for "
			    (difference $gccount$ startgccount)
			    " gcs")

		 (cond (fl-xref
			(comp-note "Cross reference being generated")
			(print (list 'File v-ifile) vp-xfile)
			(terpr vp-xfile)
			(do ((ii g-allf (cdr ii)))
			    ((null ii))
			    (print (car ii) vp-xfile)
			    (terpr vp-xfile))
			(close vp-xfile)))


		 ; the assember we use must generate the new a.out format
		 ; with a string table.  We will assume that the assembler
		 ; is in /usr/lib/lisp/as so that other sites can run
		 ; the new assembler without installing the new assembler
		 ; as /bin/as
		 (cond (fl-asm 			; assemble file 
			 (comp-note "Assembly begins")
			 (cond ((not 
				 (zerop 
				    (setq tmp
					  (apply 'process 
						 (ncons (concat 
							 "/usr/lib/lisp/as  -o "
								    v-ofile
								    '" "
								    v-sfile))))))
				(comp-gerr "Assembler detected error, code: "
					   tmp)
				(comp-note "Assembler temp file " v-sfile
					   " is not unlinked"))
			       (t (comp-note "Assembly completed successfully")
				  (sys_unlink v-sfile); unlink tmp file
				  (If fl-run then (sys_chmod v-ofile #O775))))))  

		 (setq readtable original-readtable)
		 #+monitoring
		 (errset (progn (monitor)	; turn off monitoring
				(print 'monitor-off))
			 nil)
		 (return 0))))

(def checkfatal
  (lambda nil
	  (cond ((greaterp er-fatal 0)
		 (catch (comp-err "Compilation aborted due to previous errors")
			Comp-error)
		 t))))

;--- liszt-form - i : form to compile
;	This compiles one form.
;
(def liszt-form
  (lambda (i)
     (prog (tmp v-x)
	  ; macro expand
       loop
	  (If (and (dtpr i) (eq 'macro (d-functyp (car i))))
	      then (setq i (apply (car i) i))
		   (go loop))
	  ; now look at what is left
	  (cond ((eq (car i) 'def) ; jkf mod
		 (cond (fl-verb (print (cadr i)) (terpr)(drain)))
		 (d-dodef i))
		((eq (car i) 'declare) (funcall 'complr-declare  (cdr i)))
		((eq (car i) 'eval-when) (doevalwhen i))
		((and (eq (car i) 'progn) (equal (cadr i) '(quote compile)))
		 ((lambda (internal-macros)	; compile macros too
			  (mapc 'liszt-form (cddr i)))
		       t))
		((or (and (eq (car i) 'includef) (setq tmp (eval (cadr i))))
		     (and (eq (car i) 'include ) (setq tmp (cadr i))))
		 (cond ((or (portp (setq v-x 
					 (car (errset (infile tmp) nil))))
			    (portp (setq v-x 
					 (car (errset (infile (concat '"/usr/lib/lisp"
							     tmp)) 
						      nil))))
			    (portp (setq v-x 
					 (car (errset (infile (concat tmp
								      '".l")) 
						      nil)))))
			(setq vps-include (cons piport vps-include))
			(setq piport v-x)
			(comp-note " INCLUDEing file: "  tmp)
			(setq vns-include (cons v-ifile vns-include)
			      v-ifile tmp))
		       (t (comp-gerr "Cannot open include file: " tmp))))
		((eq (car i) 'comment) nil)   ; just ignore comments
		(t (Push g-funcs `(eval ,i)))))))

;--- d-dodef :: handle the def form
; 	- form : a def form: (def name (type args . body))
;
(defun d-dodef (form)
  (prog (g-fname g-ftype g-args body lambdaform symlab)
	
 loop
	; extract the components of the def form
	(setq g-fname (cadr form))
	(If (dtpr (caddr form))
	    then (setq g-ftype (caaddr form)
		       g-args (cadaddr form)
		       body (cddaddr form)
		       lambdaform (caddr form)
		       symlab (gensym 'F))
	    else (comp-gerr "bad def form " form))
	
	
	; check for a def which uses the mit hackish &xxx forms.
	; if seen, convert to a standard form and reexamine
	(If (or (memq '&rest g-args) 
		(memq '&optional g-args)
		(memq '&aux g-args))
	    then (setq form 
		       `(def ,(cadr form) ,(lambdacvt (cdr lambdaform))))
	         (go loop))

	; check for legal function name.  
	; then look at the type of the function and update the data base.
	(If (null (atom g-fname))
	    then (comp-err "bad function name")
	    else (setq g-flocal (get g-fname g-localf))    ; check local decl.
		 ; macros are special, they are always evaluated
		 ; and sometimes compiled.
	         (If (eq g-ftype 'macro)
		     then (eval form)
		          (If (and (null macros)
				   (null internal-macros))
			      then (comp-note g-fname 
					      " macro will not be compiled")
			           (return nil))
		          (Push g-funcs `(macro ,symlab ,g-fname))
		   elseif g-flocal
		     then (If (null (or (eq g-ftype 'lambda)
					(eq g-ftype 'nlambda)))
			      then (comp-err "bad type for local fcn" g-ftype))
		   elseif (or (eq g-ftype 'lambda)
				(eq g-ftype 'lexpr))
		     then (Push g-funcs `(lambda ,symlab ,g-fname))
		          (putprop g-fname 'lambda g-functype)
		    elseif (eq g-ftype 'nlambda)
		     then (Push g-funcs `(nlambda ,symlab ,g-fname))
		          (putprop g-fname 'nlambda g-functype)
		     else (comp-err " bad function type " g-ftype)))
	(setq g-skipcode nil)	;make sure we aren't skipping code
	(forcecomment `(fcn ,g-ftype  ,g-fname))
	(If g-flocal 
	   then (comp-note g-fname " is a local function")
	        (e-writel (car g-flocal))
	   else (If (null fl-vms) then (e-write2 '".globl" symlab))
	        (e-writel symlab))
	(setq g-locs nil g-loccnt 0 g-labs nil g-loc 'reg g-cc nil
	      g-ret t g-topsym (d-genlab))
	(If fl-xref then (setq g-refseen (gensym) g-reflst nil))
	(d-clearreg)
	; set up global variables which maintain knowledge about
	; the stack.  these variables are set up as if the correct
	; number of args were passed.
	(Push g-locs (cons 'lambda 0))
	(setq g-currentargs (length g-args))
	(mapc '(lambda (x) (Push g-locs nil) (incr g-loccnt))
	      g-args)
	(d-prelude)			; do beginning stuff
	(d-lambbody lambdaform)		; emit code
	(d-fini)
	(If fl-xref then 
	    (Push g-allf
		  (cons g-fname
			(cons (cond (g-flocal (cons g-ftype 'local))
				    (t g-ftype))
			      g-reflst))))))
 

;--- d-prelude :: emit code common to beginning of all functions
;
(defun d-prelude nil
  (If g-flocal
      then (e-write3 'movl 'r10 '"-(sp)")	; (faster than pushl)
	   (e-write3 'movab `(,(* -4 g-currentargs) r6) 'r10)
	   (e-writel g-topsym)
      else
  	   (e-write2 '".word" '0x5c0)
	   (If fl-profile
	       then (e-write3 'movab 'mcounts 'r0)
		    (e-write2 'jsb 'mcount))
	   (e-write3 'movab 'linker '#.bind-reg)
	   (If (eq g-ftype 'lexpr) 
	     then 
	       (e-write4 'subl3 '$4 Lbot-reg '"-(sp)")  ; set up base for (arg)
	       (e-writel g-topsym)
	       (e-write3 'movl Np-reg oLbot-reg)	; will stack num of args
	       (e-write4 'subl3 Lbot-reg Np-reg 'r0)	; arg cnt again
	       (e-write3 'movab '"0x1400(r0)" np-plus)	; stack lispval
	       (e-write3 'movl '(0 #.oLbot-reg) '"-(sp)") ; also on runtime stk
	     else
	       ; set up old lbot register, base register for variable
	       ; references
	       (e-write3 'movl '#.Lbot-reg '#.oLbot-reg)
	       ; make sure the np register points where it should since
	       ; the caller might have given too few or too many args
	       (e-write3 'movab `(,(* 4 g-currentargs) #.oLbot-reg) 
			        '#.Np-reg)
	       (e-writel g-topsym))))

;--- d-fini :: emit code  at end of function

(defun d-fini nil
  (If g-flocal then (e-write3 'movl '"(sp)+" 'r10)
		    (e-write1 'rsb)
	       else (e-return)))


;--- d-bindtab :: emit binder table when all functions compiled
;
(defun d-bindtab nil
  (setq g-skipcode nil)	  ; make sure this isnt ignored	
  (e-writel "bind_org")
  (e-write2 ".set linker_size," (length g-lits))
  (e-write2 ".set trans_size," (length g-tran))
  (do ((ll (setq g-funcs (nreverse g-funcs)) (cdr ll)))
      ((null ll))
      (If (memq (caar ll) '(lambda nlambda macro eval))
	  then (e-write2 '".long" (cdr (assoc (caar ll) 
					      '((lambda . 0)
						(nlambda . 1)
						(macro . 2)
						(eval . 99)))))
	  else (comp-err " bad type in lit list " (car ll))))
  
  (e-write1 ".long -1")
  (e-write1 '"lit_org:")
  (d-asciiout (nreverse g-lits))
  (If g-tran then (d-asciiout (nreverse g-tran)))
  (d-asciiout (mapcar '(lambda (x) (If (eq (car x) 'eval)
				       then (cadr x)
				       else (caddr x)))
		      g-funcs))
  
  (e-write1 '"lit_end:"))

;--- d-asciiout :: print a list of asciz strings
;
(defun d-asciiout (args)
       (do ((lits args (cdr lits))
	    (form))
	   ((null lits))
	   (setq form (explode (car lits))
		 formsiz (length form))
	   (do ((remsiz formsiz)
		(curform form)
		(thissiz))
	       ((zerop remsiz))
	       (If (greaterp remsiz 60) then (sfilewrite '".ascii \"")
		   else (sfilewrite '".asciz \""))
	       (setq thissiz (min 60 remsiz))
	       (do ((count thissiz (1- count)))
		   ((zerop count)
		    (sfilewrite (concat '\" (ascii 10)))
		    (setq remsiz (difference remsiz thissiz)))
		   (If (eq ch-newline (car curform))
		       then (sfilewrite '\\012)
		    else (If (or (eq '\\ (car curform))
				 (eq '\" (car curform)))
			     then (sfilewrite '\\))
			 (sfilewrite (car curform)))
		   (setq curform (cdr curform))))))

;--- doevalwhen, process evalwhen directive. This is inadequate.
;
(def doevalwhen 
      (lambda (v-f)
	      (prog (docom dolod)
		    (setq docom (memq 'compile (cadr v-f))
			  
			  dolod (memq 'load (cadr v-f)))
		    (mapc '(lambda (frm) (cond (docom (eval frm)))
					 (cond (dolod 
						((lambda (internal-macros) 
							 (liszt-form frm))
						 t))))
			  (cddr v-f)))))


;---- dodcl - forms declare form
;	process the declare form given. We evaluate each arg
;
(defun complr-declare fexpr (forms)
	  (do ((i forms (cdr i))) 
	      ((null i))
	      (cond ((and (atom (caar i))
			  (getd (caar i)))
		     (eval (car i))) ; if this is a function
		    (t (comp-warn "Unknown declare attribute: " (car i))))))

;---> handlers for declare forms
;
(def *fexpr
  (nlambda (args)
	   (mapc '(lambda (v-a)
			  (putprop v-a 'nlambda g-functype))
		 args)))

(def nlambda
  (nlambda (args)
	   (mapc '(lambda (v-a)
			  (putprop v-a 'nlambda g-functype))
		 args)))

(def special
  (nlambda (v-l) 
	   (mapc '(lambda (v-a)
			  (putprop v-a t g-spec) ) 
		 v-l)
	   t))
(def unspecial
  (nlambda (v-l) 
	   (mapc '(lambda (v-a) 
			  (putprop v-a nil g-spec)) 
		 v-l)
	   t))

(def *expr 
  (nlambda (args) 
	   (mapc 
	    '(lambda (v-a)
		   (cond ((atom v-a) (putprop v-a 'lambda g-functype))
			 (t (comp-warn "Bad declare form " v-a 
				       " in list " args))))
	    args)
	   t))

(def *lexpr 
  (nlambda (args) 
	   (mapc '(lambda (v-a)
			  (putprop v-a 'lexpr g-functype))
		 args)
	   t))	; ignore

(def fixnum
  (nlambda (args)
	   nil)) ; ignore

(def macarray 
  (nlambda (v-l)
	   (mapc '(lambda (x)
			  (If (dtpr x)
			      then (putprop (car x) (cdr x) g-arrayspecs)
				   (putprop (car x) 'array  g-functype)
			      else (comp-err "Bad macerror form" x)))
		 v-l)))

(def flonum
  (nlambda (args)
	   nil)) ; ignore

(def macros 
  (nlambda (args) (setq macros (car args))))

(def specials
  (nlambda (args) (setq special (car args))))

(def localf
  (nlambda (args) (mapc '(lambda (ar)
				 (If (null (get ar g-localf))
				     then (putprop ar 
						   (cons (d-genlab) -1)
						   g-localf)))
			args)))
;---> end declare form handlers









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

; this routine is copied from ccb.l so we can make it a local function
; in both files

;--- d-genlab :: generate a pseudo label
;
(defun d-genlab nil
  (gensym 'L))


;--- d-autorunhead
;
(defun d-printautorun nil
  (sfilewrite
".set	exit,1
.word	0x0000
subl2	$8,sp
movl	8(sp),(sp)  #  argc
movab	12(sp),r0
movl	r0,4(sp)  #  argv
QL1:
tstl	(r0)+  #  null args term ?
bneq	QL1
cmpl	r0,*4(sp)  #  end of 'env' or 'argv' ?
blss	QL2
tstl	-(r0)  # envp's are in list
QL2:
movab	dr,8(sp)
movab	ln,4(sp)
movab	4(sp),r1
movl	sp,r2
pushl	r0	#stack environment
pushl	r1
pushab	ln
calls	$3,_execve
chmk	$exit
ln:
.asciz	\"/usr/ucb/lisp\"
dr:
.asciz	\"-f\"
.set	exece,59
_execve:
.word	0x0000
chmk	$exece
chmk	$exit
ret
"))
