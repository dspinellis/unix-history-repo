(include "chead.l")
(Liszt-file tlev  "@(#)tlev.l	1.6	2/27/82")


;;; ----	t l e v				top level interface
;;;

(eval-when (compile eval)
  (or (get 'syscall 'version) (load 'syscall)))


;--- lisztinit : called upon compiler startup. If there are any args
;	       on the command line, we build up a call to liszt, which
;	       will do the compile. Afterwards we exit.
;
(def lisztinit
   (lambda nil
      (setq fl-asm nil)		; insure it as correct value in case of int
      (cond ((greaterp (argv -1) 1)      ; build up list of args
	     (signal 2 'liszt-interrupt-signal)  ; die on int
	     (do ((i (1- (argv -1)) (1- i)) (arglis))
		 ((lessp i 1)
		  (setq user-top-level nil)
		  (exit (apply 'liszt arglis)))
		 (setq arglis (cons (argv i) arglis))))
	    (t (patom compiler-name)
	       (patom " [")(patom franz-minor-version-number)(patom "]")
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
			 g-decls
			 g-fname g-trueop g-falseop
			 tem temr starttime startptime startgccount
			 fl-asm fl-warn fl-verb fl-inter fl-xref fl-uci
			 fl-run fl-case fl-anno
			 liszt-process-forms in-line-lambda-number
			 g-skipcode g-dropnpcnt g-complrname)

		 ; turn on monitoring if it exists
		 #+monitoring
		 (errset (progn (monitor t)	; turn it on
				(print 'monitor-on)
				(terpr))
			 nil)
		 (setq starttime (sys_time)   ; real time in seconds
		       startptime (ptime)
		       startgccount $gccount$)
		 (setq in-line-lambda-number (sys_time))
		 (cond ((null (boundp 'internal-macros))
			(setq internal-macros nil)))
		 (cond ((null (boundp 'macros))
			(setq macros nil)))
		 (setq er-fatal 0)
		 (setq vps-include nil  
		       vns-include nil)  ;stack of ports and names
		 (setq twa-list nil)
		 (setq liszt-eof-forms nil)

		 ; look for lisztrc file and return if error occured
		 ; in reading it
		 (cond ((eq (do-lisztrc-check) 'error)
			(return 1)))
		 
		 ; set up once only g variables
		 (setq g-comments nil
		       g-current nil		; current function name
		       g-funcs nil
		       g-lits nil
		       g-trueloc nil
		       g-tran nil
		       g-allf nil		; used in xrefs
		       g-reguse (copy '((r4 0 . nil) (r3 0 . nil)
					(r2 0 . nil) (r7 0 . nil)
					(r1 0 . nil)))
		       g-trancnt 0
		       g-ignorereg nil
		       g-trueop  'jneq		; used in e-gotot
		       g-falseop 'jeql		; used in e-gotonil
		       g-compfcn nil
		       g-litcnt 0)
		 (setq g-spec (gensym 'S))	; flag for special atom
		 (setq g-fname "")		; no function yet
		 (setq special nil)		; t if all vrbs are special
		 (setq g-functype (gensym)
		       g-vartype  (gensym)
		       g-bindtype (gensym)
		       g-calltype (gensym)
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
		       fl-anno nil	; annotate 
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
				      ((eq 'a (car j)) (setq fl-anno t))
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

		 ; g-complrname is a symbol which should be unique to
		 ; each fasl'ed file. It will contain the string which
		 ; describes the name of this file and the compiler
		 ; version.
		 (If fl-anno
		    then (setq g-complrname (concat "fcn-in-" v-ifile))
			 (Push g-funcs
			       `(eval (setq ,g-complrname
					    ,(get_pname
						(concat v-ifile
							" compiled by "
							compiler-name
							" on "
							(status ctime)))))))
							
		 
		 (setq readtable (makereadtable nil))	; use new readtable


		 ; if the macsyma flag is set, change the syntax to the
		 ; maclisp standard syntax.  We must be careful that we
		 ; dont clobber any syntax changes made by files preloaded
		 ; into the compiler.

		 (cond (fl-macl (setsyntax '\/ 'vescape) 	;  143 = vesc

				(cond ((eq 'vescape (getsyntax '\\))
				       (setsyntax '\\ 'vcharacter)))

				(cond ((eq 'vleft-bracket (getsyntax '\[))
				       (setsyntax '\[ 'vcharacter)
				       (setsyntax '\] 'vcharacter)))
				(setq ibase  8.)
				(sstatus uctolc t)
				
				(d-makespec 'ibase)	; to be special
				(d-makespec 'base)
				(d-makespec 'tty)

				(errset (cond ((null (getd 'macsyma-env))
					       (load 'machacks)))
					nil))
		       (fl-uci (load "ucifnc")
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

		; main loop of the compiler.  It reads a form and
		; compiles it. It continues to compile forms from
		; liszt-process-forms was long at that list is
		; non-empty.  This allows one form to spawn off other
		; forms to be compiled (an alternative to (progn 'compile))
		;
	        (cond ((atom (errset 		; list for debugging,
						; errset for production.
			      (do ((i (read piport '<<end-of-file>>) 
				      (read piport '<<end-of-file>>))) 
				  ((eq i '<<end-of-file>>) nil)
				  (setq liszt-process-forms
					(cons i liszt-process-forms))
				  (do ((this (car liszt-process-forms)
					     (car liszt-process-forms)))
				      ((null liszt-process-forms))
				      (unpush liszt-process-forms)
				      (catch (liszt-form this) Comp-error)))))
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

		 (d-printdocstuff)		; describe this compiler

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
						 (ncons
						    (concat
						       lisp-library-directory
						       "/as  -o "
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
		 (sstatus nofeature complr)
		 (return 0))))

(def checkfatal
  (lambda nil
	  (cond ((greaterp er-fatal 0)
		 (catch (comp-err "Compilation aborted due to previous errors")
			Comp-error)
		 t))))

;--- do-lisztrc-check
; look for a liszt init file named
;  .lisztrc  or  lisztrc or $HOME/.lisztrc or $HOME/lisztrc
; followed by .o or .l
; return the symbol 'error' if an error occured while reading.
;
(defun do-lisztrc-check nil
   (do ((dirs `("." ,(getenv 'HOME)) (cdr dirs)))
       ((null dirs))
       (cond ((do ((name '(".lisztrc" "lisztrc") (cdr name)))
		  ((null name))
		  (cond ((do ((ext '(".o" ".l") (cdr ext))
			      (file))
			     ((null ext))
			     (cond ((probef
				       (setq file (concat (car dirs)
							  "/"
							  (car name)
							  (car ext))))
				    (cond ((atom (errset (load file)))
					   (comp-msg
					      "Error loading liszt init file "
					      file N
					      "Compilation aborted" N)
					   (return 'error)))
				    (return t))))
			 (return t))))
	      (return t)))))

      
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
		((eq (car i) 'declare) (funcall 'declare  (cdr i)))
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
					 (car
					    (errset
					       (infile
						  (concat
						     lisp-library-directory
						     "/"
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
		(t ; we have to macro expand
		   ; certain forms we would normally
		   ; just dump in the eval list.  This is due to hacks in
		   ; the mit lisp compiler which are relied upon by certain
		   ; code from mit.
		   (setq i (d-fullmacroexpand i))
		   
		   (Push g-funcs `(eval ,i)))))))

;--- d-dodef :: handle the def form
; 	- form : a def form: (def name (type args . body))
;
(defun d-dodef (form)
  (prog (g-fname g-ftype g-args body lambdaform symlab annotate
		 g-compfcn g-decls)

     (setq annotate 'empty)
	
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
			  (If fl-anno then (setq annotate nil)) ; no arg info
		   elseif g-flocal
		     then (If (null (or (eq g-ftype 'lambda)
					(eq g-ftype 'nlambda)))
			      then (comp-err "bad type for local fcn" g-ftype))
		   elseif (or (eq g-ftype 'lambda)
				(eq g-ftype 'lexpr))
		     then (Push g-funcs `(lambda ,symlab ,g-fname))
		          (putprop g-fname 'lambda g-functype)
			  (If fl-anno
			     then (If (eq g-ftype 'lambda)
				     then (setq annotate
						(cons (length g-args)
						      (length g-args)))
				     else (setq annotate nil)))
		    elseif (eq g-ftype 'nlambda)
		     then (Push g-funcs `(nlambda ,symlab ,g-fname))
		          (putprop g-fname 'nlambda g-functype)
			  (If fl-anno then (setq annotate nil))
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
	(setq g-compfcn t)	; now compiling a function
	(Push g-locs (cons 'lambda 0))
	(Push g-labs nil)		; no labels in a lambda
	(setq g-currentargs (length g-args))
	(mapc '(lambda (x) (Push g-locs nil) (incr g-loccnt))
	      g-args)
	(d-prelude)			; do beginning stuff
	(d-lambbody lambdaform)		; emit code
	(d-fini)
	(setq g-compfcn nil)		; done compiling a fcn
	(If fl-xref then 
	    (Push g-allf
		  (cons g-fname
			(cons (cond (g-flocal (cons g-ftype 'local))
				    (t g-ftype))
			      g-reflst))))
	(If (and fl-anno (not (eq 'empty annotate)))
	   then (Push g-funcs `(eval (putprop
					',g-fname
					(list ',annotate
					      ,g-complrname)
					'fcn-info))))))
 



;--- d-genlab :: generate a pseudo label
;
(defun d-genlab nil
  (gensym 'L))


;--- liszt-interrupt-signal
; if we receive a interrupt signal (commonly a ^C), then
; unlink the .s file if we are generating a temporary one
; and exit
(defun liszt-interrupt-signal (sig)
   (If (and fl-asm (boundp 'v-sfile) v-sfile)
      then (sys_unlink v-sfile))
   (exit 1))
