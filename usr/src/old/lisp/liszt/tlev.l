(include-if (null (get 'chead 'version)) "../chead.l")
(Liszt-file tlev
   "$Header: tlev.l,v 1.17 87/12/15 17:08:51 sklower Exp $")

;;; ----	t l e v				top level interface
;;;
;;;				-[Tue Nov 22 09:21:27 1983 by jkf]-

;--- lisztinit : called upon compiler startup. If there are any args
;	       on the command line, we build up a call to liszt, which
;	       will do the compile. Afterwards we exit.
;
(def lisztinit
   (lambda nil
      (setq fl-asm nil)		; insure it as correct value in case of int
      (let ((args (command-line-args)))
	 (if args
	    then (signal 2 'liszt-interrupt-signal)  ; die on int
		 (signal 15 'liszt-interrupt-signal)  ; die on sigterm
		 (setq user-top-level nil)
		 (exit (apply 'liszt args))
	    else (patom compiler-name)
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
			 asm-exit-status ntem temgc temcp
			 rootreal g-arrayspecs out-path
			 g-decls g-stdref pre-eval include-files
			 g-fname g-trueop g-falseop g-didvectorcode
			 tem temr starttime startptime startgccount
			 fl-asm fl-warn fl-warnfatal fl-verb fl-inter
			 fl-xref fl-uci fl-run fl-case fl-anno g-optionalp
			 liszt-process-forms in-line-lambda-number
			 g-skipcode g-dropnpcnt g-complrname g-fname)

		 ;in case "S" switch given, set asm-exit-status
		 ;  to 0 (so garbage won't be returned).
		 (setq asm-exit-status 0)

		 ; turn on monitoring if it exists
		 #+monitoring
		 (errset (progn (monitor t)	; turn it on
				(print 'monitor-on)
				(terpr))
			 nil)
		 (setq starttime (sys:time)   ; real time in seconds
		       startptime (ptime)
		       startgccount $gccount$)
		 (setq in-line-lambda-number (sys:time))
		 (cond ((null (boundp 'internal-macros))
			(setq internal-macros nil)))
		 (cond ((null (boundp 'macros))
			(setq macros nil)))
		 (setq er-fatal 0  er-warn 0)
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
		       g-reguse #+(or for-vax for-tahoe)
		       			   (copy '((r4 0 . nil) (r3 0 . nil)
						  (r2 0 . nil); (r7 0 . nil)
						  (r1 0 . nil)))
		       		#+for-68k (copy '((a0 0 . nil) (a1 0 . nil)
						  (d1 0 . nil) (d2 0 . nil)
						  (d4 0 . nil) (d5 0 . nil)))
		       g-trancnt 0
		       g-ignorereg nil
		       g-trueop  #+(or for-vax for-tahoe) 'jneq	;used in e-gotot
		       	 	 #+for-68k 'jne
		       g-falseop #+(or for-vax for-tahoe) 'jeql	;u. in e-gotonil
		       		 #+for-68k 'jeq
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
		       g-tranloc  (gensym)
		       g-stdref   (gensym)
		       g-optionalp (gensym))

		 ; declare these special

		 (sstatus feature complr)
		 (d-makespec 't)		; always special

		 ; process input form
		 (setq fl-asm t		; assembler file assembled
		       fl-warn t	; print warnings
		       fl-warnfatal nil	; warnings are fatal
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
		 (if (not (eq '|| (setq tem (getenv 'LISZT))))
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
				      ((eq 'e (car j)) (setq pre-eval (cadr i)
							     i (cdr i)))
				      ((eq 'i (car j)) (push (cadr i)
							     include-files)
				       		       (pop i))
				      ((eq 'w (car j)) (setq fl-warn nil))
				      ((eq 'W (car j)) (setq fl-warnfatal t))
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
				   (setq v-root
					 (apply 'concat
						(reverse (cddr temr)))))
				  (t (setq v-root (car i)
					   rootreal t))))

			   (t (comp-gerr "Extra input file name: " (car i)))))

		 ;no transfer tables in vms
		 (cond (fl-vms (setq fl-tran nil)))

		 ; if verbose mode, print out the gc messages and
		 ; fasl messages, else turn them off.
		 (cond (fl-verb (setq $gcprint t
				      $ldprint t))
		       (t (setq $gcprint nil
				 $ldprint nil)))

		 ; eval arg after -e
		 (if pre-eval
		    then (if (null (errset
				      (eval (readlist (exploden pre-eval)))))
			    then (comp-gerr "-e form caused error: "
					    pre-eval)))

		 ; load file after -i arg
		 (if include-files
		    then (catch
			    (mapc
			       '(lambda (file)
				   (if (null (errset (load file)))
				      then (comp-err
					      "error when loading -i file: "
					      file)))
			       include-files)
			    Comp-error))

		 ; -c says set reader to xlate uc to lc
		 (cond (fl-case (sstatus uctolc t)))

		 ; If we are a cross compiler, then don't try to
		 ; assemble our output...
		 ;
		 #+for-vax
		 (if (or (status feature 68k) (status feature tahoe))
		     then (setq fl-asm nil))
		 #+for-tahoe
		 (if (or (status feature vax) (status feature 68k))
		     then (setq fl-asm nil))
		 #+for-68k
		 (if (or (status feature vax) (status feature tahoe))
		     then (setq fl-asm nil))

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
		 ; strategy: if fl-asm is t (assemble) use (v-root).s
		 ;	     else use /tmp/(PID).s
		 ;  
		 ; direct asm to tty temporarily
		 (setq v-sfile "tty")
		 (setq vp-sfile nil)
		 (if (null fl-tty) then
		     (cond (fl-asm (setq v-sfile
					 (concat '"/tmp/Lzt"
							  (boole 1 65535
								 (sys:getpid))
							  '".s")))
			   (t (setq v-sfile
				    (if v-ofile
					then v-ofile
					else (concat v-root '".s")))))
		     
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
		       ((null fl-profile) (setq v-ofile (concat v-root ".o")))
		       (t (setq v-ofile (concat v-root ".o"))))

		 ; determine the name of the .x file (xref file)
		 ; strategy: if fl-xref and v-ofile is true, then use
		 ; v-ofile(minus .o).x, else use (v-root).x
		 ;
		 (if fl-xref
		    then ; check for ending with .X for any X
			 (setq v-xfile
			       (if v-ofile
				  then (let ((ex (nreverse
						    (exploden v-ofile))))
					  (if (eq #/. (cadr ex))
					     then (implode
						     (nreverse
							`(#/x #/.
							   ,@(cddr ex))))
					     else (concat v-ofile ".x")))
				  else (concat v-root ".x")))
			 (if (portp
				(setq vp-xfile
				      (car (errset (outfile v-xfile)))))
			    thenret
			    else (comp-gerr "Can't open the .x file: "
					    v-xfile)))
		 (cond ((checkfatal) (return 1)))

		 ; g-complrname is a symbol which should be unique to
		 ; each fasl'ed file. It will contain the string which
		 ; describes the name of this file and the compiler
		 ; version.
		 (if fl-anno
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

		 (if fl-verb 
		     then (comp-msg "Compilation begins with " compiler-name )
		          (comp-msg "source: "  v-ifile ", result: "
				    (cond (fl-asm v-ofile) (t v-sfile))))

		 (setq piport vp-ifile)		; set to standard input
		 (setq liszt-root-name v-root
		       liszt-file-name v-ifile)


		 (if fl-run then (d-printautorun))
	
		 (if fl-profile then (e-write1 '".globl mcount"))
	loop

		; main loop of the compiler.  It reads a form and
		; compiles it. It continues to compile forms from
		; liszt-process-forms was long at that list is
		; non-empty.  This allows one form to spawn off other
		; forms to be compiled (an alternative to (progn 'compile))
		;
	        (cond ((atom (list 		; list for debugging,
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
		 (sstatus uctolc nil)	; turn off case conversion
		 			; so bindtab will not have |'s
					; to quote lower case
		 (d-bindtab)

		 (d-printdocstuff)		; describe this compiler

		 (cond ((portp vp-sfile)
			(close vp-sfile)))  ; close assembler language file

		 ; if warnings are to be considered fatal, and if we
		 ; have seen to many warnings, make it fatal
		 (cond ((and fl-warnfatal (> er-warn 0))
			(comp-gerr "Too many warnings")))
		 
		 ; check for fatal errors and don't leave if so
		 (cond ((checkfatal) 
			(if fl-asm   			; unlink .s file
			    then (sys:unlink v-sfile))  ; if it is a tmp
			(return 1)))		; and ret with error status

		 (comp-note "Compilation complete")

		 (setq tem (Divide (difference (sys:time) starttime) 60))
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
				      (setq asm-exit-status
					    (*process
					       (concat
						  lisp-library-directory
						  "/as "
			 #+(or for-vax for-tahoe) "-V"   ; use virt mem
						  " -o "
						  v-ofile
						  " "
						  v-sfile)))))
				(comp-gerr "Assembler detected error, code: "
					   asm-exit-status)
				(comp-note "Assembler temp file " v-sfile
					   " is not unlinked"))
			       (t (comp-note "Assembly completed successfully")
				  (errset (sys:unlink v-sfile)); unlink tmp
				  			       ; file
				  (if fl-run
				      then (errset
					    (sys:chmod v-ofile #O775)))))))

		 #+(and sun (not unisoft))
		 (if (and v-ofile fl-run)
		     then (if (null
			       (errset (let ((port (fileopen v-ofile "r+")))
					    (fseek port 20 0)
					    (tyo 0 port)
					    (tyo 0 port)
					    (tyo 128 port)
					    (tyo 0 port)
					    (close port))))
			      then (comp-err
				    "Error while fixing offset in object file: "
				    v-ofile)))

		 (setq readtable original-readtable)
		 #+monitoring
		 (errset (progn (monitor)	; turn off monitoring
				(print 'monitor-off))
			 nil)
		 (sstatus nofeature complr)
		 (return asm-exit-status))))

(def checkfatal
  (lambda nil
	  (cond ((greaterp er-fatal 0)
		 (catch (comp-err "Compilation aborted due to previous errors")
			Comp-error)
		 t))))

;--- do-lisztrc-check
; look for a liszt init file named
;  .lisztrc  or  lisztrc or $HOME/.lisztrc or $HOME/lisztrc
; followed by .o or .l or nothing
; return the symbol 'error' if an error occured while reading.
;
(defun do-lisztrc-check nil
   (do ((dirs `("." ,(getenv 'HOME)) (cdr dirs))
	(val)
	($gcprint nil)
	($ldprint nil))
       ((null dirs))
       (if (setq val
		 (do ((name '(".lisztrc" "lisztrc") (cdr name))
		      (val))
		     ((null name))
		     (if (setq val
			       (do ((ext '(".o" ".l" "") (cdr ext))
				    (file))
				   ((null ext))
				   (if (probef
					  (setq file (concat (car dirs)
							     "/"
							     (car name)
							     (car ext))))
				      then (if (atom (errset (load file)))
					      then (comp-msg
					"Error loading liszt init file "
						      file N
						      "Compilation aborted" N)
						   (return 'error)
					      else (return t)))))
			then (return val))))
	  then (return val))))

      
;--- liszt-form - i : form to compile
;	This compiles one form.
;
(def liszt-form
  (lambda (i)
     (prog (tmp v-x)
	  ; macro expand
       loop
	  (setq i (d-macroexpand i))
	  ; now look at what is left
	  (cond ((not (dtpr i)) (Push g-funcs `(eval ,i)))
		((eq (car i) 'def)
		 (cond (fl-verb (print (cadr i)) (terpr)(drain)))
		 (d-dodef i))
		((memq (car i) '(liszt-declare declare))
		 (funcall 'liszt-declare  (cdr i)))
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
  (prog (g-ftype g-args body lambdaform symlab g-arginfo g-compfcn g-decls)

     
     (setq g-arginfo 'empty)
	
 loop
	; extract the components of the def form
	(setq g-fname (cadr form))
	(if (dtpr (caddr form))
	    then (setq g-ftype (caaddr form)
		       g-args (cadaddr form)
		       body (cddaddr form)
		       lambdaform (caddr form)
		       symlab (gensym 'F))
	    else (comp-gerr "bad def form " form))
	
	; check for a def which uses the mit hackish &xxx forms.
	; if seen, convert to a standard form and reexamine
	; the vax handles these forms in a special way.
	#+for-68k
	(if (or (memq '&rest g-args) 
		(memq '&optional g-args)
		(memq '&aux g-args))
	    then (setq form 
		       `(def ,(cadr form) ,(lambdacvt (cdr lambdaform))))
	         (go loop))
	
	; check for legal function name.  
	; then look at the type of the function and update the data base.
	(if (null (atom g-fname))
	    then (comp-err "bad function name")
	    else (setq g-flocal (get g-fname g-localf))    ; check local decl.
		 ; macros are special, they are always evaluated
		 ; and sometimes compiled.
		 (if (and (not g-flocal) (eq g-ftype 'macro))
		     then (eval form)
			  (if (and (null macros)
				   (null internal-macros))
			      then (comp-note g-fname
					      " macro will not be compiled")
				   (return nil))
			  (Push g-funcs `(macro ,symlab ,g-fname))
			  (if fl-anno then (setq g-arginfo nil)) ; no arg info
		  elseif g-flocal
		     then (if (null (or (eq g-ftype 'lambda)
					(eq g-ftype 'nlambda)))
			      then (comp-err
				       "bad type for local fcn: " g-ftype))
			  (if (or (memq '&rest g-args)
				  (memq '&optional g-args)
				  (memq '&aux g-args))
			      then (comp-err
				       "local functions can't use &keyword's "
				       g-fname))
		  elseif (or (eq g-ftype 'lambda)
			     (eq g-ftype 'lexpr))
		     then (push `(lambda ,symlab ,g-fname) g-funcs)
			  (putprop g-fname 'lambda g-functype)
		  elseif (eq g-ftype 'nlambda)
		     then (Push g-funcs `(nlambda ,symlab ,g-fname))
			  (putprop g-fname 'nlambda g-functype)
		     else (comp-err " bad function type " g-ftype)))
	(setq g-skipcode nil)	;make sure we aren't skipping code
	(forcecomment `(fcn ,g-ftype ,g-fname))
	(if g-flocal 
	   then (comp-note g-fname " is a local function")
	        (e-writel (car g-flocal))
	   else (if (null fl-vms) then (e-write2 '".globl" symlab))
	        (e-writel symlab))
	(setq g-locs nil g-loccnt 0 g-labs nil g-loc 'reg g-cc nil
	      g-ret t g-topsym (d-genlab))
	(if fl-xref then (setq g-refseen (gensym) g-reflst nil))
	(d-clearreg)
	#+for-68k (init-regmaskvec)
	; set up global variables which maintain knowledge about
	; the stack.  these variables are set up as if the correct
	; number of args were passed.
	(setq g-compfcn t)	; now compiling a function
	(push nil g-labs)		; no labels in a lambda
	(setq g-currentargs (length g-args))
	(d-prelude)			; do beginning stuff
	
	; on the vax, we handle & keywords in a special way in
	; d-outerlambdacomp.  This function also sets g-arginfo.
	#+(or for-vax for-tahoe)
	(d-outerlambdacomp g-fname g-args (cddr lambdaform))
	
	#+for-68k
	(progn
	    (push (cons 'lambda 0) g-locs)
	    (mapc '(lambda (x)
		       (push nil g-locs)
		       (incr g-loccnt))
		  g-args)
	    ; set g-arginfo if this is a lambda. If it is a lexpr, then
	    ; we don't give all the info we could.
	    (setq g-arginfo
	     (if (eq g-ftype 'lambda)
		 then (cons g-loccnt g-loccnt)))
	    (d-lambbody lambdaform))

	(d-fini)
	(setq g-compfcn nil)		; done compiling a fcn
	(if fl-xref then 
	    (Push g-allf
		  (cons g-fname
			(cons (cond (g-flocal (cons g-ftype 'local))
				    (t g-ftype))
			      g-reflst))))
	(if (and fl-anno (not (eq 'empty g-arginfo)))
	   then (Push g-funcs `(eval (putprop
					',g-fname
					(list ',g-arginfo
					      ,g-complrname)
					'fcn-info))))
	; by storing argument count information during compilation
	; we can arg number check calls to this function which occur
	; further on. 
	(if (not (eq 'empty g-arginfo))
	   then (putprop g-fname (list g-arginfo) 'fcn-info))))

;--- d-lambdalistcheck :: scan lambda var list for & forms
; return
;  (required optional rest op-p body)
; required - list of required args
; optional - list of (variable default [optional-p])
; rest - either nil or the name of a variable for optionals
; op-p - list of variables set to t or nil depending if optional exists
; body - body to compile (has &aux's wrapped around it in lambdas)
;
#+(or for-vax for-tahoe)
(defun d-lambdalistcheck (list body)
   (do ((xx list (cdr xx))
	(state 'req)
	(statechange)
	(arg)
	(req)(optional)(rest)(op-p)(aux))
       ((null xx)
	(list (nreverse req)
	      (nreverse optional)
	      rest
	      (nreverse op-p)
	      (d-lambda-aux-body-convert body (nreverse aux))))
       (setq arg (car xx))
       (if (memq arg '(&optional &rest &aux))
	  then (setq statechange arg)
	  else (setq statechange nil))
       (caseq state
	      (req
		 (if statechange
		    then (setq state statechange)
		  elseif (and (symbolp arg) arg)
		    then (push arg req)
		    else (comp-err " illegal lambda variable " arg)))
	      (&optional
		 (if statechange
		    then (if (memq statechange '(&rest &aux))
			    then (setq state statechange)
			    else (comp-err "illegal form in lambda list "
					   xx))
		  elseif (symbolp arg)
		    then ; optional which defaults to nil
			 (push (list arg nil) optional)
		  elseif (dtpr arg)
		    then (if (and (symbolp (car arg))
				  (symbolp (caddr arg)))
			    then ; optional with default
				 (push arg optional)
				 ; save op-p
				 (if (cddr arg)
				    then (push (caddr arg) op-p)))
		    else (comp-err "illegal &optional form "
				   arg)))
	      (&rest
		 (if statechange
		    then (if (eq statechange '&aux)
			    then (setq state statechange)
			    else (comp-err "illegal lambda variable form "
					   xx))
		  elseif rest
		    then (comp-err
			    "more than one rest variable in lambda list"
			    arg)
		    else (setq rest arg)))
	      (&aux
		 (if statechange
		    then (comp-err "illegal lambda form " xx)
		  elseif (and (symbolp arg) arg)
		    then (push (list arg nil) aux)
		  elseif (and (dtpr arg) (and (symbolp (car arg))
					      (car arg)))
		    then (push arg aux)))
	      (t (comp-err "bizzarro internal compiler error ")))))

;--- d-lambda-aux-body-convert :: convert aux's to lambdas
; give a function body and a list of aux variables
; and their inits, place a lambda initializing body around body
; for each lambda (basically doing a let*).
;
#+(or for-vax for-tahoe)
(defun d-lambda-aux-body-convert (body auxlist)
   (if (null auxlist)
      then body
      else `(((lambda (,(caar auxlist))
		,@(d-lambda-aux-body-convert body (cdr auxlist)))
	     ,(cadar auxlist)))))

;--- d-outerlambdacomp :: compile a functions outer lambda body
; This function compiles the lambda expression which defines
; the function.   This lambda expression differs from the kind that
; appears within a function because
;  1. we aren't sure that the correct number of arguments have been stacked
;  2. the keywords &optional, &rest, and &aux may appear
;
; funname - name of function
; lambdalist - the local argument list, (with possible keywords)
; body - what follows the lambdalist
;
; 
;
#+(or for-vax for-tahoe)
(defun d-outerlambdacomp (funname lambdalist body)
   (let (((required optional rest op-p newbody)
	  (d-lambdalistcheck lambdalist body))
	 (g-decls g-decls)
	 (reqnum 0) maxwithopt labs (maxnum -1) args)
       (d-scanfordecls body)
       ; if this is a declared lexpr, we aren't called
       ;
       (if (and (null optional) (null rest))
	   then ; simple, the number of args is required
		; if lexpr or local function, then don't bother
		(if (and (not g-flocal)
			 (not (eq g-ftype 'lexpr)))
		    then (d-checkforfixedargs
			     funname
			     (setq reqnum (setq maxnum (length required)))))
	   else ; complex, unknown number of args
		; cases:
		;  optional, no rest
		;  optional, with rest
		; no optional, rest + required
		; no optional, rest + no required
		(setq reqnum (length required)
		      maxwithopt (+ reqnum (length optional))
		      maxnum (if rest then -1 else maxwithopt))
		; determine how many args were given
		(e-sub3 '#.lbot-reg '#.np-reg '#.lbot-reg)
		#+for-vax (e-write4 'ashl '$-2 '#.lbot-reg '#.lbot-reg)
		#+for-tahoe (e-write4 'shar '$2 '#.lbot-reg '#.lbot-reg)
		;
		(if (null optional)
		    then ; just a rest
			 (let ((oklab (d-genlab))
			       (lllab (d-genlab))
			       (nopushlab (d-genlab)))
			     (if (> reqnum 0)
				 then (e-cmp '#.lbot-reg `($ ,reqnum))
				      (e-write2 'jgeq oklab)
				      ; not enough arguments given
				      (d-wnaerr funname reqnum -1)
				      (e-label oklab))
			     (e-pushnil 1)
			     (if (> reqnum 0)
				 then (e-sub `($ ,reqnum) '#.lbot-reg)
				 else (e-tst '#.lbot-reg))
			     (e-write2 'jleq nopushlab)
			     (e-label lllab)
			     (e-quick-call '_qcons)
			     (d-move 'reg 'stack)
			     #+for-vax (e-write3 'sobgtr '#.lbot-reg lllab)
			     #+for-tahoe (progn (e-sub '($ 1) '#.lbot-reg)
						(e-write2 'bgtr lllab))
			     (e-label nopushlab))
		    else ; has optional args
			 ; need one label for each optional plus 2
			 (do ((xx optional (cdr xx))
			      (res (list (d-genlab) (d-genlab))))
			     ((null xx) (setq labs res))
			     (push (d-genlab) res))
			 ; push nils for missing optionals
			 ; one case for required amount and one for
			 ; each possible number of optionals
			 (e-write4 'casel
				   '#.lbot-reg `($ ,reqnum)
				   `($ ,(- maxwithopt reqnum)))
			 #+for-tahoe (e-write2 '.align '1)
			 (e-label (car labs))
			 (do ((xx (cdr labs) (cdr xx))
			      (head (car labs)))
			     ((null xx))
			     (e-write2 '.word (concat (car xx) "-" head)))
			 ; get here (when running code) if there are more
			 ; than the optional number of args or if there are
			 ; too few args.  If &rest is given, it is permitted
			 ; to have more than the required number
			 (let ((dorest (d-genlab))
			       (again (d-genlab))
			       (afterpush (d-genlab)))
			     (if rest
				 then ; check if there are greater than
				      ; the required number
				      ; preserve arg #
				      (C-push '#.lbot-reg)
				      (e-sub `($ ,maxwithopt) '#.lbot-reg)
				      (e-write2 'jgtr dorest)
				      (C-pop '#.lbot-reg))
			     ; wrong number of args
			     (d-wnaerr funname reqnum maxnum)
			     (if rest
				 then ; now cons the rest forms
				      (e-label dorest)
				      (e-pushnil 1)   ; list ends with nil
				      (e-label again)
				      (e-quick-call '_qcons)
				      (d-move 'reg 'stack)
				      ; and loop
				  #+for-vax (e-write3 'sobgtr '#.lbot-reg again)
				  #+for-tahoe (progn (e-sub '($ 1) '#.lbot-reg)
						     (e-write2 'bgtr again))
				      ; arg #
				      (C-pop '#.lbot-reg)
				      (e-goto afterpush))
			     ; push the nils on the optionals
			     (do ((xx (cdr labs) (cdr xx)))
				 ((null xx))
				 (e-label (car xx))
				 ; if we have exactly as many arguments given
				 ; as the number of optionals, then we stack
				 ; a nil if there is a &rest after
				 ; the optionals
				 (if (null (cdr xx))
				     then (if rest
					      then (e-pushnil 1))
				     else (e-pushnil 1)))
			     (e-label afterpush))))
       ; for optional-p's stack t's
       (mapc '(lambda (form) (d-move 'T 'stack)) op-p)

       ; now the variables must be shallow bound
       ; creat a list of all arguments
       (setq args (append required
			  (mapcar 'car optional)
			  (if rest then (list rest))
			  op-p))

       (push (cons 'lambda 0) g-locs)
       (mapc '(lambda (x)
		  (push nil g-locs))
	     args)
       (setq g-loccnt (length args))
       (d-bindlamb args)  ; do shallow binding if necessary
       ;
       ; if any of the optionals have non null defaults or
       ; optional-p's, we have to evaluate their defaults
       ; or set their predicates.
       ; first, see if it is necessary
       (if (do ((xx optional (cdr xx)))
	       ((null xx) nil)
	       (if (or (cadar xx)  ; if non null default
		       (caddar xx)); or predicate
		   then (return t)))
	   then (makecomment '(do optional defaults and preds))
		; create labels again
		; need one label for each optional plus 1
		(do ((xx optional (cdr xx))
		     (res (list (d-genlab) )))
		    ((null xx) (setq labs res))
		    (push (d-genlab) res))
		; we need to do something if the argument count
		; is between the number of required arguments and
		; the maximum number of args with optional minus 1.
		; we have one case for the required number and
		; one for each optional except the last optional number
		;
		(let ((afterthis (d-genlab)))
		    (e-write4 'casel
			      '#.lbot-reg `($ ,reqnum)
			      `($ ,(- maxwithopt reqnum 1)))
		    #+for-tahoe (e-write2 '.align '1)
		    (e-label (car labs))
		    (do ((xx (cdr labs) (cdr xx))
			 (head (car labs)))
			((null xx))
			(e-write2 '.word (concat (car xx) "-" head)))
		    (e-goto afterthis)
		    (do ((ll (cdr labs) (cdr ll))
			 (op optional (cdr op))
			 (g-loc nil)
			 (g-cc nil)
			 (g-ret nil))
			((null ll))
			(e-label (car ll))
			(if (caddar op)
			    then (d-exp `(setq ,(caddar op) nil)))
			(if (cadar op)
			    then (d-exp `(setq ,(caar op) ,(cadar op)))))
		    (e-label afterthis)))

       ; now compile the function
       (d-clearreg)
       (setq g-arginfo
	     (if (eq g-ftype 'nlambda)
		 then nil
		 else (cons reqnum (if (>& maxnum 0) then maxnum else nil))))
       (makecomment '(begin-fcn-body))
       (d-exp (do ((ll newbody (cdr ll))
		   (g-loc)
		   (g-cc)
		   (g-ret))
		  ((null (cdr ll)) (car ll))
		  (d-exp (car ll))))
       (d-unbind)))

#+(or for-vax for-tahoe)
(defun d-checkforfixedargs (fcnname number)
   (let ((oklab (d-genlab)))
      (makecomment `(,fcnname should-have-exactly ,number args))
      ; calc -4*# of args
      (e-sub '#.np-reg '#.lbot-reg)
      (e-cmp '#.lbot-reg `($ ,(- (* number 4))))
      (e-write2 'jeql oklab)
      (d-wnaerr fcnname number number)
      (e-label oklab)))

;--- d-wnaerr  :: generate code to call wrong number of args error
; name is the function name,
; min is the minumum number of args for this function
; max is the maximum number (-1 if there is no maximum)
;  we encode the min and max in the way shown below.
;
#+(or for-vax for-tahoe)
(defun d-wnaerr (name min max)
   (makecomment `(arg error for fcn ,name min ,min max ,max))
   (e-move 'r10 '#.lbot-reg)
   (C-push `($ ,(+ (* min 1000) (+ max 1))))
   (C-push (e-cvt (d-loclit name nil)))
   #+for-vax (e-write3 'calls '$2 '_wnaerr)
   #+for-tahoe (e-write3 'callf '$12 '_wnaerr))

;--- d-genlab :: generate a pseudo label
;
(defun d-genlab nil
  (gensym 'L))

;--- liszt-interrupt-signal
; if we receive a interrupt signal (commonly a ^C), then
; unlink the .s file if we are generating a temporary one
; and exit
(defun liszt-interrupt-signal (sig)
   (if (and fl-asm (boundp 'v-sfile) v-sfile)
      then (sys:unlink v-sfile))
   (exit 1))
