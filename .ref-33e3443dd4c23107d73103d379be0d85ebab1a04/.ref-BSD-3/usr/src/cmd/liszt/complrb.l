;--- file: complrb.l
(include "compmacs.l")

(setq compiler-name '"Lisp Compiler V3.0")

(setq old-top-level (getd 'top-level))
(setq original-readtable readtable)
(setq raw-readtable (makereadtable t))

;--- lcfinit : called upon compiler startup. If there are any args
;	       on the command line, we build up a call to lcf, which
;	       will do the compile. Afterwards we exit.
;
(def lcfinit
  (lambda nil
	  (cond ((greaterp (argv -1) 1)      ; build up list of args
		 (do ((i (sub1 (argv -1)) (sub1 i)) (arglis))
		     ((lessp i 1) 
		      (exit (apply 'liszt arglis)))
		     (setq arglis (cons (argv i) arglis))))
		(t (patom compiler-name)
		   (terpr poport)
		   (putd 'top-level old-top-level)))))

(putd 'top-level (getd 'lcfinit))




;--- lcf - v-x : list containing file name to compile and optionaly
;		 and output file name for the assembler source.
;
(def liszt
  (nlambda (v-x)
	   (prog (piport v-root v-ifile v-sfile v-ofile 
			 vp-ifile vp-sfile vps-crap
			 vps-include
			 k-pid v-crap tmp rootreal
			 tem temr starttime startptime startgccount
			 fl-asm fl-warn fl-verb fl-inter)

		 (setq starttime (syscall 13)   ; real time in seconds
		       startptime (ptime)
		       startgccount $gccount$)
		 (setq k-lams (setq k-nlams (setq k-macros nil)))
		 (cond ((null (boundp 'internal-macros))
			(setq internal-macros nil)))
		 (cond ((null (boundp 'macros))
			(setq macros nil)))
		 (setq k-free nil) 
		 (setq er-fatal 0)
		 (setq k-ptrs nil)
		 (setq k-disp -4)
		 (setq k-fnum 0)	; function number
		 (setq w-bind nil)
		 (setq vps-include nil)
		 (setq twa-list nil)

		 (setq x-spec (gensym 'S))	; flag for special atom
		 ; declare these special
		 (flag nil x-spec)
		 (flag t x-spec)

		 (sstatus feature complr)

		 ; process input form
		 (setq fl-asm t		; assembler file assembled
		       fl-warn t	; print warnings
		       fl-verb t	; be verbose
		       fl-macl nil	; compile maclisp file
		       fl-inter nil	; print intermediate forms
		       )

		 (do ((i v-x (cdr i)))	; for each argument
		     ((null i))
		     (setq tem (aexplodec (car i)))

		     (cond ((eq '- (car tem))	; if switch
			    (do ((j (cdr tem) (cdr j)))
				((null j))
				(cond ((eq 'S (car j)) (setq fl-asm nil))
				      ((eq 'm (car j)) (setq fl-macl t))
				      ((eq 'o (car j)) (setq v-ofile (cadr i)
							     i (cdr i)))
				      ((eq 'w (car j)) (setq fl-warn t))
				      ((eq 'q (car j)) (setq fl-verb nil))
				      ((eq 'i (car j)) (setq fl-inter t))
				      (t (comp-gerr "Unknown switch: "
						    (car j))))))
			   ((null v-root)
			    (setq temr (reverse tem))
			    (cond ((and (eq 'l (car temr))
					(eq '"." (cadr temr)))
				   (setq rootreal nil)
				   (setq v-root (apply 'concat (reverse (cddr temr)))))
				  (t (setq v-root (car i)
					   rootreal t))))

			   (t (comp-gerr "Extra input file name: " (car i)))))

			   

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


		 (setq k-pid (apply 'concat (cons 'F (cvt (syscall 20)))))
		 ; determine the name of the .s file
		 ; strategy: if fl-asm is t (only assemble) use (v-root).s
		 ;	     else use /tmp/(k-pid).s
		 ;  
		 (cond 	(fl-asm (setq v-sfile (concat '"/tmp/" 
						      k-pid 
						      '".s")))
			(t (setq v-sfile (concat v-root '".s"))))

		 (cond ((not (portp (setq vp-sfile 
					  (car (errset (outfile v-sfile) 
						       nil)))))
			(comp-gerr "Couldn't open the .s file: "
				   (or v-sfile))))
				     
		 
		 ; determine the name of the .o file (object file)
		 ; strategy: if we aren't supposed to assemble the .s file
		 ;	      don't worry about a name
		 ; 	     else if a name is given, use it
		 ;	     else if use (v-root).o
		 (cond ((or v-ofile (null fl-asm)))		;ignore
		       (t (setq v-ofile (concat v-root '".o"))))

		 (cond ((checkfatal) (return 1)))

		 (setq readtable (makereadtable nil))	; use new readtable


		 ; make i/o descriptors to point to crap file then
		 ; unlink crap file so if we die while compiling the crap
		 ; file will disappear
		 (setq v-crap (concat k-pid k-fnum 'crap))
		 (setq tmp (outfile v-crap))		; create output first
		 (setq vps-crap (cons (infile v-crap) tmp))
		 (apply 'syscall `(10 ',v-crap))	; unlink it

		 (emit1 `(".." ,k-pid ,k-fnum :))
		 (emit1 '".long linker")
		 (emit1 '".long BINDER")

		 ; if the macsyma flag is set, change the syntax to the
		 ; maclisp standard syntax.  We must be careful that we
		 ; dont clobber any syntax changes made by files preloaded
		 ; into the compiler.

		 (cond (fl-macl (setsyntax '\/ 143) 	;  143 = vesc

				(cond ((equal 143 (status syntax \\))
				       (setsyntax '\\ 2)))

				(setsyntax '\| 138)	;  138 = vdq
				(cond ((equal 138 (status syntax \"))
				       (setsyntax '\" 2)))
				(cond ((equal 198 (status syntax \[))
				       (setsyntax '\[ 2)
				       (setsyntax '\] 2)))
				(setq ibase  8.)
				(sstatus uctolc t)
				
				(flag 'ibase x-spec)	; to be special
				(flag 'base  x-spec)
				(flag 'tty   x-spec)

				(errset (cond ((null (getd 'macsyma-env))
					       (load 'machacks)))
					nil)))

		 (cond ((checkfatal) (return 1)))  ; leave if fatal errors	

		 (comp-note "Compilation begins with " (or compiler-name))
		 (comp-note "source: " (or v-ifile) ", result: "
			    (cond (fl-asm v-ofile) (t v-sfile)))
		 (setq piport vp-ifile)		; set to standard input

	loop
		;(cond ((atom (errset (do ((i (read) (read))) 
		;			  ((eq i 'eof) nil)
		;			  (cleanup)
		;			  (lcfform i))))
		;	(patom '"error during compilation, I quit")))

	        (cond ((atom (errset 
			      (do ((i (read piport '<<end-of-file>>) 
				      (read piport '<<end-of-file>>))) 
				  ((eq i '<<end-of-file>>) nil)
				  (cleanup)
				  (catch (lcfform i) Comp-error))))
		       (comp-note "Lisp error during compilation")
		       (setq piport nil)
		       (setq er-fatal (add1 er-fatal))
		       (return 1)))

		 (close piport)

		 (cond ((checkfatal) (return 1)))
			
		 ; if doing special character stuff (maclisp) reassert
		 ; the state

		 (cond (vps-include
			(comp-note  " done include")
			(setq piport (car vps-include))
			(setq vps-include (cdr vps-include))
			(go loop)))

		 ; reset input base
		 (setq ibase 10.)


		 (close (cdr vps-crap))

		 (setq vp-ifile (car vps-crap))		; read crap file

		 ((lambda (readtable)
			  (do ((i (read vp-ifile '<<end-of-file>>) 
				  (read vp-ifile '<<end-of-file>>)))
			      ((eq i '<<end-of-file>>) nil)
			      (setq w-bind (cons (list 0 i 'Crap) w-bind)))

			  (cm-alist))
		  raw-readtable)

		 (close vp-sfile)		; close assembler language file
		 (comp-note "Compilation complete")

		 (setq tem (Divide (difference (syscall 13) starttime) 60))
		 (comp-note " Real time: " (car tem) " minutes, "
			    (cadr tem) " seconds")
		 (setq tem (ptime))
		 (setq temr (Divide (difference (car tem) (car startptime))
				    3600))
		 (comp-note " CPU time: " (car temr) " minutes, "
			    (quotient (cadr temr) 60.0) " seconds")
		 (setq temr (Divide (difference (cadr tem) (cadr startptime))
				    3600))
		 (comp-note " of which " (car temr) " minutes and "
			    (quotient (cadr temr) 60.0) 
			    " seconds were for the "
			    (difference $gccount$ startgccount)
			    " gcs which were done")


		 (cond (fl-asm 			; assemble file 
			 (comp-note "Assembly begins")
			 (cond ((not 
				 (zerop 
				    (setq tmp
					  (apply 'process 
						 (ncons (concat '"as -o "
								    v-ofile
								    '" "
								    v-sfile))))))
				(comp-gerr "Assembler detected error, code: "
					   (or tmp)))
			       (t (comp-note "Assembly completed successfully")))))
		 (cond (fl-asm (apply 'syscall `(10 ',v-sfile))))

		 (setq readtable original-readtable)
		 (return 0))))

(def checkfatal
  (lambda nil
	  (cond ((greaterp er-fatal 0)
		 (comp-note "Compilation aborted")
		 t))))


;--- lcfform - i : form to compile
;	This compiles one form.
;
(def lcfform
  (lambda (i)
     (prog (tmp v-x)
	  ; macro expand
	  (setq i (cmacroexpand i))
	  ; now look at what is left
	  (cond ((eq (car i) 'def) ; jkf mod
		 (cond (fl-verb (print (cadr i)) (terpr)(drain)))
		 (dodef i))
		((eq (car i) 'declare) (dodcl i))
		((eq (car i) 'eval-when) (doevalwhen i))
		((and (eq (car i) 'progn) (equal (cadr i) '(quote compile)))
		 ((lambda (internal-macros)	; compile macros too
			  (mapc 'lcfform (cddr i)))
		       t))
		((or (eq (car i) '"%include")
		     (eq (car i) '"include"))
		 (cond ((or (portp (setq v-x 
					 (car (errset (infile (cadr i)) nil))))
			    (portp (setq v-x 
					 (car (errset (infile (concat '"/usr/lib/lisp"
							     (cadr i))) 
						      nil)))))
			(setq vps-include (cons piport vps-include))
			(setq piport v-x)
			(comp-note " INCLUDEing file: " (cadr i)))
		       (t (comp-gerr "Cannot open include file: " (cadr i)))))
		(t ((lambda (readtable) 
			    (print i (cdr vps-crap))
			    (terpr (cdr vps-crap)))
		    raw-readtable))))))

;--- cmacroexpand - i : functional form
;	the form is macro expanded on the top level as many times as
;	possible.
;
(def cmacroexpand
  (lambda (i)
	  (cond ((atom i) i)
		(t (do ((j (ismacro (car i)) (ismacro (car i)))
			(tmp))
		       ((null j)  i)
		       (cond ((bcdp j)
			      (putd (setq tmp (Gensym nil))
				    (mfunction (getentry j) 'nlambda)))
			     (t (setq tmp (cons 'nlambda (cdr j)))))
		       (setq i (apply tmp i))
		       (cond ((atom i) (return i))))))))

(def dodef
  (lambda (v-f)
	  (prog (v-n v-t v-c w-save w-ret w-labs w-locs)
		(setq k-current (setq v-n (cadr v-f)))	; v-n <= name of func
		; add function to approp. list
		(cond ((or (eq (setq v-t (caaddr v-f)) 'lambda)
			   (eq v-t 'lexpr))
		       (setq k-lams (cons (list v-n t) k-lams)
			     k-ftype v-t
			     v-t 'lambda))
		      ((eq v-t 'nlambda)
		       (setq k-nlams (cons (list v-n t) k-nlams)
			     k-ftype 'nlambda))
		      ((eq v-t 'macro)
		       (setq k-macros (cons (list v-n (caddr v-f)) k-macros))
		       (setq k-ftype 'macro)
		       (eval v-f)
		       ; if macros is nil, we do not compile this macro
		       (cond ((and (null macros)
				   (null internal-macros))
			      (return nil))))
		      (t (comp-err (or v-n) " has an unknown function type"
				   (v-f))))


		(setq v-c (concat k-pid k-fnum))	; v-c <= unique name
		(setq k-fnum (add1 k-fnum))
		(cm-bind v-c v-n v-t)			; update k-regs
		(setq v-t (f-func (cdaddr v-f))) ; do parse
		(emit3 '# v-c v-n)			; put out header
		(cm-alst4 v-n)
		(cond (fl-inter (print v-t)(terpr)))
		(cm-emit v-t v-c))))			; emit code
 
;--- doevalwhen, process evalwhen directive. This is inadequate.
;
 (def doevalwhen 
      (lambda (v-f)
	      (prog (docom dolod)
		    (setq docom (member 'compile (cadr v-f))
			  
			  dolod (member 'load (cadr v-f)))
		    (mapc '(lambda (frm) (cond (docom (eval frm)))
					 (cond (dolod 
						((lambda (internal-macros) 
							 (lcfform frm))
						 t))))
			  (cddr v-f)))))


;---- dodcl - v-f declare form
;	process the declare form given. We evaluate each arg
;
(def dodcl 
  (lambda (v-f)
	  (setq v-f (cdr v-f))
	  (do ((i (car v-f) (car v-f))) 
	      ((null i))
	      (setq v-f (cdr v-f))
	      (cond ((getd (car i)) (eval i)) ; if this is a function
		    (t (comp-warn "Unknown declare attribute: " (car i)))))))

;---> handlers for declare forms
;
(def *fexpr
  (nlambda (args)
	   (mapc '(lambda (v-x)
			  (setq k-nlams (cons (list v-x t) k-nlams)))
		 args)))
(def special
  (nlambda (v-l) 
	   (mapc '(lambda (v-a)
			  (unflag v-a x-con) 
			  (flag v-a x-spec)) 
		 v-l)
	   t))
(def unspecial
  (nlambda (v-l) 
	   (mapc '(lambda (v-a) 
			  (unflag v-a x-spec)) 
		 v-l)
	   t))

(def *expr (nlambda (args) nil))	; ignore

(def macros (nlambda (args) (setq macros (car args))))
;---> end declare form handlers


(def cm-bind
  (lambda (v-lab v-atm v-type)
	  (setq w-bind (cons (list v-lab v-atm v-type) w-bind))))

(def cm-emit 
  (lambda (v-t v-nm) 
	  (setq k-back (setq k-regs nil)) 
	  (setq k-code v-t) 
	  (prog (v-i v-l) 
		(emit2 '".globl" v-nm)
		(emit1 (list v-nm ':))
	   next (cond ((null k-code) (return))) 
		(setq v-i (car k-code)) 
		(setq k-code (cdr k-code)) 
		(setq v-l (get (car v-i) x-emit)) 
		(cond ((null (cdr v-i)) 
		       (funcall v-l) 
		       (go next)) 
		      ((ifflag (car v-i) x-asg) 
		       (setq v-t (e-reg (cadr v-i) nil))) 
		      (t (setq v-t (cadr v-i)))) 
		(apply v-l (rplaca (cdr v-i) v-t)) 
		(go next))))

;--- cm-alist  - print out the list of special lispvalues we reference
;		 in compiled code
;

(def cm-alist
  (lambda nil
	  (prog (cm-alv)
		(cond (faslflag (emit1 '".text"))
		      (t (emit1 '".data")))
		(emit1 '".align 2")
		(emit1 '"lbnp: .long _bnp")
		(emit1 '"lfun: .long __qfuncl")
		(emit1 '"lf4: .long __qf4")
		(emit1 '"lf3: .long __qf3")
		(emit1 '"lf2: .long __qf2")
		(emit1 '"lf1: .long __qf1")
		(emit1 '"lf0: .long __qf0")
		(emit2 '"lgc: .long" 0)
		(emit1 '"linker:" )
		(mapc 'cm-alst1 (reverse k-ptrs))
		(emit2 '".long" -1)
		(cond (faslflag (emit1 '".data"))
		      (t (emit1 '".text")))
		(emit1 '".align 2")
		(emit1 '"B:")
		(emit1 '"BINDER:")
		(mapc 'cm-alst2 (reverse w-bind))
		(emit4 '".long" -1 -1 -1)
		(emit1 '"litstrt:")
		(mapc 'cm-alst3 (reverse cm-alv))
		(emit1 '"litend:")
		(cleanup))))


(def cm-alst1
  (lambda (v-x)
	  (prog (v-g)
		(setq v-g (Gensym 's))
		(emit2 '".long" (list v-g '-B))
		(putprop v-g (car v-x) 'label)
		(setq cm-alv (cons v-g cm-alv)))))

(def cm-alst2
  (lambda (v-x)
	  (prog (v-g)
		(emit2 '".long" (car v-x))
		(setq v-g (Gensym 's))
		(emit2 '".long" (list v-g '-B))
		(putprop v-g (cadr v-x) 'label)
		(setq cm-alv (cons v-g cm-alv))
		(setq v-g (caddr v-x))
		(emit2 '".long"
		       (cond ((eq v-g 'lambda) 0)
			     ((eq v-g 'nlambda) 1)
			     ((eq v-g 'macro) 2)
			     ((eq v-g 'Crap) 99)
			     (t 'UDEF_TYPE))))))

(def cm-alst3
  (lambda (v-x)
	  ($pr$ v-x)
	  ($pr$ '": ")
	  (setq v-x  (get v-x 'label))
	  (cm-alst4 v-x)))

;--- cm-alst4  - v-x : s-expression
;	the given expression is exploded and printed as a string to the
;	assembler, this requires that each character be individually
;	noted and that the number of bytes on a line be limited.
;
(def cm-alst4
  (lambda (v-x)
	  ($pr$ '".byte ")
	  (do ((l (explode v-x) (cdr l))
	       (cnt 1 (add1 cnt)))
	      ((null l) ($pr$ 0) ($terpri))
	      ($pr$ '\')
	      ($pr$ (car l))
	      (cond ((greaterp cnt 13) ($terpri) ($pr$ '".byte ") (setq cnt 0))
		    (t ($pr$ '\,))))))
;--- w-save
;	stack the values of w-ret and w-labs
;
(def w-save
  (lambda nil (setq w-save (cons `(,w-ret ,w-labs ,w-locs) w-save))))

;--- w-unsave
;	restore the values of w-ret  and w-labs, popping them
; off the w-save stack
;
(def w-unsave
  (lambda nil (setq w-ret (caar w-save) 
		    w-labs (cadar w-save)
		    w-locs (caddar w-save)
		    w-save (cdr w-save))))


;--- f-exp - v-e form to evaluate
;	   - v-r location to place result in.
;	   - v-t restof stuff (intermidiate forms)
;
;	This is the real workhorse of the compiler.
;
(def f-exp 
  (lambda (v-e v-r v-t) 
	  (prog (v-f v-i v-tem) 
	    begin (cond ; atoms
			((f-one v-e) 
			 ; if the symbol has not been declared special and is
			 ; not a local variable, we declare it special.
			 (g-specialchk v-e)
			 (return (f-addi (list 'get v-r v-e) v-t)))

			; lambda expressions, we do the correct thing.
			; should check for bad forms here rather than call
			; f-chkf
			((not (atom (setq v-f (car v-e)))) 
			 (setq v-f (cmacroexpand v-f))
			 ; must check if the expression changes to an atom
			 (cond ((atom v-f) 
				(setq v-e (cons v-f (cdr v-e)))
				(go begin)))

			 (cond ((eq 'lambda (car v-f))
				(return (f-lambexp v-e v-r v-t)))
			       ; this case is necessary to compile
			       ; ('add 1 2)  which the interpreter will
			       ; handle and I guess we should too
			       ((eq 'quote (car v-f))
				(comp-warn "Bizzare function name " (or v-f) N)
				(setq v-e (cons (cadr v-f) (cdr v-e)))
				(go begin))
			       (t (comp-err " Illegal expression: "
					    (or v-f) 
					    N))))

			; macro expand and continue
			((and (or (setq v-e (cmacroexpand v-e)) t)
			      (cond ((or (atom v-e)
					 (not (atom (car v-e))))
				     (go begin))  	; if reduce to atom
							; or lambda exp
				    (t (setq v-f (car v-e))))
			      nil))

			; special functions
			((setq v-i (get v-f x-spf)) (go special)) 
			((setq v-i (get v-f x-spfq))
			 (put v-f x-spfq nil)
			 (go special))
			((setq v-i (get v-f x-spfn)) (go special)) 
			((setq v-i (get v-f x-spfh)) 
			 (setq v-e (funcall v-i v-e)) 
			 (go normal)) 

			; macro within compiler
			((setq v-i (get v-f 'x-spfm))
			 (setq v-e (funcall v-i v-e))
			 (go begin))

			; nlambbdas, we quote the args
			((isnlam v-f) 
			 (setq v-e (list v-f (list 'quote (cdr v-e)))) 
			 (go normal)) 


			; cxr form where x is elt of {a d}
			((setq v-i (chain v-f)) 
			 (setq v-t (f-addi 
				    (list 'chain 
					   v-r 
					   (setq v-r (f-use (Gensym nil)))
					  v-i)
				    v-t)) 
			 (setq v-e (cadr v-e))   ; calc expr to new v-r
			 (go begin)) 

			; if this is not the last form before a return,
			; we go to normal to do a function invocation
			; otherwise we look to see if tail merging is
			; possible.
			((not (eq (caar v-t) 'return)) (go normal)) 
			((or (eq (setq v-i w-bv) t) 
			     (not (equal v-f w-name))) (go normal)) 
			((not (f-iter (cdr v-e) (reverse v-i))) (go normal)) ) 

		; do tail merging.
		(setq v-t (f-addi '(repeat) v-t)) 
		(setq v-e (reverse (cdr v-e))) 
	iterate (cond ((null v-e) (return v-t)) 
		      ((equal (car v-e) (car v-i)) (go next))) 
		(setq v-t (f-addi (list 'set 
					(setq v-r (f-reg 'set))
					(car v-i)) 
				  v-t)) 
		(setq v-t (f-exp (car v-e) v-r v-t)) 
	   next (setq v-e (cdr v-e)) 
		(setq v-i (cdr v-i)) 
		(go iterate) 

		; the function will be handled specially by the compiler
	special (cond ((setq v-i (funcall v-i (cdr v-e) v-r v-t))
		       (return v-i))) 

		; normal handling, call function.
		; if this is a system function, do it quickly
	 normal (cond ((setq v-i (get (car v-e) 'x-sysf))	; system fcn
		       (setq v-t 
			     (f-pusha (cdr v-e) 
				      (Gensym nil) 
				      (f-addi `(call ,(f-make v-r r-xv) 
						     ,v-i
						     ,(length (cdr v-e))) 
					      v-t))))
		      (t (setq v-t 
			       (f-pusha `((quote ,(car v-e)) ,@(cdr v-e))
				       (Gensym nil)
				       (f-addi `(call ,(f-make v-r r-xv) 
						      nil
						      ,(length v-e)) 
					       v-t))))) 
		 
		(return v-t))))

;--- g-specialchk - v-e : expression
; if v-e is a symbol and not declared special and not a local variable
; we complain and delare it special
; v-e is returned.
;
(def g-specialchk
  (lambda (v-e)
	  (cond ((and (symbolp v-e) 
		      (not (get v-e x-spec))
		      (not (member v-e w-locs)))
		 (flag v-e x-spec)
		 (comp-warn (or v-e) " declared special by compiler")))
	  v-e))


;--- f-lambexp - v-e : lambda expression: ((lambda (x y z) exp) a b c)
;	       - v-r : weather where result should be placed
;	       - v-t : tail
;
;	This compiled a lambda expression.  This is a very simple do-expression
; with the difference that returns are not allowed from within it.

(def f-lambexp
  (lambda (v-e v-r v-t)
	  (f-pusha (cdr v-e) 
		   (Gensym nil)
		   (f-lambbody (cdar v-e) v-r (length (cadar v-e)) v-t))))

;--- f-lambbody - v-e : args + body of lambda ((a b c) exp1 exp2 ...)
;		- v-ags : number of args pushed for this lambda, it will
;			  normally equal the length of (cadr v-e) but
;			  in the case of the top level lambda expression
;			  in a function it will be 0
;		- v-r : psreg to place result in
;		- v-t : tail
;	We emit the intermediate expressions necessary to evaluate the
;	lambda body
;
(def f-lambbody
  (lambda (v-e v-r v-ags v-t)
	  (w-save)			; stack old values
	  (prog (w-ret w-labs tmp)
		(setq tmp `((begin ,v-ags)
			    ,@(mapcar '(lambda (arg) (setq w-locs
							   (cons arg w-locs))
						    `(bind ,arg))
				      (car v-e))
			    ,@(f-seq (cdr v-e) 
				     v-r 
				     `((end nil)
				       ,@v-t))))
		(w-unsave)
		(return tmp))))

;--- f-func - v-l    : function args and body.
;
;	result is: (entry type) 	; type is lambda,lexpr, macro 
;						or nlambda
;		   ..body.. 
;		   
;		   (fini) 
;
(def f-func 
  (lambda (v-l) 
	  `((entry ,k-ftype)
	    ,@(f-lambbody v-l 'xv 0 '((fini))))))


;--- f-prog - v-l : args + prog body
;	    - v-r : psreg to store result in
;	    - v-t : tail
;
(def f-prog
  (lambda (v-l v-r v-t)
	  (w-save)
	  (prog (w-ret tmp retlb w-labs)
		(setq tmp (length (car v-l))	; number of locals
		      retlb (Gensym nil)	; label to leave prog
		      w-labs (Gensym nil)	; hang labels here
		      w-ret `(,v-r . (go ,retlb)))
		
		(setq tmp `((pushnil ,tmp)	; start out with nils
			    (begin ,tmp)		; declare variables
			    ,@(mapcar '(lambda (arg) (setq w-locs
							   (cons arg w-locs))
						    `(bind ,arg))
				      (car v-l))	; bind locals
			    ,@(f-seqp (cdr v-l) (Gensym nil) 
				      `((get ,v-r nil)
					(end ,retlb)
					,@v-t))))
		(w-unsave)
		(return tmp))))


