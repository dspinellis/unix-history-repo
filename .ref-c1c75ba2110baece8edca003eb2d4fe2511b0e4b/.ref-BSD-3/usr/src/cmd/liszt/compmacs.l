;---file: compmacs.l
;----------- macros for the compiler -------------


(declare (special old-top-level compiler-name
		  readtable original-readtable raw-readtable
		  poport piport
		  v-root v-ifile v-sfile
		  vps-include vps-crap vp-sfile
		  er-fatal ibase
		  macros
		  x-spec
		  fl-asm fl-macl faslflag fl-inter
		  k-macros k-lams k-nlams k-free internal-macros
		  k-fnum k-current k-code k-ptrs k-ftype  k-pid
		  k-back k-regs
		  twa-list
		  s-inst
		  x-con x-asg x-dont			; check on this
		  x-reg x-leap x-opt
		  x-emit
		  w-vars w-labs w-ret w-save
		  r-xv
		  x-spfh x-spfn x-spfq x-spf
		  w-bind
		  w-name w-bv w-locs w-atmt cm-alv v-cnt
		  $gccount$))

(def $pr$ (macro (x) `(patom ,(cadr x) vp-sfile)))

(def put 
  (macro (x)
	 ((lambda (atm prp arg)
		  `(progn (putprop ,atm ,arg ,prp) ,atm))
	  (cadr x) (caddr x) (cadddr x))))

;--- comp-err
;    comp-warn
;    comp-note
;    comp-gerr
; these are the compiler message producing macros.  The form is
; (comp-xxxx val1 val2 val3 ... valn) , all values are printed according
;  to this scheme. If vali is an atom, it is patomed, if vali is a
;  list, it is evaluated and printed. If vali is N a newline is printed
; 
; furthermore
;    the name of the current function is printed first
;    after comp-err prints the message, it does a throw to Comp-err .
;    errors are preceeded by Error: 
;	warnings by %Warning: and
;	notes by %Note:
;     The message is sent to the message file
;
(def comp-err
  (macro (l)
	 `(progn ,@(comp-msg 
			     `( Error: (or k-current) ": " ,@(cdr l) N))
		 (throw nil Comp-error))))

(def comp-warn
  (macro (l)
	 `(progn ,@(comp-msg
			     `( %Warning: (or k-current) ": " ,@(cdr l) N)))))

(def comp-note
  (macro (l)
	 `(progn ,@(comp-msg
			     `( %Note:  ,@(cdr l) N)))))

(def comp-gerr
  (macro (l)
	 `(progn ,@(comp-msg
			`(?Error: ,@(cdr l) N)) 
		 (setq er-fatal (add1 er-fatal)))))
;--- comp-msg - port
;	      - lst
;  prints the lst to the given port.  The lst is printed in the manner
; described above, that is atoms are patomed, and lists are evaluated
; and printed, and N prints a newline.   The output is always drained.
;
(eval-when (compile eval)
  (def comp-msg
       (lambda (lis)
	       (cond ((null lis) `((drain)))
		     (t `(,(cond ((atom (car lis))
				  (cond ((eq (car lis) 'N)
					 `(terpr))
					(t `(patom ',(car lis)))))
				 (t `(print ,(car lis))))
			   ,@(comp-msg (cdr lis))))))))

