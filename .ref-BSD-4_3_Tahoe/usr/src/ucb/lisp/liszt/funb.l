(include-if (null (get 'chead 'version)) "../chead.l")
(Liszt-file funb
   "$Header: funb.l,v 1.13 87/12/15 17:02:17 sklower Exp $")

;;; ----	f u n b				function compilation
;;;
;;;				-[Wed Aug 24 17:14:56 1983 by layer]-

;--- c-declare :: handle the "declare" form
; if a declare is seen inside a function definition, we just 
; ignore it.  We probably should see what it is declareing, as it
; might be declaring a special.
;
(defun c-declare nil nil)

;--- c-do :: compile a "do" expression
;
; a do has this form:
;  (do vrbls tst . body)
; we note the special case of tst being nil, in which case the loop
; is evaluated only once, and thus acts like a let with labels allowed.
; The do statement is a cross between a prog and a lambda. It is like
; a prog in that labels are allowed. It is like a lambda in that
; we stack the values of all init forms then bind to the variables, just
; like a lambda expression (that is the initial values of even specials
; are stored on the stack, and then copied into the value cell of the
; atom during the binding phase. From then on the stack location is
; not used).
;
(defun c-do nil
   (let (b-vrbls b-tst b-body chklab bodylab x-repeat x-vrbs x-fst
		 g-loc g-cc oldreguse (g-decls g-decls))
       (forcecomment '(beginning do))
       (setq g-loc 'reg  chklab (d-genlab)   bodylab (d-genlab))

       (if (and (cadr v-form) (atom (cadr v-form)))
	   then (setq v-form (d-olddo-to-newdo (cdr v-form))))

       (push (cons 'do 0) g-locs)		; begin our frame

       (setq b-vrbls (cadr v-form)
	     b-tst   (caddr v-form)
	     b-body  (cdddr v-form))

       (d-scanfordecls b-body)

       ; push value of init forms on stack
       (d-pushargs (mapcar '(lambda (x)
				(if (atom x)
				    then nil ; no init form => nil
				    else (cadr x)))
			   b-vrbls))

       ; now bind to  the variables in the vrbls form
       (d-bindlamb (mapcar '(lambda (x)
				(if (atom x) then x
				    else (car x)))
			   b-vrbls))

       ; search through body for all labels and assign them gensymed labels
       (push (cons (d-genlab)
		   (do ((ll b-body (cdr ll))
			(res))
		       ((null ll) res)
		       (if (and (car ll) (symbolp (car ll)))
			   then (Push res
				      (cons (car ll) (d-genlab))))))
	     g-labs)

       ; if the test is non nil, we do the test
       ; another strange thing, a test form of (pred) will not return
       ; the value of pred if it is not nil! it will return nil -- in this
       ; way, it is not like a cond clause
       (d-clearreg)
       (if b-tst then (e-label chklab)
	   (let ((g-cc (cons nil bodylab)) g-loc g-ret)
	       (d-exp (car b-tst)))	; eval test
	   ; if false, do body
	   (if (cdr b-tst) 
	       then (setq oldreguse (copy g-reguse))
		    (d-exps (cdr b-tst))
		    (setq g-reguse oldreguse)
	       else  (d-move 'Nil 'reg))
	   (e-goto (caar g-labs))		; leave do
	   (e-label bodylab))		; begin body

       ; process body
       (do ((ll b-body (cdr ll))
	    (g-cc) (g-loc)(g-ret))
	   ((null ll))
	   (if (or (null (car ll)) (not (symbolp (car ll))))
	       then (d-exp (car ll))
	       else (e-label (cdr (assoc (car ll) (cdar g-labs))))
		    (d-clearreg)))

       (if b-tst
	   then ; determine all repeat forms which must be
		; evaluated, and all the variables affected.
		; store the results in x-repeat and  x-vrbs
		; if there is just one repeat form, we calculate
		; its value directly into where it is stored,
		; if there is more than one, we stack them
		; and then store them back at once.
		(do ((ll b-vrbls (cdr ll)))
		    ((null ll))
		    (if (and (dtpr (car ll)) (cddar ll))
			then (Push x-repeat (caddar ll))
			     (Push x-vrbs   (caar ll))))
		(if x-vrbs 
		    then (if (null (cdr x-vrbs))  ; if just one repeat
			     then (let ((g-loc (d-locv (car x-vrbs)))
					(g-cc nil))
				      (d-exp (car x-repeat)))
			     else (setq x-fst (car x-repeat))
				  (d-pushargs (nreverse
						  (cdr x-repeat)))
				  (let ((g-loc (d-locv (car x-vrbs)))
					(g-cc)
					(g-ret))
				      (d-exp x-fst))
				  (do ((ll (cdr x-vrbs) (cdr ll)))
				      ((null ll))
				      (d-move 'unstack
					      (d-locv (car ll)))
				      (setq g-locs (cdr g-locs))
				      (decr g-loccnt))))
		(e-goto chklab))

       (e-label (caar g-labs))			; end of do label
       (d-clearreg)
       (d-unbind)
       (setq g-labs (cdr g-labs))))

;--- d-olddo-to-newdo  :: map old do to new do
;
; form of old do is  (do var tst . body)
; where var is a symbol, not nil
;
(defun d-olddo-to-newdo (v-l)
  `(do ((,(car v-l) ,(cadr v-l) ,(caddr v-l)))
       (,(cadddr v-l))
       ,@(cddddr v-l)))

;--- cc-dtpr :: check for dtprness
;
(defun cc-dtpr nil
  (d-typesimp (cadr v-form) #.(immed-const 3)))

;--- cc-eq :: compile an "eq" expression
;
(defun cc-eq nil
   (let ((arg1 (cadr v-form))
	 (arg2 (caddr v-form))
	 arg1loc
	 arg2loc)
       (if (setq arg2loc (d-simple arg2))
	   then (if (setq arg1loc (d-simple arg1))
		    then ; eq <simple> <simple>
			 (d-cmp arg1loc arg2loc)
		    else ; eq <nonsimple> <simple>
			 (let ((g-loc 'reg)	; put <nonsimple> in reg
			       ; must rebind because
			       ; cc->& may have modified
			       (g-trueop #+(or for-vax for-tahoe) 'jneq
					 #+for-68k 'jne)
			       (g-falseop #+(or for-vax for-tahoe) 'jeql
					  #+for-68k 'jeq)
			       g-cc
			       g-ret)
			     (d-exp arg1))
			 (d-cmp 'reg arg2loc))
	   else ; since second is nonsimple, must stack first
		; arg out of harms way
		(let ((g-loc 'stack)
		      (g-trueop #+(or for-vax for-tahoe) 'jneq #+for-68k 'jne)
		      (g-falseop #+(or for-vax for-tahoe) 'jeql #+for-68k 'jeq)
		      g-cc
		      g-ret)
		    (d-exp arg1)
		    (push nil g-locs)
		    (incr g-loccnt)
		    (setq g-loc 'reg)		; second arg to reg
		    (d-exp arg2))
		(d-cmp 'unstack 'reg)
		(setq g-locs (cdr g-locs))
		(decr g-loccnt)))
   (d-invert))

;--- cc-equal :: compile `equal'
;
(defun cc-equal nil
  (let ((lab1 (d-genlab))
	(lab11 (d-genlab))
	lab2)
       (d-pushargs (cdr v-form))
       (e-cmp '(-8 #.np-reg) '(-4 #.np-reg))
       (e-gotonil lab1)
       (d-calltran 'equal '2)		 ; not eq, try equal.
       (d-clearreg)
       #+(or for-vax for-tahoe) (e-tst (e-cvt 'reg))
       #+for-68k (e-cmpnil (e-cvt 'reg))
       (e-gotot lab11)		
       (if g-loc then (d-move 'Nil g-loc))
       (if (cdr g-cc) then (e-goto (cdr g-cc))
	   else (e-goto (setq lab2 (d-genlab))))
       (e-writel lab1)
       (e-dropnp 2)
       (e-writel lab11)
       (if g-loc then (d-move 'T g-loc))
       (if (car g-cc) then (e-goto (car g-cc)))
       (if lab2 then (e-writel lab2))
       (setq g-locs (cddr g-locs))
       (setq g-loccnt (- g-loccnt 2))))

;--- c-errset :: compile an errset expression
;
; the errset has this form: (errset 'value ['tag])
; where tag defaults to t.
;
(defun c-errset nil
  (let ((g-loc 'reg)
	(g-cc nil)
	(g-ret nil)
	(finlab (d-genlab))
	(beglab (d-genlab)))
       (d-exp (if (cddr v-form) then (caddr v-form) else t))
       (d-pushframe #.F_CATCH (d-loclit 'ER%all nil) 'reg)
       (push nil g-labs)		; disallow labels
       ; If retval is non zero then an error has throw us here so we 
       ; must recover the value thrown (from _lispretval) and leave
       ; If retval is zero then we shoud calculate the expression 
       ; into r0  and put a cons cell around it
       (e-tst '_retval)
       (e-write2 #+(or for-vax for-tahoe) 'jeql #+for-68k 'jeq beglab)
       (e-move '_lispretval (e-cvt 'reg))
       (e-write2 #+(or for-vax for-tahoe) 'jbr #+for-68k 'jra finlab)
       (e-label beglab)
       (let ((g-loc 'stack)
	     (g-cc nil))
	    (d-exp (cadr v-form)))
       (d-move 'Nil 'stack)	; haven't updated g-loc, g-loccnt but it
				; shouldn't hurt (famous last words)
       (e-quick-call '_qcons)
       (e-label finlab)
       (d-popframe)
       (unpush g-locs)		; remove (catcherrset . 0)
       (unpush g-labs)		; remove nil
       (d-clearreg)))

;--- cm-fixnum-cxr :: open code a fixnum-cxr expression.
; 
; fixnum-cxr is a compile only hacky function which accesses an element
; of a fixnum space and boxes the resulting fixnum.  It can be used
; for rapid access to user defined structures.
;
(defun cm-fixnum-cxr ()
  `(internal-fixnum-box (cxr ,@(cdr v-form))))

(defun c-internal-fixnum-box ()
  (let ((g-cc nil)
	(g-ret nil)
	(g-loc '#.fixnum-reg))
       #+for-68k (d-regused '#.fixnum-reg)
       (d-exp (cadr v-form))
       (e-call-qnewint)))

;--- cc-offset-cxr
; return a pointer to the address of the object instead of the object.
;
(defun cc-offset-cxr nil
  (d-supercxr nil t))

;--- cc-fixp :: check for a fixnum or bignum
;
(defun cc-fixp nil
  (d-typecmplx (cadr v-form) 
	       '#.(immed-const (plus 1_2 1_9))))

;--- cc-floatp :: check for a flonum
;
(defun cc-floatp nil
  (d-typesimp (cadr v-form) #.(immed-const 4)))

;--- c-funcall :: compile a funcall
;
; we open code a funcall the resulting object is a compiled lambda.
; We don't open code nlambda and macro funcalls since they are
; rarely used and it would waste space to check for them
(defun c-funcall nil
   (if (null (cdr v-form))
      then (comp-err "funcall requires at least one argument " v-form))
   (let ((g-locs g-locs)
	 (g-loccnt g-loccnt)
	 (args (length (cdr v-form)))
	 (g-loc nil)
	 (g-ret nil)
	 (g-cc nil))
      (d-pushargs (cdr v-form))
      (rplaca (nthcdr (1- args) g-locs) 'funcallfcn)

      (d-exp '(cond ((and (symbolp funcallfcn)
			  (getd funcallfcn))
		     (setq funcallfcn (getd funcallfcn)))))
	     
      (d-exp `(cond ((and (bcdp funcallfcn) (eq 'lambda (getdisc funcallfcn)))
			(Internal-bcdcall ,args t))
		       (t (Internal-bcdcall  ,args nil))))))

;--- c-Internal-bcdcall
; this is a compiler internal function call.  when this occurs, there
;  are argnum objects stacked, the first of which is a function name
;  or bcd object.  If dobcdcall is t then we want to do a bcdcall of
;  the first object stacked.  If it is not true then we want to
;  call the interpreter funcall function to handle it.
;
(defun c-Internal-bcdcall nil
   (let ((argnum (cadr v-form))
	 (dobcdcall (caddr v-form)))
      (cond (dobcdcall (d-bcdcall argnum))
	    (t (d-calltran 'funcall argnum)))))

;--- cc-function :: compile a function function
;
; function is an nlambda, which the interpreter treats as 'quote'
; If the argument is a lambda expression, then Liszt will generate
; a new function and generate code to return the name of
; that function.  If the argument is a symbol, then 'symbol
; is compiled.   It would probably be better to return the function
; cell of the symbol, but Maclisp returns the symbol and it
; would cause compatibility problems.
;
(defun cc-function nil
   (if (or (null (cdr v-form))
	   (cddr v-form))
      then (comp-err "Wrong number of arguments to 'function': " v-form))
   (let ((arg (cadr v-form)))
      (if (symbolp arg)
	 then (d-exp `',arg)
       elseif (and (dtpr arg)
		   (memq (car arg) '(lambda nlambda lexpr)))
	 then (let ((newname (concat "in-line-lambda:"
				     (setq in-line-lambda-number
					   (add1 in-line-lambda-number)))))
		 (Push liszt-process-forms
		       `(def ,newname ,arg))
		 (d-exp `',newname))
	 else (comp-err "Illegal argument to 'function': " v-form))))

;--- c-get :: do a get from the prop list
;
(defun c-get nil
  (if (not (eq 2 (length (cdr v-form))))
      then (comp-err "Wrong number of args to get " v-form))
  (d-pushargs (cdr v-form))		; there better be 2 args
  (e-quick-call '_qget)
  (d-clearreg)
  (setq g-locs (cddr g-locs))
  (setq g-loccnt (- g-loccnt 2)))

;--- cm-getaccess :: compile a getaccess instruction
;
(defun cm-getaccess nil `(cdr ,(cadr v-form)))

;--- cm-getaux :: compile a getaux instruction
;
(defun cm-getaux  nil `(car ,(cadr v-form)))

;--- cm-getd :: compile a getd instruction
;
; the getd function is open coded to look in the third part of a symbol
; cell
;
(defun cm-getd nil `(cxr 2 ,(cadr v-form)))

;--- cm-getdata :: compile a getdata instruction
;
; the getdata function is open coded to look in the third part of an 
; array header.
(defun cm-getdata nil `(cxr 2 ,(cadr v-form)))

;--- cm-getdisc  :: compile a getdisc expression
; getdisc accessed the discipline field of a binary object.
;
(defun cm-getdisc nil `(cxr 1 ,(cadr v-form)))

;--- c-go :: compile a "go" expression
;
; we only compile the (go symbol)type expression, we do not
; allow symbol to be anything by a non null symbol.
;
(defun c-go nil
   ; find number of frames we have to go down to get to the label
   (do ((labs g-labs (cdr labs))
	(locs g-locs)
	(locals 0)
	(specials 0)
	(catcherrset 0)
	(label))
       ((null labs)
	(comp-err "go label not found for expression: " (or v-form)))

       (if (car labs) 		; if we have a set of labels to look at...
	   then (if (setq label
			  (do ((lbs (cdar labs) (cdr lbs)))
			      ((null lbs))
			      (if (eq (caar lbs) (cadr v-form))
				  then (return (cdar lbs)))))
		    then (if (not (eq labs g-labs))
			     then (comp-note g-fname ": non local go used : "
					     (or v-form)))
			 ; three stack to pop: namestack, bindstack
			 ;   and execution stack
			 (e-pop locals)
			 (if (greaterp specials 0)
			     then (e-unshallowbind specials))
			 (if (greaterp catcherrset 0)
			     then (comp-note g-fname
					     ": Go through a catch or errset "
					     v-form)
				  (do ((i 0 (1+ i)))
				      ((=& catcherrset i))
				      (d-popframe)))
			 (e-goto label)
			 (return)))
       ; tally all locals, specials and catcherrsets used in this frame
       (do ()
	   ((dtpr (car locs))
	    (if (eq 'catcherrset (caar locs))
	       then (incr catcherrset)
	     elseif (eq 'progv (caar locs))
	       then (comp-err "Attempt to 'go' through a progv"))
	    (setq specials (+ specials (cdar locs))
		  locs (cdr locs)))
	   (setq locs (cdr locs))
	   (incr locals))))
			
;--- cc-ignore :: just ignore this code
;
(defun cc-ignore nil
  nil)

;--- c-lambexp :: compile a lambda expression
;
(defun c-lambexp nil
  (let ((g-loc (if (or g-loc g-cc) then 'reg))
	(g-cc nil)
	(g-locs (cons (cons 'lambda 0) g-locs))
	(g-labs (cons nil g-labs)))
       (d-pushargs (cdr v-form))		; then push vals
       (d-lambbody (car v-form))
       (d-clearreg)))

;--- d-lambbody :: do a lambda body
;	- body : body of lambda expression, eg (lambda () dld)
;
(defun d-lambbody (body)
   (let ((g-decls g-decls))
      (d-scanfordecls (cddr body))		; look for declarations
      (d-bindlamb (cadr body))		; bind locals
      (d-clearreg)
      (d-exp (do ((ll (cddr body) (cdr ll))
		  (g-loc)
		  (g-cc)
		  (g-ret))
		 ((null (cdr ll)) (car ll))
		 (d-exp (car ll))))

      (d-unbind)))				; unbind this frame

;--- d-bindlamb :: bind  variables in lambda list
;	- vrbs : list of lambda variables, may include nil meaning ignore
;
(defun d-bindlamb (vrbs)
  (let ((res (d-bindlrec (reverse vrbs) g-locs 0 g-loccnt)))
       (if res then (e-setupbind)
		    (mapc '(lambda (vrb) (e-shallowbind (car vrb) (cdr vrb)))
			  res)
		    (e-unsetupbind))))
  
;--- d-bindlrec :: recusive routine to bind lambda variables
;	- vrb : list of variables yet to bind
;	- locs : current location in g-loc
;	- specs : number of specials seen so far
;	- lev  : how far up from the bottom of stack we are.
; returns: list of elements, one for each special, of this form:
;		(<specialvrbname> stack <n>)
;	where specialvrbname is the name of the special variable, and n is
;	the distance from the top of the stack where its initial value is 
;	located
; also: puts the names of the local variables in the g-locs list, as well
;	as placing the number of special variables in the lambda header.
;
(defun d-bindlrec (vrb locs specs lev)
   (if vrb 
       then (let ((spcflg (d-specialp (car vrb)))
		  retv)
		(if spcflg then (setq specs (1+ specs)))

		(if (cdr vrb)		; if more vrbls to go ...
		    then (setq retv (d-bindlrec (cdr vrb)
						(cdr locs)
						specs
						(1- lev)))
		    else (rplacd (cadr locs)
				 specs))	; else fix up lambda hdr

		(if (not spcflg) then (rplaca locs (car vrb))
		    else (Push retv `(,(car vrb) stack ,lev)))

		retv)))

;--- d-scanfordecls
; forms - the body of a lambda, prog or do.
;  we look down the form for 'declare' forms.  They should be at the
;  beginning, but there are macros which may unintentionally put forms
;  in front of user written forms.  Thus we check a little further than
;  the first form.
(defun d-scanfordecls (forms)
   ; look for declarations in the first few forms
   (do ((count 3 (1- count)))
       ((= 0 count))
       (cond ((and (dtpr (car forms))
		   (eq 'declare (caar forms))
		   (apply 'liszt-declare (cdar forms)))))
       (setq forms (cdr forms))))

;--- c-list :: compile a list expression
;
; this is compiled as a bunch of conses with a nil pushed on the
; top for good measure
;
(defun c-list nil
  (prog (nargs)
	(setq nargs (length (cdr v-form)))
	(makecomment '(list expression))
	(if (zerop nargs)
	    then (d-move 'Nil 'reg)	; (list) ==> nil
		 (return))
	(d-pushargs (cdr v-form))
	#+(or for-vax for-tahoe) (e-write2 'clrl '#.np-plus) ; stack one nil
	#+for-68k (L-push (e-cvt 'Nil))

       ; now do the consing
       (do ((i (max 1 nargs) (1- i)))
	   ((zerop i))
	   (e-quick-call '_qcons)
	   (d-clearreg)
	   (if (> i 1) then (L-push (e-cvt 'reg))))

       (setq g-locs (nthcdr nargs g-locs)
	     g-loccnt (- g-loccnt nargs))))

;--- d-mapconvert - access : function to access parts of lists
;		  - join	 : function to join results
;		  - resu	 : function to apply to result
;		  - form	 : mapping form
;	This function converts maps to an equivalent do form.
;
;  in this function, the variable vrbls contains a list of forms, one form
;  per list we are mapping over.  The form of the form is 
;    (dummyvariable  realarg  (cdr dummyvariable))
; realarg may be surrounded by (setq <variable which holds result> realarg)
; in the case that the result is the list to be mapped over (this only occurs
; with the function mapc).
;
(defun d-mapconvert (access join resu form )
   (prog (vrbls finvar acc accform compform
		tmp testform tempvar lastvar)

       (setq finvar (gensym 'X)   ; holds result

	     vrbls
	     (reverse
		 (maplist '(lambda (arg)
			       ((lambda (temp)
				    (cond ((or resu (cdr arg))
					   `(,temp ,(car arg)
					      (cdr ,temp)))
					  (t `(,temp
						(setq ,finvar
						       ,(car arg))
						(cdr ,temp)))))
				(gensym 'X)))
			  (reverse (cdr form))))

	     ; the access form will either be nil or car.  If it is
	     ; nil, then we are doing something like a maplist,
	     ; if the access form is car, then we are doing something
	     ; like a mapcar.
	     acc (mapcar '(lambda (tem)
			      (cond (access `(,access ,(car tem)))
				    (t (car tem))))
			 vrbls)

	     accform (cond ((or (atom (setq tmp (car form)))
				(null (setq tmp (d-macroexpand tmp)))
				(not (member (car tmp) '(quote function))))
			    `(funcall ,tmp ,@acc))
			   (t `(,(cadr tmp) ,@acc)))

	     ; the testform checks if any of the lists we are mapping
	     ; over is nil, in which case we quit.
	     testform (cond ((null (cdr vrbls)) `(null ,(caar vrbls)))
			    (t `(or ,@(mapcar '(lambda (x)
						   `(null ,(car  x)))
					      vrbls)))))

       ; in the case of mapcans and mapcons, you need two
       ; extra variables to simulate the nconc.
       ; testvar gets intermediate results and lastvar
       ; points to then end of the list
       (if (eq join 'nconc)
	   then (setq tempvar (gensym 'X)
		      lastvar (gensym 'X)
		      vrbls `((,tempvar) (,lastvar) ,@vrbls)))

       (return
	   `((lambda
		 (,finvar)
		 (liszt-internal-do
		     ( ,@vrbls)
		     (,testform)
		     ,(cond ((eq join 'nconc)
			     `(cond ((setq ,tempvar ,accform)
				     (cond (,lastvar
					     (liszt-internal-do
						 ()
						 ((null (cdr ,lastvar)))
						 (setq ,lastvar
						       (cdr ,lastvar)))
					     (rplacd ,lastvar ,tempvar))
					   (t (setq ,finvar
						     (setq ,lastvar
							   ,tempvar)))))))
			    (join `(setq ,finvar (,join ,accform ,finvar)))
			    (t accform)))
		 ,(cond ((eq resu 'identity) finvar)
			(resu `(,resu ,finvar))
			(t finvar)))
	     nil ))))

; apply to successive elements, return second arg
(defun cm-mapc nil
	  (d-mapconvert 'car nil nil (cdr v-form)))

; apply to successive elements, return list of results
(defun cm-mapcar nil
	  (d-mapconvert 'car 'cons 'nreverse (cdr v-form)))

; apply to successive elements, returned nconc of results
(defun cm-mapcan nil
	  (d-mapconvert 'car 'nconc 'identity (cdr v-form)))

; apply to successive sublists, return second arg
(defun cm-map nil
	  (d-mapconvert nil nil nil (cdr v-form)))

; apply to successive sublists, return list of results
(defun cm-maplist nil
	  (d-mapconvert nil 'cons 'reverse (cdr v-form)))

; apply to successive sublists, return nconc of results
(defun cm-mapcon nil
	  (d-mapconvert nil 'nconc 'identity (cdr v-form)))

;--- cc-memq :: compile a memq expression
;
#+(or for-vax for-tahoe)
(defun cc-memq nil
  (let ((loc1 (d-simple (cadr v-form)))
	(loc2 (d-simple (caddr v-form)))
	looploc finlab)
       (if loc2
	   then (d-clearreg 'r1)
		(if loc1
		    then (d-move loc1 'r1)
		    else (let ((g-loc 'r1)
			       g-cc
			       g-ret)
			     (d-exp (cadr v-form))))
		(d-move loc2 'reg)
	   else (let ((g-loc 'stack)
		      g-cc
		      g-ret)
		    (d-exp (cadr v-form)))
		(push nil g-locs)
		(incr g-loccnt)
		(let ((g-loc 'reg)
		      g-cc
		      g-ret)
		    (d-exp (caddr v-form)))
		(L-pop 'r1)
		(d-clearreg 'r1)
		(unpush g-locs)
		(decr g-loccnt))
       ; now set up the jump addresses
       (if (null g-loc)
	   then (setq loc1 (if (car g-cc) thenret else (d-genlab))
		      loc2 (if (cdr g-cc) thenret else (d-genlab)))
	   else (setq loc1 (d-genlab)
		      loc2 (d-genlab)))

       (setq looploc (d-genlab))
       (e-tst 'r0)
       (e-write2 'jeql loc2)
       (e-label looploc)
       (e-cmp 'r1 '(4 r0))
       (e-write2 'jeql loc1)
       (e-move '(0 r0) 'r0)
       (e-write2 'jneq looploc)
       (if g-loc
	   then (e-label loc2)		; nil result
		(d-move 'reg g-loc)
		(if (cdr g-cc)
		    then (e-goto (cdr g-cc))
		    else (e-goto (setq finlab (d-genlab))))
	   else (if (cdr g-cc)
		    then (e-goto (cdr g-cc))
		    else (e-label loc2)))
       (if g-loc
	   then (e-label loc1)		; non nil result
		(d-move 'reg g-loc)
		(if (car g-cc) then (e-goto (car g-cc)))
	   else (if (null (car g-cc)) then (e-label loc1)))
       (if finlab then (e-label finlab))))

#+for-68k
(defun cc-memq nil
   (let ((loc1 (d-simple (cadr v-form)))
	 (loc2 (d-simple (caddr v-form)))
	 looploc finlab
	 (tmp-data-reg (d-alloc-register 'd nil)))
       (d-clearreg tmp-data-reg)
       (d-clearreg 'a0)
       (if loc2
	   then (if loc1
		    then (d-move loc1 tmp-data-reg)
		    else (let ((g-loc tmp-data-reg)
			       g-cc
			       g-ret)
			     (d-exp (cadr v-form))))
		(d-move loc2 'reg)
	   else (let ((g-loc 'stack)
		      g-cc
		      g-ret)
		    (d-exp (cadr v-form)))
		(push nil g-locs)
		(incr g-loccnt)
		(let ((g-loc 'reg)
		      g-cc
		      g-ret)
		    (d-exp (caddr v-form)))
		(L-pop tmp-data-reg)
		(unpush g-locs)
		(decr g-loccnt))
       ; now set up the jump addresses
       (if (null g-loc)
	   then (setq loc1 (if (car g-cc) thenret else (d-genlab))
		      loc2 (if (cdr g-cc) thenret else (d-genlab)))
	   else (setq loc1 (d-genlab)
		      loc2 (d-genlab)))
       (setq looploc (d-genlab))
       (e-cmpnil 'd0)
       (e-write2 'jeq loc2)
       (e-move 'd0 'a0)
       (e-label looploc)
       (e-cmp tmp-data-reg '(4 a0))
       (e-write2 'jeq loc1)
       (e-move '(0 a0) 'a0)
       (e-cmpnil 'a0)
       (e-write2 'jne looploc)
       (e-move 'a0 'd0)
       (if g-loc
	   then (e-label loc2)			; nil result
		(d-move 'reg g-loc)
		(if (cdr g-cc)
		    then (e-goto (cdr g-cc))
		    else (e-goto (setq finlab (d-genlab))))
	   else (if (cdr g-cc)
		    then (e-goto (cdr g-cc))
		    else (e-label loc2)))
       (if g-loc
	   then (e-label loc1)			; non nil result
		(d-move 'a0 g-loc)		;a0 was cdr of non-nil result
		(if (car g-cc) then (e-goto (car g-cc)))
	   else (if (null (car g-cc)) then (e-label loc1)))
       (if finlab then (e-label finlab))))
