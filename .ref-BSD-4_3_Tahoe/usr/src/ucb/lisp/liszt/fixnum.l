(include-if (null (get 'chead 'version)) "../chead.l")
(Liszt-file fixnum
   "$Header: /usr/src/local/franz/liszt/RCS/fixnum.l,v 1.16 88/04/26 11:50:18 sklower Exp $")

;;; ----	f i x n u m		fixnum compilation
;;;
;;;				-[Fri Aug 26 14:07:53 1983 by layer]-

;  There are a few functions in lisp which are only permitted to take
; fixnum operands and produce fixnum results.  The compiler recognizes
; these functions and open codes them.
;

;--- d-fixnumexp :: compute a fixnum from an expression
;	x - a lisp expression which must return a fixnum
;
; This is an almost equivalent to d-exp, except that
; 1] it will do clever things if the expression can be open coded in a 
;    fixnum way.
; 2] the result must be a fixnum, and is left in r5 unboxed.
;
(defun d-fixnumexp (x)
  (d-fixnumcode (d-fixexpand x)))


;--- c-fixnumop :: compute a fixnum result
;  This is the extry point into this code from d-exp.  The form to evaluate
; is in v-form.  The only way we could get here is if the car of v-form
; is a function which we've stated is a fixnum returning function. 
;
(defun c-fixnumop nil
  (d-fixnumexp v-form)
  (d-fixnumbox))

;--- d-fixnumbox :: rebox a fixnum in r5
;
#+(or for-vax for-tahoe)
(defun d-fixnumbox ()
   (let (x)
	(e-write3 'moval (concat "*$5120[" '#.fixnum-reg "]") 'r0)
	(e-sub3 '($ 1024) '#.fixnum-reg 'r1)
	(e-write2 'blssu (setq x (d-genlab)))
	(e-call-qnewint)
	(e-writel x)
	(d-clearreg)))

#+for-68k
(defun d-fixnumbox ()
   (let (x)
	(d-regused '#.fixnum-reg)
	(e-move '#.fixnum-reg 'd0)
	(e-write3 'asll '($ 2) 'd0)
	; add onto the base of the fixnums
	(e-add (e-cvt '(fixnum 0)) 'd0)
	(e-move '#.fixnum-reg 'd1) 
	(e-sub '($ 1024) 'd1)
	(e-write2 'jcs (setq x (d-genlab)))	;branch carry set
	(e-call-qnewint)
	(e-writel x)
	(d-clearreg)))

;--- d-fixexpand  :: pass over a fixnum expression doing local optimizations
; 
; This code gets the first look at the operands of a fixnum expression.
; It handles the strange cases, like (+) or (/ 3), and it also insures
; that constants are folded (or collapsed as we call it here).
; 
; things to watch out for:
; (+ x y z) we can fold x,y,z , likewise in the case of *
; (- x y z) we can only fold y and z since they are negated but x is not,
;	    likewise for /
(defun d-fixexpand (x)
  (prog nil
	(setq x (d-macroexpand x))
    loop
	(if (and (dtpr x) (symbolp (car x)) (get (car x) 'fixop))
	    then (if (memq (car x) '(+ *))
		     then  (setq x (cons (car x)
					 (d-collapse (cdr x) (car x))))
		     else  (setq x
				 (cons (car x)
				       (cons (cadr x)
					     (d-collapse (cddr x) (car x))))))
	         (if (null (cdr x))
		     then  ; (- or +) => 0 (* or /) => 1
		         (setq x
			       (cdr (assq (car x)
					  '((+ . 0) (- . 0)
					    (* . 1) (/ . 1)))))
		         (go loop)
		  elseif (null (cddr x)) then
		           ; (+ n) => n, (- n) => (- 0 n), (* n) => n,
			   ; (/ n) => (/ 1 n)
			  (setq x
				(if (memq (car x) '(* +))
				    then (cadr x)
				 elseif (eq (car x) '-)
				    then `(- 0 ,(cadr x))
				 elseif (eq (car x) '/)
				    then `(/ 1 ,(cadr x))
				    else (comp-err
					     "Internal fixexpand error ")))
			  (go loop)))
	(return x)))

;--- d-toplevmacroexpand :: expand top level form if macro
; a singe level of macro expansion is done.  this is a nice general
; routine and should be used by d-exp.
;**** out of date **** will be removed soon
(defun d-toplevmacroexpand (x)
  (let ((fnbnd (and (dtpr x) (symbolp (car x)) (getd (car x)))))
       (if (and fnbnd (or (and (bcdp fnbnd) (eq (getdisc fnbnd) 'macro))
			  (and (dtpr fnbnd) (eq (car fnbnd) 'macro))))
	   then (d-toplevmacroexpand (apply fnbnd x))
	   else x)))


;--- d-collapse :: collapse (fold) constants
; 
; this is used to reduce the number of operations. since we know that
; fixnum operations are commutative.
;
(defun d-collapse (form op)
  (let (const res conlist)
       ; generate list of constants (conlist) and non constants (res)
       (do ((xx form (cdr xx)))
	   ((null xx))
	   (if (numberp (car xx))
	       then (if (fixp (car xx))
			then (setq conlist (cons (car xx) conlist))
			else (comp-err "Illegal operand in fixnum op " 
				       (car xx)))
	       else (setq res (cons (car xx) res))))

       ; if no constants found thats ok, but if we found some,
       ; then collapse and return the form with the collapsed constant
       ; at the end.

       (if (null conlist)
	   then form 	; no change
	   else (setq res (nreverse 
		 (cons (apply (cond ((or (eq op '/) (eq op '*)) 'times)
				    (t 'plus)) 
			      (cons (cond ((or (eq op '/) (eq op '*)) 1)
					  (t 0))
				    conlist))
		       res))))))


;---- d-fixnumcode :: emit code for prescanned fixnum expression
;	expr -  a expression which should return an unboxed fixnum value 
;		in r5.
;  This function checks if the expression is indeed a guaranteed fixnum 
; arithmetic expression, and if so , generates code for the operation.
; If the expression is not a fixnum operation, then a normal evaluation
; of the cdr of the expression is done, which will grab the fixnum value
; and put it in r5.
;
#+(or for-vax for-tahoe)
(defun d-fixnumcode (expr)
  (let ((operator (and (dtpr expr) 
		       (symbolp (car expr)) 
		       (get (car expr) 'fixop)))
	(g-ret nil)
	tmp)
       ; the existance of a fixop property on a function says that it is a
       ; special fixnum only operation.
       (if (null operator) 
	   then (let ((g-loc '#.fixnum-reg) g-cc) ; non fixnum op, do normal
		     (d-exp `(cdr ,expr)))	; eval to get unboxed number
	   else (do ((xx (cdr expr) (cdr xx))	; fixnum op, scan all args
		     (lop) (rop) (res) (opnd))
		    ((null xx))
		    (setq opnd (car xx))
		    (if (fixp opnd) 
			then (setq rop `(immed ,opnd))
	   	     elseif (and (symbolp opnd) 
				 (setq rop (d-simple `(cdr ,opnd))))
			    thenret
			else (if (and lop (not (eq lop '#.unCstack)))
				 then (C-push (e-cvt lop))
				 (setq lop '#.unCstack))
			     (d-fixnumcode (d-fixexpand opnd))
			     (setq rop 'r5))
		    (if (null lop) 
			then (if (cdr xx) 
				 then (setq lop rop)
				 else (e-move (e-cvt rop) 'r5))
			else (if (cdr xx) 
				 then (setq res '#.Cstack)
				 else (setq res 'r5))
			     (if (setq tmp (d-shiftcheck operator rop))
				 then (e-write4 #+for-vax 'ashl 
						#+for-tahoe 'shal
						(e-cvt (list 'immed tmp))
						(e-cvt lop)
						(e-cvt res))
				 else (e-write4 operator (e-cvt rop) 
						(e-cvt lop) 
						(e-cvt res)))
			     (if (cdr xx) 
				 then (setq lop '#.unCstack)
				 else (setq lop "r5")))))))

#+for-68k
(defun d-fixnumcode (expr)
   (let ((operator (and (dtpr expr)
			(symbolp (car expr))
			(get (car expr) 'fixop)))
	 (g-ret nil)
	 tmp)
       ; the existance of a fixop property on a function says that it is a
       ; special fixnum only operation.
       (makecomment `(d-fixnumcode ,expr))
       (if (null operator) 
	   then (let ((g-loc '#.fixnum-reg) g-cc) ; non fixnum op, do normal
		    (d-exp `(cdr ,expr)))	  ; eval to get unboxed number
		(d-regused '#.fixnum-reg)
	   else (do ((xx (cdr expr) (cdr xx))	  ; fixnum op, scan all args
		     (lop) (rop) (res) (opnd))
		    ((null xx))
		    (setq opnd (car xx))
		    (if (fixp opnd) 
			then (setq rop `(immed ,opnd))
		     elseif (and (symbolp opnd)
				 (setq rop (d-simple `(cdr ,opnd))))
			thenret
			else (if (and lop (not (eq lop '#.unCstack)))
				 then (C-push (e-cvt lop))
				      (setq lop '#.unCstack))
			     (d-fixnumcode (d-fixexpand opnd))
			     (setq rop '#.fixnum-reg))
		    (if (null lop) 
			then (if (cdr xx) 
				 then (setq lop rop)
				 else (e-move
						(e-cvt rop)
						'#.fixnum-reg))
			else (if (cdr xx) 
				 then (setq res '#.Cstack)
				 else (setq res '#.fixnum-reg))
			     (if (setq tmp (d-shiftcheck operator rop))
				 then (d-asll tmp (e-cvt lop) (e-cvt res))
				 else (e-move (e-cvt lop) 'd0)
				      (e-write3 operator (e-cvt rop) 'd0)
				      (e-move 'd0 (e-cvt res)))
			     (if (cdr xx) 
				 then (setq lop '#.unCstack)
				 else (setq lop '#.fixnum-reg)))))
       (makecomment '(d-fixnumcode done))))

;--- d-shiftcheck	:: check if we can shift instead of multiply
; return t if the operator is a multiply and the operand is an
; immediate whose value is a power of two.
(defun d-shiftcheck (operator operand)
   (and (eq operator #+(or for-vax for-tahoe) 'lmul
	    	     #+for-68k 'mull3)
	(dtpr operand)
	(eq (car operand) 'immed)
	(cdr (assoc (cadr operand) arithequiv))))

; this table is incomplete 
;
(setq arithequiv '((1 . 0) (2 . 1) (4 . 2) (8 . 3) (16 . 4) (32 . 5)
		   (64 . 6) (128 . 7) (256 . 8) (512 . 9) (1024 . 10)
		   (2048 . 11) (4096 . 12) (8192 . 13) (16384 . 14)
		   (32768 . 15) (65536 . 16) (131072 . 17)))


;--- cc-oneplus  :: compile 1+ form			= cc-oneplus =
;  1+ increments a fixnum only. We generate code to check if the number
; to be incremented is a small fixnum less than or equal to 1022.  This
; check is done by checking the address of the fixnum's box.  If the
; number is in that range, we just increment the box pointer by 4.
; otherwise we call we call _qoneplus which does the add and calls
; _qnewint
;
#+(or for-vax for-tahoe)
(defun cc-oneplus nil
  (if (null g-loc)
      then (if (car g-cc) then (e-goto (car g-cc)))
      else (let ((argloc (d-simple (cadr v-form)))
		 (lab1 (d-genlab))
		 (lab2 (d-genlab)))
		(if (null argloc) 
		    then (let ((g-loc 'r0) g-cc g-ret)
			      (d-exp (cadr v-form)))
			 (setq argloc 'reg))
		(e-cmp (e-cvt argloc) '($ #.(+ 5120 (* 4 1022))))
		(e-write2 'jleq lab1)
		(if (not (eq argloc 'r0)) then (d-move argloc 'reg))
		(e-quick-call '_qoneplus)
		(if (and g-loc (not (eq g-loc 'reg)))
		    then (d-move 'reg g-loc))
		(if (car g-cc)
		    then (e-goto (car g-cc))
		    else (e-goto lab2))
		(e-label lab1)
		(e-add3 '($ 4) (e-cvt argloc) (e-cvt g-loc))
		(if (car g-cc) then (e-goto (car g-cc)))
		(e-label lab2))))

#+for-68k
(defun cc-oneplus nil
   (if (null g-loc)
       then (if (car g-cc) then (e-goto (car g-cc)))
       else (let ((argloc (d-simple (cadr v-form)))
		  (lab1 (d-genlab))
		  (lab2 (d-genlab)))
		(if (null argloc) 
		    then (let ((g-loc 'areg) g-cc g-ret)
			     (d-exp (cadr v-form)))
			 (setq argloc 'areg))
		; ($ (+ Fixzero (* 4 1022))
		(d-cmp argloc '(fixnum 1022))
		(e-write2 'jle lab1)
		(if (not (eq argloc 'areg)) then (d-move argloc 'areg))
		(e-quick-call '_qoneplus)
		(if (and g-loc (not (eq g-loc 'reg)))
		    then (d-move 'reg g-loc))
		(if (car g-cc)
		    then (e-goto (car g-cc))
		    else (e-goto lab2))
		(e-label lab1)
		(if (not (eq argloc 'reg))
		    then (d-move argloc 'reg))
		(e-write3 'addql "#4" 'd0)
		(if (and g-loc (not (eq g-loc 'reg)))
		    then (d-move 'reg g-loc))
		(if (car g-cc) then (e-goto (car g-cc)))
		(e-label lab2))))
			


;--- cc-oneminus :: compile the 1- form
; just like 1+ we check to see if we are decrementing an small fixnum.
; and if we are we just decrement the pointer to the fixnum and save
; a call to qinewint.  The valid range of fixnums we can decrement are
; 1023 to -1023.  This requires two range checks (as opposed to one for 1+).
;
#+(or for-vax for-tahoe)
(defun cc-oneminus nil
  (if (null g-loc)
      then (if (car g-cc) then (e-goto (car g-cc)))
      else (let ((argloc (d-simple (cadr v-form)))
		 (lab1 (d-genlab))
		 (lab2 (d-genlab))
		 (lab3 (d-genlab)))
		(if (null argloc) 
		    then (let ((g-loc 'r0) g-cc)
			      (d-exp (cadr v-form)))
		         (setq argloc 'reg))
		(e-cmp (e-cvt argloc) '($ #.(- 5120 (* 4 1024))))
		(e-write2 'jleq lab1)	; not within range
		(e-cmp (e-cvt argloc) '($ #.(+ 5120 (* 4 1023))))
		(e-write2 'jleq lab2)	; within range
		; not within range, must do it the hard way.
		(e-label lab1)
		(if (not (eq argloc 'r0)) then (d-move argloc 'reg))
		(e-quick-call '_qoneminus)
		(if (and g-loc (not (eq g-loc 'reg)))
		    then (d-move 'reg g-loc))
		(if (car g-cc)
		    then (e-goto (car g-cc))
		    else (e-goto lab3))
		(e-label lab2)
		; we are within range, just decrement the pointer by the
		; size of a word (4 bytes).
		(e-sub3 '($ 4) (e-cvt argloc) (e-cvt g-loc))
		(if (car g-cc) then (e-goto (car g-cc)))
		(e-label lab3))))

#+for-68k
(defun cc-oneminus nil
  (if (null g-loc)
      then (if (car g-cc) then (e-goto (car g-cc)))
      else (let ((argloc (d-simple (cadr v-form)))
		 (lab1 (d-genlab))
		 (lab2 (d-genlab))
		 (lab3 (d-genlab)))
		(if (null argloc) 
		    then (let ((g-loc 'areg) g-cc)
			      (d-exp (cadr v-form)))
		         (setq argloc 'areg))
		; ($ (- Fixzero (* 4 1024)))
		(d-cmp argloc '(fixnum -1024))
		(e-write2 'jle lab1)	; not within range
		(d-cmp argloc '(fixnum 1023))
		(e-write2 'jle lab2)	; within range
		; not within range, must do it the hard way.
		(e-label lab1)
		(if (not (eq argloc 'areg)) then (d-move argloc 'areg))
		(e-quick-call '_qoneminus)
		(if (and g-loc (not (eq g-loc 'reg)))
		    then (d-move 'reg g-loc))
		(if (car g-cc)
		    then (e-goto (car g-cc))
		    else (e-goto lab3))
		(e-label lab2)
		; we are within range, just decrement the pointer by the
		; size of a word (4 bytes).
		(if (not (eq argloc 'reg))
		    then (d-move argloc 'reg))
		(e-sub '($ 4) 'd0)
		(if (and g-loc (not (eq g-loc 'reg)))
		    then (d-move 'reg g-loc))
		(if (car g-cc) then (e-goto (car g-cc)))
		(e-label lab3))))

;--- cm-<  :: compile a < expression
; 
; the operands to this form can either be fixnum or flonums but they
; must be of the same type.
;
; We can compile the form just like an eq form since all we want is
; a compare and a jump.  The comparisons are inverted since that is
; the way eq expects it.

(defun cm-< nil
   (if (not (= 2 (length (cdr v-form))))
      then (comp-err "incorrect number of arguments to < " v-form))
   ; only can do fixnum stuff if we know that one of the args is
   ; a fixnum.
   ;
   (if (or (fixp (cadr v-form)) (fixp (caddr v-form)))
      then `(<& ,(cadr v-form) ,(caddr v-form))
      else `(lessp ,(cadr v-form) ,(caddr v-form))))

;--- c-<& :: fixnum <
;
; We can compile the form just like an eq form since all we want is
; a compare and a jump.  The comparisons are inverted since that is
; the way eq expects it.

(defun cc-<& nil
   (let ((g-trueop  #+(or for-vax for-tahoe) 'jgeq #+for-68k 'jpl)
	 (g-falseop #+(or for-vax for-tahoe) 'jlss #+for-68k 'jmi)
	 (v-form `(eq (cdr ,(cadr v-form)) (cdr ,(caddr v-form)))))
      (cc-eq)))

;--- cm->  :: compile a > expression
;
; the operands to this form can either be fixnum or flonums but they
; must be of the same type.  
; We can compile the form just like an eq form since all we want is
; a compare and a jump.  The comparisons are inverted since that is
; the way eq expects it.
(defun cm-> nil
   (if (not (= 2 (length (cdr v-form))))
      then (comp-err "incorrect number of arguments to > " v-form))
   ; only can do fixnum stuff if we know that one of the args is
   ; a fixnum.
   ;
   (if (or (fixp (cadr v-form)) (fixp (caddr v-form)))
      then `(>& ,(cadr v-form) ,(caddr v-form))
      else `(greaterp ,(cadr v-form) ,(caddr v-form))))

;--- cc->& :: compile a fixnum > function
;
; We can compile the form just like an eq form since all we want is
; a compare and a jump.  The comparisons are inverted since that is
; the way eq expects it.
(defun cc->& nil
   (let ((g-trueop  #+(or for-vax for-tahoe) 'jleq #+for-68k 'jle)
	 (g-falseop #+(or for-vax for-tahoe) 'jgtr #+for-68k 'jgt)
	 (v-form `(eq (cdr ,(cadr v-form)) (cdr ,(caddr v-form)))))
      (cc-eq)))

;--- cm-=  : compile an = expression
;  The = function is a strange one.  It can compare two fixnums or two
; flonums which is fine on a pdp-10 where they are the same size, but
; is a real pain on a vax where they are different sizes.
; We thus can see if one of the arguments is a fixnum and assume that
; the other one is and then  call =&, the fixnum equal code.
;
(defun cm-= nil
   (if (not (= 2 (length (cdr v-form))))
      then (comp-err "incorrect number of arguments to = : " v-form))
   (if (or (fixp (cadr v-form)) (fixp (caddr v-form)))
      then `(=& ,(cadr v-form) ,(caddr v-form))
      else `(equal ,(cadr v-form) ,(caddr v-form))))

;--- cm-=&
;
; if the number is within the small fixnum range, we can just
; do pointer comparisons.
;
(defun cm-=& nil
   (if (or (and (fixp (cadr v-form))
		(< (cadr v-form) 1024)
		(> (cadr v-form) -1025))
	   (and (fixp (caddr v-form))
		(< (caddr v-form) 1024)
		(> (caddr v-form) -1025)))
      then `(eq ,(cadr v-form) ,(caddr v-form))
      else `(eq (cdr ,(cadr v-form)) (cdr ,(caddr v-form)))))

; this should be converted
#+(or for-vax for-tahoe)
(defun c-\\ nil
   (d-fixop 'ediv  'remainder))

#+(or for-vax for-tahoe)
(defun d-fixop (opcode lispopcode)
   (prog (op1 op2 rop1 rop2 simpleop1)
       (if (not (eq 3 (length v-form))) ; only handle two ops for now
	   then (d-callbig lispopcode (cdr v-form) nil)
	   else (setq op1 (cadr v-form)
		      op2 (caddr v-form))
		(if (fixp op1)
		    then (setq rop1 `($ ,op1)  ; simple int
			       simpleop1 t)	    
		    else (if (setq rop1 (d-simple `(cdr ,op1)))
			     then (setq rop1 (e-cvt rop1))
			     else (let ((g-loc 'reg) g-cc g-ret)
				      (d-exp op1))
				  (setq rop1 '(0 r0))))
		(if (fixp op2)
		    then (setq rop2 `($ ,op2))
		    else (if (setq rop2 (d-simple `(cdr ,op2)))
			     then (setq rop2 (e-cvt rop2))
			     else (C-push rop1)
				  (setq rop1 '#.unCstack)
				  (let ((g-loc 'reg)
					g-cc g-ret)
				      (d-exp op2))
				  (setq rop2 '(0 r0))))
		(if (eq opcode 'ediv)
		    then (if (not simpleop1)
			     then #+for-vax (progn (e-move rop1 'r2) ;need quad
						(e-write4 'ashq '$-32 'r1 'r1))
			          #+for-tahoe (let ((x (d-genlab)))
						(e-write2 'clrl 'r2)
						(e-move rop1 'r3)
						(e-write2 'jgeq x)
						(e-write3 'mnegl '($ 1) 'r2)
						(e-writel x))
				  (setq rop1 #+for-vax 'r1 #+for-tahoe 'r2))
				  				; word div.
			 (e-write5 'ediv rop2 rop1 'r0 'r5)
		    else (e-write4 opcode rop2 rop1 'r5))
		(d-fixnumbox)
		(d-clearreg))))
