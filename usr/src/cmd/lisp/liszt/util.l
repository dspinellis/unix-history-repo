(include "chead.l")
(Liszt-file util  "@(#)util.l	1.2	10/7/81")

;;; ----	u t i l			general utility functions
;;;


;--- d-cmp :: compare two IADR values
;
(defun d-cmp (arg1 arg2)
  (e-write3 'cmpl (e-cvt arg1) (e-cvt arg2)))


;--- d-handlecc :: handle g-cc
; at this point the Z condition code has been set up and if g-cc is
; non nil, we must jump on condition to the label given in g-cc
;
(defun d-handlecc nil
  (If (car g-cc) then (e-gotot (car g-cc))
   elseif (cdr g-cc) then (e-gotonil (cdr g-cc))))


;--- d-invert :: handle inverted condition codes
; this routine is called if a result has just be computed which alters
; the condition codes such that Z=1 if the result is t, and Z=0 if the
; result is nil (this is the reverse of the usual sense).  The purpose
; of this routine is to handle g-cc and g-loc.  That is if g-loc is 
; specified, we must convert the value of the Z bit of the condition 
; code to t or nil and store that in g-loc.  After handling g-loc we
; must handle g-cc, that is if the part of g-cc is non nil which matches
; the inverse of the current condition code, we must jump to that.
;
(defun d-invert nil
  (If (null g-loc) 
      then (If (car g-cc) then (e-gotonil (car g-cc))
	    elseif (cdr g-cc) then  (e-gotot (cdr g-cc)))
      else (let ((lab1 (d-genlab))
		 (lab2 (If (cdr g-cc) thenret else (d-genlab))))
		(e-gotonil lab1)
		; Z=1, but remember that this implies nil due to inversion
		(d-move 'Nil g-loc)
		(e-goto lab2)
		(e-label lab1)
		; Z=0, which means t
		(d-move 'T g-loc)
		(If (car g-cc) then (e-goto (car g-cc)))
		(If (null (cdr g-cc)) then (e-label lab2)))))
			

;--- d-noninvert :: handle g-cc and g-loc assuming cc non inverted
; 
; like d-invert except Z=0 implies nil, and Z=1 implies t
;
(defun d-noninvert nil
  (If (null g-loc) 
      then (If (car g-cc) then (e-gotot (car g-cc))
	    elseif (cdr g-cc) then  (e-gotonil (cdr g-cc)))
      else (let ((lab1 (d-genlab))
		 (lab2 (If (cdr g-cc) thenret else (d-genlab))))
		(e-gotot lab1)
		; Z=0, this implies nil
		(d-move 'Nil g-loc)
		(e-goto lab2)
		(e-label lab1)
		; Z=1, which means t
		(d-move 'T g-loc)
		(If (car g-cc) then (e-goto (car g-cc)))
		(If (null (cdr g-cc)) then (e-label lab2)))))

;--- d-macroexpand :: macro expand a form as much as possible
;
; only macro expands the top level though.
(defun d-macroexpand (form)
  (prog nil
	loop
	(If (and (dtpr form) 
		 (symbolp (car form))
		 (eq 'macro (d-functyp (car form))))
	    then (setq form (apply (car form) form))
	    (go loop))
	(return form)))

;--- d-fullmacroexpand :: macro expand down all levels
; this is not always possible to due since it is not always clear
; if a function is a lambda or nlambda, and there are lots of special
; forms.  This is just a first shot at such a function, this should
; be improved upon.
;
(defun d-fullmacroexpand (form)
   (If (not (dtpr form))
       then form
       else (setq form (d-macroexpand form))	; do one level
            (If (and (dtpr form) (symbolp (car form)))
		then (let ((func (getd (car form))))
			  (If (or (and (bcdp func)
				       (eq 'lambda (getdisc func)))
				  (and (dtpr func)
				       (memq (car func) '(lambda lexpr)))
				  (memq (car form) '(or and)))
			      then `(,(car form)
				      ,@(mapcar 'd-fullmacroexpand
						(cdr form)))
			    elseif (eq (car form) 'setq)
			      then (d-setqexpand form)
			    else form))
		else form)))

;--- d-setqexpand :: macro expand a setq statemant
; a setq is unusual in that alternate values are macroexpanded.
;
(defun d-setqexpand (form)
   (If (oddp (length (cdr form)))
       then (comp-err "wrong number of args to setq " form)
       else (do ((xx (reverse (cdr form)) (cddr xx))
		 (res))
		((null xx) (cons 'setq res))
		(setq res `(,(cadr xx)
			     ,(d-fullmacroexpand (car xx))
			     ,@res)))))
   


;--- d-move :: emit instructions to move value from one place to another
;
(defun d-move (from to)
  (makecomment `(from ,(e-uncvt from) to ,(e-uncvt to)))
  (cond ((eq 'Nil from) (e-write2 'clrl (e-cvt to)))
	(t (e-write3 'movl (e-cvt from) (e-cvt to)))))

;--- d-movespec :: move from loc to loc where the first addr given is
;	           an EIADR
;	- from : EIADR 
;	- to   : IADR
;
(defun d-movespec (from to)
  (makecomment `(fromspec ,from to ,(e-uncvt to)))
  (e-write3 'movl from (e-cvt to)))


;--- d-tst :: test the given value (set the cc)
;
(defun d-tst (arg)
  (e-write2 'tstl (e-cvt arg)))

;--- d-typesimp ::  determine the type of the argument 
;
(defun d-typesimp (arg val)
  (let ((argloc (d-simple arg)))
	(If (null argloc) then (let ((g-loc 'reg)
				     g-cc g-ret)
				    (d-exp arg))
			       (setq argloc 'reg))
	(e-write4 'ashl '$-9 (e-cvt argloc) 'r0)
	(e-write3 'cmpb '"_typetable+1[r0]" val)
	(d-invert)))

;--- d-typecmplx  :: determine if arg has one of many types
;	- arg : lcode argument to be evaluated and checked
;	- vals : fixnum with a bit in position n if we are to check type n
;
(defun d-typecmplx (arg vals)
  (let ((argloc (d-simple arg))
	(reg))
       (If (null argloc) then (let ((g-loc 'reg)
				    g-cc g-ret)
				   (d-exp arg))
			      (setq argloc 'reg))
       (setq reg 'r0)
       (e-write4 'ashl '$-9 (e-cvt argloc) reg)
       (e-write3 'cvtbl (concat "_typetable+1[" reg "]") reg)
       (e-write4 'ashl reg '$1 reg)
       (e-write3 'bitw vals reg)
       (d-noninvert)))

       
;---- register handling routines.

;--- d-allocreg :: allocate a register 
;  name - the name of the register to allocate or nil if we should
;	  allocate the least recently used.
;
(defun d-allocreg (name)
  (If name 
      then (let ((av (assoc name g-reguse)))
		(If av then (rplaca (cdr av) (1+ (cadr av)))) ; inc used count
		name)
      else ; find smallest used count
	   (do ((small (car g-reguse))
		(smc (cadar g-reguse))
		(lis (cdr g-reguse) (cdr lis)))
	       ((null lis)
		(rplaca (cdr small) (1+ smc))
		(car small))
	       (If (< (cadar lis) smc)
		   then (setq small (car lis)
			      smc   (cadr small))))))


;--- d-bestreg :: determine the register which is closest to what we have
;  name - name of variable whose subcontents we want
;  pat  - list of d's and a's which tell which part we want
;
(defun d-bestreg (name pat)
  (do ((ll g-reguse (cdr ll))
       (val)
       (best)
       (tmp)
       (bestv -1))
      ((null ll) (If best then (rplaca (cdr best) (1+ (cadr best)))
			       (list (car best)
				     (If (> bestv 0) 
					 then (rplacd (nthcdr (1- bestv) 
							      (setq tmp 
								    (copy pat)))
						      nil)
					      tmp
					 else nil)
				     (nthcdr bestv pat))))
      (If (and (setq val (cddar ll))
	       (eq name (car val)))
	  then (If (> (setq tmp (d-matchcnt pat (cdr val)))
		      bestv)
		   then (setq bestv tmp
			      best  (car ll))))))

;--- d-matchcnt :: determine how many parts of a pattern match
; want - pattern we want to achieve
; have - pattern whose value exists in a register
; 
; we return a count of the number of parts of the pattern match.
; If this pattern will be any help at all, we return a value from 
; 0 to the length of the pattern.
; If this pattern will not work at all, we return a number smaller
; than -1.  
; For `have' to be useful for `want', `have' must be a substring of 
; `want'.  If it is a substring, we return the length of `have'.
; 
(defun d-matchcnt (want have)
  (let ((length 0))
       (If (do ((hh have (cdr hh))
		(ww want (cdr ww)))
	       ((null hh) t)
	       (If (or (null ww) (not (eq (car ww) (car hh))))
		   then (return nil)
		   else (incr length)))
	   then  length
	   else  -2)))



;--- d-clearreg :: clear all values in registers or just one
; if no args are given, clear all registers.
; if an arg is given, clear that register
;
(defun d-clearreg n
  (cond ((zerop n) 
	 (mapc '(lambda (x) (rplaca (cdr x) 0)
		     (rplacd (cdr x) nil))
	       g-reguse))
	(t (let ((av (assoc (arg 1) g-reguse)))
		(If av then (rplaca (cdr av) 0)
			    (rplacd (cdr av) nil))))))


;--- d-clearuse :: clear all register which reference a given variable
;
(defun d-clearuse (varib)
  (mapc '(lambda (x)
		 (If (eq (caddr x) varib) then (rplacd (cdr x) nil)))
	g-reguse))


;--- d-inreg :: declare that a value is in a register
; name - register name
; value - value in a register
;
(defun d-inreg (name value)
  (let ((av (assoc name g-reguse)))
       (If av then (rplacd (cdr av) value))
       name))

