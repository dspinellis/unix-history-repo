(include-if (null (get 'chead 'version)) "../chead.l")
(Liszt-file util
   "$Header: util.l,v 1.15 87/12/15 17:09:21 sklower Exp $")

;;; ----	u t i l			general utility functions
;;;
;;;				-[Tue Aug 16 17:17:32 1983 by layer]-


;--- d-handlecc :: handle g-cc
; at this point the Z condition code has been set up and if g-cc is
; non nil, we must jump on condition to the label given in g-cc
;
(defun d-handlecc nil
   (if (car g-cc)
       then (e-gotot (car g-cc))
    elseif (cdr g-cc)
       then (e-gotonil (cdr g-cc))))

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
  (if (null g-loc) 
      then (if (car g-cc) then (e-gotonil (car g-cc))
	    elseif (cdr g-cc) then  (e-gotot (cdr g-cc)))
      else (let ((lab1 (d-genlab))
		 (lab2 (if (cdr g-cc) thenret else (d-genlab))))
		(e-gotonil lab1)
		; Z=1, but remember that this implies nil due to inversion
		(d-move 'Nil g-loc)
		(e-goto lab2)
		(e-label lab1)
		; Z=0, which means t
		(d-move 'T g-loc)
		(if (car g-cc) then (e-goto (car g-cc)))
		(if (null (cdr g-cc)) then (e-label lab2)))))
			
;--- d-noninvert :: handle g-cc and g-loc assuming cc non inverted
; 
; like d-invert except Z=0 implies nil, and Z=1 implies t
;
(defun d-noninvert nil
  (if (null g-loc) 
      then (if (car g-cc) then (e-gotot (car g-cc))
	    elseif (cdr g-cc) then  (e-gotonil (cdr g-cc)))
      else (let ((lab1 (d-genlab))
		 (lab2 (if (cdr g-cc) thenret else (d-genlab))))
		(e-gotot lab1)
		; Z=0, this implies nil
		(d-move 'Nil g-loc)
		(e-goto lab2)
		(e-label lab1)
		; Z=1, which means t
		(d-move 'T g-loc)
		(if (car g-cc) then (e-goto (car g-cc)))
		(if (null (cdr g-cc)) then (e-label lab2)))))

;--- d-macroexpand :: macro expand a form as much as possible
;
; only macro expands the top level though.
(defun d-macroexpand (i)
   (prog (first type)
      loop
      (if (and (dtpr i) (symbolp (setq first (car i))))
	 then (if (eq 'macro (setq type (d-functyp first 'macro-ok)))
		 then (setq i (apply first i))
		      (go loop)
	       elseif (eq 'cmacro type)
		 then (setq i (apply (get first 'cmacro) i))
		      (go loop)))
      (return i)))

;--- d-fullmacroexpand :: macro expand down all levels
; this is not always possible to due since it is not always clear
; if a function is a lambda or nlambda, and there are lots of special
; forms.  This is just a first shot at such a function, this should
; be improved upon.
;
(defun d-fullmacroexpand (form)
   (if (not (dtpr form))
       then form
       else (setq form (d-macroexpand form))	; do one level
            (if (and (dtpr form) (symbolp (car form)))
		then (let ((func (getd (car form))))
			  (if (or (and (bcdp func)
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
   (if (oddp (length (cdr form)))
       then (comp-err "wrong number of args to setq " form)
       else (do ((xx (reverse (cdr form)) (cddr xx))
		 (res))
		((null xx) (cons 'setq res))
		(setq res `(,(cadr xx)
			     ,(d-fullmacroexpand (car xx))
			     ,@res)))))
   
;--- d-typesimp ::  determine the type of the argument 
;
#+(or for-vax for-tahoe)
(defun d-typesimp (arg val)
  (let ((argloc (d-simple arg)))
	(if (null argloc)
	    then (let ((g-loc 'reg)
		       g-cc g-ret)
		     (d-exp arg))
		 (setq argloc 'reg))
	#+for-vax (e-write4 'ashl '$-9 (e-cvt argloc) 'r0)
	#+for-tahoe (e-write4 'shar '$9 (e-cvt argloc) 'r0)
	(e-write3 'cmpb '"_typetable+1[r0]" val)
	(d-invert)))

#+for-68k
(defun d-typesimp (arg val)
   (let ((argloc (d-simple arg)))
       (if (null argloc)
	   then (let ((g-loc 'reg)
		      g-cc g-ret)
		    (d-exp arg))
		(setq argloc 'reg)
	   else (e-move (e-cvt argloc) 'd0))
       (e-sub '#.nil-reg 'd0)
       (e-write3 'moveq '($ 9) 'd1)
       (e-write3 'asrl 'd1 'd0)
       (e-write3 'lea '"_typetable+1" 'a5)
       (e-write3 'cmpb val '(% 0 a5 d0))
       (d-invert)))

;--- d-typecmplx  :: determine if arg has one of many types
;	- arg : lcode argument to be evaluated and checked
;	- vals : fixnum with a bit in position n if we are to check type n
;
#+(or for-vax for-tahoe)
(defun d-typecmplx (arg vals)
  (let ((argloc (d-simple arg))
	(reg))
       (if (null argloc) then (let ((g-loc 'reg)
				    g-cc g-ret)
				   (d-exp arg))
			      (setq argloc 'reg))
       (setq reg 'r0)
       #+for-vax (e-write4 'ashl '$-9 (e-cvt argloc) reg)
       #+for-tahoe (e-write4 'shar '$9 (e-cvt argloc) reg)
       (e-write3 'cvtbl (concat "_typetable+1[" reg "]") reg)
       (e-write4 #+for-vax 'ashl #+for-tahoe 'shal reg '$1 reg)
       (e-write3 'bitw vals reg)
       (d-noninvert)))

#+for-68k
(defun d-typecmplx (arg vals)
   (let ((argloc (d-simple arg))
	 (l1 (d-genlab))
	 (l2 (d-genlab)))
       (makecomment '(d-typecmplx: type check))
       (if (null argloc)
	   then (let ((g-loc 'reg)
		      g-cc g-ret)
		    (d-exp arg))
		(setq argloc 'reg)
	   else (e-move (e-cvt argloc) 'd0))
       (e-sub '#.nil-reg 'd0)
       (e-write3 'moveq '($ 9) 'd1)
       (e-write3 'asrl 'd1 'd0)
       (e-write3 'lea '"_typetable+1" 'a5)
       (e-add 'd0 'a5)
       (e-write3 'movb '(0 a5) 'd0)
       (e-write2 'extw 'd0)
       (e-write2 'extl 'd0)
       (e-write3 'moveq '($ 1) 'd1)
       (e-write3 'asll 'd0 'd1)
       (e-move 'd1 'd0)
       (e-write3 'andw vals 'd0)
       (d-noninvert)
       (makecomment '(d-typecmplx: end))))

;---- register handling routines.

;--- d-allocreg :: allocate a register 
;  name - the name of the register to allocate or nil if we should
;	  allocate the least recently used.
;
(defun d-allocreg (name)
  (if name 
      then (let ((av (assoc name g-reguse)))
		(if av then (rplaca (cdr av) (1+ (cadr av)))) ; inc used count
		name)
      else ; find smallest used count
	   (do ((small (car g-reguse))
		(smc (cadar g-reguse))
		(lis (cdr g-reguse) (cdr lis)))
	       ((null lis)
		(rplaca (cdr small) (1+ smc))
		(car small))
	       (if (< (cadar lis) smc)
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
      ((null ll)
       (if best
	   then (rplaca (cdr best) (1+ (cadr best)))
		(list (car best)
		      (if (> bestv 0) 
			  then (rplacd (nthcdr (1- bestv)
					       (setq tmp
						     (copy pat)))
				       nil)
			       tmp
			  else nil)
		      (nthcdr bestv pat))))
      (if (and (setq val (cddar ll))
	       (eq name (car val)))
	  then (if (> (setq tmp (d-matchcnt pat (cdr val)))
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
       (if (do ((hh have (cdr hh))
		(ww want (cdr ww)))
	       ((null hh) t)
	       (if (or (null ww) (not (eq (car ww) (car hh))))
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
		(if av
		   then
			#+for-68k (d-regused (car av))
			(rplaca (cdr av) 0)
			(rplacd (cdr av) nil)
		   else nil)))))

;--- d-clearuse :: clear all register which reference a given variable
;
(defun d-clearuse (varib)
  (mapc '(lambda (x)
		 (if (eq (caddr x) varib) then (rplacd (cdr x) nil)))
	g-reguse))

;--- d-inreg :: declare that a value is in a register
; name - register name
; value - value in a register
;
(defun d-inreg (name value)
  (let ((av (assoc name g-reguse)))
       (if av then (rplacd (cdr av) value))
       name))

(defun e-setup-np-lbot nil
   (e-move '#.np-reg '#.np-sym)
   (e-move '#.lbot-reg '#.lbot-sym))

;---------------MC68000 only routines
#+for-68k
(progn 'compile

;--- d-regtype :: find out what type of register the operand goes
;		  in.
; eiadr - an EIADR
;
(defun d-regtype (eiadr)
   (if (symbolp eiadr)
       then (if (memq eiadr '(d0 d1 d2 d3 d4 d5 d6 d7 reg)) then 'd
	     elseif (memq eiadr '(a0 a1 a2 a3 a4 a5 a6 a7 sp areg)) then 'a)
    elseif (or (eq '\# (car eiadr))
	       (eq '$ (car eiadr))
	       (and (eq '* (car eiadr))
		    (eq '\# (cadr eiadr))))
       then 'd
       else 'a))

;--- d-regused :: declare that a reg is used in a function
;	regname - name of the register that is going to be used
;		  (ie, 'd0 'a2...)
;
(defun d-regused (regname)
   (let ((regnum (diff (cadr (exploden regname)) 48))
	 (regtype (car (explode regname))))
       (if (memq regname '(a0 a1 d0 d1))
	   thenret
	elseif (equal 'd regtype)
	   then (rplacx regnum g-regmaskvec t) regname
	   else (rplacx (plus regnum 8) g-regmaskvec t) regname)))

;--- d-makemask :: make register mask for moveml instr
;
(defun d-makemask ()
   (do ((ii 0 (1+ ii))
	(mask 0))
       ((greaterp ii 15) mask)
       (if (cxr ii g-regmaskvec)
	   then (setq mask (plus mask (expt 2 ii))))))

;--- init-regmaskvec :: initalize hunk structure to all default
;			save mask.
;
; nil means don't save it, and t means save the register upon function entry.
; order in vector: d0 .. d7, a0 .. a7.
; d3 : lbot (if $global-reg$ is t then save)
; d7 : _nilatom
; a2 : _np
; a3 : literal table ptr
; a4 : old _lbot (if $global-reg$ is t don't save)
; a5 : intermediate address calc
;
(defun init-regmaskvec ()
   (setq g-regmaskvec
	 (makhunk
	     (if $global-reg$
		 then (quote (nil nil nil t   nil nil nil t
			      nil nil t   t   t   t   nil nil))
		 else (quote (nil nil nil nil nil nil nil t
			      nil nil t   t   t   t   nil nil))))))

;--- Cstackspace :: calc local space on C stack
; space = 4 * (no. of register variables saved on stack)
;
(defun Cstackspace ()
   (do ((ii 0 (1+ ii))
	(retval 0))
       ((greaterp ii 15) (* 4 retval))
       (if (cxr ii g-regmaskvec) then (setq retval (1+ retval)))))

;--- d-alloc-register :: allocate a register
;  type - type of register (a or d)
;  name - the name of the register to allocate or nil if we should
;	  allocate the least recently used.
;
(defun d-alloc-register (type name)
   (if name 
       then (let ((av (assoc name g-reguse)))
		(d-regused name)
		(if av then (rplaca (cdr av) (1+ (cadr av)))) ; inc used count
		name)
       else ; find smallest used count
	    (let ((reguse))
		(do ((cur g-reguse (cdr cur)))
		    ((null cur))
		    (if (eq type (car (explode (caar cur))))
			then (setq reguse (cons (car cur) reguse))))
		(do ((small (car reguse))
		     (smc (cadar reguse))
		     (lis (cdr reguse) (cdr lis)))
		    ((null lis)
		     (rplaca (cdr small) (1+ smc))
		     (d-regused (car small))
		     (car small))
		    (if (< (cadar lis) smc)
			then (setq small (car lis)
				   smc   (cadr small)))))))

); end 68000 only routines
