(include-if (null (get 'chead 'version)) "../chead.l")
(Liszt-file instr
   "$Header: instr.l,v 1.9 87/12/15 17:03:01 sklower Exp $")

;;; ----	i n s t r		emulate machine instructions
;;;
;;;				-[Thu Jan  5 18:40:50 1984 by jkf]-


;  The routines in this file emulate instructions, usually VAX-11
; ones.  Routines names with the prefix "e-" take EIADR's, and
; those with "d-" take IADR's as arguments.
;  Some of the simple routines are accually macros, and can be found in
; ../cmacros.l


;--- d-add :: emit an add intruction
; 68000 has a quick add for $1 - $8
;
; (the one for the vax is a macro in cmacros.l)
#+for-68k
(defun e-add (src dst)
  (if (and (dtpr src)
	   (eq '$ (car src))
	   (and (>& (cadr src) 0) (<& (cadr src) 9)))
      then (e-write3 'addql src dst)
      else (e-write3 'addl src dst)))

;--- e-sub :: emit an add intruction (check for quick add: (immed 1 - 8))
;
#+for-68k
(defun e-sub (src dst)
   (if (and (dtpr src)
	    (eq '$ (car src))
	    (zerop (cadr src)))
       thenret
    elseif (and (dtpr src)
		(numberp (cadr src))
		(and (>& (cadr src) 0) (<& (cadr src) 9)))
       then (e-write3 'subql src dst)
       else (e-write3 'subl src dst)))

; NOTE: The cmp routines emis instructions to test the condition codes
;	by arg1 - arg2 (ie, arg1 is subtracted from arg2).  On the
;	68000 the args must be reversed.

;--- e-cmp :: compare two EIADR values
;
; NOTE: for 68000, this does "cmpl dst,src"
;
#+for-68k
(defun e-cmp (src dst)
   (if (and (symbolp src)
	    (memq src '(d0 d7 a0 a1 a2 d3 d1 d2 a3 a4 a5 sp d6 a6 d4 d5)))
       then ; the form is "cmp <ea>,Rx"
	    (e-write3 'cmpl dst src)
    elseif (and (dtpr dst)
		(or (memq (car dst) '($ \#))
		    (and (eq '* (car dst))
			 (eq '\# (cadr dst)))))
       then ; the form is "cmp #const,<ea>"
	    (if (and (dtpr src)
		     (or (memq (car src) '($ \#))
			 (and (eq '* (car src))
			      (eq '\# (cadr src)))))
		then ; we have "cmp #n,#m"
		     ; and we can't do it in one cmp
		     (d-regused 'd6)
		     (e-write3 'movl src 'd6)
		     (e-write3 'cmpl dst 'd6)
		else ; we have "cmp #n,<ea>"
		     (e-write3 'cmpl dst src))
    elseif (and (dtpr src)
		(dtpr dst)
		(eq '+ (car src))
		(eq '+ (car dst)))
       then ; the form is "cmp An@+,Am@+"
	    (e-write3 'cmpml dst src)
       else ; addressing modes are too complicated to
	    ; do in 1 instruction...
	    (d-regused 'd6)
	    (e-write3 'movl src 'd6)
	    (e-write3 'cmpl dst 'd6)))

;--- e-move :: move value from one place to anther
; this corresponds to d-move except the args are EIADRS
;
(defun e-move (from to)
   (if (and (dtpr from)
	    (eq '$ (car from))
	    (eq 0 (cadr from)))
       then (e-write2 'clrl to)
       else (e-write3 'movl from to)))

;--- d-move :: emit instructions to move value from one place to another
;
(defun d-move (from to)
  (makecomment `(from ,(e-uncvt from) to ,(e-uncvt to)))
  #+(or for-vax for-tahoe)
  (cond ((eq 'Nil from) (e-move '($ 0) (e-cvt to)))
	(t (e-move (e-cvt from) (e-cvt to))))

  #+for-68k
  (let ((froma (e-cvt from))
	(toa (e-cvt to)))
       (if (and (dtpr froma)
		(eq '$ (car froma))
	        (and (>& (cadr froma) -1) (<& (cadr froma) 65))
		(atom toa)
		(eq 'd (nthchar toa 1)))
           then ;it's a mov #immed,Dn, where 0 <= immed <= 64
		;  i.e., it's a quick move
	        (e-write3 'moveq froma toa)
           else (cond ((eq 'Nil froma) (e-write3 'movl '#.nil-reg toa))
		      (t (e-write3 'movl froma toa))))))

;--- d-movespec :: move from loc to loc where the first addr given is
;	           an EIADR
;	- from : EIADR 
;	- to   : IADR
;
(defun d-movespec (from to)
  (makecomment `(fromspec ,from to ,(e-uncvt to)))
  (e-move from (e-cvt to)))

;--- d-ashl :: emit shift code (don't know what direction to shift)
#+for-68k
(defun d-ashl (count src dst)
  (let ((genlab1 (d-genlab))
	(genlab2 (d-genlab)))
       (e-write3 'movl src dst)
       (e-write2 'tstl count)
       (e-write2 'bmi genlab1)
       (e-write3 'asll count dst)
       (e-write2 'bra genlab2)
       (e-label genlab1)
       (e-write3 'asrl count dst)
       (e-writel genlab2)))

;--- d-asrl :: emit shift right code
#+for-68k
(defun d-asrl (count src dst)
   (e-write3 'movl src dst)
   (if (and (numberp count) (greaterp count 8))
       then (e-write3 'moveq (concat "#" count) 'd0)
	    (e-write3 'asrl 'd0 dst)
       else (e-write3 'asrl (concat "#" count) dst)))

;--- d-asll :: emit shift left code
#+for-68k
(defun d-asll (count src dst)
  (e-write3 'movl src dst)
  (if (and (numberp count) (greaterp count 8))
      then (e-write3 'moveq `($ ,count) 'd0)
           (e-write3 'asll 'd0 dst)
      else (e-write3 'asll `($ ,count) dst)))
