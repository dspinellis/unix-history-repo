/*
 *$Header: qfuncl.c,v 1.9 84/02/29 17:23:24 sklower Exp $
 *$Locker:  $
 *
 * Copyright (c) 1982, by the Regents, University of California
 *
 *			-[Tue Mar 22 15:42:27 1983 by layer]-
 *
 * "quick" functions file.
 *
 * This is written in assembler but must be passed through the C preprocessor
 * before being assembled.
 *
 */

#include "ltypes.h"
#include "config.h"
 
/* important offsets within data types for atoms */
#define Atomfnbnd 8

/*  for arrays */
#define Arrayaccfun 0

/* register defines */
#define FIXREG	d2

#ifdef NPINREG
#define _np a2
#define _lbot d3
#endif


#ifdef PROF
	.set	indx,0
#define Profile \
	lea	prbuf+indx,a0 \
	.set 	indx,indx+4 \
	jsr 	mcount 
#define Profile2 \
	movl	a0,sp@-
	lea	prbuf+indx,a0 \
	.set 	indx,indx+4 \
	jsr 	mcount 
	movl	sp@+,a0
#else
#define Profile
#define Profile2
#endif

#ifdef PORTABLE
#define	NILtest(p)	cmpl	#/**/OFFSET,p
#define	NILsub(p)	subl	#/**/OFFSET,p
#else
#define NILtest(p)
#define NILsub(p)
#endif


	.text
	
/*   transfer  table linkage routine  */
	.globl	_qlinker
_qlinker:
	Profile
	link	a6,#-28
	tstb	sp@(-132)
	moveml	#036000,a6@(-28)		|a(2,3,4,5)

	tstl	_exception	        	|any pending exceptions
	jeq	noexc
	tstl	_sigintcnt			|is it because of SIGINT
	jeq	noexc				|if not, just leave
	movl	#2,sp@-				|else push SIGINT
	jsr	_sigcall
noexc:
	movl	a6@(4),a4			|get return pc
	movl	a4@(-6),a4			|get pointer to table
	movl	a4@(4),a5			|get atom pointer
retry:						|come here after undeffunc err
	movl	a5@(8),a0			|get function binding
	cmpl	a0,d7				|if nil,
	jeq	nonex				|then leave
	tstl	2*4+_stattab			|see if linkin posble (Strans)
	jeq	nolink				|no, it isn't
	movl	a0,d0				|check type of function
	NILsub(d0)
	moveq	#9,d1
	asrl	d1,d0
	lea	_typetable+1,a3
	movb	a3@(0,d0:L),d1
	cmpb	#/**/BCD,d1
	jeq	linkin				|bcd, link it in!
	cmpb	#/**/ARRAY,d1			|how about array?
	jeq	doarray				|yep

nolink:
	movl	a5,sp@-				|non, bcd, call interpreter
	jsr	_Ifuncal
	moveml	a6@(-28),#036000
	unlk	a6
	rts

/*
 * handle arrays by pushing the array descriptor on the table and checking
 * for a bcd array handler
 */
doarray:
	movl	a0@(Arrayaccfun),d0		|get access func addr shifted
	NILsub(d0)
	movl	#9,d1
	asrl	d1,d0
	lea	_typetable+1,a3
	cmpb	#/**/BCD,a3@(0,d0:L)		|bcd??
	jne	nolink				|no, let funcal handle it
	movl	a0,a2@+				|store array header on stack
	movl	a2,_np
	movl	a0@,a0				|movl *(a0),a0 on VAX
	movl	a0@,a0
	jsr	a0@
	subql	#4,_np
	moveml	a6@(-28),#036000
	unlk	a6
	rts
	
	
linkin:	
	movl	a0@(4),d0			|check type of function discipline
	NILsub(d0)
	movl	#9,d1
	asrl	d1,d0
	lea	_typetable+1,a3
	cmpb	#/**/STRNG,a3@(0,d0:L)		|is it string?
	jeq	nolink				|yes, it is a c call,
						|so dont link in
	movl	a0@,a0				|get function addr
	movl	a0,a4@				|put fcn addr in table
	jbsr	a0@
	moveml	a6@(-28),#036000
	unlk	a6
	rts


nonex:	movl	a4,sp@-				|preserve table address
	movl	a5,sp@-				|non existant fcn
	jsr	_Undeff				|call processor
	movl	d0,a5				|back in r1
	addql	#4,sp
	movl	sp@+,a4				|restore table address
	jra	retry				|for the retry.


	.data
	.globl	__erthrow
__erthrow: 
	.asciz	"Uncaught throw from compiled code"
	.text

	.globl _tynames
_tynames:
	.long	_nilatom			|nothing here
	.long	20*4+_lispsys			|str_name
	.long	21*4+_lispsys			|atom_name
	.long	19*4+_lispsys			|int_name
	.long	23*4+_lispsys			|dtpr_name
	.long	22*4+_lispsys			|doub_name
	.long	58*4+_lispsys			|funct_name
	.long	103*4+_lispsys			|port_name
	.long	47*4+_lispsys			|array_name
	.long	_nilatom			|nothing here
	.long	50*4+_lispsys			|sdot_name
	.long	53*4+_lispsys			|val_nam

	.long	_nilatom			| hunk2_nam
	.long	_nilatom			| hunk4_nam
	.long	_nilatom			| hunk8_nam
	.long	_nilatom			| hunk16_nam
	.long	_nilatom			| hunk32_nam
	.long	_nilatom			| hunk64_nam
	.long	_nilatom			| hunk128_nam
	.long	124*4+_lispsys			|vector_nam
	.long	125*4+_lispsys			|vectori_nam

/*	Quickly allocate small fixnums  */

	.globl	_qnewint
_qnewint:
	Profile
	cmpl	#1024,FIXREG
	bge	alloc
	cmpl	#-1024,FIXREG
	bmi	alloc
	movl	FIXREG,d0
	asll	#2,d0
	addl	#_Fixzero,d0
	rts
alloc:
	movl	_int_str,a0			|move next cell addr to r0
	NILtest(a0)
	jmi	callnewi			|if no space, allocate
	movl	4*24+_lispsys,a1
	addql	#1,a1@				|inc count of ints
	movl	a0@,_int_str			|advance free list
	movl	FIXREG,a0@			|put baby to bed.
	movl	a0,d0
	rts
callnewi:
	movl	FIXREG,sp@-
	movl	a2,_np				|gc could occur
	movl	a2,_lbot
	jsr	_newint
	movl	d0,a0
	movl	sp@+,a0@
	rts

/*  _qoneplus adds one to the boxed fixnum in r0
 * and returns a boxed fixnum.
 */

	.globl	_qoneplus
_qoneplus:
	Profile
	movl	a0@,FIXREG
	addql	#1,FIXREG
	bra	_qnewint

/* _qoneminus  subtracts one from the boxes fixnum in r0 and returns a
 * boxed fixnum
 */
	.globl	_qoneminus
_qoneminus:
	Profile
	movl	a0@,FIXREG
	subql	#1,FIXREG
	bra	_qnewint

/*
 *	_qnewdoub quick allocation of a initialized double (float) cell.
 *	This entry point is required by the compiler for symmetry reasons.
 *	Passed to _qnewdoub in d0,d1 is a double precision floating point
 *	number.  This routine allocates a new cell, initializes it with
 *	the given value and then returns the cell.
 */

	.globl	_qnewdoub
    
_qnewdoub:
	Profile
	movl	_doub_str,a0			|move next cell addr to r0
	NILtest(a0)
	jmi	callnewd			|if no space, allocate
	|incl	*_lispsys+30*4			|inc count of doubs
	lea	30*4+_lispsys,a1
	addl	#1,a1@
	movl	a0@,_doub_str			|advance free list
strdb:
	movl	d0,a0@				|put baby to bed.
	movl	d1,a0@(4)			|put baby to bed.
	rts

callnewd:
	movl	d0,sp@-				|stack initial value
	movl	d1,sp@-				|stack initial value
	movl	a2,_np				|gc could occur
	movl	a2,_lbot
	jsr	_newdoub
	movl	d0,a0
	movl	sp@+,d1				|restore initial value
	movl	sp@+,d0				|restore initial value
	bra	strdb



/*
 * quick cons call, the car and cdr are stacked on the namestack
 * and this function is jsb'ed to.
 */
	.globl	_qcons
_qcons:
	Profile
	movl	_dtpr_str,a0			|move next cell addr to a0
	NILtest(a0)
	jmi	getnew				|if ran out of space jump
	movl	28*4+_lispsys,a1		|inc count of dtprs
	addql	#1,a1@
	movl	a0@,_dtpr_str			|advance free list
storit:	movl	a2@-,a0@			|store in cdr
	movl	a2@-,a0@(4)			|store in car
	movl	a0,d0
	rts

getnew:	movl	a2,_np
	jsr	_newdot				|must gc to get one
	jra	storit				|now initialize it.

/*
 * Fast equivalent of newdot, entered by jsb
 */

	.globl	_qnewdot
_qnewdot:
	Profile
	movl	_dtpr_str,a0			|mov next cell addr t0 r0
	NILtest(a0)
	jmi	mustallo			|if ran out of space

	movl	a0,sp@-
	movl	28*4+_lispsys,a0		|inc count of dtprs
	addql	#1,a0@
	movl	sp@+,a0

	movl	a0@,_dtpr_str			|advance free list
	clrl	a0@				|clrq (r0)
	clrl	a0@(4)
	rts
mustallo:
	movl	a2,_np				|gc could occur
	jsr	_newdot
	rts


/*
 * this is called exactly like popnames would be from C
 * but has been carefully improved so that it doesn't
 * have to alter the stack.
 */
	.globl	_qpopnames
_qpopnames:
	movl	_bnp,a1
	movl	sp,a0
	movl	a0@(4),d0
	jra	.L130
.L20001:
	movl	a1@(4),a0
	movl	a1@,a0@
.L130:
	subql	#8,a1
	cmpl	a1,d0
	jls	.L20001
	movl	a1,_bnp
	rts

/*
 * _qget : fast get subroutine
 *  (get 'atom 'ind)
 * called with a2@(-8) equal to the atom
 *	       a2@(-4) equal to the indicator
 * no assumption is made about _lbot
 * unfortunately, the atom may not in fact be an atom, it may
 * be a list or nil, which are special cases.
 * For nil, we grab the nil property list (stored in a special place)
 * and for lists we punt and call the C routine since it is  most likely
 * and error and we havent put in error checks yet.
 */

	.globl	_qget
_qget:
	Profile
	movl	a2@(-4),a1			|put indicator in a1
	movl	a2@(-8),a0			|and atom into a0
	cmpl	a0,d7
	jeq	nilpli				|jump if atom is nil
	movl	a0,d0				|check type
	NILsub(d0)
	movl	#9,d1
	asrl	d1,d0
	lea	_typetable+1,a5
	cmpb	#/**/ATOM,a5@(0,d0:L)		|is it a symbol??
	jne	notsymb				|nope
	movl	a0@(4),a0			|yes, put prop list in
						|	a0 to begin scan
	cmpl	a0,d7
	jeq	fail				|if no prop list,
						|	we lose right away
lp:	cmpl	a0@(4),a1			|is car of list = to indicator?
	jeq	good				|jump if so
	movl	a0@,a0				|else cddr
	movl	a0@,a0				|	down list
	cmpl	a0,d7
	jne	lp				|and jump if more list to go.

fail:	movl	a0,d0
	subql	#8,a2
	rts					|return with a0 eq to nil

good:	movl	a0@,a0				|return cadr of list
	movl	a0@(4),d0
	subql	#8,a2
	rts

nilpli:	movl	64*4+_lispsys,a0		|want nil prop list,
						|	get it specially
	cmpl	a0,d7
	jne	lp				|and process if anything there
	movl	a0,d0
	subql	#8,a2
	rts					|else fail
	
notsymb:
	lea	a2@(-8),a0			|set up lbot before callin
	movl	a0,_lbot
	movl	a2,_np
	jsr	_Lget				|not a symbol, call C routine
						|	to error check
	subql	#8,a2
	rts					|and return what it returned.


/*
 *  prunel  - return a list of dtpr cells to the free list
 * this is called by the pruneb after it has discarded the top bignum 
 * the dtpr cells are linked through their cars not their cdrs.
 * this returns with an rsb
 *
 * method of operation: the dtpr list we get is linked by car's so we
 * go through the list and link it by cdr's, then have the last dtpr
 * point to the free list and then make the free list begin at the
 * first dtpr.
 */
qprunel:
	movl	a0,d0				|remember first dtpr location
	movl	28*4+_lispsys,a1		|dec count of dtprs
rep:	
	subql	#1,a2@
	movl	a0@(4),a0@			|make cdr (forward lnk) == car
	jeq	endoflist			|if nil, then end of list
	movl	a0@,a0				|advance to next dtpr
	jra	rep				|and loop around
endoflist:
	movl	_dtpr_str,a0@			|make last 1 pnt to free list
	movl	d0,_dtpr_str			|& free list begin at 1st one
	rts

/*
 * qpruneb - called by the arithmetic routines to free an sdot and the dtprs
 * which hang on it.
 * called by
 *	pushl	sdotaddr
 *	jsb	_qpruneb
 */
	.globl	_qpruneb
_qpruneb:
	Profile
	movl	48*4+_lispsys,a0		|decr count of used sdots
	subql	#1,a0@
	movl	sp@(4),a0			|get address
	movl	_sdot_str,a0@			|have new sdot pnt to free lst
	movl	a0,_sdot_str			|strt free list at new sdot
	movl	a0@(4),a0			|get address of first dtpr
	jne	qprunel				|if exists, prune it
	rts					|else return.


/*
 * _qprunei 	 
 *	called by the arithmetic routines to free a fixnum cell
 * calling sequence
 *	pushl	fixnumaddr
 *	jsb	_qprunei
 */

	.globl	_qprunei
_qprunei:
	Profile
	movl	a1,sp@-
	movl	sp@(4),a0			|get address of fixnum
	cmpl	#4*1023+_Fixzero,a0		|is it a small fixnum
	jmi	skipit				|if so, leave
	movl	24*4+_lispsys,a1		|decr count of used ints
	subql	#1,a1@
	movl	_int_str,a0@			|link the fixnum into the
						|  free list
	movl	a0,_int_str
skipit:
	movl	sp@+,a1
	rts
Iclear:
	clrl	d0
	rts
	.text
	.globl	_Itstbt
_Itstbt:
	movl	a5,d1
	NILsub(d1)
	lsrl	#2,d1
	movl	d1,d0
	andl	#7,d0
	lsrl	#3,d1
	lea	_bitmapi,a0
	bset	d0,a0@(0,d1:L)
	beq	.L14
	moveq	#1,d0
	bra	.L12
.L14:
	clrl	d0
.L12:	rts

/*
 * this routine returns an assembly language entry pt.
 * it is put here to match the vax verison.
 */
	.globl	_gstart
	.globl	_proflush
_gstart:
	movl	#start,d0
_proflush:
	rts
/*
 * The definition of mcount must be present even when the C code
 * isn't being profiled, since lisp code may reference it.
 */
.globl _mcount
#ifdef SunGotItsActTogetherAboutTakingMcountOutOfCrt0 
.globl	mcount
#endif

_mcount:
mcount:
#ifdef PROF
	movl	a0@,a1
	jne	incr
	movl	_countbase,a1
	jeq	return
	addql	#8,_countbase
	movl	sp@,a1@+
	movl	a1,a0@
incr:
	addql	#1,a1@
return:
#endif
	rts

/*
 * pushframe : stack a frame 
 * When this is called, the optional arguments and class have already been
 * pushed on the stack as well as the return address (by virtue of the jsb)
 * , we push on the rest of the stuff (see h/frame.h)
 * for a picture of the save frame
 */
	.globl	_pushframe
	.globl	_qpushframe
	.globl	_Pushframe
_pushframe:
_qpushframe:
_Pushframe:
	movl	sp@,a0
	movl	_errp,sp@-
	movl	_bnp,sp@-
	movl	_np,sp@-
	movl	_lbot,sp@-
	movl	sp,d0		| return addr of lbot on stack
	subl	#56,sp
	moveml	#0x7cfc,sp@(12)	| save fp,a5-a2,d7-d2
	clrl	_retval		| set retval to C_INITIAL
#ifdef SPISFP
	subl	#8,sp
	movl	_xsp,sp@(16)	
	movl	sp,sp@(12)
#endif
	jmp	a0@		| return through return address

#ifdef SPISFP
/*
 * This is necessary on the sun-II beta testing version since the C
 * compiler makes refence to temporaries and restoring registers relative
 * to the stack pointer.  See explicative comments in ../vax/qfuncl.c
 * for Iretfrm and Ipushf
 */
	.globl	_Ipushf
_Ipushf:
	movl	sp@(16),a0
	addl	#96,a0
	movl	sp@(12),a0@-
	movl	sp@(8),a0@-
	movl	sp@(4),a0@-
	movl	sp@,a0@-
	movl	_errp,a0@-
	movl	_bnp,a0@-
	movl	_np,a0@-
	movl	_lbot,a0@-
	movl	a0,d0		| return addr of lbot on stack
	moveml	#0x7cfc,a0@(-44)	| save fp,a5-a2,d7-d2
	movl	_xsp,a0@(-48)
	movl	sp,a0@(-52)
	clrl	_retval		| set retval to C_INITIAL
	rts
#endif

/*
 * qretfromfr
 * called with frame to ret to in a5.  The popnames has already been done.
 * we must restore all registers, and jump to the ret addr. the popping
 * must be done without reducing the stack pointer since an interrupt
 * could come in at any time and this frame must remain on the stack.
 * thus we can't use popr.
 */

	.globl	_qretfromfr

_qretfromfr:
	movl	a5,d0			| return error frame location
	movl	a5,a0			| prepare to pop off
	moveml	a0@(-44),#0x7cfc	| restore registers
#ifndef SPISFP
	lea	a0@(-56),sp
	movl	a0@+,_lbot
	movl	a0@+,_np
	movl	a0@(8),a0		| return address
	jmp	a0@
#else
	movl	a0@(-52),sp
	movl	a0@(-48),_xsp
	movl	a0@+,_lbot
	movl	a0@+,_np
	movl	a0@(8),sp@		| return address
	rts
#endif

/*
 * Ancillary code for small thunks generated so that
 * c routines can be passed the address of something
 * to call which will pass onto lisp functions
 */
	.globl	_thcpy
_thcpy:
	movl	sp@,a0
	movl	a0@+,sp@-
	movl	a0@+,sp@-
	jsr	_dothunk
	lea	sp@(12),sp
	rts
#ifndef SPISFP
/* Copyright (c) 1982, Regents, University of California
   This is here because for the sun II beta test version, you
   can't do alloca */
	.text
	.globl	_alloca
_alloca:
	movl	sp@,d0
	movl	sp@(4),d1
	subl	#1,d1
	orl	#3,d1
	addl	#1,d1
	subl	d1,sp
	tstb	sp@(-132)
	movl	d0,sp@
	movl	sp,d0
	addl	#8,d0
	rts

#endif
	.globl	_vlsub
_vlsub:
	movl	sp@(4),a0
	addql	#8,a0
	movl	sp@(8),a1
	addql	#8,a1	| this should clear the carry bit.
#if sun_4_1c || sun_4_2beta
	subxl	a0@-,a1@-
	subxl	a0@-,a1@-
#else
	subxl	a1@-,a0@-	| This is the correct version
	subxl	a1@-,a0@-
#endif
	rts

/*
 * We want to be able to redefine read and write to check
 * certain lisp values.  Rather than have 4 variants, we
 * put the assembly language (obtained by adb rather than
 * violating source) here under ifdef control.
 */


.globl	__read
.globl	__write

#if sun_4_1c || sun_4_2beta || sun_4_2
.globl _vadvise
__read:
	pea     3:w
	jmp	_docall
__write:
	pea	4:w
_docall:
	trap    #0
	bcss   _bad
_vadvise:
#endif
#ifdef os_masscomp
__read:
	moveq    #0x3,d0
	jmp     _docall
__write:
	moveq    #0x4,d0
_docall:
	movl	a7@(4),d1
	movl	a7@(8),a0
	movl	a7@(12),a1
	trap	#0
	bcss	_bad
#endif
#ifdef os_unisoft || os_unix_ts
	.globl	_vfork
_vfork:
	jmp	_fork
__read:
	movw    #0x3,d0
	jmp     _docall
__write:
	movw    #0x4,d0
_docall:
	movl    a7@(4),a0
	movl    a7@(8),d1
	movl    a7@(12),a1
	trap	#0x0
	bcs	_bad
#endif
	rts
_bad:
	jmp	cerror

/* This must be at the end of the file.  If we are profiling, allocate
 * space for the profile buffer
 */
#ifdef PROF
	.data
	.comm	_countbase,4
	.lcomm	prbuf,indx+4
	.text
#endif
