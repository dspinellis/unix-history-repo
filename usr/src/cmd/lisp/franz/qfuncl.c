
	.asciz	"@(#)qfuncl.c	35.4	5/26/82"
/* 
 * lisp to C or lisp interface code
 * This is written in assembler but must be passed through the C preprocessor
 * before being assembled.
 */

/* we need the type numbers here, and it would be nice if we could include
 * global.h but we can't due to the other C code in that file,
 * we should seperate type numbers and other stuff, but for now:
 */

#define	UNBO	-1
#define	STRNG	0
#define	ATOM	1
#define	INT	2
#define	DTPR	3
#define DOUB	4
#define	BCD	5
#define	PORT	6
#define	ARRAY	7
#define OTHER   8
#define SDOT	9
#define VALUE	10

/* important offsets within data types for atoms */
#define Atomfnbnd 8

/*  for arrays */
#define Arrayaccfun 0

#ifdef PROF
	.set	indx,0
#define Profile \
	movab	prbuf+indx,r0 \
	.set 	indx,indx+4 \
	jsb 	mcount 
#else
#define Profile
#endif


/*
 * non existant function, call c function to take care of it,
 * we could process it here but wish to minimize assembly language
 * code.
 * we should never return from this call
 * the addr of the atom is already stacked
 */

nonexf:
	calls	$1,_Undeff		# call handler
	clrl	r0			# return nil to compiled code
	rsb				# if ever should return here



/*   transfer  table linkage routine  */

	.globl	_qlinker
_qlinker:
	.word 	0xfc0			# save all possible registers
	Profile
	tstl	_exception	        # any pending exceptions
	jeql	noexc
	tstl	_sigintcnt		# is it because of SIGINT
	jeql	noexc			# if not, just leave
	pushl	$2			# else push SIGINT
	calls	$1,_sigcall
noexc:
	movl	16(fp),r0		# get return pc
	addl2	-4(r0),r0		# get pointer to table
	movl	4(r0),r1		# get atom pointer
retry:					# come here after undef func error
	movl	Atomfnbnd(r1),r2	# get function binding
	jleq	nonex			# if none, leave
	tstl	_stattab+2*4		# see if linking possible (Strans)
	jeql	nolink			# no, it isn't
	ashl	$-9,r2,r3		# check type of function
	cmpb	$/**/BCD,_typetable+1[r3]	
	jeql	linkin			# bcd, link it in!
	cmpb	$/**/ARRAY,_typetable+1[r3] # how about array?
	jeql	doarray			# yep


nolink:
	pushl	r1			# non, bcd, call interpreter
	calls	$1,_Lfuncal
	ret

/*
 * handle arrays by pushing the array descriptor on the table and checking
 * for a bcd array handler
 */
doarray:
	ashl	$-9,Arrayaccfun(r2),r3	# get access function addr shifted
	cmpb	$/**/BCD,_typetable+1[r3]	# bcd??
	jneq	nolink			# no, let funcal handle it
	movl	r2,(r6)+		# store array header on stack
	movl	*(r2),r2	# get in func addr
	jmp	2(r2)			# jump in beyond calls header
	
	
linkin:	
	ashl	$-9,4(r2),r3		# check type of function discipline
	cmpb	$0,_typetable+1[r3]	# is it string?
	jeql	nolink			# yes, it is a c call, so dont link in
	movl	(r2),r2			# get function addr
	movl	r2,(r0)			# put fcn addr in table
	jmp	2(r2)			# enter fcn after mask

nonex:	pushl	r0			# preserve table address
	pushl	r1			# non existant fcn
	calls	$1,_Undeff		# call processor
	movl	r0,r1			# back in r1
	movl	(sp)+,r0		# restore table address
	jbr	retry			# for the retry.


	.globl	__erthrow		# errmessage for uncaught throws
__erthrow: 
	.byte	'U,'n,'c,'a,'u,'g,'h,'t,' ,'t,'h,'r,'o,'w
	.byte	' ,'f,'r,'o,'m,' ,'c,'o,'m,'p,'i,'l,'e,'d
	.byte	' ,'c,'o,'d,'e,0

	.globl _tynames
_tynames:
	.long	0				# nothing here
	.long	_lispsys+20*4	# str_name
	.long	_lispsys+21*4	# atom_name
	.long	_lispsys+19*4	# int_name
	.long	_lispsys+23*4	# dtpr_name
	.long	_lispsys+22*4	# doub_name
	.long	_lispsys+58*4	# funct_name
	.long	_lispsys+103*4	# port_name
	.long	_lispsys+47*4	# array_name
	.long	0				# nothing here
	.long	_lispsys+50*4	# sdot_name
	.long	_lispsys+53*4	# val_nam

/*	Quickly allocate small fixnums  */

	.globl	_qnewint
_qnewint:
	Profile
	cmpl	r5,$1024
	jgeq	alloc
	cmpl	r5,$-1024
	jlss	alloc
	moval	Fixzero[r5],r0
	rsb
alloc:
	movl	_int_str,r0	# move next cell addr to r0
	jlss	callnewi	# if no space, allocate
	incl	*_lispsys+24*4	# inc count of ints
	movl	(r0),_int_str	# advance free list
	movl	r5,(r0)		# put baby to bed.
	rsb
callnewi:
	pushl	r5
	calls	$0,_newint
	movl	(sp)+,(r0)
	rsb


/*  _qoneplus adds one to the boxed fixnum in r0
 * and returns a boxed fixnum.
 */

	.globl	_qoneplus
_qoneplus:
	Profile
	addl3	(r0),$1,r5
	jbr	_qnewint

/* _qoneminus  subtracts one from the boxes fixnum in r0 and returns a
 * boxed fixnum
 */
	.globl	_qoneminus
_qoneminus:
	Profile
	subl3	$1,(r0),r5
	jbr	_qnewint

/*
 *	_qnewdoub quick allocation of a initialized double (float) cell.
 *	This entry point is required by the compiler for symmetry reasons.
 *	Passed to _qnewdoub in r4,r5 is a double precision floating point
 *	number.  This routine allocates a new cell, initializes it with
 *	the given value and then returns the cell.
 */

	.globl	_qnewdoub
    
_qnewdoub:
	Profile
	movl	_doub_str,r0	# move next cell addr to r0
	jlss	callnewd	# if no space, allocate
	incl	*_lispsys+30*4	# inc count of doubs
	movl	(r0),_doub_str	# advance free list
	movq	r4,(r0)		# put baby to bed.
	rsb

callnewd:
	movq	r4,-(sp)	# stack initial value
	calls	$0,_newdoub
	movq	(sp)+,(r0)	# restore initial value
	rsb

	.globl	_qcons

/*
 * quick cons call, the car and cdr are stacked on the namestack
 * and this function is jsb'ed to.
 */

_qcons:
	Profile
	movl	_dtpr_str,r0	# move next cell addr to r0
	jlss	getnew		# if ran out of space jump
	incl	*_lispsys+28*4	# inc count of dtprs
	movl	(r0),_dtpr_str	# advance free list
storit:	movl	-(r6),(r0)	# store in cdr
	movl	-(r6),4(r0)	# store in car
	rsb

getnew:	calls	$0,_newdot	# must gc to get one
	jbr	storit		# now initialize it.

/*
 * Fast equivalent of newdot, entered by jsb
 */

	.globl	_qnewdot
_qnewdot:
	Profile
	movl	_dtpr_str,r0	# mov next cell addr t0 r0
	jlss	mustallo	# if ran out of space
	incl	*_lispsys+28*4	# inc count of dtprs
	movl	(r0),_dtpr_str	# advance free list
	clrq	(r0)
	rsb
mustallo:
	calls	$0,_newdot
	rsb

/*  prunel  - return a list of dtpr cells to the free list
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
	movl	r0,r2		# remember first dtpr location
rep:	decl	*_lispsys+28*4	# decrement used dtpr count
	movl	4(r0),r1	# put link value into r1
	jeql	endoflist	# if nil, then end of list
	movl	r1,(r0)		# replace cdr with save value as car
	movl	r1,r0		# advance to next dtpr
	jbr	rep		# and loop around
endoflist:
	movl	_dtpr_str,(r0)	# make last one point to free list
	movl	r2,_dtpr_str	# and free list begin at first one
	rsb

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
	movl	4(sp),r0		# get address
	decl	*_lispsys+48*4		# decr count of used sdots
	movl	_sdot_str,(r0)		# have new sdot point to free list
	movl	r0,_sdot_str		# start free list at new sdot
	movl	4(r0),r0		# get address of first dtpr
	jneq	qprunel			# if exists, prune it
	rsb				# else return.


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
	movl	4(sp),r0		# get address of fixnum
	cmpl	r0,$_gstart		# is it a small fixnum
	jlss	skipit			# if so, leave
	decl	*_lispsys+24*4		# decr count of used ints
	movl	_int_str,(r0)		# link the fixnum into the free list
	movl	r0,_int_str
skipit:
	rsb


	.globl	_qpopnames
_qpopnames:			# equivalent of C-code popnames, entered by jsb.
	movl	(sp)+,r0	# return address
	movl	(sp)+,r1	# Lower limit
	movl	_bnp,r2		# pointer to bind stack entry
qploop:
	subl2	$8,r2		# for(; (--r2) > r1;) {
	cmpl	r2,r1		# test for done
	jlss	qpdone		
	movl	(r2),*4(r2)	# r2->atm->a.clb = r2 -> val;
	brb	qploop		# }
qpdone:
	movl	r1,_bnp		# restore bnp
	jmp	(r0)		# return

/*
 * _qget : fast get subroutine
 *  (get 'atom 'ind)
 * called with -8(r6) equal to the atom
 *	      -4(r6) equal to the indicator
 * no assumption is made about r7
 * unfortunately, the atom may not in fact be an atom, it may
 * be a list or nil, which are special cases.
 * For nil, we grab the nil property list (stored in a special place)
 * and for lists we punt and call the C routine since it is  most likely
 * and error and we havent put in error checks yet.
 */

	.globl	_qget
_qget:
	Profile
	movl	-4(r6),r1	# put indicator in r1
	movl	-8(r6),r0	# and atom into r0
	jeql	nilpli		# jump if atom is nil
	ashl	$-9,r0,r2	# check type
	cmpb	_typetable+1[r2],$1 # is it a symbol??
	jneq	notsymb		# nope
	movl	4(r0),r0	# yes, put prop list in r1 to begin scan
	jeql	fail		# if no prop list, we lose right away
lp:	cmpl	r1,4(r0)	# is car of list eq to indicator?
	jeql	good		# jump if so
	movl	*(r0),r0	# else cddr down list
	jneq	lp		# and jump if more list to go.

fail:	subl2	$8,r6		# unstack args
	rsb			# return with r0 eq to nil

good:	movl	(r0),r0		# return cadr of list
	movl	4(r0),r0
	subl2	$8,r6		#unstack args
	rsb

nilpli:	movl	_lispsys+64*4,r0 # want nil prop list, get it specially
	jneq	lp		# and process if anything there
	subl2	$8,r6		#unstack args
	rsb			# else fail
	
notsymb:
	movab	-8(r6),r7	# must set up r7 before calling
	calls	$0,_Lget	# not a symbol, call C routine to error check
	subl2	$8,r6		#unstack args
	rsb			# and return what it returned.

/*
 * _qexarith 	exact arithmetic
 * calculates x=a*b+c  where a,b and c are 32 bit 2's complement integers
 * whose top two bits must be the same (i.e. the are members of the set
 * of valid fixnum values for Franz Lisp).  The result, x, will be 64 bits
 * long but since each of a, b and c had only 31 bits of precision, the
 * result x only has 62 bits of precision.  The lower 30 bits are returned
 * in *plo and the high 32 bits are returned in *phi.  If *phi is 0 or -1 then
 * x doesn't need any more than 31 bits plus sign to describe, so we
 * place the sign in the high two bits of *plo and return 0 from this
 * routine.  A non zero return indicates that x requires more than 31 bits
 * to describe.
 */

	.globl	_qexarith
/* qexarith(a,b,c,phi,plo)
 * int *phi, *plo;
 */
_qexarith:
	emul	4(sp),8(sp),12(sp),r2   #r2 = a*b + c to 64 bits
	extzv	$0,$30,r2,*20(sp)	#get new lo
	extv	$30,$32,r2,r0		#get new carry
	beql	out			# hi = 0, no work necessary
	movl	r0,*16(sp)		# save hi
	mcoml	r0,r0			# Is hi = -1 (it'll fit in one word)
	bneq	out			# it doesn't
	bisl2	$0xc0000000,*20(sp)	# alter low so that it is ok.
out:	rsb



/*
 * pushframe : stack a frame 
 * When this is called, the optional arguments and class have already been
 * pushed on the stack as well as the return address (by virtue of the jsb)
 * , we push on the rest of the stuff (see h/frame.h)
 * for a picture of the save frame
 */
	.globl	_qpushframe

_qpushframe:
	Profile
	movl	_errp,-(sp)
	movl	_bnp,-(sp)
	movl	r6,-(sp)
	movl	r7,-(sp)
	pushr	$0x3f00		# save r13(fp), r12(ap),r11,r10,r9,r8
	movab	6*4(sp),r0	# return addr of lbot on stack
	clrl	_retval		# set retval to C_INITIAL
	jmp	*40(sp)		# return through return address

/*
 * qretfromfr
 * called with frame to ret to in r11.  The popnames has already been done.
 * we must restore all registers, and jump to the ret addr. the popping
 * must be done without reducing the stack pointer since an interrupt
 * could come in at any time and this frame must remain on the stack.
 * thus we can't use popr.
 */

	.globl	_qretfromfr

_qretfromfr:
	Profile
	movl	r11,r0		# return error frame location
	subl3	$24,r11,sp	# set up sp at bottom of frame
	movl	sp,r1		# prepare to pop off
	movq	(r1)+,r8	# r8,r9
	movq	(r1)+,r10	# r10,r11
	movq	(r1)+,r12	# r12,r13
	movl	(r1)+,r7	# r7 (lbot)
	movl	(r1)+,r6	# r6 (np)
	jmp	*40(sp)		# jump out of frame
	

/* This must be at the end of the file.  If we are profiling, allocate
 * space for the profile buffer
 */
#ifdef PROF
	.data
	.lcomm	prbuf,indx+4
	.text
#endif
