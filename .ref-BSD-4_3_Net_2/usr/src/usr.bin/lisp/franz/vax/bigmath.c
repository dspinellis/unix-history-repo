/* $Header: bigmath.c 1.4 83/06/09 00:50:06 sklower Exp $ */

#include "config.h"

	.globl	_dmlad
/*
	routine for destructive multiplication and addition to a bignum by
	two fixnums.

	from C, the invocation is dmlad(sdot,mul,add);
	where sdot is the address of the first special cell of the bignum
	mul is the multiplier, add is the fixnum to be added (The latter
	being passed by value, as is the usual case.


	Register assignments:

	r11 = current sdot
	r10 = carry
	r9  = previous sdot, for relinking.
*/
_dmlad:	.word	0x0e00
	movl	4(ap),r11		#initialize cell pointer
	movl	12(ap),r10		#initialize carry
loop:	emul	8(ap),(r11),r10,r0	#r0 gets cell->car times mul + carry
/*	ediv	$0x40000000,r0,r10,(r11)#cell->car gets prod % 2**30
					#carry gets quotient
*/
	extzv	$0,$30,r0,(r11)
	extv	$30,$32,r0,r10
	movl	r11,r9			#save last cell for fixup at end.
	movl	4(r11),r11		#move to next cell
	bneq	loop			#done indicated by 0 for next sdot
	tstl	r10			#if carry zero no need to allocate
	beql	done			#new bigit
	mcoml	r10,r3			#test to see if neg 1.
	bneq	alloc			#if not must allocate new cell.
	tstl	(r9)			#make sure product isn't -2**30
	beql	alloc
	movl	r0,(r9)			#save old lower half of product.
	brb	done
alloc:	jsb	_qnewdot			#otherwise allocate new bigit
	movl	r10,(r0)		#store carry
	movl	r0,4(r9)		#save new link cell
done:	movl	4(ap),r0
	ret
	.globl _dodiv
/*
	routine to destructively divide array representation of a bignum by 
	1000000000

	invocation:
		remainder = dodiv(top,bottom)
		int *top, *bottom;
	where *bottom is the address of the biggning of the array, *top is
	the top of the array.

	register assignments:
	r0 = carry
	r1 & r2 = 64bit temporary
	r3 = pointer
*/
_dodiv:	.word	0
	clrl		r0		#no carry to begin.
	movl		8(ap),r3	#get pointer to array.
loop2:	emul		$0x40000000,r0,(r3),r1
	ediv		$1000000000,r1,(r3),r0
	acbl		4(ap),$4,r3,loop2
	ret
	.globl	_dsneg
/*
	dsneg(top, bot);
	int *top, *bot;

	routine to destructively negate a bignum stored in array format
	lower order stuff at higher addresses. It is assume that the
	result will be positive.
*/
_dsneg:	.word	0
	movl	4(ap),r1	#load up address.
	clrl	r2		#set carry
loop3:	mnegl	(r1),r0		#negate and take carry into account.
	addl2	r2,r0
	extzv	$0,$30,r0,(r1)
	extv	$30,$2,r0,r2
	acbl	8(ap),$-4,r1,loop3
				#decrease r1, and branch back if appropriate.
	ret

/*
	bignum add routine
	basic data representation is each bigit is a positive number
	less than 2^30, except for the leading bigit, which is in
	the range -2^30 < x < 2^30.
*/

	.globl	_adbig
	.globl	Bexport
	.globl	backfr
/*
	Initialization section
*/
_adbig:	.word	0x0fc0		#save registers 6-11
	movl	4(ap),r1	#arg1 = addr of 1st bignum
	movl	8(ap),r2	#arg2 = addr of 2nd bignum
	clrl	r5		#r5   = carry
	movl	$0xC0000000,r4	#r4   = clear constant.
	movl	sp,r10		#save start address of bignum on stack.
				#note well that this is 4 above the actual
				#low order word.
/*
	first loop is to waltz through both bignums adding
	bigits, pushing them onto stack. 
*/
loop4:	addl3	(r1),(r2),r0	#add bigits
	addl2	r5,r0		#add carry
	bicl3	r4,r0,-(sp)	#save sum, no overflow possible
	extv	$30,$2,r0,r5	#sign extend two high order bits
				#to be next carry.
	movl	4(r1),r1	#get cdr
	bleq	out1		#negative indicates end of list.
	movl	4(r2),r2	#get cdr of second bignum
	bgtr	loop4		#if neither list at end, do it again
/*
	second loop propagates carries through higher order words.
	It assumes remaining list in r1.
*/
loop5:	addl3	(r1),r5,r0	#add bigits and carry
	bicl3	r4,r0,-(sp)	#save sum, no overflow possible
	extv	$30,$2,r0,r5	#sign extend two high order bits
				#to be next carry.
	movl	4(r1),r1	#get cdr
out2:	bgtr	loop5		#negative indicates end of list.
out2a:	pushl	r5
/*
	suppress unnecessary leading zeroes and -1's

	WARNING:  this code is duplicated in C in divbig.c
*/
iexport:movl	sp,r11		#more set up for output routine
ckloop:	
Bexport:tstl	(r11)		#look at leading bigit
	bgtr	copyit		#if positive, can allocate storage etc.
	blss	negchk		#if neg, still a chance we can get by
	cmpl	r11,r10		#check to see that
	bgeq	copyit		#we don't pop everything off of stack
	tstl	(r11)+		#incr r11
	brb	ckloop		#examine next
negchk:
	mcoml	(r11),r3		#r3 is junk register
	bneq	copyit		#short test for -1
	tstl	4(r11)		#examine next bigit
	beql	copyit		#if zero must must leave as is.
	cmpl	r11,r10		#check to see that
	bgeq	copyit		#we don't pop everything off of stack
	tstl	(r11)+		#incr r11
	bisl2	r4,(r11)	#set high order two bits
	brb	negchk		#try to supress more leading -1's
/*
	The following code is an error exit from the first loop
 	and is out of place to avoid a jump around a jump.
*/
out1:	movl	4(r2),r1	#get next addr of list to continue.
	brb	out2		#if second list simult. exhausted, do
				#right thing.
/*
	loop6 is a faster version of loop5 when carries are no
	longer necessary.
*/
loop6a: pushl	(r1)		#get datum
loop6:	movl	4(r1),r1	#get cdr
	bgtr	loop6a		#if not at end get next cell
	brb	out2a

/*
	create linked list representation of bignum
*/
copyit:	subl3	r11,r10,r2	#see if we can get away with allocating an int
	bneq	on1		#test for having popped everything
	subl3	$4,r10,r11	#if so, fix up pointer to bottom
	brb	intout		#and allocate int.
on1:	cmpl	r2,$4		#if = 4, then can do
	beql	intout
	calls	$0,_newsdot	#get new cell for new bignum
backfr:
#ifdef PORTABLE
	movl	r0,*_np
	addl2	$4,_np
#else
	movl	r0,(r6)+	#push address of cell on
				#arg stack to save from garbage collection.
				#There is guaranteed to be slop for a least 1
				#push without checking.
#endif
loop7:	movl	-(r10),(r0)	#save bigit
	movl	r0,r9		#r9 = old cell, to link
	cmpl	r10,r11		#have we copy'ed all the bigits?
	bleq	Edone
	jsb	_qnewdot	#get new cell for new bignum
	movl	r0,4(r9)	#link new cell to old
	brb	loop7
Edone:	
	clrl	4(r9)		#indicate end of list with 0
#ifdef PORTABLE
	subl2	$4,_np
	movl	*_np,r0
#else
	movl	-(r6),r0	#give resultant address.
#endif
	ret
/*
	export integer
*/
intout: pushl	(r11)
	calls	$1,_inewint
	ret
	.globl	_mulbig
/*
	bignum multiplication routine

	Initialization section
*/
_mulbig:.word	0x0fc0		#save regs 6-11
	movl	4(ap),r1	#get address of first bignum
	movl	sp,r11		#save top of 1st bignum
mloop1:	pushl	(r1)		#get bigit
	movl	4(r1),r1	#get cdr
	bgtr	mloop1		#repeat if not done
	movl	sp,r10		#save bottom of 1st bignum, top of 2nd bignum
	
	movl	8(ap),r1	#get address of 2nd bignum
mloop2:	pushl	(r1)		#get bigit
	movl	4(r1),r1	#get cdr
	bgtr	mloop2		#repeat if not done
	movl	sp,r9		#save bottom of 2nd bignum
	subl3	r9,r11,r6	#r6 contains sum of lengths of bignums
	subl2	r6,sp
	movl	sp,r8		#save bottom of product bignum
/*
	Actual multiplication
*/
m1:	movc5	$0,(r8),$0,r6,(r8)#zap out stack space
	movl	r9,r7		#r7 = &w[j +n] (+4 for a.d.) through calculation
	subl3	$4,r10,r4	#r4 = &v[j]

m3:	movl	r7,r5		#r7 = &w[j+n]
	subl3	$4,r11,r3	#r3 = &u[i]
	clrl	r2		#clear carry.

m4:	addl2	-(r5),r2	#add w[i + j] to carry (no ofl poss)
	emul	(r3),(r4),r2,r0 #r0 = u[i] * v[j] + sext(carry)
	extzv	$0,$30,r0,(r5)	#get new bigit
	extv	$30,$32,r0,r2	#get new carry

m5:	acbl	r10,$-4,r3,m4	#r3 =- 4; if(r3 >= r10) goto m4; r10 = &[u1];
	movl	r2,-(r5)	#save w[j] = carry

m6:	subl2	$4,r7		#add just &w[j+n] (+4 for autodec)
	acbl	r9,$-4,r4,m3	#r4 =- 4; if(r4>=r9) goto m5; r9 = &v[1]

	movl	r9,r10		#set up for output routine
	movl	$0xC0000000,r4	#r4   = clear constant.
	movq	20(fp),r6	#restor _np and _lbot !
	brw	iexport		#do it!
/*
 The remainder of this file are routines used in bignum division.
 Interested parties should consult Knuth, Vol 2, and divbig.c.
 These files are here only due to an optimizer bug.
*/
	.align	1
	.globl	_calqhat
_calqhat:
	.word	0xf00
	movl	4(ap),r11		# &u[j] into r11
	movl	8(ap),r10		# &v[1] into r10
	cmpl	(r10),(r11)		# v[1] == u[j] ??
	beql	L102			
	# calculate qhat and rhat simultaneously,
	#  qhat in r0
	#  rhat in r1
	emul	(r11),$0x40000000,4(r11),r4 # u[j]b+u[j+1] into r4,r5
	ediv	(r10),r4,r0,r1		# qhat = ((u[j]b+u[j+1])/v[1]) into r0
					# (u[j]b+u[j+1] -qhat*v[1]) into r1
					# called rhat
L101:
	# check if v[2]*qhat > rhat*b+u[j+2]
	emul	r0,4(r10),$0,r2		# qhat*v[2] into r3,r2
	emul	r1,$0x40000000,8(r11),r8 #rhat*b + u[j+2] into r9,r8
	# give up if r3,r2 <= r9,r8, otherwise iterate
	subl2	r8,r2			# perform r3,r2 - r9,r8
	sbwc	r9,r3
	bleq	L103			# give up if negative or equal
	decl	r0			# otherwise, qhat = qhat - 1
	addl2	(r10),r1		# since dec'ed qhat, inc rhat by v[1]
	jbr	L101
L102:	
	# get here if v[1]==u[j]
	# set qhat to b-1
	# rhat is easily calculated since if we substitute b-1 for qhat in
	# the formula, then it simplifies to (u[j+1] + v[1])
	# 
	addl3	4(r11),(r10),r1		# rhat = u[j+1] + v[1]
	movl	$0x3fffffff,r0		# qhat = b-1
	jbr	L101
	
L103:
	ret

	.align	1
	.globl	_mlsb
_mlsb:
	.word	.R2
	movl	4(ap),r11
	movl	8(ap),r10
	movl	12(ap),r9
	movl	16(ap),r8
	clrl	r0
loop8:	addl2	(r11),r0
	emul	r8,-(r9),r0,r2
	extzv	$0,$30,r2,(r11)
	extv	$30,$32,r2,r0
	acbl	r10,$-4,r11,loop8
	ret
	.set	.R2,0xf00
	.align	1
	.globl	_adback
_adback:
	.word	.R3
	movl	4(ap),r11
	movl	8(ap),r10
	movl	12(ap),r9
	clrl	r0
loop9:	addl2	-(r9),r0
	addl2	(r11),r0
	extzv	$0,$30,r0,(r11)
	extv	$30,$2,r0,r0
	acbl	r10,$-4,r11,loop9
	ret
	.set	.R3,0xe00
	.align	1
	.globl	_dsdiv
_dsdiv:
	.word	.R4
	movl	8(ap),r11
	clrl	r0
loopa:	emul	r0,$0x40000000,(r11),r1
	ediv	12(ap),r1,(r11),r0
	acbl	4(ap),$4,r11,loopa
	ret
	.set	.R4,0x800
	.align	1
	.globl	_dsmult
_dsmult:
	.word	.R5
	movl	4(ap),r11
	clrl	r0
loopb:	emul	12(ap),(r11),r0,r1
	extzv	$0,$30,r1,(r11)
	extv	$30,$32,r1,r0
	acbl	8(ap),$-4,r11,loopb
	movl	r1,4(r11)
	ret
	.set	.R5,0x800
	.align	1
	.globl	_dsrsh
_dsrsh:
	.word	.R7
	movl	8(ap),r11	# bot
	movl	16(ap),r5	# mask
	movl	12(ap),r4	# shift count
	clrl	r0
L201:	emul	r0,$0x40000000,(r11),r1
	bicl3	r5,r1,r0
	ashq	r4,r1,r1
	movl	r1,(r11)
	acbl	4(ap),$4,r11,L201
	ret
	.set	.R7,0x800
	.align	1
	.globl	_dsadd1
_dsadd1:
	.word	.R8
	movl	4(ap),r11
	movl	$1,r0
L501:	emul	$1,(r11),r0,r1
	extzv	$0,$30,r1,(r11)
	extv	$30,$32,r1,r0
	acbl	8(ap),$-4,r11,L501
	movl	r1,4(r11)
	ret
	.set	.R8,0x800
/*
	myfrexp (value, exp, hi, lo)
		double value;
		int *exp, *hi, *lo;

	myfrexp returns three values, exp, hi, lo,
	Such that value = 2**exp*tmp, where tmp = (hi*2**-23+lo*2**-53)
	is uniquely determined subect to .5< abs(tmp) <= 1.0
	

	Entry point
*/
	.text
	.globl	_myfrexp
_myfrexp:
	.word	0x0000		# We use r2, but do not save it

	clrl	*12(ap)		# Make for easy exit later
	clrl	*16(ap)		# 
	clrl	*20(ap)		# 
	movd	4(ap),r0	# Fetch "value"
	bneq	L301		# if zero return zero exponent + mantissa
	ret
L301:
	extzv	$7,$8,r0,r2	# r2 := biased exponent
	movab	-129(r2),*12(ap)# subtract bias, store exp
	insv	$154,$7,$8,r0	# lie about exponent to get out
				# high 24 bits easily with emodd.
/*
	This instruction does the following:

		Extend the long floating value in r0 with 0, and
		multiply it by 1.0.  Store the integer part of the result
		in hi, and the fractional part of the result in r0-r1.
*/
	emodd	r0,$0,$0f1.0,*16(ap),r0	# How did you like
					# THAT, sports fans? [jfr's comment]

	tstd	r0		# if zero, exit;
	bneq	L401
	ret
L401:
	insv	$158,$7,$8,r0	# lie about exponent to get out
				# next 30 bits easily with emodd.
				# (2^29 takes 30 bits).
	emodd	r0,$0,$0f1.0,*20(ap),r0	# Get last bits out likewise!
	ret				# (r0 should be zero by now!)
	.globl	_inewint
_inewint:.word	0
	movl	4(ap),r0
	cmpl	r0,$1024
	jgeq	Ialloc
	cmpl	r0,$-1024
	jlss	Ialloc
	moval	_Fixzero[r0],r0
	ret
Ialloc:
	calls	$0,_newint
	movl	4(ap),0(r0)
	ret
	.globl	_blzero
_blzero:				# blzero(where,howmuch)
					# char *where;
					# zeroes a block of length howmuch
					# beginning at where.
	.word	0
	movc5	$0,*4(ap),$0,8(ap),*4(ap)
	ret
