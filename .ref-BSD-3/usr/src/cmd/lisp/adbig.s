
#	bignum add routine
#	basic data representation is each bigit is a positive number
#	less than 2^30, except for the leading bigit, which is in
#	the range -2^30 < x < 2^30.

	.globl	_adbig
	.globl	Bexport
	.globl	backfr
#
#	Initialization section
#
_adbig:	.word	0x0fc0		#save registers 6-11
	movl	4(ap),r1	#arg1 = addr of 1st bignum
	movl	8(ap),r2	#arg2 = addr of 2nd bignum
	clrl	r5		#r5   = carry
	movl	$0xC0000000,r4	#r4   = clear constant.
	movl	sp,r10		#save start address of bignum on stack.
				#note well that this is 4 above the actual
				#low order word.
#
#	first loop is to waltz through both bignums adding
#	bigits, pushing them onto stack. 
#
loop1:	addl3	(r1),(r2),r0	#add bigits
	addl2	r5,r0		#add carry
	bicl3	r4,r0,-(sp)	#save sum, no overflow possible
	extv	$30,$2,r0,r5	#sign extend two high order bits
				#to be next carry.
	movl	4(r1),r1	#get cdr
	bleq	out1		#negative indicates end of list.
	movl	4(r2),r2	#get cdr of second bignum
	bgtr	loop1		#if neither list at end, do it again
#
#	second loop propagates carries through higher order words.
#	It assumes remaining list in r1.
#
loop2:	addl3	(r1),r5,r0	#add bigits and carry
	bicl3	r4,r0,-(sp)	#save sum, no overflow possible
	extv	$30,$2,r0,r5	#sign extend two high order bits
				#to be next carry.
	movl	4(r1),r1	#get cdr
out2:	bgtr	loop2		#negative indicates end of list.
out2a:	pushl	r5
#
#	suppress unnecessary leading zeroes and -1's
#
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
#
#	The following code is an error exit from the first loop
# 	and is out of place to avoid a jump around a jump.
#
out1:	movl	4(r2),r1	#get next addr of list to continue.
	brb	out2		#if second list simult. exhausted, do
				#right thing.
#
#	loop3 is a faster version of loop2 when carries are no
#	longer necessary.
#
loop3a: pushl	(r1)		#get datum
loop3:	movl	4(r1),r1	#get cdr
	bgtr	loop3a		#if not at end get next cell
	brb	out2a

#
#	create linked list representation of bignum
#
copyit:	subl3	r11,r10,r2	#see if we can get away with allocating an int
	bneq	on1		#test for having popped everything
	subl3	$4,r10,r11	#if so, fix up pointer to bottom
	brb	intout		#and allocate int.
on1:	cmpl	r2,$4		#if = 4, then can do
	beql	intout
	calls	$0,_newsdot	#get new cell for new bignum
backfr:	movl	r0,(r6)+	#push address of cell on
				#arg stack to save from garbage collection.
				#There is guaranteed to be slop for a least one
				#push without checking.
	movl	r0,r8		#r8 = result of adbig
loop4:	movl	-(r10),(r0)	#save bigit
	movl	r0,r9		#r9 = old cell, to link
	cmpl	r10,r11		#have we copy'ed all the bigits?
	bleq	done
	calls	$0,_newsdot	#get new cell for new bignum
	movl	r0,4(r9)	#link new cell to old
	brb	loop4
done:	
	clrl	4(r9)		#indicate end of list with 0
	movl	-(r6),r0	#give resultant address.
	ret
#
#	export integer
#
intout: pushl	(r11)
	calls	$1,_inewint
	ret
	.globl	_mulbig
#
#	bignum multiplication routine
#
#	Initialization section
#
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
#
#	Actual multiplication
#
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
