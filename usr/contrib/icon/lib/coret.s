/*
 * coret(coexpr,value) - suspend current co-expression and activate
 *  activator with value, without changing activator's activator.
 *
 * Outline:
 *    create procedure frame
 *    save sp and boundary in current co-expression stack header
 *    change current stack to coexpr
 *    get sp and boundary from new co-expression stack header
 *    return value in new stack
 */
Global(_boundary)	/* Icon/C boundary */
Global(_current)	/* current co-expression */
Global(_file)		/* current file name */
Global(_line)		/* current line number */
Global(_deref)		/* dereference */

Global(_coret)
#ifdef VAX
_coret:
	Mask	STDSV
	calls	$0,_setbound
	subl2	$8,sp		# Make room on stack for line and file
	movl	_line,-4(fp)	# and put them in the frame
	movl	_file,-8(fp)
	movl	_current+4,r2	# r2  <- pointer to current stack header
	movl	sp,16(r2)	# save the stack pointer,
	movl	ap,20(r2)	#  address pointer,
	movl	_boundary,24(r2) #  and boundary for the current co-expression
				#  in its stack header
	movl	ap,r4		# save ap for later use (to get the
				#  result that we were passed
	movl	8(r2),r3	# r3 points to activator
	movl	r3,_current+4	# make new stack header current
	movl	16(r3),sp	# get new sp,
	movl	20(r3),ap	#  ap,
	movl	24(r3),fp	#  fp,
	movl	fp,_boundary	#  and boundary
	movq	8(r4),16(ap)	# copy arg0 of caller to our arg0, apparently
				#  because we have two fake arguments (?)
	moval	16(ap),r4	# point r4 at our new result

	movl	(r4),r1		# get type field of new result
	bitl	$F_NQUAL,r1	# if return value points into the old
	jeql	f1		#   co-expression, then it needs
	bitl	$F_VAR,r1	#   dereferencing
	jeql	f1
	bitl	$F_TVAR,r1
	jneq	f2
	movl	4(r4),r1	# get pointer field of result into r1
	jbr	f3
f2:
	bicl2	$~TYPEMASK,r1	# isolate type bits by turning off others
	cmpl	$T_TVSUBS,r1	# if we have a substring t.v., we have
	jneq	f1		#  to dereference it.
	movl	4(r4),r1	# point r1 at the string of the
	movl	16(r1),r1	#  trapped variable
f3:
	cmpl	r1,16(r2)	# if pointer is between old sp and sbase,
	jlss	f1		#  it needs dereferencing
	cmpl	r1,12(r2)	
	jgtr	f1
	pushl	r4
	calls	$1,_deref	# so, dereference it
f1:
	movl	-4(fp),_line	# restore line number
	movl	-8(fp),_file	#  and file name
	calls	$0,_clrbound
	ret			# return.  This return will use the dummy
				#  frame built above and we should land in
#endif VAX
#ifdef PORT
DummyFcn(_coret)
#endif PORT
#ifdef PDP11
/ coret(coexpr,value) - suspend current co-expression and activate
/ activator with value, without changing activator's activator.

/ NOTE:  this code is highly dependent on stack frame layout.

/ Outline:
/    create procedure frame
/    save sp and boundary in current co-expression stack header
/    change current stack to coexpr
/    get sp and boundary from new co-expression stack header
/    return value in new stack

/ Register usage:
/    r2:  pointer to current co-expression stack header
/    r3:  pointer to new co-expression stack header
/    r4:  pointer to arguments to activate
/    r5:  procedure frame pointer
Global(csv)		/ save registers
Global(cret)            / return as from C

_coret:
	jsr	r5,csv		/ create procedure frame
	mov	_line,(sp)	/ save current line number
	mov	_file,-(sp)	/   and file name
	mov	_current+2,r2	/ r2 <- pointer to current stack header
	mov	sp,8.(r2)	/ save sp
	mov	_boundary,12.(r2)  / save boundary
 	mov	r5,r4		/ r4 <- pointer to top of stack
        mov     4(r2),r3        / r3 <- pointer to activator
 	mov	r3,_current+2	/ make new stack header current
	mov	8.(r3),sp	/ get new sp
	mov	12.(r3),r5	/ get new r5 and
	mov	r5,_boundary	/   new boundary
	mov	6(r4),10.(r5)	/ copy value from old stack
	mov	8.(r4),12.(r5)
        mov     r5,r4           / r4 <- address of result on new stack
        add     $10.,r4
        mov     (r4), r1        / get type field of return value into r1
	bit	$F_NQUAL,r1  	/ if return value points into the old
	beq	1f		/   co-expression, then it needs
	bit	$F_VAR,r1  	/   dereferencing
	beq	1f
	bit	$F_TVAR,r1
	bne	2f
	mov	2(r4),r1	/ get pointer field into r1
	br	3f
2:
	bic	$!TYPEMASK,r1	/ check type code for substring t.v.
	cmp	$T_TVSUBS,r1	/   if not, it doesn't need
	bne	1f		/   dereferencing
	mov	2(r4),r1	/ get pointer field from b_tvsubs
	mov	8.(r1),r1	/   block into r1
3:
	cmp	r1,8.(r2)	/ if pointer is between old
	blo	1f		/   sp and sbase it needs
 	cmp	r1,6.(r2)	/   dereferencing
 	bhi	1f
        mov     r4,-(sp)         / dereference result
        jsr     pc,_deref
        tst     (sp)+
1:
	mov	-8.(r5),_line	/ restore line number
	mov	-10.(r5),_file	/   and file name
	jmp     cret	        / return in new stack
#endif PDP11
