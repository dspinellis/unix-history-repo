/*
 * coact(coexpr,value) - suspend current co-expression and activate
 *  coexpr with value.
 *
 * Outline:
 *    create procedure frame
 *    save sp and boundary in current co-expression stack header
 *    dereference result if it is local to co-expression
 *    change current stack to coexpr
 *    set activator in new co-expression stack header
 *    get sp and boundary from new co-expression stack header
 *    return value in new stack
 */
Global(_boundary)	/* Icon/C boundary */
Global(_current)	/* current co-expression */
Global(_file)		/* current file name */
Global(_line)		/* current line number */
Global(_deref)		/* dereference */
Global(_runerr)		/* runtime error */
Global(_coact)

#ifdef VAX
_coact:
	Mask	STDSV
	calls	$0,_setbound
	subl2	$8,sp		# Make room on stack for line and file
	movl	_line,-4(fp)	# and put them in the frame
	movl	_file,-8(fp)
	movl	_current+4,r2	# r2  <- pointer to current stack header
	movl	sp,16(r2)	# save the stack pointer,
	movl	ap,20(r2)	#  address pointer,
	movl	_boundary,24(r2)#  and boundary for the current co-expression
				#  in its stack header
	moval	8(ap),r4	# point r4 at coexp argument on stack
	pushl	r4		#  and
	calls	$1,_deref	# dereference the co-expression
	cmpl	$D_ESTACK,(r4)+	# see if we indeed have a co-expression
				#  and if we don't, it's runnerr 118,
				#  "co-expression expected"
	jeql	f1
	tstl	-(r4)		# back up to point at bogus co-expression
	pushl	r4		#  and call runerr with the bogon as
	pushl	$118		#  its argument
	calls	$2,_runerr
	
f1:
	movl	(r4)+,r3	# point r3 at the co-expression stack header
	movl	$D_ESTACK,4(r3) # create the descriptor for the activator
	movl	r2,8(r3)	#  (r2 has pointer to previously current
				#   co-expression, which is the activator)
	movl	r3,_current+4	# make the new co-expression current
	movl	16(r3),sp	# get stack pointer,
	movl	20(r3),ap	#  address pointer,
	movl	24(r3),fp	#  and frame pointer/boundary from header
	movl	fp,_boundary
	movl	4(ap),r1	# get nargs in r1
	movaq	8(ap)[r1],r0	# point r0 at target for result on stack,
	movl	r0,r1		#  and save the pointer
	movq	(r4),(r1)	# copy value from old stack to new
	movl	r1,r4		# point r4 at address of result on new stack
	movl	(r4),r1		# get type field of new result
	bitl	$F_NQUAL,r1	# if return value points into the old
	jeql	f11		#   co-expression, then it needs
	bitl	$F_VAR,r1	#   dereferencing
	jeql	f11
	bitl	$F_TVAR,r1
	jneq	f2
	movl	4(r4),r1	# get pointer field of result into r1
	jbr	f3
f2:
	bicl2	$~TYPEMASK,r1	# isolate type bits by turning off others
	cmpl	$T_TVSUBS,r1	# if we have a substring t.v., we have
	jneq	f11		#  to dereference it.
	movl	4(r4),r1	# point r1 at the string of the
	movl	16(r1),r1	#  trapped variable (cmt??)
f3:
	cmpl	r1,16(r2)	# if pointer is between old sp and sbase,
	jlss	f11		#  it needs dereferencing
	cmpl	r1,12(r2)	
	jgtr	f11
	pushl	r4
	calls	$1,_deref	# so, dereference it
f11:
	movl	-4(fp),_line	# restore line number
	movl	-8(fp),_file	#  and file name
	calls	$0,_clrbound
	ret			# return.  This return will use the dummy
				#  frame built above and we should land in
				#  first frame built above
#endif VAX
#ifdef PORT
DummyFcn(_coact)
#endif PORT
#ifdef PDP11
/ coact(coexpr,value) - suspend current co-expression and activate
/ coexpr with value.

/ NOTE:  this code is highly dependent on stack frame layout.

/ Outline:
/    create procedure frame
/    save sp and boundary in current co-expression stack header
/    dereference result if it is local to co-expression
/    change current stack to coexpr
/    set activator in new co-expression stack header
/    get sp and boundary from new co-expression stack header
/    return value in new stack

/ Register usage:
/    r2:  pointer to current co-expression stack header
/    r3:  pointer to new co-expression stack header
/    r4:  pointer to arguments to activate
/    r5:  procedure frame pointer
Global(csv)		/ save registers
Global(cret)            / return as from C
_coact:
	jsr	r5,csv		/ create procedure frame
	mov	_line,(sp)	/ save current line number
	mov	_file,-(sp)	/   and file name
	mov	_current+2,r2	/ r2 <- pointer to current stack header
	mov	sp,8.(r2)	/ save sp
	mov	_boundary,12.(r2)  / save boundary
	mov	r5,r4		/ r4 <- pointer to coexpr
	add	$6,r4
	mov	r4,-(sp)	/ dereference coexpr
	jsr	pc,_deref
        tst     (sp)+
	cmp	$D_ESTACK,(r4)+	/ check type field of coexpr
	beq	1f
	tst	-(r4)
	mov	r4,-(sp)
	mov	$118.,-(sp)	/ runerr 118 - co-expression expected
	jsr	pc,_runerr
1:
	mov	(r4)+,r3	/ r3 <- pointer to new stack header
	mov	$D_ESTACK,2(r3)	/ set activator field of new stack header
	mov	r2,4(r3)
	mov	r3,_current+2	/ make new stack header current
	mov	8.(r3),sp	/ get new sp
	mov	12.(r3),r5	/ get new r5 and
	mov	r5,_boundary	/   new boundary
        mov     4(r5),r0        / r0 <- location of result on new stack
        asl     r0              /    (r0 <- 6 + 4*nargs)
        asl     r0
        add     r5,r0
        add     $6,r0
        mov     r0,r1           / remember address of result on new stack
	mov	(r4)+,(r0)+  	/ copy value from old stack
	mov	(r4)+,(r0)
        mov     r1,r4           / r4 <- address of result on new stack
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
        mov     r4,(sp)         / dereference it
        jsr     pc,_deref
        tst     (sp)+
1:
	mov	-8.(r5),_line	/ restore line number
	mov	-10.(r5),_file	/   and file name
	jmp     cret	        / return in new stack
#endif PDP11
