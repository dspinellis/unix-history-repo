# C library -- setjmp, longjmp

#	longjmp(a,v)
# will generate a "return(v)" from
# the last call to
#	setjmp(a)
# by restoring r6-pc from 'a'
# and doing a return.
#

.globl	_setjmp
.globl	_longjmp

	.align	1
_setjmp:
	.word	0x0000
	movl	4(ap),r0
	movq	r6,(r0)+
	movq	r8,(r0)+
	movq	r10,(r0)+
	movq	8(fp),(r0)+		# ap, fp
	movab	8(ap),(r0)+		# sp
	movl	16(fp),(r0)		# pc
	clrl	r0
	ret

	.align	1
_longjmp:
	.word	0x0000
	movl	8(ap),r0  #  return(v)
	movl	4(ap),r1
	movq	(r1)+,r6
	movq	(r1)+,r8
	movq	(r1)+,r10
	movq	(r1)+,r12
	movl	(r1)+,sp
	tstl	r0
	bneq	L1
	movzbl	$1,r0
L1:
	jmp	*(r1)
