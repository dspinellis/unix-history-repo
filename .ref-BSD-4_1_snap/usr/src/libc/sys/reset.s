# @(#)reset.s	4.1 (Berkeley) 12/21/80
# C library -- reset, setexit

#	reset(x)
# will generate a "return" from
# the last call to
#	setexit()
# by restoring r6 - r12, ap, fp
# and doing a return.
# The returned value is x; on the original
# call the returned value is 0.
#
# useful for going back to the main loop
# after a horrible error in a lowlevel
# routine.

.globl	_setexit
.globl	_reset

	.align	1
_setexit:
	.word	0x0000
	movab	setsav,r0
	movq	r6,(r0)+
	movq	r8,(r0)+
	movq	r10,(r0)+
	movq	8(fp),(r0)+		# ap, fp
	movab	4(ap),(r0)+		# sp
	movl	16(fp),(r0)		# pc
	clrl	r0
	ret

	.align	1
_reset:
	.word	0x0000
	movl	4(ap),r0	# returned value
	movab	setsav,r1
	movq	(r1)+,r6
	movq	(r1)+,r8
	movq	(r1)+,r10
	movq	(r1)+,r12
	movl	(r1)+,sp
	jmp 	*(r1)

	.data
setsav:	.space	10*4
